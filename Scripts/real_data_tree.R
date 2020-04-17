library(tidyverse)
library(lme4)
library(scales)
library(tidyverse)
library(readxl)
library(reshape2)
library(rpart)
library(ggplot2)
library(rpart.plot)
library(randomForest)
library(randomcoloR)
#
library(gridExtra)
library(grid)
library(lattice)
library(ggpubr)
#
library(caret)
library(doParallel)
library(magrittr)
library(dplyr)
library(gbm)
# library(party)
library(lattice)
# library(earth)

#---randomForestFunction---#########################################################################################################################################################################

randomForestFunction <- function(name="confirmed_cum_per_million",dd=training_ready_sub2){
  # enable parallel processing
  # dd_fun <- eval(parse(text=paste(dd)))  
  mod_formula <- as.formula(paste(name,"~","."))
  # mod_formula <- as.formula(paste(name,"~","."," -time"))
  as.formula(paste(name,"~","."," -time"))
  fit <- rpart(mod_formula, data = dd, method="anova", #"anova", "poisson", "class" or "exp"
               control=rpart.control(minsplit=2, cp=0.0001))
  fitrf <- randomForest(mod_formula, data = dd, importance = TRUE, na.action = na.omit)
  return(fitrf)
}

#---caretFunction---#########################################################################################################################################################################
# debugging
name="confirmed_cum_per_million"
dd="training_ready_sub2"

caretFunction <- function(name="confirmed_cum_per_million",dd=training_ready_sub2, num_cores = detectCores(), nasaction = na.omit, n_trees = (1:30)*25, gbm_flag=T, bayesglm_flag=F, gam_flag=F, glm_flag=F, rf_flag=T, all_flag=F){
  # enable parallel processing
  print('Number of cores being used = ')
  print(paste0(num_cores, ", of possible ", detectCores()," cores"))
  registerDoParallel(num_cores)
  # dd_fun <- eval(parse(text=paste(dd))) 
  dd_fun <- dd
  mod_formula <- as.formula(paste(name,"~","."))
  
  # set seed for reproducibility
  set.seed(825)
  # implemnting CARET with 10-fold cross-valication
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    # number = 2,
    ## repeated ten times
    repeats = 10,
    # repeats = 2,
    allowParallel = TRUE)
  tuneLength.num <- 5
  # Train a stochastic gradient boosting model ('gbm')
  #   grid search
  if(gbm_flag==T | all_flag==T){
    gbm_flag=T
    print("Training gbm.mod...")
    gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                            n.trees = n_trees, 
                            shrinkage = c(0.1, 0.01),
                            n.minobsinnode = c(20))
    # nrow(gbmGrid)
    gbm.mod <- train(mod_formula,
                     data = training_ready_sub2, 
                     method = "gbm", 
                     trControl = fitControl, 
                     verbose = FALSE, 
                     ## Now specify the exact models 
                     ## to evaluate:
                     tuneGrid = gbmGrid,
                     na.action = nasaction)
    gbm.mod
  }else{gbm.mod <- NA}

  if(bayesglm_flag==T | all_flag==T){
    bayesglm_flag=T
    print("Training bayesglm.mod...")
    # Train a multivariatee adaptive regression spline with a poison
    bayesglm.mod <- train(mod_formula,
                            data = training_ready_sub2,
                            method = "bayesglm",
                            trControl = fitControl,
                            na.action = nasaction)
    bayesglm.mod
  }else{bayesglm.mod <- NA}
  
  if(gam_flag==T | all_flag==T){
    gam_flag=T
    print("Training gam.mod...")
    gam.mod <- train(mod_formula,
                     data = training_ready_sub2,
                     method = "gam",
                     trControl = fitControl,
                     tuneLength=tuneLength.num,
                     na.action = nasaction)
    gam.mod
  }else{gam.mod <- NA}
  
  if(glm_flag==T | all_flag==T){
    glm_flag=T
    print("Training glm.mod ...")
    glm.mod <- train(mod_formula,
                       data = training_ready_sub2,
                       method = "glm",
                       trControl = fitControl,
                       tuneLength=tuneLength.num,
                       na.action = nasaction)
    glm.mod
  }else{glm.mod <- NA}
  
  if(rf_flag==T | all_flag==T){
    rf_flag=T
    print("Training rf.mod ...")
    rf.mod <- train(mod_formula,
                    data = training_ready_sub2,
                    method = "rf",
                    trControl = fitControl,
                    tuneLength=tuneLength.num,
                    na.action = nasaction)
    rf.mod
  }else{rf.mod <- NA}
  
  flag_list <- c(gbm_flag, bayesglm_flag, gam_flag, glm_flag, rf_flag)
  if(sum(flag_list)>1){
    tmp_list <- list(gbm=gbm.mod,bayesglm=bayesglm.mod,gam=gam.mod,glm=glm.mod,rf=rf.mod)[flag_list]
    resamps <- resamples(tmp_list)
    
    print('training models finished. selecing best model baesd upon RMSE of training data with cross validation')
    resamps
    ss <- summary(resamps)
    
    trellis.par.set(caretTheme())
    # dotplot(resamps, metric = "MAE")
    x = dotplot(resamps, metric = "RMSE")
    print(x)
    
    # compare models based upon:
    comp_metric = 'RMSE'
    
    # select best model by training Rsquared
    model_performance_df = resamps[['values']] %>%
      select(-Resample) %>%
      select(contains(comp_metric))
    mean_model_perf = colMeans(model_performance_df)
    model_name_long = names(mean_model_perf[which.min(mean_model_perf)])
    model_name_tmp2 = paste0(strsplit(model_name_long, "~")[[1]][1], ".mod")
    best_model_tmp2 = get(model_name_tmp2)$finalModel
  }else{
    model_name_tmp2 = paste0(c("gbm.mod","bayesglm.mod","gam.mod","glm.mod","rf.mod")[flag_list])
    best_model_tmp2 = get(model_name_tmp2)$finalModel
  }
  
  return(list(model_name_tmp = model_name_tmp2, best_model_tmp = best_model_tmp2))
}

#---predictFunction---#########################################################################################################################################################################

predictFunction <- function(name=best_model, mod_name=model_name, dd=testing_ready_pred, n_trees = (1:30)*25, nasaction = na.omit){
  # enable parallel processing
  # predict_df <- eval(parse(text=paste(dd)))
  if("gbm.mod" == mod_name){
    dd %<>% mutate_if(is.factor,as.character)  
    dd %<>% mutate_if(is.character,as.numeric)
    predict_tmp1 <- predict.gbm(name, dd, na.action = nasaction, n.trees = n_trees)
    predict_tmp <- rowMeans(predict_tmp1)
  }else if("bayesglm.mod" == mod_name){
    dd %<>% mutate_if(is.factor,as.character)  
    dd %<>% mutate_if(is.character,as.numeric)
    dd %<>% mutate_if(is.integer,as.numeric)
    predict_tmp <- predict(name, dd, na.action = nasaction, n.trees = n_trees)
  }else if("gam.mod" == mod_name){
    dd %<>% mutate_if(is.factor,as.character)  
    dd %<>% mutate_if(is.character,as.numeric)
    predict_tmp <- predict.gam(name, dd, na.action = nasaction)
  }else if("glm.mod" == mod_name){
    dd %<>% mutate_if(is.factor,as.character)  
    dd %<>% mutate_if(is.character,as.numeric)
    dd %<>% mutate_if(is.integer,as.numeric)
    predict_tmp <- predict(name, dd, na.action = nasaction, n.trees = n_trees)
  }else if("rf.mod" == mod_name){
    dd %<>% mutate_if(is.factor,as.character)  
    dd %<>% mutate_if(is.character,as.numeric)
    predict_tmp <- predict(name, dd, na.action = nasaction)
  }
  return(predict_tmp)
}

# p1 <- predictFunction(name=model_name,dd=testing_ready_pred[1:(breaker-1),], n_trees = number_trees, nasaction = na.pass)

#---initialFlags---#########################################################################################################################################################################
# TRUE if you want to evaluate multiple models
caret_flag <- T
number_trees <- (1:30)*25
gbm_flag=F
bayesglm_flag=F
gam_flag=F
glm_flag=F
rf_flag=T
all_flag=F

# TRUE if you want to scale by population
incidence_flag <- T
# TRUE if you want to do deaths instead of cases
death_flag <- F
incidence_start_point <- 0.3
count_start_point <- 100
# if we are doing deaths, we want incidence start point to be about 5.9% of the case one becuase that's the approx mortality rate
if(death_flag==T){
  incidence_start_point <- incidence_start_point*(5.9/100)
  count_start_point <- count_start_point*(5.9/100)
}

nLags <- 10
projectionTime <- 14
NPIflag1 <- "autofill"
NPIflag2 <- "lastNPI"

#---dataSetup---#########################################################################################################################################################################
data_clean <- read.csv("./InputData/ML_features.csv")
data_clean$date <- as.Date(data_clean$date)

# Looking at the data
glimpse(data_clean)
summary(data_clean)

testing_countries <- c("USA")
# testing_countries <- c("ITA")
# testing_countries <- c("BRA")
# testing_countries <- c("ESP")
# testing_countries <- c("GBR")

# make country lists, these are the ones that we have NPI data collected for
# https://docs.google.com/spreadsheets/d/1vrKvs52OAxuB7x2kT9r1q6IcIBxGEQsNRHsK_o7h3jo/edit#gid=378237553
# training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","HUB","KOR","USA","SWE","AUT","CHE","DEU","FRA")
training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","HUB","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","TUR","BEL","ANT","PRT","ISR","RUS","NOR","IRL","AUS","IND","DNK","CHL","CZE","JPN","UKR","MAR","ARG","SGP","ROU")
# training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","USA","SWE","AUT","CHE","DEU","FRA")
training_countries <- training_countries_all[which(training_countries_all != testing_countries)]

#---trainingTestingDataFrames---#########################################################################################################################################################################
# create training dataframe
for(i in 1:length(training_countries)){
  training_subset <- subset(data_clean,ISO3 %in% training_countries[i])
  if(incidence_flag==T & death_flag == F){
    start <- which(training_subset$confirmed_cum_per_million >= incidence_start_point)[1]
  }else if(incidence_flag==T & death_flag == T){
      start <- which(training_subset$death_cum_per_million >= incidence_start_point)[1]
  }else if(incidence_flag==F & death_flag == F){
    start <- which(training_subset$confirmed_cum >= count_start_point)[1]
  }else if(incidence_flag==F & death_flag == T){
    start <- which(training_subset$death_cum >= count_start_point)[1]
  }
  
  training_subset_aligned <- training_subset[start:nrow(training_subset),]
  training_subset_aligned$time <- c(1:nrow(training_subset_aligned))
  if(i==1){
    training_ready <- training_subset_aligned
  }else{
    training_ready <- as.data.frame(rbind(training_ready,training_subset_aligned))
  }
}
# create testing dataframe
for(i in 1:length(testing_countries)){
  testing_subset <- subset(data_clean,ISO3 %in% testing_countries[i])
  if(incidence_flag==T & death_flag == F){
    start <- which(testing_subset$confirmed_cum_per_million >= incidence_start_point)[1]
  }else if(incidence_flag==T & death_flag == T){
    start <- which(testing_subset$death_cum_per_million >= incidence_start_point)[1]
  }else if(incidence_flag==F & death_flag == F){
    start <- which(testing_subset$confirmed_cum >= count_start_point)[1]
  }else if(incidence_flag==F & death_flag == T){
    start <- which(testing_subset$death_cum >= count_start_point)[1]
  }
  testing_subset_aligned <- testing_subset[start:nrow(testing_subset),]
  tmp <- testing_subset_aligned[1:projectionTime,]
  tmp[,grep("cum|Social_Distancing|Quaranting_Cases|Close_Border|Google|date|confirmed", colnames(tmp))] <- NA
  tmp$Social_Distancing <- NA
  tmp$Quaranting_Cases <- NA
  tmp$Close_Border <- NA
  testing_subset_aligned_predictNA <- rbind(testing_subset_aligned,tmp)
  testing_subset_aligned_predictNA$time <- c(1:nrow(testing_subset_aligned_predictNA))
  if(i==1){
    testing_ready <- testing_subset_aligned_predictNA
  }else{
    testing_ready <- as.data.frame(rbind(testing_ready,testing_subset_aligned_predictNA))
  }
}

#---NPIflag1---#########################################################################################################################################################################
# Here we write a little loop that takes care of the fact that the Johns hopkins count data is 
# Updated much more frequently than the NPI data.  So 
# setting the NPIflag1 to "autofill" is our method of saying that we want to fill all the NAs in the time period with the last empirical time points' NPI values
# We will worry about the NPIflag2 later to specify if we want to fill the projection timeperiod the same way
# NPIflag1 <- "autofill"

peek_at_NPIs_training1 <- training_ready[,c(c("date","time","Country.x","ISO3","confirmed"),names(training_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google",names(training_ready))])]
NPInames <- names(training_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google",names(training_ready))]
# View(training_ready[,c(NPInames)])
counter <- 1
prevcountry <- training_ready$Country.x[1]
if(NPIflag1 == "autofill"){
  for(i in 2:nrow(training_ready)){
    curcountry <- training_ready$Country.x[i]
    if(curcountry == prevcountry){
      counter <- counter+1
    }else{
      counter <- 1
    }

    for(j in NPInames){
      if(is.na(training_ready[[j]][i]) && counter > 14){
        training_ready[[j]][i] <- training_ready[[j]][i-1]
        }
    }
    prevcountry <- curcountry
  }
}
peek_at_NPIs_training2 <- training_ready[,c(c("date","time","Country.x","ISO3","confirmed"),names(training_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google",names(training_ready))])]


peek_at_NPIs_testing1 <- testing_ready[,c(c("date","time","Country.x","ISO3","confirmed"),names(testing_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google",names(testing_ready))])]
NPInames <- names(testing_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google",names(testing_ready))]
counter <- 1
prevcountry <- testing_ready$Country.x[1]
if(NPIflag1 == "autofill"){
  for(i in 2:nrow(testing_ready)){
    curcountry <- testing_ready$Country.x[i]
    if(curcountry == prevcountry){
      counter <- counter+1
    }else{
      counter <- 1
    }
    
    for(j in NPInames){
      if(is.na(testing_ready[[j]][i]) && counter > 14){
        testing_ready[[j]][i] <- testing_ready[[j]][i-1]
      }
    }
    prevcountry <- curcountry
  }
}
peek_at_NPIs_testing2 <- testing_ready[,c(c("date","time","Country.x","ISO3","confirmed"),names(testing_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google",names(testing_ready))])]


#---first plot---#########################################################################################################################################################################
# lineColors <- c("firebrick","darkgoldenrod1", "darkviolet", "limegreen", "dodgerblue")
plot1 <- ggplot() 
if(incidence_flag==T && death_flag==F){
  plot1 <- plot1 +
    geom_line(data=training_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size=0.8,alpha=.7)+
    # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1) +
    geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
    geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
    geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
    labs(x=paste0("Days Since ",incidence_start_point," Cumulative Counts per Million"), y = "Confirmed Cumulative Cases per Million", title="")
}else if(incidence_flag==T && death_flag==T){
  plot1 <- plot1 +
    geom_line(data=training_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size=0.8,alpha=.7)+
    # geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
    geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
    geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
    geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
    labs(x=paste0("Days Since ",incidence_start_point," Cumulative Deaths per Million"), y = "Confirmed Cumulative Deaths per Million", title="")
}else if(incidence_flag==F && death_flag==F){
  plot1 <- plot1 +
    geom_line(data=training_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size=0.8,alpha=.7)+
    # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
    geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
    geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
    geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
    labs(x=paste0("Days Since ",count_start_point," Cumulative Counts"), y = "Confirmed Cumulative Cases", title="")
}else if(incidence_flag==F && death_flag==T){
  plot1 <- plot1 +
    geom_line(data=training_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size=0.8,alpha=.7)+
    # geom_line(data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
    geom_line(data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
    geom_line(data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
    geom_line(data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
    labs(x=paste0("Days Since ",count_start_point," Cumulative Deaths"), y = "Confirmed Cumulative Deaths", title="")
}
plot1 <- plot1 +
  guides(color=guide_legend(title="")) +
  theme(legend.title=element_text(size=14))+
  theme(axis.text.x = element_text(color="black",size = 13, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text(color="black",size = 13, angle = 0),
        axis.title.x = element_text(color="black",size = 13, angle = 0),
        axis.title.y = element_text(color="black",size = 13, angle = 90)
  )+
  # scale_x_continuous(breaks=seq(1, 10, 1))+
  scale_colour_manual(values=randomColor(length(training_countries)), aesthetics = "colour") +
  theme(legend.text=element_text(size=9))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#---tree_Models---#########################################################################################################################################################################
str(training_ready)
str(testing_ready)

# Create a Random Forest model with default parameters
if(death_flag==F){
  training_ready_sub2 <- subset(training_ready, select=-c(date,Country.x,Country.y,ISO3,confirmed,death,Source,FullName,recovered,time))
  training_ready_sub2 <- training_ready_sub2[,grep("death|MalePercent|FemalePercent", colnames(training_ready_sub2),invert = T)]
  for(i in (nLags+1):100){
    training_ready_sub2 <- training_ready_sub2[,grep(paste0(sprintf("lag_%02d", i)), colnames(training_ready_sub2),invert = T)]
  }
  training_ready_sub2 <- subset(training_ready_sub2, select=-c(Percent_house_Multi_generation,Percent_house_Three_generation,Percent_house_Skip_generation,Num_Tests_cum))
  training_ready_sub2 %<>% mutate_if(is.factor,as.character)  
  training_ready_sub2 %<>% mutate_if(is.character,as.numeric)
  training_ready_sub2 %<>% mutate_if(is.integer,as.numeric)
  # remove more columns we don't want in the model
  testing_ready_sub2 <- subset(testing_ready, select=-c(date,Country.x,Country.y,ISO3,confirmed,death,Source,FullName,recovered,time))
  testing_ready_sub2 <- testing_ready_sub2[,grep("death|MalePercent|FemalePercent", colnames(testing_ready_sub2),invert = T)]
  for(i in (nLags+1):100){
    testing_ready_sub2 <- testing_ready_sub2[,grep(paste0(sprintf("lag_%02d", i)), colnames(testing_ready_sub2),invert = T)]
  }
  testing_ready_sub2 <- subset(testing_ready_sub2, select=-c(Percent_house_Multi_generation,Percent_house_Three_generation,Percent_house_Skip_generation,Num_Tests_cum))
  testing_ready_sub2 %<>% mutate_if(is.factor,as.character)  
  testing_ready_sub2 %<>% mutate_if(is.character,as.numeric)
  training_ready_sub2 %<>% mutate_if(is.integer,as.numeric)
  # na.omit returns the object with incomplete cases removed. na.pass returns the object unchanged.
  if(incidence_flag==T){
    training_ready_sub2 <- subset(training_ready_sub2, select=-c(confirmed_cum))
    testing_ready_sub2 <- subset(testing_ready_sub2, select=-c(confirmed_cum))
    if(caret_flag == T){
      caretRun <- caretFunction(name="confirmed_cum_per_million",dd=training_ready_sub2,n_trees = number_trees,gbm_flag=gbm_flag, bayesglm_flag=bayesglm_flag, gam_flag=gam_flag, glm_flag=glm_flag, rf_flag=rf_flag, all_flag=all_flag)
      best_model <- caretRun[["best_model_tmp"]]
      model_name <- caretRun[["model_name_tmp"]]
    }
    else{
      best_model <- randomForestFunction(name="confirmed_cum_per_million",dd=training_ready_sub2)
      # fit <- rpart(confirmed_cum_per_million ~ ., data = training_ready_sub2, method="anova", #"anova", "poisson", "class" or "exp"
      #              control=rpart.control(minsplit=2, cp=0.0001))
      # fitrf <- randomForest(confirmed_cum_per_million ~ ., data = training_ready_sub2, importance = TRUE, na.action = na.omit)
    }
  }else{
    # training_ready_sub2 <- subset(training_ready_sub2, select=-c(confirmed_cum_per_million))
    # testing_ready_sub2 <- subset(testing_ready_sub2, select=-c(confirmed_cum_per_million))
    if(caret_flag == T){
      caretRun <- caretFunction(name="confirmed_cum",dd=training_ready_sub2,n_trees = number_trees,gbm_flag=gbm_flag, bayesglm_flag=bayesglm_flag, gam_flag=gam_flag, glm_flag=glm_flag, rf_flag=rf_flag, all_flag=all_flag)
      best_model <- caretRun[["best_model_tmp"]]
      model_name <- caretRun[["model_name_tmp"]]
    }
    else{
      best_model <- randomForestFunction(name="confirmed_cum",dd=training_ready_sub2)
      # fit <- rpart(confirmed_cum ~ ., data = training_ready_sub2, method="anova", #"anova", "poisson", "class" or "exp"
      # control=rpart.control(minsplit=2, cp=0.0001))
      # fitrf <- randomForest(confirmed_cum ~ ., data = training_ready_sub2, importance = TRUE, na.action = na.omit)
    }
  }
}else if(death_flag==T){
  training_ready_sub2 <- subset(training_ready, select=-c(date,Country.x,Country.y,ISO3,confirmed,death,Source,FullName,recovered,time))
  training_ready_sub2 <- training_ready_sub2[,grep("confirmed|MalePercent|FemalePercent", colnames(training_ready_sub2),invert = T)]
  for(i in (nLags+1):100){
    training_ready_sub2 <- training_ready_sub2[,grep(paste0(sprintf("lag_%02d", i)), colnames(training_ready_sub2),invert = T)]
  }
  training_ready_sub2 <- subset(training_ready_sub2, select=-c(Percent_house_Multi_generation,Percent_house_Three_generation,Percent_house_Skip_generation,Num_Tests_cum))
  training_ready_sub2 %<>% mutate_if(is.factor,as.character)  
  training_ready_sub2 %<>% mutate_if(is.character,as.numeric)
  training_ready_sub2 %<>% mutate_if(is.integer,as.numeric)
  # remove more columns we don't want in the model
  testing_ready_sub2 <- subset(testing_ready, select=-c(date,Country.x,Country.y,ISO3,confirmed,death,Source,FullName,recovered,time))
  testing_ready_sub2 <- testing_ready_sub2[,grep("confirmed|MalePercent|FemalePercent", colnames(testing_ready_sub2),invert = T)]
  for(i in (nLags+1):100){
    testing_ready_sub2 <- testing_ready_sub2[,grep(paste0(sprintf("lag_%02d", i)), colnames(testing_ready_sub2),invert = T)]
  }
  testing_ready_sub2 <- subset(testing_ready_sub2, select=-c(Percent_house_Multi_generation,Percent_house_Three_generation,Percent_house_Skip_generation,Num_Tests_cum))
  testing_ready_sub2 %<>% mutate_if(is.factor,as.character)  
  testing_ready_sub2 %<>% mutate_if(is.character,as.numeric)
  training_ready_sub2 %<>% mutate_if(is.integer,as.numeric)
  # na.omit returns the object with incomplete cases removed. na.pass returns the object unchanged.
  if(incidence_flag==T){
    training_ready_sub2 <- subset(training_ready_sub2, select=-c(death_cum))
    testing_ready_sub2 <- subset(testing_ready_sub2, select=-c(death_cum))
    if(caret_flag == T){
      caretRun <- caretFunction(name="death_cum_per_million",dd=training_ready_sub2,n_trees = number_trees,gbm_flag=gbm_flag, bayesglm_flag=bayesglm_flag, gam_flag=gam_flag, glm_flag=glm_flag, rf_flag=rf_flag, all_flag=all_flag)
      best_model <- caretRun[["best_model_tmp"]]
      model_name <- caretRun[["model_name_tmp"]]
    }
    else{
      best_model <- randomForestFunction(name="death_cum_per_million",dd=training_ready_sub2)
      # fit <- rpart(death_cum_per_million ~ ., data = training_ready_sub2, method="anova", #"anova", "poisson", "class" or "exp"
      #              control=rpart.control(minsplit=2, cp=0.0001))
      # fitrf <- randomForest(death_cum_per_million ~ ., data = training_ready_sub2, importance = TRUE, na.action = na.omit)
    }
  }else{
    # training_ready_sub2 <- subset(training_ready_sub2, select=-c(death_cum_per_million))
    # testing_ready_sub2 <- subset(testing_ready_sub2, select=-c(death_cum_per_million))
    if(caret_flag == T){
      caretRun <- caretFunction(name="death_cum",dd=training_ready_sub2,n_trees = number_trees,gbm_flag=gbm_flag, bayesglm_flag=bayesglm_flag, gam_flag=gam_flag, glm_flag=glm_flag, rf_flag=rf_flag, all_flag=all_flag)
      best_model <- caretRun[["best_model_tmp"]]
      model_name <- caretRun[["model_name_tmp"]]
    }
    else{
      best_model <- randomForestFunction(name="death_cum",dd=training_ready_sub2)
      # fit <- rpart(death_cum ~ ., data = training_ready_sub2, method="anova", #"anova", "poisson", "class" or "exp"
      #              control=rpart.control(minsplit=2, cp=0.0001))
      # fitrf <- randomForest(death_cum ~ ., data = training_ready_sub2, importance = TRUE, na.action = na.omit)
    }
  }
}

#---NPIflag2---#########################################################################################################################################################################
# setting the NPIflag2 to "lastNPI" is our method of saying that we want to fill all the NAs in the forecasting period with the last empirical time points' NPI values
# NPIflag2 <- "lastNPI"
testing_ready_pred <- testing_ready_sub2
breaker <- nrow(testing_ready_pred)-projectionTime+1
# testing_ready_pred[(breaker-1):(breaker+1),grep("confirmed_cum_per_million", colnames(testing_ready_pred))]

# Note this code assumes that there are no NAs present in the NPI data
NPInames <- names(testing_ready_pred)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google",names(testing_ready_pred))]
if(NPIflag2 == "lastNPI"){
  for(i in breaker:nrow(testing_ready_pred)){
    for(j in NPInames){
      if(is.na(testing_ready_pred[[j]][i])){testing_ready_pred[[j]][i] <- testing_ready_pred[[j]][i-1]}
    }
  }
}
# Check before and after if you so desire, for the filling in of NPI data in the forecasting period.
# testing_ready$Social_Distancing
# testing_ready_pred$Social_Distancing

#---makePrediction---#########################################################################################################################################################################
# p1 <- predict(best_model, testing_ready_pred[1:(breaker-1),], na.action = na.pass, n.trees = number_trees)
p1 <- predictFunction(name=best_model, mod_name=model_name,dd=testing_ready_pred[1:(breaker-1),], n_trees = number_trees)
for(i in breaker:nrow(testing_ready_pred)){
  for(l in 1:nLags){
    if(l==1){
      if(incidence_flag==T && death_flag==F){
        testing_ready_pred[i,c(paste0("confirmed_cum_per_million_lag_01"))] <- testing_ready_pred[i-1,c(paste0("confirmed_cum_per_million"))]
      }else if(incidence_flag==T && death_flag==T){
        testing_ready_pred[i,c(paste0("death_cum_per_million_lag_01"))] <- testing_ready_pred[i-1,c(paste0("death_cum_per_million"))]
      }else if(incidence_flag==F && death_flag==F){
        testing_ready_pred[i,c(paste0("confirmed_cum_lag_01"))] <- testing_ready_pred[i-1,c(paste0("confirmed_cum"))]
      }else if(incidence_flag==F && death_flag==T){
        testing_ready_pred[i,c(paste0("death_cum_lag_01"))] <- testing_ready_pred[i-1,c(paste0("death_cum"))]
      }
      if(NPIflag2 != "lastNPI"){
        # Some other forecasting of the NPI data
      }
    }else{
      if(incidence_flag==T && death_flag==F){
        testing_ready_pred[i,c(paste0(sprintf("confirmed_cum_per_million_lag_%02d", l)))] <- testing_ready_pred[i-1,c(paste0(sprintf("confirmed_cum_per_million_lag_%02d", l-1)))]
      }else if(incidence_flag==T && death_flag==T){
        testing_ready_pred[i,c(paste0(sprintf("death_cum_per_million_lag_%02d", l)))] <- testing_ready_pred[i-1,c(paste0(sprintf("death_cum_per_million_lag_%02d", l-1)))]
      }else if(incidence_flag==F && death_flag==F){
        testing_ready_pred[i,c(paste0(sprintf("confirmed_cum_lag_%02d", l)))] <- testing_ready_pred[i-1,c(paste0(sprintf("confirmed_cum_lag_%02d", l-1)))]
      }else if(incidence_flag==F && death_flag==T){
        testing_ready_pred[i,c(paste0(sprintf("death_cum_lag_%02d", l)))] <- testing_ready_pred[i-1,c(paste0(sprintf("death_cum_lag_%02d", l-1)))]
      }
    }
  }
  if(incidence_flag==T && death_flag==F){
    # testing_ready_pred[i,c(paste0("confirmed_cum_per_million"))] <- predict(best_model, testing_ready_pred[i,], na.action = na.pass, n.trees = number_trees)
    testing_ready_pred[i,c(paste0("confirmed_cum_per_million"))] <- 0
    testing_ready_pred[i,c(paste0("confirmed_cum_per_million"))] <- predictFunction(name=best_model, mod_name=model_name,dd=testing_ready_pred[i,], n_trees = number_trees)
  }else if(incidence_flag==T && death_flag==T){
    # testing_ready_pred[i,c(paste0("death_cum_per_million"))] <- predict(best_model, testing_ready_pred[i,], na.action = na.pass, n.trees = number_trees)
    testing_ready_pred[i,c(paste0("death_cum_per_million"))] <- 0
    testing_ready_pred[i,c(paste0("death_cum_per_million"))] <- predictFunction(name=best_model, mod_name=model_name,dd=testing_ready_pred[i,], n_trees = number_trees)
  }else if(incidence_flag==F && death_flag==F){
    # testing_ready_pred[i,c(paste0("confirmed_cum"))] <- predict(best_model, testing_ready_pred[i,], na.action = na.pass, n.trees = number_trees)
    testing_ready_pred[i,c(paste0("confirmed_cum"))] <- 0
    testing_ready_pred[i,c(paste0("confirmed_cum"))] <- predictFunction(name=best_model, mod_name=model_name,dd=testing_ready_pred[i,], n_trees = number_trees)
  }else if(incidence_flag==F && death_flag==T){
    # testing_ready_pred[i,c(paste0("death_cum"))] <- predict(best_model, testing_ready_pred[i,], na.action = na.pass, n.trees = number_trees)
    testing_ready_pred[i,c(paste0("death_cum"))] <- 0
    testing_ready_pred[i,c(paste0("death_cum"))] <- predictFunction(name=best_model, mod_name=model_name,dd=testing_ready_pred[i,], n_trees = number_trees)
  }
  # testing_ready_pred[(breaker-5):(i),grep("confirmed_cum_per_million", colnames(testing_ready_pred))]
  if(i==breaker){
    # pN <- predict(best_model, testing_ready_pred[i,], na.action = na.pass, n.trees = number_trees)
    pN <- predictFunction(name=best_model, mod_name=model_name,dd=testing_ready_pred[i,], n_trees = number_trees)
    pAll <- c(p1,pN)
  }else{
    # pN <- predict(best_model, testing_ready_pred[i,], na.action = na.pass, n.trees = number_trees)
    pN <-  predictFunction(name=best_model, mod_name=model_name,dd=testing_ready_pred[i,], n_trees = number_trees)
    pAll <- c(pAll,pN)
  }
}

# Collect the data to be rady for ggplot
pAll <- as.data.frame(pAll)
pAll$time <- testing_ready_pred$time

if(incidence_flag==T && death_flag==F){
  plot1Data_tmp <- testing_ready[,c("FullName","time","confirmed_cum_per_million")]
}else if(incidence_flag==T && death_flag==T){
  plot1Data_tmp <- testing_ready[,c("FullName","time","death_cum_per_million")]
}else if(incidence_flag==F && death_flag==F){
  plot1Data_tmp <- testing_ready[,c("FullName","time","confirmed_cum")]
}else if(incidence_flag==F && death_flag==T){
  plot1Data_tmp <- testing_ready[,c("FullName","time","death_cum")]
}

# Organize the data to be rady for ggplot
plot1Data <- merge(pAll,plot1Data_tmp,by="time")
colnames(plot1Data) <- c("time","prediction","country","actual")
plot1Data <- plot1Data[order(plot1Data$time),]
plot1Data$prediction <- as.numeric(plot1Data$prediction)
plot1Data$actual <- as.numeric(plot1Data$actual)
plot1Data$country <- as.character(plot1Data$country)
str(plot1Data)

# Reshape the data to be rady for ggplot
m1 <- reshape2::melt(plot1Data,id=c("country","time"))
m1$time <- as.numeric(m1$time)
m1$value <- as.numeric(m1$value)
m1$variable <- as.factor(m1$variable)
m1$country <- as.factor(m1$country)
str(m1)
m1 <- m1[order(m1$time),]

#---plotPrediction---#########################################################################################################################################################################
plot_predict <- ggplot() 
plot_predict <- plot_predict +
  # geom_line(data=subset(m1, variable == "actual"), aes(x = time, y = value, group = country, color = country), size=0.8,alpha=.7)+
  geom_line(data=subset(m1, variable == "actual"), aes(x = time, y = value, group = country, color = country), size = 3, colour = 'red', alpha = 0.1) +
  geom_line(data=subset(m1, variable == "actual"), aes(x = time, y = value, group = country, color = country), size = 2, colour = 'red', alpha = 0.2) +
  geom_line(data=subset(m1, variable == "actual"), aes(x = time, y = value, group = country, color = country), size = 1, colour = 'red', alpha = 0.5) +
  geom_line(data=subset(m1, variable == "prediction"), aes(x = time, y = value, group = country, color = country), lwd=.8, colour = 'deepskyblue3', linetype = "solid",alpha=.7)
# geom_line(data=subset(m1, variable == "prediction"), aes(x = time, y = value, group = country, color = country), size = 3, colour = 'red', alpha = 0.1, linetype = "3313") +
# geom_line(data=subset(m1, variable == "prediction"), aes(x = time, y = value, group = country, color = country), size = 2, colour = 'red', alpha = 0.2, linetype = "3313") +
# geom_line(data=subset(m1, variable == "prediction"), aes(x = time, y = value, group = country, color = country), size = 1, colour = 'red', alpha = 0.5, linetype = "3313") +
if(incidence_flag==T && death_flag==F){
  plot_predict <- plot_predict +
    labs(x=paste0("Days Since ",incidence_start_point," Cumulative Counts per Million"), y = "Confirmed Cumulative Cases per Million", title="")
}else if(incidence_flag==T && death_flag==T){
  plot_predict <- plot_predict +
    labs(x=paste0("Days Since ",incidence_start_point," Cumulative per Million"), y = "Confirmed Cumulative Deaths per Million", title="")
}else if(incidence_flag==F && death_flag==F){
  plot_predict <- plot_predict +
    labs(x=paste0("Days Since ",count_start_point," Cumulative Counts"), y = "Confirmed Cumulative Cases", title="")
}else if(incidence_flag==F && death_flag==T){
  plot_predict <- plot_predict +
    labs(x=paste0("Days Since ",count_start_point," Cumulative Deaths"), y = "Confirmed Cumulative Deaths", title="")
}
plot_predict <- plot_predict +
  guides(color=guide_legend(title="")) +
  theme(legend.title=element_text(size=14))+
  theme(axis.text.x = element_text(color="black",size = 13, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text(color="black",size = 13, angle = 0),
        axis.title.x = element_text(color="black",size = 13, angle = 0),
        axis.title.y = element_text(color="black",size = 13, angle = 90)
  )+
  # scale_x_continuous(breaks=seq(1, 10, 1))+
  # scale_colour_manual(values=c(lineColors))
  theme(legend.text=element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#---variableImportancePlot---#########################################################################################################################################################################

# Plot variable importance
if(caret_flag==T){
  if("gbm.mod" == model_name){
    print("Model chosen by caret is: gbm")
    df_tmp <- varImp(best_model)
    df <- as.data.frame(df_tmp)
    colnames(df) = c('imp')
  }else if("bayesglm.mod" == model_name){
    print("Model chosen by caret is: earth")
    df_tmp <- varImp(best_model)
    df <- as.data.frame(df_tmp)
    colnames(df) = c('imp')
  }else if("gam.mod" == model_name){
    print("Model chosen by caret is: gam")
    df_tmp <- varImp(best_model)
    df <- as.data.frame(df_tmp)
    colnames(df) = c('imp')
  }else if("glm.mod" == model_name){
    print("Model chosen by caret is: glm")
    df_tmp <- varImp(best_model)
    df <- as.data.frame(df_tmp)
    colnames(df) = c('imp')
  }else if("rf.mod"== model_name){
    print("Model chosen by caret is: randomForest")
    df_tmp <- varImp(best_model)
    df <- as.data.frame(df_tmp)
    colnames(df) = c('imp')
  }
}else{
  df <- data.frame(imp = best_model[["importanceSD"]])
}

df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
plot_varimp <- ggplot2::ggplot(df2) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()

#---cumulativePlot---#########################################################################################################################################################################
rmse_val <- sqrt( sum( (plot1Data$prediction[1:(nrow(plot1Data)-projectionTime)] - plot1Data$actual[1:(nrow(plot1Data)-projectionTime)])^2 ) / (nrow(plot1Data)-projectionTime) )
rmse_val <- round(rmse_val,3)
gl <- list(plot1,plot_predict,plot_varimp)
grid.arrange(grobs = gl, top = textGrob(paste0(testing_ready$FullName[1]," ",model_name," RMSE = ",rmse_val), gp=gpar(fontsize=20)), layout_matrix = rbind(c(1,1,1,1,1,3,3,3),
                                                                                                                                                           c(1,1,1,1,1,3,3,3),
                                                                                                                                                           c(1,1,1,1,1,3,3,3),
                                                                                                                                                           c(1,1,1,1,1,3,3,3),
                                                                                                                                                           c(2,2,2,2,2,3,3,3),
                                                                                                                                                           c(2,2,2,2,2,3,3,3),
                                                                                                                                                           c(2,2,2,2,2,3,3,3),
                                                                                                                                                           c(2,2,2,2,2,3,3,3)))
# grid.arrange(grobs = gl, top = textGrob(paste0(testing_ready$FullName[1]," ",model_name," RMSE = ",rmse_val), gp=gpar(fontsize=20)), layout_matrix = rbind(c(1,1,1,1,1,2,2,2),
#                                                                                                                         c(1,1,1,1,1,2,2,2),
#                                                                                                                         c(1,1,1,1,1,2,2,2),
#                                                                                                                         c(3,3,3,3,3,3,3,3),
#                                                                                                                         c(3,3,3,3,3,3,3,3),
#                                                                                                                         c(3,3,3,3,3,3,3,3),
#                                                                                                                         c(3,3,3,3,3,3,3,3),
#                                                                                                                         c(3,3,3,3,3,3,3,3)))


# rpart.plot(best_model)
# 

