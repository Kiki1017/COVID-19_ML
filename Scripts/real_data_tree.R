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

#---randomForestFunction---#########################################################################################################################################################################

randomForestFunction <- function(name,dd){
  # enable parallel processing
  dd_fun <- eval(parse(text=paste(dd)))  
  mod_formula <- as.formula(paste(name,"~","."))
  fit <- rpart(mod_formula, data = dd_fun, method="anova", #"anova", "poisson", "class" or "exp"
               control=rpart.control(minsplit=2, cp=0.0001))
  fitrf <- randomForest(mod_formula, data = dd_fun, importance = TRUE, na.action = na.omit)
  return(fitrf)
}

#---caretFunction---#########################################################################################################################################################################
# debugging
# name="confirmed_cum_per_million"
# dd="training_ready_sub2"

caretFunction <- function(name,dd, num_cores = 8){
  # enable parallel processing
  print('Number of cores available = ')
  print(getDoParWorkers())
  registerDoParallel(num_cores)
  dd_fun <- eval(parse(text=paste(dd))) 
  mod_formula <- as.formula(paste(name,"~","."))
  
  # set seed for reproducibility
  set.seed(825)
  # implemnting CARET with 10-fold cross-valication
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10,
    allowParallel = TRUE)
  tuneLength.num <- 5
  # Train a stochastic gradient boosting model ('gbm')
  #   grid search
  print("Training gbm...")
  gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                          n.trees = (1:30)*25, 
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
                   na.action = na.pass)
  gbm.mod
  
  # Train a multivariatee adaptive regression spline with a poison
  # earth.pois.mod <- train(confirmed_cum_per_million ~ .,
  #                         data = training_ready_sub2,
  #                         method = "earth",
  #                         glm=list(family=poisson),
  #                         trControl = fitControl,
  #                         na.action = na.exclude)
  # earth.pois.mod
  # gam.mod <- train(confirmed_cum_per_million ~ .,
  #                  data = training_ready_sub2,
  #                  method = "gam",
  #                  trControl = fitControl,
  #                  tuneLength=tuneLength.num,
  #                  na.action = na.exclude)
  # gam.mod
  print("Training party.mod ...")
  party.mod <- train(confirmed_cum_per_million ~ .,
                     data = training_ready_sub2,
                     method = "ctree",
                     trControl = fitControl,
                     tuneLength=tuneLength.num,
                     na.action = na.exclude)
  party.mod
  
  print("Training rf.mod ...")
  rf.mod <- train(confirmed_cum_per_million ~ .,
                  data = training_ready_sub2,
                  method = "rf",
                  trControl = fitControl,
                  tuneLength=tuneLength.num,
                  na.action = na.exclude)
  resamps <- resamples(list(gbm=gbm.mod,
                            rf=rf.mod,
                            party=party.mod))
  
  print('training models finished. selecing best model baesd upon RMSE of training data with cross validation')
  resamps
  ss <- summary(resamps)
  library(lattice)
  
  trellis.par.set(caretTheme())
  # dotplot(resamps, metric = "MAE")
  dotplot(resamps, metric = "RMSE")
  
  # compare models based upon:
  comp_metric = 'RMSE'
  
  # select best model by training Rsquared
  model_performance_df = resamps[['values']] %>%
    select(-Resample) %>%
    select(contains(comp_metric))
  mean_model_perf = colMeans(model_performance_df)
  model_name_long = names(mean_model_perf[which.min(mean_model_perf)])
  model_name = paste0(strsplit(model_name_long, "~")[[1]][1], ".mod")
  best_model_tmp = get(model_name)$finalModel
  
  return(best_model_tmp)
}

#---initialFlags---#########################################################################################################################################################################
# TRUE if you want to evaluate multiple models
caret_flag <- F
# TRUE if you want to scale by population
incidence_flag <- T
# TRUE if you want to do deaths instead of cases
death_flag <- F
incidence_start_point <- 0.3
# if we are doing deaths, we want incidence start point to be about 5.9% of the case one becuase that's the approx mortality rate
if(death_flag==T){incidence_start_point <- incidence_start_point*(5.9/100)}
count_start_point <- 100
nLags <- 7
projectionTime <- 10
NPIflag1 <- "autofill"
NPIflag2 <- "lastNPI"

#---dataSetup---#########################################################################################################################################################################
data_clean <- read.csv("./InputData/ML_features.csv")
data_clean$date <- as.Date(data_clean$date)

# Looking at the data
glimpse(data_clean)
summary(data_clean)

# testing_countries <- c("USA")
testing_countries <- c("GBR")
# testing_countries <- c("BRA")
# testing_countries <- c("ESP")
# testing_countries <- c("ZAF")

# make country lists, these are the ones that we have NPI data collected for

training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","HUB","KOR","USA","SWE","AUT","CHE","DEU","FRA")
# training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","USA","SWE","AUT","CHE","DEU","FRA")
training_countries <- training_countries_all[which(training_countries_all != testing_countries)]

#---trainingTestingDataFrames---#########################################################################################################################################################################
# create training dataframe
for(i in 1:length(training_countries)){
  training_subset <- subset(data_clean,ISO3 %in% training_countries[i])
  if(incidence_flag==T){
    start <- which(training_subset$confirmed_cum_per_million >= incidence_start_point)[1]
  }else{
    start <- which(training_subset$confirmed_cum >= count_start_point)[1]
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
  if(incidence_flag==T){
    start <- which(testing_subset$confirmed_cum_per_million >= incidence_start_point)[1]
  }else{
    start <- which(testing_subset$confirmed_cum >= count_start_point)[1]
  }
  testing_subset_aligned <- testing_subset[start:nrow(testing_subset),]
  tmp <- testing_subset_aligned[1:projectionTime,]
  tmp[,grep("cum", colnames(tmp))] <- NA
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

peek_at_NPIs_training1 <- training_ready[,c(c("date","time","Country.x","ISO3","confirmed"),names(training_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border",names(training_ready))])]
NPInames <- names(training_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border",names(training_ready))]
if(NPIflag1 == "autofill"){
  for(i in 1:nrow(training_ready)){
    for(j in NPInames){
      if(is.na(training_ready[[j]][i])){training_ready[[j]][i] <- training_ready[[j]][i-1]}
    }
  }
}
peek_at_NPIs_training2 <- training_ready[,c(c("date","time","Country.x","ISO3","confirmed"),names(training_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border",names(training_ready))])]

peek_at_NPIs_testing1 <- testing_ready[,c(c("date","time","Country.x","ISO3","confirmed"),names(testing_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border",names(testing_ready))])]
NPInames <- names(testing_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border",names(testing_ready))]
if(NPIflag1 == "autofill"){
  for(i in 1:(nrow(testing_ready)-projectionTime)){
    for(j in NPInames){
      if(is.na(testing_ready[[j]][i])){testing_ready[[j]][i] <- testing_ready[[j]][i-1]}
    }
  }
}
peek_at_NPIs_testing2 <- testing_ready[,c(c("date","time","Country.x","ISO3","confirmed"),names(testing_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border",names(testing_ready))])]


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
  theme(legend.text=element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#---tree_Models---#########################################################################################################################################################################
str(training_ready)
str(testing_ready)

# Create a Random Forest model with default parameters
if(death_flag==F){
  training_ready_sub2 <- subset(training_ready, select=-c(date,Country.x,Country.y,ISO3,confirmed,death,Source,FullName,recovered))
  training_ready_sub2 <- training_ready_sub2[,grep("death", colnames(training_ready_sub2),invert = T)]
  training_ready_sub2 <- training_ready_sub2[,grep("MalePercent", colnames(training_ready_sub2),invert = T)]  
  training_ready_sub2 <- training_ready_sub2[,grep("FemalePercent", colnames(training_ready_sub2),invert = T)]
  for(i in (nLags+1):100){
    training_ready_sub2 <- training_ready_sub2[,grep(paste0(sprintf("lag_%02d", i)), colnames(training_ready_sub2),invert = T)]
  }
  training_ready_sub2 <- subset(training_ready_sub2, select=-c(Percent_house_Multi_generation,Percent_house_Three_generation,Percent_house_Skip_generation,Num_Tests_cum))
  training_ready_sub2 %<>% mutate_if(is.factor,as.character)  
  training_ready_sub2 %<>% mutate_if(is.character,as.numeric)
  # remove more columns we don't want in the model
  testing_ready_sub2 <- subset(testing_ready, select=-c(date,Country.x,Country.y,ISO3,confirmed,death,Source,FullName,recovered))
  testing_ready_sub2 <- testing_ready_sub2[,grep("death", colnames(testing_ready_sub2),invert = T)]
  testing_ready_sub2 <- testing_ready_sub2[,grep("MalePercent", colnames(testing_ready_sub2),invert = T)]  
  testing_ready_sub2 <- testing_ready_sub2[,grep("FemalePercent", colnames(testing_ready_sub2),invert = T)]
  for(i in (nLags+1):100){
    testing_ready_sub2 <- testing_ready_sub2[,grep(paste0(sprintf("lag_%02d", i)), colnames(testing_ready_sub2),invert = T)]
  }
  testing_ready_sub2 <- subset(testing_ready_sub2, select=-c(Percent_house_Multi_generation,Percent_house_Three_generation,Percent_house_Skip_generation,Num_Tests_cum))
  testing_ready_sub2 %<>% mutate_if(is.factor,as.character)  
  testing_ready_sub2 %<>% mutate_if(is.character,as.numeric)
  # na.omit returns the object with incomplete cases removed. na.pass returns the object unchanged.
  if(incidence_flag==T){
    training_ready_sub2 <- subset(training_ready_sub2, select=-c(confirmed_cum))
    testing_ready_sub2 <- subset(testing_ready_sub2, select=-c(confirmed_cum))
    if(caret_flag == T){
      best_model <- caretFunction(name="confirmed_cum_per_million",dd="training_ready_sub2")
    }
    else{
      best_model <- randomForestFunction(name="confirmed_cum_per_million",dd="training_ready_sub2")
      # fit <- rpart(confirmed_cum_per_million ~ ., data = training_ready_sub2, method="anova", #"anova", "poisson", "class" or "exp"
      #              control=rpart.control(minsplit=2, cp=0.0001))
      # fitrf <- randomForest(confirmed_cum_per_million ~ ., data = training_ready_sub2, importance = TRUE, na.action = na.omit)
    }
  }else{
    # training_ready_sub2 <- subset(training_ready_sub2, select=-c(confirmed_cum_per_million))
    # testing_ready_sub2 <- subset(testing_ready_sub2, select=-c(confirmed_cum_per_million))
    if(caret_flag == T){
      best_model <- caretFunction(name="confirmed_cum",dd="training_ready_sub2")
    }
    else{
      best_model <- randomForestFunction(name="confirmed_cum",dd="training_ready_sub2")
      # fit <- rpart(confirmed_cum ~ ., data = training_ready_sub2, method="anova", #"anova", "poisson", "class" or "exp"
      # control=rpart.control(minsplit=2, cp=0.0001))
      # fitrf <- randomForest(confirmed_cum ~ ., data = training_ready_sub2, importance = TRUE, na.action = na.omit)
    }
  }
}else if(death_flag==T){
  training_ready_sub2 <- subset(training_ready, select=-c(date,Country.x,Country.y,ISO3,confirmed,death,Source,FullName,recovered))
  training_ready_sub2 <- training_ready_sub2[,grep("confirmed", colnames(training_ready_sub2),invert = T)]
  training_ready_sub2 <- training_ready_sub2[,grep("MalePercent", colnames(training_ready_sub2),invert = T)]  
  training_ready_sub2 <- training_ready_sub2[,grep("FemalePercent", colnames(training_ready_sub2),invert = T)]
  for(i in (nLags+1):100){
    training_ready_sub2 <- training_ready_sub2[,grep(paste0(sprintf("lag_%02d", i)), colnames(training_ready_sub2),invert = T)]
  }
  training_ready_sub2 <- subset(training_ready_sub2, select=-c(Percent_house_Multi_generation,Percent_house_Three_generation,Percent_house_Skip_generation,Num_Tests_cum))
  training_ready_sub2 %<>% mutate_if(is.factor,as.character)  
  training_ready_sub2 %<>% mutate_if(is.character,as.numeric)
  # remove more columns we don't want in the model
  testing_ready_sub2 <- subset(testing_ready, select=-c(date,Country.x,Country.y,ISO3,confirmed,death,Source,FullName,recovered))
  testing_ready_sub2 <- testing_ready_sub2[,grep("confirmed", colnames(testing_ready_sub2),invert = T)]
  testing_ready_sub2 <- testing_ready_sub2[,grep("MalePercent", colnames(testing_ready_sub2),invert = T)]  
  testing_ready_sub2 <- testing_ready_sub2[,grep("FemalePercent", colnames(testing_ready_sub2),invert = T)]
  for(i in (nLags+1):100){
    testing_ready_sub2 <- testing_ready_sub2[,grep(paste0(sprintf("lag_%02d", i)), colnames(testing_ready_sub2),invert = T)]
  }
  testing_ready_sub2 <- subset(testing_ready_sub2, select=-c(Percent_house_Multi_generation,Percent_house_Three_generation,Percent_house_Skip_generation,Num_Tests_cum))
  testing_ready_sub2 %<>% mutate_if(is.factor,as.character)  
  testing_ready_sub2 %<>% mutate_if(is.character,as.numeric)
  # na.omit returns the object with incomplete cases removed. na.pass returns the object unchanged.
  if(incidence_flag==T){
    training_ready_sub2 <- subset(training_ready_sub2, select=-c(death_cum))
    testing_ready_sub2 <- subset(testing_ready_sub2, select=-c(death_cum))
    if(caret_flag == T){
      best_model <- caretFunction(name="death_cum_per_million",dd="training_ready_sub2")
    }
    else{
      # best_model <- randomForestFunction(name="death_cum_per_million",dd="training_ready_sub2")
      # fit <- rpart(death_cum_per_million ~ ., data = training_ready_sub2, method="anova", #"anova", "poisson", "class" or "exp"
      #              control=rpart.control(minsplit=2, cp=0.0001))
      # fitrf <- randomForest(death_cum_per_million ~ ., data = training_ready_sub2, importance = TRUE, na.action = na.omit)
    }
  }else{
    # training_ready_sub2 <- subset(training_ready_sub2, select=-c(death_cum_per_million))
    # testing_ready_sub2 <- subset(testing_ready_sub2, select=-c(death_cum_per_million))
    if(caret_flag == T){
      best_model <- caretFunction(name="death_cum",dd="training_ready_sub2")
    }
    else{
      best_model <- randomForestFunction(name="death_cum",dd="training_ready_sub2")
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
NPInames <- names(testing_ready_pred)[grep("Social_Distancing|Quaranting_Cases|Close_Border",names(testing_ready_pred))]
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
p1 <- predict(best_model, testing_ready_pred[1:(breaker-1),], na.action = na.pass)
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
    testing_ready_pred[i,c(paste0("confirmed_cum_per_million"))] <- predict(best_model, testing_ready_pred[i,], na.action = na.pass)
  }else if(incidence_flag==T && death_flag==T){
    testing_ready_pred[i,c(paste0("death_cum_per_million"))] <- predict(best_model, testing_ready_pred[i,], na.action = na.pass)
  }else if(incidence_flag==F && death_flag==F){
    testing_ready_pred[i,c(paste0("confirmed_cum"))] <- predict(best_model, testing_ready_pred[i,], na.action = na.pass)
  }else if(incidence_flag==F && death_flag==T){
    testing_ready_pred[i,c(paste0("death_cum"))] <- predict(best_model, testing_ready_pred[i,], na.action = na.pass)
  }
  # testing_ready_pred[(breaker-5):(i),grep("confirmed_cum_per_million", colnames(testing_ready_pred))]
  if(i==breaker){
    pN <- predict(best_model, testing_ready_pred[i,], na.action = na.pass)
    pAll <- c(p1,pN)
  }else{
    pN <- predict(best_model, testing_ready_pred[i,], na.action = na.pass)
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
  geom_line(data=subset(m1, variable == "prediction"), aes(x = time, y = value, group = country, color = country), size=0.95, colour = 'limegreen', linetype = "3313",alpha=.7)
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
df <- data.frame(imp = best_model[["importanceSD"]])
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
gl <- list(plot1,plot_predict,plot_varimp)
grid.arrange(grobs = gl, top = textGrob(paste0(testing_ready$FullName[1]), gp=gpar(fontsize=15)), layout_matrix = rbind(c(1,1,1,1,2,2,2,2),
                                                                                                                        c(1,1,1,1,2,2,2,2),
                                                                                                                        c(1,1,1,1,2,2,2,2),
                                                                                                                        c(3,3,3,3,3,3,3,3),
                                                                                                                        c(3,3,3,3,3,3,3,3),
                                                                                                                        c(3,3,3,3,3,3,3,3)))

