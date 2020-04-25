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
library(zoo)
library(EpiEstim)
library(viridis)
library(RColorBrewer)
# devtools::install_github("delabj/ggCyberPunk")
library(ggCyberPunk)
# library(earth)
'%ni%' <- Negate('%in%')

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
# name="confirmed_cum_per_million"
# dd="training_ready_sub2"

caretFunction <- function(name="confirmed_cum_per_million",dd=training_ready_sub2, num_cores = detectCores(), nasaction = na.omit, n_trees = c((1:10)*100,1500,2000), gbm_flag=F, bayesglm_flag=F, gam_flag=F, glm_flag=F, rf_flag=T, all_flag=F){
  # enable parallel processing
  print('Number of cores being used = ')
  print(paste0(num_cores, ", of possible ", detectCores()," cores"))
  registerDoParallel(num_cores)
  # dd_fun <- eval(parse(text=paste(dd))) 
  dd_fun <- dd
  mod_formula <- as.formula(paste(name,"~","."))
  
  set.seed(1)

  # modellist <- list()
  if(rf_flag==T | all_flag==T){
    rf_flag=T
    print("Training rf.mod ...")
    
    x <- dd[complete.cases(dd),which(colnames(dd) %ni% name)]
    y <- dd[complete.cases(dd),which(colnames(dd) %in% name)]
    bestMtry <- tuneRF(x, y, stepFactor = 1.5, improve = 1e-5, ntree = 500, na.action=nasaction)
    bestMtry <- as.data.frame(bestMtry)
    mtry_best <- bestMtry$mtry[which.min(bestMtry$OOBError)]
    
    #train with different ntree parameters
    # for (ntree in n_trees){
    for (ntree in 1000){
      set.seed(123)
      print(" ")
      print(ntree)
      print(n_trees)
      # x <- dd[complete.cases(dd),which(colnames(dd) %ni% name)]
      # y <- dd[complete.cases(dd),which(colnames(dd) %in% name)]
      # bestMtry <- tuneRF(x, y, stepFactor = 1.5, improve = 1e-5, ntree = ntree, na.action=nasaction)
      # bestMtry <- as.data.frame(bestMtry)
      # mtry_best <- bestMtry$mtry[which.min(bestMtry$OOBError)]

      # seq(from=mtry_best*.5, to=mtry_best, length.out = length(n_trees))
      # tunegrid <- expand.grid(mtry=,ntree=n_trees)
      tunegrid <- expand.grid(.mtry = c(mtry_best))
      
      control <- trainControl(method="repeatedcv", 
                              number=10, 
                              repeats=3,
                              allowParallel = TRUE)

      rf.mod <- train(mod_formula,
                   data = dd,
                   method = 'rf',
                   # metric = 'Accuracy',
                   tuneGrid = tunegrid,
                   trControl = control,
                   ntree = ntree,
                   na.action = nasaction)
      # key <- toString(ntree)
      # modellist[[key]] <- rf.mod
    }
    
    # # set seed for reproducibility
    # set.seed(825)
    # rf.mod <- train(mod_formula,
    #                 data = dd_fun,
    #                 method = "rf",
    #                 trControl = control,
    #                 tuneGrid=tunegrid,
    #                 # metric=metric1,
    #                 # tuneLength=tuneLength.num,
    #                 ntree = n_trees,
    #                 na.action = nasaction)
  }else{rf.mod <- NA}
  
  # results <- resamples(modellist)
  # summary(results) #1000 trees looks like the best
  # dotplot(results)
  # stopCluster(cores)
  
  # # implemnting CARET with 10-fold cross-valication
  # fitControl <- trainControl(## 10-fold CV
  #   method = "repeatedcv",
  #   number = 10,
  #   # number = 2,
  #   ## repeated ten times
  #   # repeats = 10,
  #   repeats = 3,
  #   search = "random",
  #   allowParallel = TRUE)
  # tuneLength.num <- 2
  # 
  # 
  # if(rf_flag==T | all_flag==T){
  #   rf_flag=T
  #   print("Training rf.mod ...")
  #   rf.mod <- train(mod_formula,
  #                   data = dd_fun,
  #                   method = "rf",
  #                   trControl = fitControl,
  #                   tuneLength=tuneLength.num,
  #                   na.action = nasaction)
  #   rf.mod
  # }else{rf.mod <- NA}
  
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
                     data = dd_fun, 
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
                            data = dd_fun,
                            method = "bayesglm",
                            trControl = fitControl,
                            na.action = nasaction)
    bayesglm.mod
  }else{bayesglm.mod <- NA}
  
  if(gam_flag==T | all_flag==T){
    gam_flag=T
    print("Training gam.mod...")
    gam.mod <- train(mod_formula,
                     data = dd_fun,
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
                       data = dd_fun,
                       method = "glm",
                       trControl = fitControl,
                       tuneLength=tuneLength.num,
                       na.action = nasaction)
    glm.mod
  }else{glm.mod <- NA}
  
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
    # dd[,which(names(dd) %ni% c("lag_01_cut"))] %<>% mutate_if(is.factor,as.character)  
    # dd[,which(names(dd) %ni% c("lag_01_cut"))] %<>% mutate_if(is.character,as.numeric)
    # predict_tmp1 <- predict.gbm(name, dd, na.action = nasaction, n.trees = n_trees)
    predict_tmp <- rowMeans(predict_tmp1)
  }else if("bayesglm.mod" == mod_name){
    # dd[,which(names(dd) %ni% c("lag_01_cut"))] %<>% mutate_if(is.factor,as.character)  
    # dd[,which(names(dd) %ni% c("lag_01_cut"))] %<>% mutate_if(is.character,as.numeric)
    # dd[,which(names(dd) %ni% c("lag_01_cut"))] %<>% mutate_if(is.integer,as.numeric)
    predict_tmp <- predict(name, dd, na.action = nasaction, n.trees = n_trees)
  }else if("gam.mod" == mod_name){
    # dd[,which(names(dd) %ni% c("lag_01_cut"))] %<>% mutate_if(is.factor,as.character)  
    # dd[,which(names(dd) %ni% c("lag_01_cut"))] %<>% mutate_if(is.character,as.numeric)
    predict_tmp <- predict.gam(name, dd, na.action = nasaction)
  }else if("glm.mod" == mod_name){
    # dd[,which(names(dd) %ni% c("lag_01_cut"))] %<>% mutate_if(is.factor,as.character)  
    # dd[,which(names(dd) %ni% c("lag_01_cut"))] %<>% mutate_if(is.character,as.numeric)
    # dd[,which(names(dd) %ni% c("lag_01_cut"))] %<>% mutate_if(is.integer,as.numeric)
    predict_tmp <- predict(name, dd, na.action = nasaction, n.trees = n_trees)
  }else if("rf.mod" == mod_name){
    # dd[,which(names(dd) %ni% c("lag_01_cut"))] %<>% mutate_if(is.factor,as.character)  
    # dd[,which(names(dd) %ni% c("lag_01_cut"))] %<>% mutate_if(is.character,as.numeric)
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

nLags <- 1
projectionTime <- 14
NPIflag1 <- "autofill"
NPIflag2 <- "lastNPI"
# The Percent of the timeframe you want to reserve for testing a country (the rest of that country's time series is included into the model)
testingTimeFrame <- 0.5
# Columns you don't want to be in the model
listToRemove <- c("date","Country.x","Country.y","ISO3","confirmed","death","Source","FullName","recovered","confirmed_cum_per_million_lag_01")
# listToRemove <- c("date","Country.x","Country.y","ISO3","confirmed","death","Source","FullName","recovered","lag_01_cut")
# listToRemove <- c("date","Country.x","Country.y","ISO3","confirmed","death","Source","FullName","recovered","lag_01_cut","confirmed_cum_per_million_lag_01")
# listToRemove <- c("date","Country.x","Country.y","ISO3","confirmed","death","Source","FullName","recovered")
nCuts <- 1000

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
# testing_countries <- c("BEL")

# make country lists, these are the ones that we have NPI data collected for
# https://docs.google.com/spreadsheets/d/1vrKvs52OAxuB7x2kT9r1q6IcIBxGEQsNRHsK_o7h3jo/edit#gid=378237553
# training_countries_all <- c("ITA","FRA","GBR")
# training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","USA","SWE","AUT","CHE","DEU","FRA")
# training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","PRT","ISR","RUS","NOR","AUS","DNK","CHL","CZE","JPN","UKR","MAR","ARG")
# training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","HUB","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","TUR","BEL","ANT","PRT","ISR","RUS","NOR","IRL","AUS","IND","DNK","CHL","CZE","JPN","UKR","MAR","ARG","SGP","ROU")
training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","TUR","BEL","ANT","PRT","ISR","RUS","NOR","IRL","AUS","IND","DNK","CHL","CZE","JPN","UKR","MAR","ARG","SGP","ROU")

training_countries <- training_countries_all[which(training_countries_all != testing_countries)]

#---Estimating reproduction numbers, R0---#########################################################################################################################################################################
library(tidyverse)
#"SI for Serial Intervals.
# Determination of the serial interval, the time between the start of symptoms in the primary patient (infector) 
# and onset of symptoms in the patient receiving that infection from the infector (the infectee)"
# Table 1 from https://www.medrxiv.org/content/10.1101/2020.04.13.20062760v1
#"we calculate a weighted mean of the published parameters and inferred a serial interval described 
# by a gamma distribution, parameterised with mean SI of 4.56 days (credible interval: 2.54 - 7.36) 
# and standard deviation 4.53 days (credible interval 4.17 - 5.05)."

serialIntervals = tibble(
  mean_si_estimate = c(3.96, 6.3, 4.22, 4.56, 3.95, 5.21, 4.7, 7.5,6.6),
  mean_si_estimate_low_ci = c(3.53, 5.2, 3.43, 2.69,-4.47, -3.35, 3.7, 5.3, 0.7),
  mean_si_estimate_high_ci = c(4.39, 7.6, 5.01, 6.42, 12.51,13.94, 6.0, 19.0, 19.0),
  std_si_estimate = c(4.75,4.2, 0.4, 0.95, 4.24, 4.32, 2.3, 3.4, NA),
  std_si_estimate_low_ci = c(4.46, 3.1, NA, NA, 4.03, 4.06, 1.6, NA, NA),
  std_si_estimate_high_ci = c(5.07, 5.3, NA, NA, 4.95, 5.58, 3.5, NA, NA),
  sample_size = c(468,48,135,93,45,54,28,16,90),
  population = c("China", "Shenzhen","Taijin","Singapore","Taijin","Singapore", "SE Asia", "Wuhan","Italy"),
  source = c(
    "Zhanwei Du et al. Serial Interval of COVID-19 among Publicly Reported Confirmed Cases. Emerging Infectious Disease journal 26, (2020)",
    "Bi, Q. et al. Epidemiology and Transmission of COVID-19 in Shenzhen China: Analysis of 391 cases and 1,286 of their close contacts. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.03.20028423",
    "Tindale, L. et al. Transmission interval estimates suggest pre-symptomatic spread of COVID-19. Epidemiology (2020) doi:10.1101/2020.03.03.20029983",
    "Tindale, L. et al. Transmission interval estimates suggest pre-symptomatic spread of COVID-19. Epidemiology (2020) doi:10.1101/2020.03.03.20029983",
    "Ganyani, T. et al. Estimating the generation interval for COVID-19 based on symptom onset data. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.05.20031815",
    "Ganyani, T. et al. Estimating the generation interval for COVID-19 based on symptom onset data. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.05.20031815",
    "Nishiura, H., Linton, N. M. & Akhmetzhanov, A. R. Serial interval of novel coronavirus (COVID-19) infections. Int. J. Infect. Dis. (2020) doi:10.1016/j.ijid.2020.02.060",
    "Li, Q. et al. Early Transmission Dynamics in Wuhan, China, of Novel Coronavirus-Infected Pneumonia. N. Engl. J. Med. (2020) doi:10.1056/NEJMoa2001316",
    "Cereda, D. et al. The early phase of the COVID-19 outbreak in Lombardy, Italy. arXiv [q-bio.PE] (2020)")
)

unk=function(x) ifelse(is.na(x),"unk",x)

SItable1 = serialIntervals %>% mutate(
  `Mean SI\n(95% CrI) days`=paste0(mean_si_estimate,"\n(",unk(mean_si_estimate_low_ci),"-",
                                   unk(mean_si_estimate_high_ci),")"),
  `Std SI\n(95% CrI) days`=paste0(unk(std_si_estimate),"\n(",unk(std_si_estimate_low_ci),"-",unk(std_si_estimate_high_ci),")")
) %>% select(-contains("estimate")) %>% select(
  `Reference`=source,
  `Mean SI\n(95% CrI) days`,
  `Std SI\n(95% CrI) days`,
  `N`=sample_size,
  `Population`=population
)

wtSIs = serialIntervals %>% summarise(
  mean_si = weighted.mean(mean_si_estimate,sample_size,na.rm = TRUE),
  min_mean_si = weighted.mean(mean_si_estimate_low_ci,sample_size,na.rm = TRUE),
  max_mean_si = weighted.mean(mean_si_estimate_high_ci,sample_size,na.rm = TRUE),
  std_si  = weighted.mean(ifelse(is.na(std_si_estimate_low_ci),NA,1)*std_si_estimate,sample_size,na.rm = TRUE),
  min_std_si  = weighted.mean(std_si_estimate_low_ci,sample_size,na.rm = TRUE),
  max_std_si  = weighted.mean(std_si_estimate_high_ci,sample_size,na.rm = TRUE)
  #total = sum(sample_size)
) %>% mutate(
  std_mean_si = (max_mean_si - min_mean_si) / 3.92, # TODO: fit gamma
  std_std_si = (max_std_si - min_std_si) / 3.92
)

config = make_config(list(si_parametric_distr = "G",
                          mean_si = wtSIs$mean_si, 
                          std_mean_si = wtSIs$std_mean_si,
                          min_mean_si = wtSIs$min_mean_si, 
                          max_mean_si = wtSIs$max_mean_si,
                          std_si = wtSIs$std_si, 
                          std_std_si = wtSIs$std_si,
                          min_std_si = wtSIs$min_std_si, 
                          max_std_si = wtSIs$max_std_si), 
                     method="uncertain_si")

#---trainingTestingDataFrames---#########################################################################################################################################################################
# create training dataframe
pdf("R0_plot.pdf",width = 11,height = 8.5)
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

  toCalcR0 <- training_subset_aligned[,c("date","confirmed")]
  colnames(toCalcR0) <- c("dates","I")
  toCalcR0$I[toCalcR0$I<0] <- NA
  #Get of erroneous negative counts... they sneak throught the API sometimes. 
  # But if thre is a negative at teh end... are the last one lets just make it equal to the n-1 one
  if(is.na(tail(toCalcR0$I,1))){
    toCalcR0$I[length(toCalcR0$I)] <- toCalcR0$I[length(toCalcR0$I)-1]
  }
  # If the NA is not at the end, Lets linearly interpolate them:
  toCalcR0$I <- na.approx(toCalcR0$I)

  res_uncertain_si <- estimate_R(toCalcR0,
                                 method = "uncertain_si",
                                 config = config)
  training_subset_aligned$R0 <- NA 
  training_subset_aligned$R0[head(res_uncertain_si[["R"]]$`t_start`,1):tail(res_uncertain_si[["R"]]$`t_start`,1)] <- res_uncertain_si[["R"]]$`Mean(R)`
  # Autofill beginning R0s with first value
  training_subset_aligned$R0[1:head(res_uncertain_si[["R"]]$`t_start`,1)] <- mean(head(res_uncertain_si[["R"]]$`Mean(R)`,5))
  # Autofill ending R0s with last value
  training_subset_aligned$R0[tail(res_uncertain_si[["R"]]$`t_start`,1):nrow(training_subset_aligned)] <- mean(tail(res_uncertain_si[["R"]]$`Mean(R)`,5))
  listToLag <- c("R0","Google_Retail_recreation","Google_Grocery_pharmacy","Google_Parks","Google_Transit_stations","Google_Workplaces","Google_Residential")
  for(npi in 1:length(listToLag)){
    # Add 3 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi],"_Lag_3")]] <- lag(training_subset_aligned[[paste0(listToLag[npi])]],3)
    training_subset_aligned[[paste0(listToLag[npi],"_Lag_3")]][1:3] <- mean(training_subset_aligned[[paste0(listToLag[npi])]][1:5])
    # Add 7 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi],"_Lag_7")]] <- lag(training_subset_aligned[[paste0(listToLag[npi])]],7)
    training_subset_aligned[[paste0(listToLag[npi],"_Lag_7")]][1:7] <- mean(training_subset_aligned[[paste0(listToLag[npi])]][1:5])
    # Add 14 day lag factor for R0
    training_subset_aligned[[paste0(listToLag[npi],"_Lag_14")]] <- lag(training_subset_aligned[[paste0(listToLag[npi])]],14)
    training_subset_aligned[[paste0(listToLag[npi],"_Lag_14")]][1:14] <- mean(training_subset_aligned[[paste0(listToLag[npi])]][1:5])
  }

  plot.new()
  plot(res_uncertain_si, legend = T)
  mtext(training_countries[i], outer=TRUE,  cex=1, line=-.5)
  
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
  
  toCalcR0 <- testing_subset_aligned[,c("date","confirmed")]
  colnames(toCalcR0) <- c("dates","I")
  toCalcR0$I[toCalcR0$I<0] <- NA
  #Get of erroneous negative counts... they sneak throught the API sometimes. 
  #Get of erroneous negative counts... they sneak throught the API sometimes. 
  # But if thre is a negative at teh end... are the last one lets just make it equal to the n-1 one
  if(is.na(tail(toCalcR0$I,1))){
    toCalcR0$I[length(toCalcR0$I)] <- toCalcR0$I[length(toCalcR0$I)-1]
  }
  # If the NA is not at the end, Lets linearly interpolate them:
  toCalcR0$I <- na.approx(toCalcR0$I)
  res_uncertain_si <- estimate_R(toCalcR0,
                                 method = "uncertain_si",
                                 config = config)
  testing_subset_aligned$R0 <- NA 
  testing_subset_aligned$R0[head(res_uncertain_si[["R"]]$`t_start`,1):tail(res_uncertain_si[["R"]]$`t_start`,1)] <- res_uncertain_si[["R"]]$`Mean(R)`
  # Autofill beginning R0s with first value
  testing_subset_aligned$R0[1:head(res_uncertain_si[["R"]]$`t_start`,1)] <- mean(head(res_uncertain_si[["R"]]$`Mean(R)`,5))
  # Autofill ending R0s with last value
  testing_subset_aligned$R0[tail(res_uncertain_si[["R"]]$`t_start`,1):nrow(testing_subset_aligned)] <- mean(tail(res_uncertain_si[["R"]]$`Mean(R)`,5))
  listToLag <- c("R0","Google_Retail_recreation","Google_Grocery_pharmacy","Google_Parks","Google_Transit_stations","Google_Workplaces","Google_Residential")
  for(npi in 1:length(listToLag)){
    # Add 3 day lag factor for R0
    testing_subset_aligned[[paste0(listToLag[npi],"_Lag_3")]] <- lag(testing_subset_aligned[[paste0(listToLag[npi])]],3)
    testing_subset_aligned[[paste0(listToLag[npi],"_Lag_3")]][1:3] <- testing_subset_aligned[[paste0(listToLag[npi])]][1]
    # Add 7 day lag factor for R0
    testing_subset_aligned[[paste0(listToLag[npi],"_Lag_7")]] <- lag(testing_subset_aligned[[paste0(listToLag[npi])]],7)
    testing_subset_aligned[[paste0(listToLag[npi],"_Lag_7")]][1:7] <- testing_subset_aligned[[paste0(listToLag[npi])]][1]
    # Add 14 day lag factor for R0
    testing_subset_aligned[[paste0(listToLag[npi],"_Lag_14")]] <- lag(testing_subset_aligned[[paste0(listToLag[npi])]],14)
    testing_subset_aligned[[paste0(listToLag[npi],"_Lag_14")]][1:14] <- testing_subset_aligned[[paste0(listToLag[npi])]][1]
  }
  plot.new()
  plot(res_uncertain_si, legend = T)
  mtext(training_countries[i], outer=TRUE,  cex=1, line=-.5)
  
  tmp <- testing_subset_aligned[1:projectionTime,]
  tmp[,grep("cum|Social_Distancing|Quaranting_Cases|Close_Border|Google|R0|date|confirmed", colnames(tmp))] <- NA
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
dev.off()

testing_ready_OG <- testing_ready
training_ready_OG <- training_ready
# Let's use some of the country's data for training
NrowToSaveForTesting <- round(nrow(testing_ready_OG)*testingTimeFrame)
breakpoint <- nrow(testing_ready_OG)-NrowToSaveForTesting
testing_ready <- testing_ready_OG[(breakpoint+1):nrow(testing_ready_OG),]
training_ready <- as.data.frame(rbind(training_ready_OG,training_ready_OG[1:breakpoint,]))


if(incidence_flag==T && death_flag==F){
  # Even breaks coming from histogram
  h <- hist(training_ready$confirmed_cum_per_million_lag_01,breaks = nCuts)
  # Unevenly spaced breaks coming from quantiles
  # myBreaks <- quantile(training_ready$confirmed_cum_per_million_lag_01,seq(0, 1, by= 1/nCuts))
  myBreaks <- h$breaks
  training_ready$lag_01_cut <- as.numeric(cut(training_ready$confirmed_cum_per_million_lag_01, breaks = myBreaks,
                                   include.lowest = T, right = TRUE, dig.lab = 3,
                                   ordered_result = T))
  testing_ready$lag_01_cut <- as.numeric(cut(testing_ready$confirmed_cum_per_million_lag_01, breaks = myBreaks,
                                  include.lowest = T, right = TRUE, dig.lab = 3,
                                  ordered_result = T))
}else if(incidence_flag==T && death_flag==T){
  #   # geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
  myBreaks <- quantile(training_ready$death_cum_per_million_lag_01,seq(0, 1, by= 0.03))
  training_ready$lag_01_cut <- as.numeric(cut(training_ready$death_cum_per_million_lag_01, breaks = myBreaks,
                                   include.lowest = T, right = TRUE, dig.lab = 3,
                                   ordered_result = T))
  testing_ready$lag_01_cut <- as.numeric(cut(testing_ready$death_cum_per_million_lag_01, breaks = myBreaks,
                                  include.lowest = T, right = TRUE, dig.lab = 3,
                                  ordered_result = T))
}else if(incidence_flag==F && death_flag==F){
  #   geom_line(data=training_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size=0.8,alpha=.7)+
  myBreaks <- quantile(training_ready$confirmed_cum_lag_01,seq(0, 1, by= 0.03))
  training_ready$lag_01_cut <- as.numeric(cut(training_ready$confirmed_cum_lag_01, breaks = myBreaks,
                                   include.lowest = T, right = TRUE, dig.lab = 3,
                                   ordered_result = T))
  testing_ready$lag_01_cut <- as.numeric(cut(testing_ready$confirmed_cum_lag_01, breaks = myBreaks,
                                  include.lowest = T, right = TRUE, dig.lab = 3,
                                  ordered_result = T))
}else if(incidence_flag==F && death_flag==T){
  #   geom_line(data=training_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size=0.8,alpha=.7)+
  myBreaks <- quantile(training_ready$death_cum_lag_01,seq(0, 1, by= 0.03))
  training_ready$lag_01_cut <- as.numeric(cut(training_ready$death_cum_lag_01, breaks = myBreaks,
                                   include.lowest = T, right = TRUE, dig.lab = 3,
                                   ordered_result = T))
  testing_ready$lag_01_cut <- as.numeric(cut(testing_ready$death_cum_lag_01, breaks = myBreaks,
                                  include.lowest = T, right = TRUE, dig.lab = 3,
                                  ordered_result = T))
}



#---NPIflag1---#########################################################################################################################################################################
# Here we write a little loop that takes care of the fact that the Johns hopkins count data is 
# Updated much more frequently than the NPI data.  So 
# setting the NPIflag1 to "autofill" is our method of saying that we want to fill all the NAs in the time period with the last empirical time points' NPI values
# We will worry about the NPIflag2 later to specify if we want to fill the projection timeperiod the same way
# NPIflag1 <- "autofill"

peek_at_NPIs_training1 <- training_ready[,c(c("date","time","Country.x","ISO3","confirmed"),names(training_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(training_ready))])]
NPInames <- names(training_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(training_ready))]
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
peek_at_NPIs_training2 <- training_ready[,c(c("date","time","Country.x","ISO3","confirmed"),names(training_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(training_ready))])]


peek_at_NPIs_testing1 <- testing_ready[,c(c("date","time","Country.x","ISO3","confirmed"),names(testing_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(testing_ready))])]
NPInames <- names(testing_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(testing_ready))]
# counter <- 1
# prevcountry <- testing_ready$Country.x[1]
if(NPIflag1 == "autofill"){
  for(i in 2:nrow(testing_ready)){
    # curcountry <- testing_ready$Country.x[i]
    # if(curcountry == prevcountry){
    #   counter <- counter+1
    # }else{
    #   counter <- 1
    # }
    
    for(j in NPInames){
      if(is.na(testing_ready[[j]][i])){
        testing_ready[[j]][i] <- testing_ready[[j]][i-1]
      }
    }
    # prevcountry <- curcountry
  }
}
peek_at_NPIs_testing2 <- testing_ready[,c(c("date","time","Country.x","ISO3","confirmed"),names(testing_ready)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(testing_ready))])]


#---first plot---#########################################################################################################################################################################
# lineColors <- c("firebrick","darkgoldenrod1", "darkviolet", "limegreen", "dodgerblue")
colorList <- c(randomColor(length(training_countries)+1))
alphabeticalList <- sort(c(unique(as.character(testing_ready_OG$FullName)),unique(as.character(training_ready$FullName))))
alphabeticalIndex <- which(alphabeticalList == unique(as.character(testing_ready_OG$FullName)))
colorList[alphabeticalIndex] <- "red"

plot1 <- ggplot() 
if(incidence_flag==T && death_flag==F){
    # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1) +
    # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
    # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
    # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
    # geom_glowing_line(aes(x = testing_ready$time[1:(nrow(testing_ready)-projectionTime)], y = testing_ready$confirmed_cum_per_million[1:(nrow(testing_ready)-projectionTime)], color = FullName), alpha = 1, size = 1, glow_alpha = 0.03)+
    # geom_glowing_line(aes(x = training_ready$time, y = training_ready$confirmed_cum_per_million, color = FullName, fill = FullName), alpha = 1, size = 1, glow_alpha = 0.03)+
  plot1 <- testing_ready_OG %>%
    ggplot(aes(x = time, y = confirmed_cum_per_million, color = FullName))+
    geom_glowing_line()+
    labs(x=paste0("Days Since ",incidence_start_point," Cumulative Counts per Million"), y = "Confirmed Cumulative Cases per Million", title="")
  plot1 <- plot1 +
    geom_line(data=training_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size=0.8,alpha=.7)
}else if(incidence_flag==T && death_flag==T){
  # plot1 <- plot1 +
  #   geom_line(data=training_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size=0.8,alpha=.7)+
  #   # geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
  #   geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
  #   geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
  #   geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
  #   labs(x=paste0("Days Since ",incidence_start_point," Cumulative Deaths per Million"), y = "Confirmed Cumulative Deaths per Million", title="")
  plot1 <- testing_ready_OG %>%
    ggplot(aes(x = time, y = death_cum_per_million, color = FullName))+
    geom_glowing_line()+
    labs(x=paste0("Days Since ",incidence_start_point," Cumulative Deaths per Million"), y = "Confirmed Cumulative Deaths per Million", title="")
  plot1 <- plot1 +
    geom_line(data=training_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size=0.8,alpha=.7)
}else if(incidence_flag==F && death_flag==F){
  # plot1 <- plot1 +
  #   geom_line(data=training_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size=0.8,alpha=.7)+
  #   # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
  #   geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
  #   geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
  #   geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
  #   labs(x=paste0("Days Since ",count_start_point," Cumulative Counts"), y = "Confirmed Cumulative Cases", title="")
  plot1 <- testing_ready_OG %>%
    ggplot(aes(x = time, y = confirmed_cum, color = FullName))+
    geom_glowing_line()+
    labs(x=paste0("Days Since ",count_start_point," Cumulative Counts"), y = "Confirmed Cumulative Cases", title="")
  plot1 <- plot1 +
    geom_line(data=training_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size=0.8,alpha=.7)
}else if(incidence_flag==F && death_flag==T){
  # plot1 <- plot1 +
  #   geom_line(data=training_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size=0.8,alpha=.7)+
  #   # geom_line(data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
  #   geom_line(data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
  #   geom_line(data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
  #   geom_line(data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
  #   labs(x=paste0("Days Since ",count_start_point," Cumulative Deaths"), y = "Confirmed Cumulative Deaths", title="")
  plot1 <- testing_ready_OG %>%
    ggplot(aes(x = time, y = death_cum, color = FullName))+
    geom_glowing_line()+
    labs(x=paste0("Days Since ",count_start_point," Cumulative Deaths"), y = "Confirmed Cumulative Deaths", title="")
  plot1 <- plot1 +
    geom_line(data=training_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size=0.8,alpha=.7)
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
  scale_colour_manual(values=colorList, aesthetics = "colour") +
  theme(legend.text=element_text(size=9))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
# plot1

#---second plot---#########################################################################################################################################################################
# lineColors <- c("firebrick","darkgoldenrod1", "darkviolet", "limegreen", "dodgerblue")
plot2 <- ggplot() 
if(incidence_flag==T && death_flag==F){
  # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1) +
  # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
  # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
  # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
  # geom_glowing_line(aes(x = testing_ready$time[1:(nrow(testing_ready)-projectionTime)], y = testing_ready$confirmed_cum_per_million[1:(nrow(testing_ready)-projectionTime)], color = FullName), alpha = 1, size = 1, glow_alpha = 0.03)+
  # geom_glowing_line(aes(x = training_ready$time, y = training_ready$confirmed_cum_per_million, color = FullName, fill = FullName), alpha = 1, size = 1, glow_alpha = 0.03)+
  plot2 <- testing_ready_OG %>%
    ggplot(aes(x = time, y = R0, color = FullName))+
    geom_glowing_line()+
    labs(x=paste0("Days Since ",incidence_start_point," Cumulative Counts per Million"), y = "R0", title="")
  plot2 <- plot2 +
    geom_line(data=training_ready, aes(x = time, y = R0, group = FullName, color = FullName), size=0.8,alpha=.7)
}else if(incidence_flag==T && death_flag==T){
  # plot2 <- plot2 +
  #   geom_line(data=training_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size=0.8,alpha=.7)+
  #   # geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
  #   geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
  #   geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
  #   geom_line(data=testing_ready, aes(x = time, y = death_cum_per_million, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
  #   labs(x=paste0("Days Since ",incidence_start_point," Cumulative Deaths per Million"), y = "Confirmed Cumulative Deaths per Million", title="")
  plot2 <- testing_ready_OG %>%
    ggplot(aes(x = time, y = R0, color = FullName))+
    geom_glowing_line()+
    labs(x=paste0("Days Since ",incidence_start_point," Cumulative Deaths per Million"), y = "R0", title="")
  plot2 <- plot2 +
    geom_line(data=training_ready, aes(x = time, y = R0, group = FullName, color = FullName), size=0.8,alpha=.7,show.legend = F)
}else if(incidence_flag==F && death_flag==F){
  # plot2 <- plot2 +
  #   geom_line(data=training_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size=0.8,alpha=.7)+
  #   # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
  #   geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
  #   geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
  #   geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
  #   labs(x=paste0("Days Since ",count_start_point," Cumulative Counts"), y = "Confirmed Cumulative Cases", title="")
  plot2 <- testing_ready_OG %>%
    ggplot(aes(x = time, y = R0, color = FullName))+
    geom_glowing_line()+
    labs(x=paste0("Days Since ",count_start_point," Cumulative Counts"), y = "R0", title="")
  plot2 <- plot2 +
    geom_line(data=training_ready, aes(x = time, y = R0, group = FullName, color = FullName), size=0.8,alpha=.7,show.legend = F)
}else if(incidence_flag==F && death_flag==T){
  # plot2 <- plot2 +
  #   geom_line(data=training_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size=0.8,alpha=.7)+
  #   # geom_line(data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
  #   geom_line(data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
  #   geom_line(data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
  #   geom_line(data=testing_ready, aes(x = time, y = death_cum, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
  #   labs(x=paste0("Days Since ",count_start_point," Cumulative Deaths"), y = "Confirmed Cumulative Deaths", title="")
  plot2 <- testing_ready_OG %>%
    ggplot(aes(x = time, y = R0, color = FullName))+
    geom_glowing_line()+
    labs(x=paste0("Days Since ",count_start_point," Cumulative Deaths"), y = "R0", title="")
  plot2 <- plot2 +
    geom_line(data=training_ready, aes(x = time, y = R0, group = FullName, color = FullName), size=0.8,alpha=.7,show.legend = F)
}
plot2 <- plot2 +
  ylim(c(0,7))+
  guides(color=guide_legend(title="")) +
  theme(legend.title=element_text(size=14))+
  theme(axis.text.x = element_text(color="black",size = 13, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text(color="black",size = 13, angle = 0),
        axis.title.x = element_text(color="black",size = 13, angle = 0),
        axis.title.y = element_text(color="black",size = 13, angle = 90)
  )+
  # scale_x_continuous(breaks=seq(1, 10, 1))+
  scale_colour_manual(values=colorList, aesthetics = "colour") +
  theme(legend.text=element_text(size=9))+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#---third and fourth plots---#########################################################################################################################################################################
# lineColors <- c("firebrick","darkgoldenrod1", "darkviolet", "limegreen", "dodgerblue")
plot34Data <- testing_ready_OG[1:(nrow(testing_ready_OG)-projectionTime),c("date","R0","Social_Distancing","Quaranting_Cases","Close_Border","Google_Grocery_pharmacy","Google_Parks","Google_Residential","Google_Retail_recreation","Google_Transit_stations","Google_Workplaces")]
plot34Data <- plot34Data %>% gather(key,value,-date)
# plot34Data$date <- as.date(plot34Data$date)
plot34Data$key <- as.factor(plot34Data$key)
plot34Data$value <- as.numeric(plot34Data$value)
plot3Data <- subset(plot34Data, key %ni% c("R0","Social_Distancing","Quaranting_Cases","Close_Border"))
plot4Data <- subset(plot34Data, key %in% c("R0","Social_Distancing","Quaranting_Cases","Close_Border"))

plot3 <- ggplot() 
plot3 <- plot3 +
  # geom_line(data=training_ready, aes(x = time, y = R0, group = FullName, color = FullName), size=0.8,alpha=.7)+
  # geom_line(data=subset(plot3Data, key %in% c("R0","Social_Distancing","Quaranting_Cases","Close_Border")), aes(x = date, y = value, group = key, color = key), size=1, linetype = "solid",alpha=1) +
  geom_line(data=plot3Data, aes(x = date, y = value, group = key, color = key), size=1, linetype = "solid",alpha=1) +
  # geom_line(data=testing_ready, aes(x = time, y = R0, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
  # geom_line(data=testing_ready, aes(x = time, y = R0, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
  # geom_line(data=testing_ready, aes(x = time, y = R0, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
  labs(x=paste0("Days Since ",incidence_start_point," Cumulative Counts per Million"), y = "% Change From Baseline", title="")
plot3 <- plot3 +
  guides(color=guide_legend(title="")) +
  theme(legend.title=element_text(size=14))+
  theme(axis.text.x = element_text(color="black",size = 13, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text(color="black",size = 13, angle = 0),
        axis.title.x = element_text(color="black",size = 13, angle = 0),
        axis.title.y = element_text(color="black",size = 13, angle = 90)
  )+
  # scale_x_continuous(breaks=seq(1, 10, 1))+
  # scale_colour_manual(values=randomColor(length(unique(plot3Data$key))), aesthetics = "colour") +
  scale_colour_manual(values=brewer.pal(length(unique(plot3Data$key)), "Dark2"), aesthetics = "colour") +
  theme(legend.text=element_text(size=9))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x=NULL)

# plot4Data <- subset(plot34Data, key %in% c("R0","Social_Distancing","Quaranting_Cases","Close_Border"))
plot4Data$date <- as.Date(plot4Data$date)
plot4Data$key <- factor(plot4Data$key, levels = c("Social_Distancing", "Quaranting_Cases", "Close_Border", "R0"), ordered = T)
plot4 <- ggplot() 
plot4_NPI <- subset(plot4Data, key %ni% c("R0"))
plot4_NPI$key <- factor(plot4_NPI$key, levels = c("Social_Distancing", "Quaranting_Cases", "Close_Border", "R0"), ordered = T)
plot4_R0 <- subset(plot4Data, key %in% c("R0"))
plot4_R0$key <- factor(plot4_R0$key, levels = c("Social_Distancing", "Quaranting_Cases", "Close_Border", "R0"), ordered = T)

colorsPlot4 <- c(brewer.pal(3, "Set2")[1:2],"red",brewer.pal(3, "Set2")[3])
plot4 <- ggplot() 
plot4 <- plot4_R0 %>%
  ggplot(aes(x = date, y = value*5/(max(plot4_R0$value)), color = key))+
  geom_glowing_line()+
  labs(x=paste0("Days Since ",incidence_start_point," Cumulative Deaths per Million"), y = "R0", title="")
plot4 <- plot4 +
  geom_line(data=plot4_NPI, aes(x = date, y = value, group = key, color = key), size=1.5,alpha=.7,show.legend = F)
plot4 <- plot4 +
  guides(color=guide_legend(title="")) +
  theme(legend.title=element_text(size=14))+
  theme(axis.text.x = element_text(color="black",size = 13, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text(color="black",size = 13, angle = 0),
        axis.title.x = element_text(color="black",size = 13, angle = 0),
        axis.title.y = element_text(color="black",size = 13, angle = 90)
  )+
  # scale_x_continuous(breaks=seq(1, 10, 1))+
  # scale_colour_manual(values=randomColor(length(unique(plot3Data$key))), aesthetics = "colour") +
  scale_y_continuous(name = "NPI Policy Scale", sec.axis = sec_axis(~./(5/(max(plot4_R0$value))), name = "R0"),limits = c(0,5))+
  scale_colour_manual(values=colorsPlot4, aesthetics = "colour") +
  theme(legend.text=element_text(size=9))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x=NULL)

# plot3
# plot4
#---tree_Models---#########################################################################################################################################################################
str(training_ready)
str(testing_ready)

# Create a Random Forest model with default parameters
if(death_flag==F){
  training_ready_sub2 <- training_ready[,colnames(training_ready)[which(colnames(training_ready) %ni% listToRemove)]]
  training_ready_sub2 <- training_ready_sub2[,grep("death|MalePercent|FemalePercent", colnames(training_ready_sub2),invert = T)]
  for(i in (nLags+1):100){
    training_ready_sub2 <- training_ready_sub2[,grep(paste0(sprintf("lag_%02d", i)), colnames(training_ready_sub2),invert = T)]
  }
  training_ready_sub2 <- subset(training_ready_sub2, select=-c(Percent_house_Multi_generation,Percent_house_Three_generation,Percent_house_Skip_generation,Num_Tests_cum))
  training_ready_sub2 %<>% mutate_if(is.factor,as.character)  
  training_ready_sub2 %<>% mutate_if(is.character,as.numeric)
  training_ready_sub2 %<>% mutate_if(is.integer,as.numeric)
  # remove more columns we don't want in the model
  testing_ready_sub2 <- testing_ready[,colnames(testing_ready)[which(colnames(testing_ready) %ni% listToRemove)]]
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
      # training_ready_sub2$lag_01_cut <- training_ready$lag_01_cut
      # testing_ready_sub2 %<>% mutate_if(is.factor,as.character)  
      # testing_ready_sub2 %<>% mutate_if(is.character,as.numeric)
      # testing_ready_sub2 %<>% mutate_if(is.integer,as.numeric)
      # testing_ready_sub2$lag_01_cut <- testing_ready$lag_01_cut
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
      # training_ready_sub2$lag_01_cut <- training_ready$lag_01_cut
      # testing_ready_sub2 %<>% mutate_if(is.factor,as.character)  
      # testing_ready_sub2 %<>% mutate_if(is.character,as.numeric)
      # testing_ready_sub2 %<>% mutate_if(is.integer,as.numeric)
      # testing_ready_sub2$lag_01_cut <- testing_ready$lag_01_cut
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
  training_ready_sub2 <- training_ready[,colnames(training_ready)[which(colnames(training_ready) %ni% listToRemove)]]
  training_ready_sub2 <- training_ready_sub2[,grep("confirmed|MalePercent|FemalePercent", colnames(training_ready_sub2),invert = T)]
  for(i in (nLags+1):100){
    training_ready_sub2 <- training_ready_sub2[,grep(paste0(sprintf("lag_%02d", i)), colnames(training_ready_sub2),invert = T)]
  }
  training_ready_sub2 <- subset(training_ready_sub2, select=-c(Percent_house_Multi_generation,Percent_house_Three_generation,Percent_house_Skip_generation,Num_Tests_cum))
  training_ready_sub2 %<>% mutate_if(is.factor,as.character)  
  training_ready_sub2 %<>% mutate_if(is.character,as.numeric)
  training_ready_sub2 %<>% mutate_if(is.integer,as.numeric)
  # remove more columns we don't want in the model
  testing_ready_sub2 <- testing_ready[,colnames(testing_ready)[which(colnames(testing_ready) %ni% listToRemove)]]
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
      # training_ready_sub2$lag_01_cut <- training_ready$lag_01_cut
      # testing_ready_sub2 %<>% mutate_if(is.factor,as.character)  
      # testing_ready_sub2 %<>% mutate_if(is.character,as.numeric)
      # testing_ready_sub2 %<>% mutate_if(is.integer,as.numeric)
      # testing_ready_sub2$lag_01_cut <- testing_ready$lag_01_cut
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
      # training_ready_sub2$lag_01_cut <- training_ready$lag_01_cut
      # testing_ready_sub2 %<>% mutate_if(is.factor,as.character)  
      # testing_ready_sub2 %<>% mutate_if(is.character,as.numeric)
      # testing_ready_sub2 %<>% mutate_if(is.integer,as.numeric)
      # testing_ready_sub2$lag_01_cut <- testing_ready$lag_01_cut
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
NPInames <- names(testing_ready_pred)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(testing_ready_pred))]
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
        if("confirmed_cum_per_million_lag_01" %in% colnames(testing_ready_pred)){
          testing_ready_pred[i,c(paste0("confirmed_cum_per_million_lag_01"))] <- testing_ready_pred[i-1,c(paste0("confirmed_cum_per_million"))]
        }
        if("lag_01_cut" %in% colnames(testing_ready_pred)){
          testing_ready_pred[i,c(paste0("lag_01_cut"))] <- as.numeric(cut(testing_ready_pred[i-1,c(paste0("confirmed_cum_per_million"))], breaks = myBreaks,
                                                                          include.lowest = T, right = TRUE, dig.lab = 3,
                                                                          ordered_result = T))        
          }
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
pAll$time <- testing_ready$time
pAll$date <- testing_ready$date
areNA <- which(is.na(pAll$date))
for(i in areNA){
  pAll$date[i] <- pAll$date[i-1]+1
}

if(incidence_flag==T && death_flag==F){
  plot1Data_tmp <- testing_ready_OG[,c("FullName","time","confirmed_cum_per_million")]
}else if(incidence_flag==T && death_flag==T){
  plot1Data_tmp <- testing_ready_OG[,c("FullName","time","death_cum_per_million")]
}else if(incidence_flag==F && death_flag==F){
  plot1Data_tmp <- testing_ready_OG[,c("FullName","time","confirmed_cum")]
}else if(incidence_flag==F && death_flag==T){
  plot1Data_tmp <- testing_ready_OG[,c("FullName","time","death_cum")]
}

# Organize the data to be rady for ggplot
plot1Data <- plot1Data_tmp %>% left_join(pAll, by = c("time" = "time"))
colnames(plot1Data) <- c("country","time","Actual","Prediction","date")
plot1Data$time <- NULL
plot1Data$date <- testing_ready_OG$date
areNA <- which(is.na(plot1Data$date))
for(i in areNA){
  plot1Data$date[i] <- plot1Data$date[i-1]+1
}
plot1Data <- plot1Data[order(plot1Data$date),]
plot1Data$Prediction <- as.numeric(plot1Data$Prediction)
plot1Data$Actual <- as.numeric(plot1Data$Actual)
plot1Data$country <- as.character(plot1Data$country)
str(plot1Data)

# Reshape the data to be rady for ggplot
m1 <- reshape2::melt(plot1Data,id=c("country","date"))
m1$date <- as.numeric(m1$date)
m1$value <- as.numeric(m1$value)
m1$variable <- as.factor(m1$variable)
m1$country <- as.factor(m1$country)
str(m1)
m1 <- m1[order(m1$date),]

#---plotPrediction---#########################################################################################################################################################################

m1$date <- as.Date(m1$date)
predictColor <- rep("deepskyblue2",2)
alphabeticalList <- sort(c("Predict","Actual"))
alphabeticalIndex <- which(alphabeticalList == "Actual")
predictColor[alphabeticalIndex] <- "red"

plot_predict <- ggplot() 
plot_predict <- subset(m1, variable == "Actual") %>%
    ggplot(aes(x = date, y = value, color = variable))+
    geom_glowing_line()
plot_predict <- plot_predict +
    geom_line(data=subset(m1, variable == "Prediction"), aes(x = date, y = value, group = variable, color = variable), size=2,alpha=.95,show.legend = T,linetype = "solid")
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
  scale_colour_manual(values=predictColor)+
  theme(legend.text=element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x=NULL)
# plot_predict

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
  labs(y="Importance", x = NULL, title="")+
theme_bw()

#---cumulativePlot---#########################################################################################################################################################################
rmsedf <- plot1Data[(breakpoint+1):(nrow(plot1Data)-projectionTime),]
rmse_val <- sqrt( sum( (rmsedf$Prediction - rmsedf$Actual)^2 ,na.rm = T) / (nrow(rmsedf)) )
rmse_val <- round(rmse_val,3)
gl <- list(plot1,plot2,plot_predict,plot_varimp,plot3,plot4)
pdf("finalPlot.pdf",width = 24, height = 24)
grid.arrange(grobs = gl, top = textGrob(paste0(testing_ready$FullName[1]," ",model_name," RMSE = ",rmse_val), gp=gpar(fontsize=20)), layout_matrix = rbind(c(1,1,1,1,1,4,4,4),
                                                                                                                                                           c(1,1,1,1,1,4,4,4),
                                                                                                                                                           c(1,1,1,1,1,4,4,4),
                                                                                                                                                           c(1,1,1,1,1,4,4,4),
                                                                                                                                                           c(1,1,1,1,1,4,4,4),
                                                                                                                                                           c(2,2,2,2,NA,4,4,4),
                                                                                                                                                           c(2,2,2,2,NA,4,4,4),
                                                                                                                                                           c(2,2,2,2,5,5,5,5),
                                                                                                                                                           c(2,2,2,2,5,5,5,5),
                                                                                                                                                           c(2,2,2,2,5,5,5,5),
                                                                                                                                                           c(3,3,3,3,5,5,5,5),
                                                                                                                                                           c(3,3,3,3,6,6,6,6),
                                                                                                                                                           c(3,3,3,3,6,6,6,6),
                                                                                                                                                           c(3,3,3,3,6,6,6,6),
                                                                                                                                                           c(3,3,3,3,6,6,6,6)))
dev.off()
#
# plot(best_model)
# 

