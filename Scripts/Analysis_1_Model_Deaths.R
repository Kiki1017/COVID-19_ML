library(tidyverse)
library(EpiEstim)
library(zoo)
library(VSURF)
library(caret)
library(doParallel)
library(ggplot2)
library(randomForest)
library(ranger)
library(viridis)
library(RColorBrewer)
library(randomcoloR)
# devtools::install_github("delabj/ggCyberPunk")
library(ggCyberPunk)
library(corrplot)
#
library(gridExtra)
library(grid)
library(ggpubr)
'%ni%' <- Negate('%in%')

#---Flag setup---#########################################################################################################################################################################

# TRUE if you want to scale by population
incidence_flag <- T
# TRUE if you want to do deaths instead of cases
death_flag <- T
incidence_start_point <- 0.3
count_start_point <- 100
# if we are doing deaths, we want incidence start point to be about 5.9% of the case one becuase that's the approx mortality rate
if(death_flag==T){
  incidence_start_point <- incidence_start_point*(5.9/100)
  count_start_point <- count_start_point*(5.9/100)
}

# the number of lag factors you want
nLags <- 14
# the time you want to forecast the predictiont
forecastingTime <- 14
# autofill the missing datapoints in NPI data
NPIflag1 <- "autofill"
# use the last NPI datapoints for the forecasting period
NPIflag2 <- "lastNPI"
# The Percent of the timeframe you want to reserve for testing a country (the rest of that country's time series is included into the model)
# for example if testingTimeFrame <- 0.8, then 20% of the timeseries will be used to train, and 80% will be used to predict
testingTimeFrame <- .8
# Number of cores to use when running rf model and vsurf
num_cores = detectCores()
# What NA acion to use for models
nasaction = na.omit
# Number of trees to train on
number_trees = 1000

#---dataSetup---#########################################################################################################################################################################
# read in raw data
data_clean <- read.csv("./InputData/ML_features.csv")
data_clean$date <- as.Date(data_clean$date)

# Looking at the data
glimpse(data_clean)
summary(data_clean)

# testing_countriesList <- c("USA","BRA","GBR","ZAF","BEL","DZA")
testing_countriesList <- c("USA")
for(cc in 1:length(testing_countriesList)){
    
    
  # Choose the testing country
  testing_countries <- c(testing_countriesList[cc])
  
  
  # make country lists, these are the ones that we have NPI data collected for
  # https://docs.google.com/spreadsheets/d/1vrKvs52OAxuB7x2kT9r1q6IcIBxGEQsNRHsK_o7h3jo/edit#gid=378237553
  # training_countries_all <- c("ITA","FRA","GBR")
  # training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","USA","SWE","AUT","CHE","DEU","FRA")
  # training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","PRT","ISR","RUS","NOR","AUS","DNK","CHL","CZE","JPN","UKR","MAR","ARG")
  # training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","HUB","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","TUR","BEL","ANT","PRT","ISR","RUS","NOR","IRL","AUS","IND","DNK","CHL","CZE","JPN","UKR","MAR","ARG","SGP","ROU")
  training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","TUR","BEL","ANT","PRT","ISR","RUS","NOR","IRL","AUS","IND","DNK","CHL","CZE","JPN","UKR","MAR","ARG","SGP","ROU")
  
  training_countries <- training_countries_all[which(training_countries_all != testing_countries)]
  
  #---Estimating reproduction numbers, R0---#########################################################################################################################################################################
  # We are using the epiestim package to calculate the R0 for countries. This requires two things
  # 1. specific information about the serial intervals for COVID
  # 2. the timeseries incidence data.
  # "SI for Serial Intervals.
  # Determination of the serial interval, the time between the start of symptoms in the primary patient (infector)
  # and onset of symptoms in the patient receiving that infection from the infector (the infectee)"
  # Table 1 from https://www.medrxiv.org/content/10.1101/2020.04.13.20062760v1
  # "we calculate a weighted mean of the published parameters and inferred a serial interval described
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
    
    # Smooth out incidence using moving average with a centered window of 7 datapoints (3 to the left, 3 to the right)
    # make sure the window is an odd integer
    window <- 7
    # dim(training_subset_aligned)
    # length(rollmean(training_subset_aligned$confirmed_cum, k=window))
    training_subset_aligned$movingAverage <- c(training_subset_aligned$confirmed_cum[1:((window-1)/2)],rollmean(training_subset_aligned$confirmed_cum, k=window, align = "center"),training_subset_aligned$confirmed_cum[(nrow(training_subset_aligned)-((window-1)/2)+1):nrow(training_subset_aligned)])
    # Plot cases
    gg <- ggplot(training_subset_aligned) +
      geom_line(aes(x=date, y=confirmed_cum),color="red") +
      geom_line(aes(x=date, y=movingAverage),color="blue") +
      ggtitle(paste0("Reported cases in ", training_countries[i]))
    print(gg)
    # Add moving average day lag and one day difference variables
    training_subset_aligned[["movingAverage_Lag_01"]] <- lag(training_subset_aligned[["movingAverage"]],1)
    training_subset_aligned[["movingAverage_Lag_03"]] <- lag(training_subset_aligned[["movingAverage"]],3)
    training_subset_aligned[["movingAverage_Lag_07"]] <- lag(training_subset_aligned[["movingAverage"]],7)
    training_subset_aligned[["movingAverage_Lag_14"]] <- lag(training_subset_aligned[["movingAverage"]],14)
    training_subset_aligned[["movingAverage_diff_1_3"]] <- training_subset_aligned[["movingAverage_Lag_01"]] - training_subset_aligned[["movingAverage_Lag_03"]]
    training_subset_aligned[["movingAverage_diff_1_7"]] <- training_subset_aligned[["movingAverage_Lag_01"]] - training_subset_aligned[["movingAverage_Lag_07"]]
    training_subset_aligned[["movingAverage_diff_1_14"]] <- training_subset_aligned[["movingAverage_Lag_01"]] - training_subset_aligned[["movingAverage_Lag_14"]]
    training_subset_aligned[["movingAverage_diff_3_7"]] <- training_subset_aligned[["movingAverage_Lag_03"]] - training_subset_aligned[["movingAverage_Lag_07"]]
    training_subset_aligned[["movingAverage_diff_7_14"]] <- training_subset_aligned[["movingAverage_Lag_07"]] - training_subset_aligned[["movingAverage_Lag_14"]]
    
    
    # toCalcR0 <- training_subset_aligned[,c("date","confirmed")]
    toCalcR0 <- training_subset_aligned[,c("date","movingAverage")]
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
      training_subset_aligned[[paste0(listToLag[npi],"_Lag_03")]] <- lag(training_subset_aligned[[paste0(listToLag[npi])]],3)
      training_subset_aligned[[paste0(listToLag[npi],"_Lag_03")]][1:3] <- mean(training_subset_aligned[[paste0(listToLag[npi])]][1:5])
      # Add 7 day lag factor for R0
      training_subset_aligned[[paste0(listToLag[npi],"_Lag_07")]] <- lag(training_subset_aligned[[paste0(listToLag[npi])]],7)
      training_subset_aligned[[paste0(listToLag[npi],"_Lag_07")]][1:7] <- mean(training_subset_aligned[[paste0(listToLag[npi])]][1:5])
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
    
    # Smooth out incidence using moving average with a centered window of 7 datapoints (3 to the left, 3 to the right)
    # make sure the window is an odd integer
    window <- 7
    # dim(testing_subset_aligned)
    # length(rollmean(testing_subset_aligned$confirmed_cum_per_million, k=window))
    testing_subset_aligned$movingAverage <- c(testing_subset_aligned$confirmed_cum_per_million[1:((window-1)/2)],rollmean(testing_subset_aligned$confirmed_cum_per_million, k=window, align = "center"),testing_subset_aligned$confirmed_cum_per_million[(nrow(testing_subset_aligned)-((window-1)/2)+1):nrow(testing_subset_aligned)])
    # Plot cases
    gg <- ggplot(testing_subset_aligned) +
      geom_line(aes(x=date, y=confirmed_cum_per_million),color="red") +
      geom_line(aes(x=date, y=movingAverage),color="blue") +
      ggtitle(paste0("Reported cases in ", training_countries[i]))
    print(gg)
    # Add moving average day lag and one day difference variables
    testing_subset_aligned[["movingAverage_Lag_01"]] <- lag(testing_subset_aligned[["movingAverage"]],1)
    testing_subset_aligned[["movingAverage_Lag_03"]] <- lag(testing_subset_aligned[["movingAverage"]],3)
    testing_subset_aligned[["movingAverage_Lag_07"]] <- lag(testing_subset_aligned[["movingAverage"]],7)
    testing_subset_aligned[["movingAverage_Lag_14"]] <- lag(testing_subset_aligned[["movingAverage"]],14)
    testing_subset_aligned[["movingAverage_diff_1_3"]] <- testing_subset_aligned[["movingAverage_Lag_01"]] - testing_subset_aligned[["movingAverage_Lag_03"]]
    testing_subset_aligned[["movingAverage_diff_1_7"]] <- testing_subset_aligned[["movingAverage_Lag_01"]] - testing_subset_aligned[["movingAverage_Lag_07"]]
    testing_subset_aligned[["movingAverage_diff_1_14"]] <- testing_subset_aligned[["movingAverage_Lag_01"]] - testing_subset_aligned[["movingAverage_Lag_14"]]
    testing_subset_aligned[["movingAverage_diff_3_7"]] <- testing_subset_aligned[["movingAverage_Lag_03"]] - testing_subset_aligned[["movingAverage_Lag_07"]]
    testing_subset_aligned[["movingAverage_diff_7_14"]] <- testing_subset_aligned[["movingAverage_Lag_07"]] - testing_subset_aligned[["movingAverage_Lag_14"]]
    
    # toCalcR0 <- testing_subset_aligned[,c("date","confirmed")]
    toCalcR0 <- testing_subset_aligned[,c("date","movingAverage")]
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
    testing_subset_aligned$R0 <- NA 
    testing_subset_aligned$R0[head(res_uncertain_si[["R"]]$`t_start`,1):tail(res_uncertain_si[["R"]]$`t_start`,1)] <- res_uncertain_si[["R"]]$`Mean(R)`
    # Autofill beginning R0s with first value
    testing_subset_aligned$R0[1:head(res_uncertain_si[["R"]]$`t_start`,1)] <- mean(head(res_uncertain_si[["R"]]$`Mean(R)`,5))
    # Autofill ending R0s with last value
    testing_subset_aligned$R0[tail(res_uncertain_si[["R"]]$`t_start`,1):nrow(testing_subset_aligned)] <- mean(tail(res_uncertain_si[["R"]]$`Mean(R)`,5))
    listToLag <- c("R0","Google_Retail_recreation","Google_Grocery_pharmacy","Google_Parks","Google_Transit_stations","Google_Workplaces","Google_Residential")
    for(npi in 1:length(listToLag)){
      # Add 3 day lag factor for R0
      testing_subset_aligned[[paste0(listToLag[npi],"_Lag_03")]] <- lag(testing_subset_aligned[[paste0(listToLag[npi])]],3)
      testing_subset_aligned[[paste0(listToLag[npi],"_Lag_03")]][1:3] <- testing_subset_aligned[[paste0(listToLag[npi])]][1]
      # Add 7 day lag factor for R0
      testing_subset_aligned[[paste0(listToLag[npi],"_Lag_07")]] <- lag(testing_subset_aligned[[paste0(listToLag[npi])]],7)
      testing_subset_aligned[[paste0(listToLag[npi],"_Lag_07")]][1:7] <- testing_subset_aligned[[paste0(listToLag[npi])]][1]
      # Add 14 day lag factor for R0
      testing_subset_aligned[[paste0(listToLag[npi],"_Lag_14")]] <- lag(testing_subset_aligned[[paste0(listToLag[npi])]],14)
      testing_subset_aligned[[paste0(listToLag[npi],"_Lag_14")]][1:14] <- testing_subset_aligned[[paste0(listToLag[npi])]][1]
    }
    plot.new()
    plot(res_uncertain_si, legend = T)
    mtext(training_countries[i], outer=TRUE,  cex=1, line=-.5)
    
    tmp <- testing_subset_aligned[1:forecastingTime,]
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
  
  # preserve the original dataframes before we make changes to them...
  # testing_ready <- testing_ready_OG
  # training_ready <- training_ready_OG
  testing_ready_OG <- testing_ready
  training_ready_OG <- training_ready
  # Let's use some of the country's data for training based on testingTimeFrame
  NrowToSaveForTesting <- round(nrow(testing_ready_OG)*testingTimeFrame)
  breakpoint <- nrow(testing_ready_OG)-NrowToSaveForTesting
  testing_ready <- testing_ready_OG[(breakpoint+1):nrow(testing_ready_OG),]
  training_ready <- as.data.frame(rbind(training_ready_OG,training_ready_OG[1:breakpoint,]))
  
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
  
  #---Filtering Columns in Dataframes---#########################################################################################################################################################################
  # Columns you don't want to be in the model
  listToRemove <- c("date",
                    "Country.x",
                    "Country.y",
                    "ISO3",
                    "confirmed",
                    "death",
                    "Source",
                    "FullName",
                    "recovered",
                    "confirmed_cum_per_million_lag_01",
                    "movingAverage","movingAverage_Lag_01",
                    "movingAverage_Lag_03",
                    "movingAverage_Lag_03",
                    "movingAverage_Lag_07",
                    "movingAverage_Lag_14",
                    "lag_01_cut",
                    # "R0_Lag_03",
                    # "R0_Lag_07",
                    # "R0_Lag_14",
                    "death_cum",
                    "time"
  )
  
  # filter training data
  training_ready_sub2 <- training_ready %>%
    select(which(colnames(training_ready) %ni% listToRemove)) %>%
    # select(-contains("death_cum")) %>%
    select(-contains("confirmed")) %>%
    select(-contains("MalePercent")) %>%
    select(-contains("FemalePercent")) %>%
    select(-c(Percent_house_Multi_generation,Percent_house_Three_generation,Percent_house_Skip_generation,Num_Tests_cum)) %>%
    select(-contains("movingAverage")) %>%
    mutate_if(is.factor,as.character) %>%
    mutate_if(is.character,as.numeric) %>%
    mutate_if(is.integer,as.numeric)
  
  # filter testing data
  testing_ready_sub2 <- testing_ready %>%
    select(which(colnames(testing_ready) %ni% listToRemove)) %>%
    # select(-contains("death_cum")) %>%
    select(-contains("confirmed")) %>%
    select(-contains("MalePercent")) %>%
    select(-contains("FemalePercent")) %>%
    select(-c(Percent_house_Multi_generation,Percent_house_Three_generation,Percent_house_Skip_generation,Num_Tests_cum)) %>%
    select(-contains("movingAverage")) %>%
    mutate_if(is.factor,as.character) %>%
    mutate_if(is.character,as.numeric) %>%
    mutate_if(is.integer,as.numeric)
  
  #---VSURF Variable Selection---#########################################################################################################################################################################
  print(paste0('Number of cores being used = ',num_cores, ", of possible ", detectCores()," cores"))
  
  registerDoParallel(num_cores)
  outcomeVariable <- "death_cum_per_million"
  mod_formula <- as.formula(paste(outcomeVariable,"~","."))
  
  set.seed(15)
  
  glimpse(training_ready_sub2)
  
  # Pick the best mtry
  x <- training_ready_sub2[complete.cases(training_ready_sub2),which(colnames(training_ready_sub2) %ni% outcomeVariable)]
  y <- training_ready_sub2[complete.cases(training_ready_sub2),which(colnames(training_ready_sub2) %in% outcomeVariable)]
  bestMtry <- tuneRF(x, y, stepFactor = 1.5, improve = 1e-5, ntree = number_trees, na.action=nasaction)
  bestMtry <- as.data.frame(bestMtry)
  mtry_best <- bestMtry$mtry[which.min(bestMtry$OOBError)]
  
  # Run VSURF to get the top variables
  results.vsurf <- VSURF(x,y,
                         na.action = na.omit, 
                         mtry = mtry_best, 
                         n_tree = number_trees, 
                         parallel = TRUE, 
                         ncores = num_cores)
  
  # look at results of VSURF
  summary(results.vsurf)
  plot(results.vsurf)
  results.vsurf$varselect.thres
  results.vsurf$varselect.interp
  results.vsurf$varselect.pred
  
  # print the reduced number of variables that should be considered in model
  colnames(training_ready_sub2[,results.vsurf$varselect.thres])
  colnames(training_ready_sub2[,results.vsurf$varselect.interp])
  colnames(training_ready_sub2[,results.vsurf$varselect.pred])    # The final list of variables to be included according to the VSURF methodology.
  
  # training dataframe with reduced number of variables
  training_ready_sub_vsurf_result = select(training_ready_sub2, c("death_cum_per_million",colnames(training_ready_sub2[,results.vsurf$varselect.pred])))
  glimpse(training_ready_sub_vsurf_result)
  
  # testing dataframe with reduced number of variables
  testing_ready_sub_vsurf_result = select(testing_ready, c("death_cum_per_million",colnames(training_ready_sub2[,results.vsurf$varselect.pred])))
  glimpse(testing_ready_sub_vsurf_result)
  
  closeAllConnections()
  #---RF model---#########################################################################################################################################################################
  print(paste0('Number of cores being used = ',num_cores, ", of possible ", detectCores()," cores"))
  registerDoParallel(num_cores)
  
  tunegrid <- expand.grid(.mtry = c(mtry_best),
                          .splitrule = c('gini'),
                          .min.node.size = c(5,10,20))
  
  control <- trainControl(method="cv", 
                          number=3, 
                          # repeats=3,
                          # verboseIter = T,
                          # classProbs = T,
                          allowParallel = TRUE)
  
  rf.mod <- caret::train(mod_formula,
                         data = training_ready_sub_vsurf_result,
                         # method = 'ranger',
                         method = 'rf',
                         na.action = nasaction,
                         keep.inbag = TRUE,
                         replace = TRUE,
                         # importance = "permutation", #***
                         trControl = control)
  
  varImp(rf.mod)
  
  
  
  model_name = paste0("rf.mod")
  best_model = rf.mod
  closeAllConnections()
  #---predictFunction---#########################################################################################################################################################################
  # the predict function is called when we want to use the tree to make predictions
  predictFunction <- function(name=best_model, mod_name=model_name, dd=testing_ready_pred, n_trees = (1:30)*25, nasaction = na.omit){
    if("rf.mod" == mod_name){
      predict_tmp <- predict(name, dd, na.action = nasaction)
      # predict_tmp <- predict(name, dd, na.action = nasaction, type='se', se.method='infjack')
    }
    return(predict_tmp)
  }
  
  #---NPIflag2---#########################################################################################################################################################################
  # setting the NPIflag2 to "lastNPI" is our method of saying that we want to fill all the NAs in the forecasting period with the last empirical time points' NPI values
  # NPIflag2 <- "lastNPI"
  testing_ready_pred <- testing_ready_sub_vsurf_result
  
  breaker <- nrow(testing_ready_pred)-forecastingTime+1
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
  p1 <- predict(best_model, testing_ready_pred[1:(breaker-1),], na.action = na.pass, n.trees = number_trees)
  # p1_tmp <- predictFunction(name=best_model, mod_name=model_name, dd=testing_ready_pred[1:(breaker-1),], n_trees = number_trees)
  # p1 <- as.data.frame(cbind(p1_tmp$predictions,p1_tmp$se))
  # colnames(p1) <- c("prediction","se")
  for(i in breaker:nrow(testing_ready_pred)){
    for(l in 1:nLags){
      if(incidence_flag==T && death_flag==F){
        if(c(paste0(sprintf("death_cum_per_million_lLag_%02d", l))) %in% colnames(testing_ready_pred)){
          testing_ready_pred[i,c(paste0(sprintf("death_cum_per_million_lLag_%02d", l)))] <- testing_ready_pred[i-l,c(paste0(outcomeVariable))]
        }
      }
    }
    if(incidence_flag==T && death_flag==F){
      testing_ready_pred[i,c(paste0(outcomeVariable))] <- 0
      testing_ready_pred[i,c(paste0(outcomeVariable))] <- predictFunction(name=best_model, mod_name=model_name,dd=testing_ready_pred[i,], n_trees = number_trees)
    }
    if(i==breaker){
      pN <- predictFunction(name=best_model, mod_name=model_name,dd=testing_ready_pred[i,], n_trees = number_trees)
      pAll <- c(p1,pN)
    }else{
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
  plot1Data_tmp <- testing_ready_OG[,c("FullName","time",outcomeVariable)]
  
  #---plotPrediction---#########################################################################################################################################################################
  # Organize the data to be ready for ggplot
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
  
  # Reshape the data to be ready for ggplot
  m1 <- reshape2::melt(plot1Data,id=c("country","date"))
  m1$date <- as.numeric(m1$date)
  m1$value <- as.numeric(m1$value)
  m1$variable <- as.factor(m1$variable)
  m1$country <- as.factor(m1$country)
  str(m1)
  m1 <- m1[order(m1$date),]
  
  # Set the colors for red = actual, blue = prediction
  m1$date <- as.Date(m1$date)
  predictColor <- rep("deepskyblue2",2)
  alphabeticalList <- sort(c("Predict","Actual"))
  alphabeticalIndex <- which(alphabeticalList == "Actual")
  predictColor[alphabeticalIndex] <- "red"
  
  # make the plot
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
    scale_y_continuous(limits = c(0, NA))+
    geom_hline(yintercept=1, linetype="dashed", 
               color = "gray", size=2)+
    labs(x=NULL)
  
  #---All Country plot---#########################################################################################################################################################################
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
    # geom_glowing_line(aes(x = testing_ready$time[1:(nrow(testing_ready)-forecastingTime)], y = testing_ready$confirmed_cum_per_million[1:(nrow(testing_ready)-forecastingTime)], color = FullName), alpha = 1, size = 1, glow_alpha = 0.03)+
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
  
  #---R0 plot---#########################################################################################################################################################################
  # lineColors <- c("firebrick","darkgoldenrod1", "darkviolet", "limegreen", "dodgerblue")
  plot2 <- ggplot() 
  if(incidence_flag==T && death_flag==F){
    # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1) +
    # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 3, colour = 'red', alpha = 0.1) +
    # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 2, colour = 'red', alpha = 0.2) +
    # geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size = 1, colour = 'red', alpha = 0.5) +
    # geom_glowing_line(aes(x = testing_ready$time[1:(nrow(testing_ready)-forecastingTime)], y = testing_ready$confirmed_cum_per_million[1:(nrow(testing_ready)-forecastingTime)], color = FullName), alpha = 1, size = 1, glow_alpha = 0.03)+
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
  
  #---NPI plots---#########################################################################################################################################################################
  # lineColors <- c("firebrick","darkgoldenrod1", "darkviolet", "limegreen", "dodgerblue")
  plot34Data <- testing_ready_OG[1:(nrow(testing_ready_OG)-forecastingTime),c("date","R0","Social_Distancing","Quaranting_Cases","Close_Border","Google_Grocery_pharmacy","Google_Parks","Google_Residential","Google_Retail_recreation","Google_Transit_stations","Google_Workplaces")]
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
  # plot4 <- ggplot() 
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
  
  #---variableImportance Plot---#########################################################################################################################################################################
  
  if("rf.mod"== model_name){
    print("Model chosen by caret is: randomForest")
    df_tmp <- varImp(rf.mod)
    df <- as.data.frame(df_tmp$importance)
    colnames(df) = c('imp')
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
  rmsedf <- plot1Data[(breakpoint+1):(nrow(plot1Data)-forecastingTime),]
  rmse_val <- sqrt( sum( (rmsedf$Prediction - rmsedf$Actual)^2 ,na.rm = T) / (nrow(rmsedf)) )
  rmse_val <- round(rmse_val,3)
  gl <- list(plot1,plot2,plot_predict,plot_varimp,plot3,plot4)
  pdf(paste0("finalPlot_",testing_countries[1],"_DeathIncidence"),width = 24, height = 24)
  grid.arrange(grobs = gl, 
               top = textGrob(paste0(testing_ready$FullName[1],
                                     " ",
                                     model_name,
                                     " RMSE = ",
                                     rmse_val),
                              gp=gpar(fontsize=20)), 
               layout_matrix = rbind( c(1,1,1,1,1,4,4,4),
                                      c(1,1,1,1,1,4,4,4),
                                      c(1,1,1,1,1,4,4,4),
                                      c(1,1,1,1,1,4,4,4),
                                      c(1,1,1,1,1,4,4,4),
                                      c(2,2,2,2,5,5,5,5),
                                      c(2,2,2,2,5,5,5,5),
                                      c(2,2,2,2,5,5,5,5),
                                      c(2,2,2,2,5,5,5,5),
                                      c(2,2,2,2,5,5,5,5),
                                      c(3,3,3,3,6,6,6,6),
                                      c(3,3,3,3,6,6,6,6),
                                      c(3,3,3,3,6,6,6,6),
                                      c(3,3,3,3,6,6,6,6),
                                      c(3,3,3,3,6,6,6,6)))
  dev.off()
  #
  # plot(best_model)
  # 
  
  
  # --- Output variables for RShiny App -----
  # Saving on object in RData format
  save(plot1, plot_predict, plot2, plot3, plot4, best_model,  file = "./COVID-19_Shiny_Web_App/Inputs/USA_Cases_data.RData")
  
  
  
  # --- Correlation between NPI data and google data -----
  # look at the correlation matrix betweten the google mobility data and the data collected for NPIs
  # cerate dataframe
  NPI_google_df = tibble(data_clean[c("date", "ISO3","Google_Residential", "Google_Workplaces", "Google_Transit_stations",
                                      "Google_Parks", "Google_Grocery_pharmacy", "Google_Retail_recreation",
                                      "Social_Distancing", "Social_Distancing_Lag_03","Social_Distancing_Lag_07", "Social_Distancing_Lag_10", "Social_Distancing_Lag_14")]) #,
  # "Quaranting_Cases", "Quaranting_Cases_Lag_03", "Quaranting_Cases_Lag_07", "Quaranting_Cases_Lag_010", "Quaranting_Cases_Lag_14",
  # "Close_Border","Close_Border_Lag_03","Close_Border_Lag_07","Close_Border_Lag_010","Close_Border_Lag_14")])
  
  NPI_google_df <- NPI_google_df %>%
    drop_na() %>%
    select(-c(date, ISO3))
  glimpse(NPI_google_df)
  
  NPI_google_cor_mat <- cor(NPI_google_df)
  NPI_corrplot <- corrplot(NPI_google_cor_mat, method="circle", type="upper")

}
