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
library(reshape2)
# devtools::install_github("delabj/ggCyberPunk")
library(ggCyberPunk)
library(corrplot)
#
library(gridExtra)
library(grid)
library(ggpubr)
library(ggridges)
library(scales)
'%ni%' <- Negate('%in%')

#---Flag setup---#########################################################################################################################################################################

# TRUE if you want to scale by population
incidence_flag <- T
# TRUE if you want to do deaths instead of cases
death_flag <- F
incidence_start_point <- 00
count_start_point <- 0
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
# for(cc in 1:length(testing_countriesList)){
cc=1


# Choose the testing country
testing_countries <- c(testing_countriesList[cc])

# make country lists, these are the ones that we have NPI data collected for
# https://docs.google.com/spreadsheets/d/1vrKvs52OAxuB7x2kT9r1q6IcIBxGEQsNRHsK_o7h3jo/edit#gid=378237553
# training_countries_all <- c("ITA","FRA","GBR")
# training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","USA","SWE","AUT","CHE","DEU","FRA","DZA","ISR")
# training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","PRT","ISR","RUS","NOR","AUS","DNK","CHL","CZE","JPN","UKR","MAR","ARG")
training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","HUB","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","TUR","BEL","ANT","PRT","ISR","RUS","NOR","IRL","AUS","IND","DNK","CHL","CZE","JPN","UKR","MAR","ARG","SGP","ROU")
# training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","TUR","BEL","ANT","PRT","ISR","RUS","NOR","IRL","AUS","IND","DNK","CHL","CZE","JPN","UKR","MAR","ARG","SGP","ROU")

# training_countries <- training_countries_all[which(training_countries_all != testing_countries)]
training_countries <- training_countries_all

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
  # length(rollmean(training_subset_aligned$confirmed, k=window))
  training_subset_aligned$movingAverage <- c(training_subset_aligned$confirmed[1:((window-1)/2)],rollmean(training_subset_aligned$confirmed, k=window, align = "center"),training_subset_aligned$confirmed[(nrow(training_subset_aligned)-((window-1)/2)+1):nrow(training_subset_aligned)])
  
  training_subset_aligned$confirmedMA <- c(training_subset_aligned$confirmed[1:((window-1)/2)],rollmean(training_subset_aligned$confirmed, k=window, align = "center"),training_subset_aligned$confirmed[(nrow(training_subset_aligned)-((window-1)/2)+1):nrow(training_subset_aligned)])
  training_subset_aligned$deathsMA <- c(training_subset_aligned$death[1:((window-1)/2)],rollmean(training_subset_aligned$death, k=window, align = "center"),training_subset_aligned$death[(nrow(training_subset_aligned)-((window-1)/2)+1):nrow(training_subset_aligned)])
  
  # Plot cases
  gg <- ggplot(training_subset_aligned) +
    geom_line(aes(x=date, y=confirmed),color="red") +
    geom_line(aes(x=date, y=movingAverage),color="blue") +
    ggtitle(paste0("Reported cases in ", training_countries[i]))
  print(gg)
  # Add moving average day lag and one day difference variables
  training_subset_aligned[["movingAverage_Lag_1"]] <- lag(training_subset_aligned[["movingAverage"]],1)
  training_subset_aligned[["movingAverage_Lag_3"]] <- lag(training_subset_aligned[["movingAverage"]],3)
  training_subset_aligned[["movingAverage_Lag_7"]] <- lag(training_subset_aligned[["movingAverage"]],7)
  training_subset_aligned[["movingAverage_Lag_14"]] <- lag(training_subset_aligned[["movingAverage"]],14)
  training_subset_aligned[["movingAverage_diff_1_3"]] <- training_subset_aligned[["movingAverage_Lag_1"]] - training_subset_aligned[["movingAverage_Lag_3"]]
  training_subset_aligned[["movingAverage_diff_1_7"]] <- training_subset_aligned[["movingAverage_Lag_1"]] - training_subset_aligned[["movingAverage_Lag_7"]]
  training_subset_aligned[["movingAverage_diff_1_14"]] <- training_subset_aligned[["movingAverage_Lag_1"]] - training_subset_aligned[["movingAverage_Lag_14"]]
  training_subset_aligned[["movingAverage_diff_3_7"]] <- training_subset_aligned[["movingAverage_Lag_3"]] - training_subset_aligned[["movingAverage_Lag_7"]]
  training_subset_aligned[["movingAverage_diff_7_14"]] <- training_subset_aligned[["movingAverage_Lag_7"]] - training_subset_aligned[["movingAverage_Lag_14"]]
  
  
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
dev.off()

# preserve the original dataframes before we make changes to them...
# testing_ready <- testing_ready_OG
# training_ready <- training_ready_OG
training_ready_OG <- training_ready

#---NPI Density Animation---#########################################################################################################################################################################
training_manipulate <- training_ready_OG

peek_at_NPIs_training1 <- training_manipulate[,c(c("date","time","Country.x","ISO3","confirmed","movingAverage"),names(training_manipulate)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(training_manipulate))])]
NPInames <- names(training_manipulate)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(training_manipulate))]
# View(training_manipulate[,c(NPInames)])
counter <- 1
prevcountry <- training_manipulate$Country.x[1]
if(NPIflag1 == "autofill"){
  for(i in 2:nrow(training_manipulate)){
    curcountry <- training_manipulate$Country.x[i]
    if(curcountry == prevcountry){
      counter <- counter+1
    }else{
      counter <- 1
    }
    
    for(j in NPInames){
      if(is.na(training_manipulate[[j]][i]) && counter > 14){
        training_manipulate[[j]][i] <- training_manipulate[[j]][i-1]
      }
    }
    prevcountry <- curcountry
  }
}
peek_at_NPIs_training2 <- training_manipulate[,c(c("date","time","Country.x","ISO3","confirmed","confirmedMA","death","deathMA","movingAverage","confirmed_cum","death_cum"),names(training_manipulate)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(training_manipulate))])]

both <- peek_at_NPIs_training2
npiDensityPlotData <- both[c("date", "Country.x","confirmed","death","movingAverage","confirmed","death","Google_Residential", "Google_Workplaces", "Google_Transit_stations",
                             "Google_Parks", "Google_Grocery_pharmacy", "Google_Retail_recreation",
                             "Social_Distancing", "Quaranting_Cases", "Close_Border")]

# both <- subset(both, Country.x != "US")

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
unlink(paste0("./Output_CaseIncidence/npidensAnimation_","CaseIncidence.pdf"))
pdf(paste0("./Output_CaseIncidence/npidensAnimation_","CaseIncidence.pdf"),width = 8, height = 14)

dateRange <- seq(from=min(both$date,na.rm=T), to=max(both$date,na.rm=T), length.out = 23)
dateSplits <- seq(from=11, to=23, length.out = 4)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
NPIplotAnimation <- function(myNPI = npiList[1], myDate = as.Date("2020-03-28"), title = NA, xx = NA){
  # startDate <- dateRange[1]
  # endDate <- dateRange[dateSplits[1]]
  # bothSub <- subset(both,date<=endDate & date>=startDate)
  # npiDensityPlotData <- bothSub[c("date", "Country.x",myNPI)]
  # npiDensityPlotDataMelted1 <- melt(npiDensityPlotData, id = c("date", "Country.x"))
  # npi1 <- ggplot(npiDensityPlotDataMelted1, aes(x = `value`, y = `Country.x`, fill = ..x..)) +
  #   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 3.) +
  #   scale_y_discrete(expand = c(0.0005, 0)) +
  #   scale_fill_viridis(name = "", option = "C") +
  #   labs(title = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")),
  #        subtitle = '')+
  #   xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))+
  #   theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
  # 
  # startDate <- dateRange[dateSplits[1]]
  # endDate <- dateRange[dateSplits[2]]
  # bothSub <- subset(both,date<=endDate & date>startDate)
  # npiDensityPlotData <- bothSub[c("date", "Country.x",myNPI)]
  # npiDensityPlotDataMelted2 <- melt(npiDensityPlotData, id = c("date", "Country.x"))
  # npi2 <- ggplot(npiDensityPlotDataMelted2, aes(x = `value`, y = `Country.x`, fill = ..x..)) +
  #   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 3.) +
  #   scale_y_discrete(expand = c(0.0005, 0)) +
  #   scale_fill_viridis(name = "", option = "C") +
  #   labs(title = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")),
  #        subtitle = '')+
  #   xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))+
  #   theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
  # 
  # startDate <- dateRange[dateSplits[2]]
  # endDate <- dateRange[dateSplits[3]]
  # bothSub <- subset(both,date<=endDate & date>startDate)
  # npiDensityPlotData <- bothSub[c("date", "Country.x",myNPI)]
  # npiDensityPlotDataMelted3 <- melt(npiDensityPlotData, id = c("date", "Country.x"))
  # npi3 <- ggplot(npiDensityPlotDataMelted3, aes(x = `value`, y = `Country.x`, fill = ..x..)) +
  #   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 3.) +
  #   scale_y_discrete(expand = c(0.0005, 0)) +
  #   scale_fill_viridis(name = "", option = "C") +
  #   labs(title = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")),
  #        subtitle = '')+
  #   xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))+
  #   theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
  # 
  # startDate <- dateRange[dateSplits[3]]
  # endDate <- dateRange[dateSplits[4]]
  # bothSub <- subset(both,date<=endDate & date>=startDate)
  # npiDensityPlotData <- bothSub[c("date", "Country.x",myNPI)]
  # npiDensityPlotDataMelted4 <- melt(npiDensityPlotData, id = c("date", "Country.x"))
  # npi4 <- ggplot(npiDensityPlotDataMelted4, aes(x = `value`, y = `Country.x`, fill = ..x..)) +
  #   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 3.) +
  #   scale_y_discrete(expand = c(0.0005, 0)) +
  #   scale_fill_viridis(name = "", option = "C") +
  #   labs(title = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")),
  #        subtitle = '')+
  #   xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))+
  #   theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
  
  startDate <- myDate-5
  endDate <- myDate+5
  bothSub <- subset(both,date<=endDate & date>=startDate)
  npiDensityPlotData <- bothSub[c("date", "Country.x",myNPI)]
  # tmpy <- subset(npiDensityPlotData, Country.x == "US"); tmpy$Country.x <- "ZZZ"; tmpy[[myNPI]] <- 0
  # npiDensityPlotData <- rbind(npiDensityPlotData,tmpy)
  npiDensityPlotDataMelted5 <- melt(npiDensityPlotData, id = c("date", "Country.x"))
  npiDensityPlotDataMelted5$Country.x <- as.character(npiDensityPlotDataMelted5$Country.x)
  Clist1 <- unique(sort(npiDensityPlotDataMelted5$Country.x))
  Clist <- c(Clist1[which(Clist1!="US")], "US")
  npiDensityPlotDataMelted5$Country.x.num <- NA
  for(c in 1:nrow(npiDensityPlotDataMelted5)){
    myNum <- which(Clist == npiDensityPlotDataMelted5$Country.x[c])
    npiDensityPlotDataMelted5$Country.x.num[c] <- myNum
  }
  npiDensityPlotDataMelted5 <- npiDensityPlotDataMelted5[order(npiDensityPlotDataMelted5$Country.x.num),] 
  npi5 <- ggplot(npiDensityPlotDataMelted5, aes(x = `value`, y = `Country.x.num`, group = `Country.x.num`, fill = ..x.., point_color = ..x..)) +
    # npi5 <- ggplot(npiDensityPlotDataMelted5, aes(x = `value`, y = `Country.x`, group = `Country.x`, fill = ..x..)) +
    # geom_density_ridges_gradient() +
    # geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, gradient_lwd = 3., panel_scaling=F, bandwidth = 3) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, gradient_lwd = 3,
                                 jittered_points = TRUE,
                                 point_shape = "|", point_size = 3, size = 0.25,
                                 position = position_points_jitter(height = 0)
    ) +
    # scale_y_discrete(expand = c(0, 0)) +
    # scale_y_continuous(breaks=c(1:length(Clist)),labels=c(Clist[1:(length(Clist)-1)],""),expand=c(0.0,2)) +
    scale_y_continuous(breaks=c(1:length(Clist)),labels=Clist,expand=c(0.0,.5)) +
    scale_fill_viridis(name = "", option = "C") +
    labs(title = paste0(format(myDate, format="%B %d")), subtitle = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")))+
    theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank(), plot.title = element_text(margin = unit(c(0,0,0,0), "cm")))
  if(is.na(xx)){
    npi5 <- npi5 + xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))
  }else{
    npi5 <- npi5 + xlab(xx)
  }
  # Make commmon axis
  minx <- min(both[[myNPI]],na.rm=T)
  maxx <- max(both[[myNPI]],na.rm=T)
  lowerRange <- minx-(maxx-minx)*.15
  upperRange <- maxx+(maxx-minx)*.15
  outerBound <- c(abs(lowerRange),abs(upperRange))[which.max(c(abs(lowerRange),abs(upperRange)))]
  if((upperRange-lowerRange)>10){
    # npi1 <- npi1 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
    # npi2 <- npi2 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
    # npi3 <- npi3 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
    # npi4 <- npi4 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
    npi5 <- npi5 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(-outerBound, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))+
      scale_point_color_gradient(limits = c(-outerBound, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
    if(myNPI %in% c("confirmed","death")){
      minx <- min(both[[myNPI]],na.rm=T)
      maxx <- max(both[[myNPI]],na.rm=T)
      lowerRange <- minx-(maxx-minx)*.02
      upperRange <- maxx+(maxx-minx)*.02
      outerBound <- c(abs(lowerRange),abs(upperRange))[which.max(c(abs(lowerRange),abs(upperRange)))]
      # # npi1 <- npi1 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      # # npi2 <- npi2 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      # # npi3 <- npi3 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      # # npi4 <- npi4 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      npi5 <- npi5 + scale_x_continuous(expand = c(0.01, 0), limits = c(0,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
    }
  }else{
    if(myNPI == "Social_Distancing"){
      # npi1 <- npi1 + scale_x_continuous(expand = c(0.01, 0), limits = c(-4, 12), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
      # npi2 <- npi2 + scale_x_continuous(expand = c(0.01, 0), limits = c(-4, 12), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
      # npi3 <- npi3 + scale_x_continuous(expand = c(0.01, 0), limits = c(-4, 12), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
      # npi4 <- npi4 + scale_x_continuous(expand = c(0.01, 0), limits = c(-4, 12), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
      npi5 <- npi5 + scale_x_continuous(expand = c(0.01, 0), limits = c(-4, 12), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
    }else{
      # npi1 <- npi1 + scale_x_continuous(expand = c(0.01, 0), limits = c(-2, 7.5), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
      # npi2 <- npi2 + scale_x_continuous(expand = c(0.01, 0), limits = c(-2, 7.5), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
      # npi3 <- npi3 + scale_x_continuous(expand = c(0.01, 0), limits = c(-2, 7.5), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
      # npi4 <- npi4 + scale_x_continuous(expand = c(0.01, 0), limits = c(-2, 7.5), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
      npi5 <- npi5 + scale_x_continuous(expand = c(0.01, 0), limits = c(-2, 7.5), breaks=c(0,1,2,3,4,5)) + scale_fill_viridis(alpha= 1, limits = c(lowerRange, upperRange), oob = scales::squish, name = "", option = "D")
    }
  }
  
  if(is.na(title)){
    if(myNPI == "movingAverage"){
      title_paste <- paste0("New Daily Cases Density")
    }else{
      title_paste <- paste0(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" "))," Density")
    }
  }else{
    title_paste <- title
  }

  
  gl <- list(npi5)
  final <- grid.arrange(grobs = gl, 
                        top = textGrob(title_paste, gp=gpar(fontsize=18)), 
                        layout_matrix = rbind( c(1)),
                        common.legend = TRUE, legend="bottom"
  )
  print(final)
}

# print(NPIplotter(myNPI = npiList[1]))


# for(d_d in 1:length(d_dRange)){
#   # for(n_n in 7){
#   print("Google_Workplaces")
#   print(d_d)
#   print(d_dRange[d_d])
#   print(NPIplotAnimation(myNPI = "Google_Workplaces", myDate = d_dRange[d_d]))
# }


# install.packages("magick")
# install.packages("animation")
library(animation)
library(magick)
# d_dRange <- as.Date(c(as.Date(min(both$date,na.rm=T)):as.Date(max(both$date,na.rm=T))))
d_dRange <- as.Date(c(as.Date("2020-02-15"):as.Date("2020-05-03")))
# d_dRange <- as.Date(c(as.Date("2020-03-15"):as.Date("2020-03-30")))

# d_dRange <- seq(from=as.Date("2020-04-05"), to=as.Date("2020-04-13"), length.out = 8)

npiList <- c("confirmedMA",
             "deathMA",
             "Google_Residential", 
             "Google_Workplaces", 
             "Google_Transit_stations",
             "Google_Parks", 
             "Google_Grocery_pharmacy", 
             "Google_Retail_recreation",
             "Social_Distancing", 
             "Quaranting_Cases", 
             "Close_Border")

titleList <- c("11 Day Sliding Window of Confirmed New Daily Cases",
               "11 Day Sliding Window of Confirmed New Daily Deaths",
               "11 Day Sliding Window of Percent Change in Visits to Residential Areas", 
               "11 Day Sliding Window of Percent Change in Visits to Workplaces", 
               "11 Day Sliding Window of Percent Change in Visits to Transit Stations",
               "11 Day Sliding Window of Percent Change in Visits to Parks", 
               "11 Day Sliding Window of Percent Change in Visits to Grocery & Pharmacy", 
               "11 Day Sliding Window of Percent Change in Visits to Retail & Recreation",
               "11 Day Sliding Window of Social Distancing Scale", 
               "11 Day Sliding Window of Quarantining Cases Scale", 
               "11 Day Sliding Window of Restricting the Border Scale")

xxList <- c("Confirmed New Daily Cases",
               "Confirmed New Daily Deaths",
               "Percent Change in Visits to Residential Areas", 
               "Percent Change in Visits to Workplaces", 
               "Percent Change in Visits to Transit Stations",
               "Percent Change in Visits to Parks", 
               "Percent Change in Visits to Grocery & Pharmacy", 
               "Percent Change in Visits to Retail & Recreation",
               "Social Distancing Scale", 
               "Quarantining Cases Scale", 
               "Restricting the Border Scale")

# for(n_n in 1:length(npiList)){
for(n_n in 1:1){
  print(n_n)
  print(npiList[n_n])
  saveGIF({
    for (d_d in 1:length(d_dRange)){
      NPIplotAnimation(myNPI = npiList[n_n], myDate = d_dRange[d_d], title = titleList[n_n], xx = xxList[n_n])}
  }, interval = .2, movie.name=paste0(npiList[n_n],".gif"),ani.width = 600, ani.height = 750)
}





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

dev.off()

