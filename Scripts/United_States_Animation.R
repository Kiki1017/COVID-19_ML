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

'%ni%' <- Negate('%in%')
download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv","./InputData/Global_Mobility_Report.csv")
GoogData <- read.csv("./InputData/Global_Mobility_Report.csv", header=T, stringsAsFactors = F)
# Get just the USA country mobility stats
GoogData_sub <- subset(GoogData, country_region_code == "US" & sub_region_2 == "")
GoogData_sub$sub_region_1[GoogData_sub$sub_region_1==""] <- "USA"

names(GoogData_sub)
GoogData_sub2 <- GoogData_sub[,c("sub_region_1","date","retail_and_recreation_percent_change_from_baseline",
                                 "grocery_and_pharmacy_percent_change_from_baseline", 
                                 "parks_percent_change_from_baseline", 
                                 "transit_stations_percent_change_from_baseline",
                                 "workplaces_percent_change_from_baseline",
                                 "residential_percent_change_from_baseline")]
colnames(GoogData_sub2) <- c("Country.x","date","Google_Retail_recreation",
                             "Google_Grocery_pharmacy",
                             "Google_Parks",
                             "Google_Transit_stations",
                             "Google_Workplaces",
                             "Google_Residential")

# Merge the two dataframes by country name and date
GoogData_sub2$date <- as.Date(GoogData_sub2$date)
GoogData_sub2$Country.x <- as.character(GoogData_sub2$Country.x)
GoogData_sub2 <- as_tibble(GoogData_sub2)

# library(coronavirus)
# Update data?
# update_datasets()

# Explore the data
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv","./InputData/time_series_covid19_confirmed_US.csv")
CaseData <- read.csv("./InputData/time_series_covid19_confirmed_US.csv", header=T, stringsAsFactors = F)
CaseDataSub <- CaseData %>% select(Province_State,starts_with("X"))
for(j in 2:ncol(CaseDataSub)){
  d <- names(CaseDataSub)[j]
  d1 <- unlist(strsplit(d,"X"))[2]
  # dates in the format of: 2020-04-05
  cName <- format(as.Date(d1, format = "%m.%d.%y"),format = "%Y-%m-%d")
  jFrame <- aggregate(CaseDataSub[[d]], by=list(Country.x=CaseDataSub$Province_State), FUN=sum)
  colnames(jFrame) <- c("Country.x",cName)
  if(j == 2){
    CaseDataSubAgg <- jFrame
  }else{
    CaseDataSubAgg <- merge(CaseDataSubAgg, jFrame, by="Country.x")
  }
}
head(CaseDataSubAgg)
CaseDataSubAggDiff <- CaseDataSubAgg
for(j in 3:ncol(CaseDataSubAgg)){
  CaseDataSubAggDiff[,j] <- CaseDataSubAgg[,(j)] - CaseDataSubAgg[,(j-1)]
}
for(i in 1:nrow(CaseDataSubAggDiff)){
  window <- 7
  ts <- unlist(CaseDataSubAggDiff[i,2:ncol(CaseDataSubAggDiff)])
  CaseDataSubAggDiff[i,2:ncol(CaseDataSubAggDiff)] <- c(ts[1:((window-1)/2)],rollmean(ts, k=window, align = "center"),ts[(length(ts)-((window-1)/2)+1):length(ts)])
}
CaseDataSubAggMelt <- melt(CaseDataSubAggDiff, id="Country.x")
colnames(CaseDataSubAggMelt) <- c("Country.x", "date", "confirmed")
CaseDataSubAggMelt$date <- as.Date(CaseDataSubAggMelt$date)
str(CaseDataSubAggMelt)
str(GoogData_sub2)
merge1 <- GoogData_sub2 %>% full_join(CaseDataSubAggMelt, by = c("Country.x" = "Country.x", "date" = "date"))

download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv","./InputData/time_series_covid19_deaths_US.csv")
DeathData <- read.csv("./InputData/time_series_covid19_deaths_US.csv", header=T, stringsAsFactors = F)
DeathDataSub <- DeathData %>% select(Province_State,starts_with("X"))
for(j in 2:ncol(DeathDataSub)){
  d <- names(DeathDataSub)[j]
  d1 <- unlist(strsplit(d,"X"))[2]
  # dates in the format of: 2020-04-05
  cName <- format(as.Date(d1, format = "%m.%d.%y"),format = "%Y-%m-%d")
  jFrame <- aggregate(DeathDataSub[[d]], by=list(Country.x=DeathDataSub$Province_State), FUN=sum)
  colnames(jFrame) <- c("Country.x",cName)
  if(j == 2){
    DeathDataSubAgg <- jFrame
  }else{
    DeathDataSubAgg <- merge(DeathDataSubAgg, jFrame, by="Country.x")
  }
}
head(DeathDataSubAgg)
DeathDataSubAggDiff <- DeathDataSubAgg
for(j in 3:ncol(DeathDataSubAgg)){
  DeathDataSubAggDiff[,j] <- DeathDataSubAgg[,(j)] - DeathDataSubAgg[,(j-1)]
}
for(i in 1:nrow(DeathDataSubAggDiff)){
  window <- 7
  ts <- unlist(DeathDataSubAggDiff[i,2:ncol(DeathDataSubAggDiff)])
  DeathDataSubAggDiff[i,2:ncol(DeathDataSubAggDiff)] <- c(ts[1:((window-1)/2)],rollmean(ts, k=window, align = "center"),ts[(length(ts)-((window-1)/2)+1):length(ts)])
}
DeathDataSubAggMelt <- melt(DeathDataSubAggDiff, id="Country.x")
colnames(DeathDataSubAggMelt) <- c("Country.x", "date", "deaths")
DeathDataSubAggMelt$date <- as.Date(DeathDataSubAggMelt$date)
str(DeathDataSubAggMelt)
str(GoogData_sub2)
merge2 <- merge1 %>% full_join(DeathDataSubAggMelt, by = c("Country.x" = "Country.x", "date" = "date"))


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

merge3 <- subset(merge2, Country.x %ni% c("American Samoa","Diamond Princess","Grand Princess","USA","Guam","Northern Mariana Islands","Puerto Rico","Virgin Islands"))
training_countries <- unique(merge3$Country.x)

pdf("R0_plot.pdf",width = 11,height = 8.5)
for(i in 1:length(training_countries)){
  training_subset <- subset(merge3,Country.x %in% training_countries[i])
  training_subset <- training_subset[order(training_subset$date),]
  
  # start <- which(training_subset$confirmed > 0)[1]
  start <- which(training_subset$date == "2020-01-22")
  training_subset_aligned <- training_subset[start:nrow(training_subset),]
  
  # Smooth out incidence using moving average with a centered window of 7 datapoints (3 to the left, 3 to the right)
  # make sure the window is an odd integer
  window <- 7
  # dim(training_subset_aligned)
  # length(rollmean(training_subset_aligned$confirmed, k=window))
  training_subset_aligned$movingAverage <- training_subset_aligned$confirmed
  # training_subset_aligned$movingAverage <- c(training_subset_aligned$confirmed[1:((window-1)/2)],rollmean(training_subset_aligned$confirmed, k=window, align = "center"),training_subset_aligned$confirmed[(nrow(training_subset_aligned)-((window-1)/2)+1):nrow(training_subset_aligned)])
  # training_subset_aligned$confirmedMA <- c(training_subset_aligned$confirmed[1:((window-1)/2)],rollmean(training_subset_aligned$confirmed, k=window, align = "center"),training_subset_aligned$confirmed[(nrow(training_subset_aligned)-((window-1)/2)+1):nrow(training_subset_aligned)])
  # training_subset_aligned$deathsMA <- c(training_subset_aligned$deaths[1:((window-1)/2)],rollmean(training_subset_aligned$deaths, k=window, align = "center"),training_subset_aligned$deaths[(nrow(training_subset_aligned)-((window-1)/2)+1):nrow(training_subset_aligned)])
  
  # Plot cases
  gg <- ggplot(training_subset_aligned) +
    geom_line(aes(x=date, y=confirmed),color="red") +
    # geom_line(aes(x=date, y=movingAverage),color="blue") +
    ggtitle(paste0("Reported cases in ", training_countries[i]))
  print(gg)
  # Add moving average day lag and one day difference variables

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

training_ready_OG <- training_ready

#--Autofill---#########################################################################################################################################################################
training_manipulate <- training_ready_OG

peek_at_NPIs_training1 <- training_manipulate[,c(c("date","Country.x","confirmed","movingAverage"),names(training_manipulate)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(training_manipulate))])]
NPInames <- names(training_manipulate)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(training_manipulate))]
# View(training_manipulate[,c(NPInames)])
counter <- 1
prevcountry <- training_manipulate$Country.x[1]
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
peek_at_NPIs_training2 <- training_manipulate[,c(c("date","Country.x","confirmed","deaths","movingAverage"),names(training_manipulate)[grep("Social_Distancing|Quaranting_Cases|Close_Border|Google|R0",names(training_manipulate))])]

both <- peek_at_NPIs_training2
npiDensityPlotData <- both[c("date", "Country.x","confirmed","deaths","movingAverage","R0","Google_Residential", "Google_Workplaces", "Google_Transit_stations",
                             "Google_Parks", "Google_Grocery_pharmacy", "Google_Retail_recreation")]

#---NPI Density Animation---#########################################################################################################################################################################
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



dateRange <- seq(from=min(merge2$date,na.rm=T), to=max(merge2$date,na.rm=T), length.out = 23)
dateSplits <- seq(from=11, to=23, length.out = 4)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
NPIplotAnimation <- function(myNPI = npiList[1], myDate = as.Date("2020-03-28"), title = NA, xx = NA, merge2 = both, windowSide = 7){
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
  if(myNPI %in% c("confirmed","deaths")){
    tmpy <- subset(merge2, Country.x != "USA" )
    both <- tmpy
  }else{
    both <- merge2
  }
  
  startDate <- myDate-windowSide
  endDate <- myDate+windowSide
  bothSub <- subset(both,date<=endDate & date>=startDate)
  npiDensityPlotData <- bothSub[c("date", "Country.x",myNPI)]
  # tmpy <- subset(npiDensityPlotData, Country.x == "US"); tmpy$Country.x <- "ZZZ"; tmpy[[myNPI]] <- 0
  # npiDensityPlotData <- rbind(npiDensityPlotData,tmpy)
  npiDensityPlotDataMelted5 <- melt(npiDensityPlotData, id = c("date", "Country.x"))
  npiDensityPlotDataMelted5$Country.x <- as.character(npiDensityPlotDataMelted5$Country.x)
  Clist1 <- unique(sort(npiDensityPlotDataMelted5$Country.x))
  # if(myNPI %in% c("confirmed","deaths","R0")){
  Clist <- Clist1
  # }else{
  #   Clist <- c(Clist1[which(Clist1!="USA")], "USA")
  # }
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
  if(myNPI == "R0"){
    npi5 <- npi5 + geom_vline(xintercept=1, linetype="dashed", color = "#3b3620", size = .85, alpha = 0.6)
  }
  # npi5
  # Make commmon axis
  minx <- min(both[[myNPI]],na.rm=T)
  maxx <- max(both[[myNPI]],na.rm=T)
  lowerRange <- minx-(maxx-minx)*.15
  upperRange <- maxx+(maxx-minx)*.15
  outerBound <- c(abs(lowerRange),abs(upperRange))[which.max(c(abs(lowerRange),abs(upperRange)))]

    if(myNPI %in% c("confirmed","deaths")){
      minx <- min(both[[myNPI]],na.rm=T)
      maxx <- max(both[[myNPI]],na.rm=T)
      lowerRange <- minx-(maxx-minx)*.02
      upperRange <- maxx+(maxx-minx)*.02
      outerBound <- c(abs(lowerRange),abs(upperRange))[which.max(c(abs(lowerRange),abs(upperRange)))]
      # # npi1 <- npi1 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      # # npi2 <- npi2 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      # # npi3 <- npi3 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      # # npi4 <- npi4 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      npi5 <- npi5 + scale_x_continuous(expand = c(0.01, 0), limits = c(0,upperRange)) + scale_fill_gradientn(colours = rev(c("#D7191C","#FDAE61","#ABDDA4","#2B83BA","white")),name = "", values = rescale(seq(from=0,to=outerBound,length.out=5), to = c(0, 1)), limits=c(0,outerBound))+
        scale_point_color_gradient(limits = c(0, outerBound), oob = scales::squish, name = "", low = "#787a3a", high = "#787a3a")
    }else if(myNPI %in% c("R0")){
      minx <- min(both[[myNPI]],na.rm=T)
      maxx <- max(both[[myNPI]],na.rm=T)
      lowerRange <- minx-(maxx-minx)*.02
      upperRange <- maxx+(maxx-minx)*.02
      outerBound <- c(abs(lowerRange),abs(upperRange))[which.max(c(abs(lowerRange),abs(upperRange)))]
      # # npi1 <- npi1 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      # # npi2 <- npi2 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      # # npi3 <- npi3 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      # # npi4 <- npi4 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      npi5 <- npi5 + scale_x_continuous(expand = c(0.01, 0), limits = c(0,10)) + scale_fill_gradientn(colours = rev(c("#57090a","#D7191C","#e0ce00","#2B83BA","#0d2c40")),name = "", values = rescale(c(0,.8,1.2,outerBound), to = c(0, 1)), limits=c(0,outerBound))+
        scale_point_color_gradient(limits = c(0, outerBound), oob = scales::squish, name = "", low = "#787a3a", high = "#787a3a")
    }else{
      # npi1 <- npi1 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
      # npi2 <- npi2 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
      # npi3 <- npi3 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
      # npi4 <- npi4 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
      npi5 <- npi5 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradientn(colours = rev(c("#D7191C","#FDAE61","white","#ABDDA4","#2B83BA")),name = "", values = rescale(seq(from=-outerBound,to=outerBound,length.out=5), to = c(0, 1)), limits=c(-outerBound, outerBound))+
        scale_point_color_gradient(limits = c(0, outerBound), oob = scales::squish, name = "", low = "#787a3a", high = "#787a3a")
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
                        top = textGrob(title_paste, gp=gpar(fontsize=13)), 
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
d_dRange <- as.Date(c(as.Date(min(merge2$date,na.rm=T)):as.Date(max(merge2$date,na.rm=T))))
# d_dRange <- as.Date(c(as.Date("2020-04-05"):as.Date("2020-04-15")))

# d_dRange <- seq(from=as.Date("2020-04-05"), to=as.Date("2020-04-13"), length.out = 8)


npiList <- c("R0",
             "confirmed",
             "deaths",
             "Google_Residential",
             "Google_Workplaces",
             "Google_Transit_stations",
             "Google_Parks",
             "Google_Grocery_pharmacy",
             "Google_Retail_recreation"
            )

titleList <- c("15 Day Sliding Window of Calculated R0",
               "15 Day Sliding Window of Confirmed New Daily Cases",
               "15 Day Sliding Window of Confirmed New Daily Deaths",
               "15 Day Sliding Window of % Change in Visits to Residential Areas", 
               "15 Day Sliding Window of % Change in Visits to Workplaces", 
               "15 Day Sliding Window of % Change in Visits to Transit Stations",
               "15 Day Sliding Window of % Change in Visits to Parks", 
               "15 Day Sliding Window of % Change in Visits to Grocery & Pharmacy", 
               "15 Day Sliding Window of % Change in Visits to Retail & Recreation"
              )

xxList <- c("R0",
            "Confirmed New Daily Cases",
            "Confirmed New Daily Deaths",
            "% Change in Visits to Residential Areas", 
            "% Change in Visits to Workplaces", 
            "% Change in Visits to Transit Stations",
            "% Change in Visits to Parks", 
            "% Change in Visits to Grocery & Pharmacy", 
            "% Change in Visits to Retail & Recreation"
            )

for(n_n in 3:length(npiList)){
# for(n_n in length(npiList)){
  print(n_n)
  print(npiList[n_n])
  saveGIF({
    for (d_d in 1:length(d_dRange)){
      NPIplotAnimation(myNPI = npiList[n_n], myDate = d_dRange[d_d], title = titleList[n_n], xx = xxList[n_n], merge2 = both, windowSide = 7)}
  }, interval = .2, movie.name=paste0("USA",npiList[n_n],".gif"), ani.width = 700, ani.height = 900, ani.res = 100)
closeAllConnections()
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

dev.off()

