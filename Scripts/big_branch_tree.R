


#---Estimating reproduction numbers, R0---#####
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

#### Calculate the mean serial intervals ----

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

#### Construct a R_t timeseries for Regional breakdowns ----
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

# setwd("/Users/chris/Documents/COVID-19_ML")
data_clean <- read.csv("./InputData/ML_features.csv")
data_clean$date <- as.Date(data_clean$date)
focusCountry <- "IRN"
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


training_subset <- subset(data_clean,ISO3 %in% focusCountry)
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
res_uncertain_si <- estimate_R(toCalcR0,
                               method = "uncertain_si",
                               config = config)
# Default config will estimate R on weekly sliding windows.
# To change this change the t_start and t_end arguments.
str(res_uncertain_si)
res_uncertain_si[["R"]]$`Mean(R)`
plot(res_uncertain_si, legend = FALSE) 


