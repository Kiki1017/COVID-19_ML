# Data Cleaning
#
# Written by: Jay H Arehart
# Last Updated: Mar 22, 2020
#
#
# Script Description:
#   Clean the data and add all to be predictor variables
#
# Output
#   .csv file of features implementation in ML workflow

# Import packages
library(dplyr)
library(tidyr)
library(lubridate)

pre_autofill_Google = T
post_autofill_Google = T

# Load in raw data

raw_data_COVID <- read.csv('./InputData/data_COVID_2020_04_02.csv')

raw_data_static <- read.csv('./InputData/data_static_vars.csv')

## ---- COVID-related data cleaning and processing ----

# Manipulate original datasets (e.g., normalizing)
data_COVID <- raw_data_COVID %>%
  select(-X) %>%
  mutate(date = ymd(date))
  # mutate(date = as.Date(as.character(date), format = "%m/%d/%Y"))


## ---- Static dataset cleaning and processing ----
data_static <- raw_data_static %>%
  # normalizing the male count by age group
  mutate(age_0_14_MalePercent = age_0_14_MaleCount / (age_0_14_MaleCount +age_15_24_MaleCount +
                                                        age_25_54_MaleCount + age_55_64_MaleCount + 
                                                        age_65_plus_MaleCount)) %>%
  mutate(age_15_24_MalePercent = age_15_24_MaleCount / (age_0_14_MaleCount +age_15_24_MaleCount +
                                                        age_25_54_MaleCount + age_55_64_MaleCount + 
                                                        age_65_plus_MaleCount)) %>%
  mutate(age_25_54_MalePercent = age_25_54_MaleCount / (age_0_14_MaleCount +age_15_24_MaleCount +
                                                         age_25_54_MaleCount + age_55_64_MaleCount + 
                                                         age_65_plus_MaleCount)) %>%
  mutate(age_55_64_MalePercent = age_55_64_MaleCount / (age_0_14_MaleCount +age_15_24_MaleCount +
                                                         age_25_54_MaleCount + age_55_64_MaleCount + 
                                                         age_65_plus_MaleCount)) %>%
  mutate(age_65_plus_MalePercent = age_65_plus_MaleCount / (age_0_14_MaleCount +age_15_24_MaleCount +
                                                         age_25_54_MaleCount + age_55_64_MaleCount + 
                                                         age_65_plus_MaleCount)) %>%
  # normalizing the female count by age group
  mutate(age_0_14_FemalePercent = age_0_14_FemaleCount / (age_0_14_FemaleCount +age_15_24_FemaleCount +
                                                            age_25_54_FemaleCount + age_55_64_FemaleCount + 
                                                            age_65_plus_FemaleCount)) %>%
  mutate(age_15_24_FemalePercent = age_15_24_FemaleCount / (age_0_14_FemaleCount +age_15_24_FemaleCount +
                                                              age_25_54_FemaleCount + age_55_64_FemaleCount + 
                                                              age_65_plus_FemaleCount)) %>%
  mutate(age_25_54_FemalePercent = age_25_54_FemaleCount / (age_0_14_FemaleCount +age_15_24_FemaleCount +
                                                              age_25_54_FemaleCount + age_55_64_FemaleCount + 
                                                              age_65_plus_FemaleCount)) %>%
  mutate(age_55_64_FemalePercent = age_55_64_FemaleCount / (age_0_14_FemaleCount +age_15_24_FemaleCount +
                                                              age_25_54_FemaleCount + age_55_64_FemaleCount + 
                                                              age_65_plus_FemaleCount)) %>%
  mutate(age_65_plus_FemalePercent = age_65_plus_FemaleCount / (age_0_14_FemaleCount +age_15_24_FemaleCount +
                                                                  age_25_54_FemaleCount + age_55_64_FemaleCount + 
                                                                  age_65_plus_FemaleCount)) %>%
  # drop count variables
  select(-c(age_0_14_MaleCount, age_15_24_MaleCount, age_25_54_MaleCount, age_55_64_MaleCount, age_65_plus_MaleCount,
            age_0_14_FemaleCount, age_15_24_FemaleCount, age_25_54_FemaleCount, age_55_64_FemaleCount, age_65_plus_FemaleCount))


## ---- NPI data processing ----
library(googlesheets4)
# Load in data from google sheets
ugly_url = 'https://docs.google.com/spreadsheets/d/1vrKvs52OAxuB7x2kT9r1q6IcIBxGEQsNRHsK_o7h3jo/edit#gid=284493712'
meta_data <- sheets_get(ugly_url)
npi_countries <- as.vector(unlist(meta_data$sheets[,1]))
npi_countries <- npi_countries[c(-1,-2,-3)]

# function to create lag factors for a country's time series
create_lag <- function(country_ts, num=10, incidence=T){
  # creating lag variables for number of reported cases, deaths, and recovered
  lags <- seq(num)   # set the number of lag factors here
  lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                     sep = "_")
  lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
  return(country_df)
}

# Extract data from google sheets into a dataframe
data_npi <- data.frame()
for(i in 1:length(npi_countries)){
  npi_df_i <- read_sheet(ugly_url, sheet = npi_countries[i]) %>%
    mutate(date = ymd(date)) %>%
    # add lag factors
    mutate(Social_Distancing_Lag_3 = lag(Social_Distancing, n=3)) %>%
    mutate(Social_Distancing_Lag_7 = lag(Social_Distancing, n=7)) %>%
    mutate(Social_Distancing_Lag_10 = lag(Social_Distancing, n=10)) %>%
    mutate(Social_Distancing_Lag_14 = lag(Social_Distancing, n=14)) %>%
    mutate(Quaranting_Cases_Lag_3 = lag(Quaranting_Cases, n=3)) %>%
    mutate(Quaranting_Cases_Lag_7 = lag(Quaranting_Cases, n=7)) %>%
    mutate(Quaranting_Cases_Lag_10 = lag(Quaranting_Cases, n=10)) %>%
    mutate(Quaranting_Cases_Lag_14 = lag(Quaranting_Cases, n=14)) %>%
    mutate(Close_Border_Lag_3 = lag(Close_Border, n=3)) %>%
    mutate(Close_Border_Lag_7 = lag(Close_Border, n=7)) %>%
    mutate(Close_Border_Lag_10 = lag(Close_Border, n=10)) %>%
    mutate(Close_Border_Lag_14 = lag(Close_Border, n=14))
  data_npi <- bind_rows(data_npi,npi_df_i)
  # Don't overload API, so chill for second.
  Sys.sleep(1)
}
glimpse(data_npi)
summary(data_npi)
print("These countries have data collected for non-pharmaceutical interventions: ")
unique(data_npi$ISO3)


#---Merging data sources and writing to a csv file---#########################################################################################################################################################################

# Merge together time series data (COVID and NPI)
data_ts <- left_join(data_COVID, data_npi, by = c('date', 'ISO3'))

data_features <- left_join(data_ts, data_static, by = 'ISO3')

# Check merged dataframe
sapply(data_features, function(x) sum(is.na(x)))

summary(data_features)

#---Old way to Add Google Movement Data---#########################################################################################################################################################################
# GoogData <- read_excel("./InputData/Google_final_29_Mar.xlsx", sheet = "Google_final_29_Mar", col_names = T)
# for(i in 1:nrow(GoogData)){
#   if(is.na(GoogData$Country[i])){
#     GoogData$Country[i] <- GoogData$Country[i-1]
#   }
# }
# GoogData <- GoogData[!is.na(GoogData$Location),]
# GoogDataSpread <- GoogData %>% spread(key="Location", value="Percent_Change")
# GoogDataSpreadSeparate <- GoogDataSpread %>% separate(Country,into=c("Country","Fluff1"),convert=TRUE,sep=" March")
# GoogDataSpreadSeparate2 <- GoogDataSpreadSeparate %>% separate(Country,into=c("Country","Fluff2"),convert=TRUE,sep=" -")
# GoogDataSpreadSeparate2$Country <- as.character(GoogDataSpreadSeparate2$Country)
# data_features$Country.x <- as.character(data_features$Country.x)
# # fix any mismatching country names
# unique(data_featuresCountries[which(data_features$Country.x %ni% GoogDataSpreadSeparate2$Country)])
# GoogDataSpreadSeparate2$Country[GoogDataSpreadSeparate2$Country=="South Korea"] <- "Korea, South"
# GoogDataSpreadSeparate2$Country[GoogDataSpreadSeparate2$Country=="United States"] <- "US"
# unique(data_featuresCountries[which(data_features$Country.x %ni% GoogDataSpreadSeparate2$Country)])
# GoogDataSpreadSeparate2$Start_Date[GoogDataSpreadSeparate2$Start_Date=="Sun Feb 16"] <- "2020-02-16"
# GoogDataSpreadSeparate2 <- GoogDataSpreadSeparate2 %>%
#   mutate(Start_Date = ymd(Start_Date))
# GoogDataSpreadSeparate2$End_Date[GoogDataSpreadSeparate2$End_Date=="Sun Mar 29"] <- "2020-03-29"
# # GoogDataSpreadSeparate2$End_Date[GoogDataSpreadSeparate2$End_Date=="Sun Mar 29"] <- as.character(max(data_features$date))
# GoogDataSpreadSeparate2 <- GoogDataSpreadSeparate2 %>%
#   mutate(End_Date = ymd(End_Date))
# # initialize data_features with new variables
# for(i in 1:nrow(GoogDataSpreadSeparate2)){
#   toReplace <- which(data_features$Country.x==GoogDataSpreadSeparate2$Country[i])
#   if(length(toReplace)>0){
#     # create linear progression between two time Start_Date and End_Date
#     Grocery_pharmacy <- seq(from=1,to=1+as.numeric(GoogDataSpreadSeparate2$`Grocery & pharmacy`[i]),length.out=as.numeric(GoogDataSpreadSeparate2$End_Date[i] - GoogDataSpreadSeparate2$Start_Date[i])+1)
#     Parks <- seq(from=1,to=1+as.numeric(GoogDataSpreadSeparate2$Parks[i]),length.out=as.numeric(GoogDataSpreadSeparate2$End_Date[i] - GoogDataSpreadSeparate2$Start_Date[i])+1)
#     Residential <- seq(from=1,to=1+as.numeric(GoogDataSpreadSeparate2$Residential[i]),length.out=as.numeric(GoogDataSpreadSeparate2$End_Date[i] - GoogDataSpreadSeparate2$Start_Date[i])+1)
#     Retail_recreation <- seq(from=1,to=1+as.numeric(GoogDataSpreadSeparate2$`Retail & recreation`[i]),length.out=as.numeric(GoogDataSpreadSeparate2$End_Date[i] - GoogDataSpreadSeparate2$Start_Date[i])+1)
#     Transit_stations <- seq(from=1,to=1+as.numeric(GoogDataSpreadSeparate2$`Transit stations`[i]),length.out=as.numeric(GoogDataSpreadSeparate2$End_Date[i] - GoogDataSpreadSeparate2$Start_Date[i])+1)
#     Workplaces <- seq(from=1,to=1+as.numeric(GoogDataSpreadSeparate2$Workplaces[i]),length.out=as.numeric(GoogDataSpreadSeparate2$End_Date[i] - GoogDataSpreadSeparate2$Start_Date[i])+1)
#     # fill in the right parts of data_features with the new linear data
#     beginDate <- which(data_features[toReplace,c("date")] == GoogDataSpreadSeparate2$Start_Date[i])
#     stopDate <- which(data_features[toReplace,c("date")] == GoogDataSpreadSeparate2$End_Date[i])
#     data_features[toReplace,c("Google_Grocery_pharmacy")][beginDate:stopDate] <- Grocery_pharmacy
#     data_features[toReplace,c("Google_Parks")][beginDate:stopDate] <- Parks
#     data_features[toReplace,c("Google_Residential")][beginDate:stopDate] <- Residential
#     data_features[toReplace,c("Google_Retail_recreation")][beginDate:stopDate] <- Retail_recreation
#     data_features[toReplace,c("Google_Transit_stations")][beginDate:stopDate] <- Transit_stations
#     data_features[toReplace,c("Google_Workplaces")][beginDate:stopDate] <- Workplaces
#     # if pre_autofill_Google signifies, we fill everything pre google data with 1s
#     if(pre_autofill_Google == T){
#       data_features[toReplace,c("Google_Grocery_pharmacy")][1:(beginDate-1)] <- 1
#       data_features[toReplace,c("Google_Parks")][1:(beginDate-1)] <- 1
#       data_features[toReplace,c("Google_Residential")][1:(beginDate-1)] <- 1
#       data_features[toReplace,c("Google_Retail_recreation")][1:(beginDate-1)] <- 1
#       data_features[toReplace,c("Google_Transit_stations")][1:(beginDate-1)] <- 1
#       data_features[toReplace,c("Google_Workplaces")][1:(beginDate-1)] <- 1
#     }
#     # if post_autofill_Google signifies, we fill everything post google data with the last datapoint
#     if(post_autofill_Google == T & (stopDate+1) <= length(toReplace) ){
#       data_features[toReplace,c("Google_Grocery_pharmacy")][(stopDate+1):length(toReplace)] <- 1+as.numeric(GoogDataSpreadSeparate2$`Grocery & pharmacy`[i])
#       data_features[toReplace,c("Google_Parks")][(stopDate+1):length(toReplace)] <- 1+as.numeric(GoogDataSpreadSeparate2$Parks[i])
#       data_features[toReplace,c("Google_Residential")][(stopDate+1):length(toReplace)] <- 1+as.numeric(GoogDataSpreadSeparate2$Residential[i])
#       data_features[toReplace,c("Google_Retail_recreation")][(stopDate+1):length(toReplace)] <- 1+as.numeric(GoogDataSpreadSeparate2$`Retail & recreation`[i])
#       data_features[toReplace,c("Google_Transit_stations")][(stopDate+1):length(toReplace)] <- 1+as.numeric(GoogDataSpreadSeparate2$`Transit stations`[i])
#       data_features[toReplace,c("Google_Workplaces")][(stopDate+1):length(toReplace)] <- 1+as.numeric(GoogDataSpreadSeparate2$Workplaces[i])
#     }
#   }
# }

#---New way to Add Google Movement Data---#########################################################################################################################################################################
'%ni%' <- Negate('%in%')
download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv","./InputData/Global_Mobility_Report.csv")
GoogData <- read.csv("./InputData/Global_Mobility_Report.csv", header=T)
# Get just the overall country mobility stats
GoogData_sub <- subset(GoogData, sub_region_1 == "" & sub_region_2 == "")
GoogData_sub$country_region <- as.character(GoogData_sub$country_region)
unique(data_features$Country.x[which(data_features$Country.x %ni% GoogData_sub$country_region)])
GoogData_sub$country_region[GoogData_sub$country_region=="United States"] <- "US"
GoogData_sub$country_region[GoogData_sub$country_region=="South Korea"] <- "Korea, South"
unique(data_features$Country.x[which(data_features$Country.x %ni% GoogData_sub$country_region)])

names(GoogData_sub)
GoogData_sub2 <- GoogData_sub[,c("country_region","date","retail_and_recreation_percent_change_from_baseline",
                                 "grocery_and_pharmacy_percent_change_from_baseline", 
                                 "parks_percent_change_from_baseline", 
                                 "transit_stations_percent_change_from_baseline",
                                 "workplaces_percent_change_from_baseline",
                                 "residential_percent_change_from_baseline")]
colnames(GoogData_sub2) <- c("country_region","date","Google_Retail_recreation",
                             "Google_Grocery_pharmacy",
                             "Google_Parks",
                             "Google_Transit_stations",
                             "Google_Workplaces",
                             "Google_Residential")
data_features2 <- data_features

# Merge the two dataframes by country name and date
GoogData_sub2$date <- as.character(GoogData_sub2$date)
data_features2$date <- as.character(data_features2$date)
GoogData_sub2$country_region <- as.character(GoogData_sub2$country_region)
data_features2$Country.x <- as.character(data_features2$Country.x)
GoogData_sub2 <- as_tibble(GoogData_sub2)
data_features2 <- as_tibble(data_features2)
data_features3 <- left_join(data_features2, GoogData_sub2, by = c("date" = "date", "Country.x" = "country_region"))

# View(data_features3[c("Country.x","date","Google_Grocery_pharmacy","Google_Parks","Google_Residential","Google_Retail_recreation","Google_Transit_stations","Google_Workplaces")])

write.csv(data_features3, "./InputData/ML_features.csv", row.names = F)









