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
npi_countries <- npi_countries[c(-1,-2)]

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


## ---- Merging data sources and writing to a csv file
# Merge together time series data (COVID and NPI)
data_ts <- left_join(data_COVID, data_npi, by = c('date', 'ISO3'))

data_features <- left_join(data_ts, data_static, by = 'ISO3')

# Check merged dataframe
sapply(data_features, function(x) sum(is.na(x)))

summary(data_features)

write.csv(data_features, "./InputData/ML_features.csv", row.names = F)

