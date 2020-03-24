# Collecting virus-related data

# Written by:  Jay H Arehart
# Written on: March 20th, 2020

# Install package for daily update of data:
devtools::install_github("RamiKrispin/coronavirus")


# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(coronavirus)
library(ggpubr)

# Explore the data
data("coronavirus")
head(coronavirus)

# Load in csv files
country_codes <- read.csv('./InputData/CountryCodes.csv')

# Add ISO country codes to project
raw_data = tbl_df(merge(coronavirus, country_codes, by.x='Country.Region', by.y='Country'))

## Functions to apply to a single country  ----
# Function to create a collapsed time series of data with cummulative sums for each country (if multiple regions exist)
country_timeseries <- function(df, country, plot=F){
  output <- df %>% filter(ISO3==country) %>%
    group_by(date, type) %>%
    summarise(daily_cases = sum(cases)) %>%
    pivot_wider(names_from = type,
                values_from = daily_cases) %>%
    ungroup() %>%
    mutate(confirmed_cum = cumsum(confirmed)) %>%
    mutate(death_cum = cumsum(death)) #%>%
    # mutate(recovered_cum = cumsum(recovered))
  if(plot==T){
    # Plot cummulative sum of cases
    p1 <- ggplot(output, aes(x=date, y=confirmed_cum)) +
      geom_line() +
      ggtitle(paste0("Cummulative cases in ", country))
    # Plot daily number of reported cases
    p2 <- ggplot(output, aes(x=date, y=confirmed)) +
      geom_line() +
      ggtitle(paste0("Reported number of cases in ", country))
    # Plot the two graphs
    my_plot <- ggarrange(p1, p2, ncol=1)
    plot(my_plot)
  }
  return(output)
}

# function to create lag factors for a country's time series
create_lag <- function(country_ts, num=10){
  # creating lag variables for number of reported cases, deaths, and recovered
  lags <- seq(num)   # set the number of lag factors here
  lag_names <- paste("cum_lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                     sep = "_")
  lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
  
  if(dim(country_ts)[2] > 7){
    # country_df <- country_ts %>% mutate_at(vars(confirmed, death, recovered), funs_(lag_functions)) %>%
    country_df <- country_ts %>% mutate_at(vars(confirmed, death), funs_(lag_functions)) %>%
    select(-values1_lag1, -values1_lag2, -values2_lag1, -values2_lag2)
  }else{
    # country_df <- country_ts %>% mutate_at(vars(confirmed, death, recovered), funs_(lag_functions))
    country_df <- country_ts %>% mutate_at(vars(confirmed, death), funs_(lag_functions))
  }
  return(country_df)
}

# function for creating full dataframe 
create_COVID_ML_df <- function(coronavirus, num_cases_min = 4000, num_lag=10){
  # aggregate the raw data
  summary_df <- coronavirus %>%
    group_by(Country.Region, type) %>%
    summarise(total_cases = sum(cases)) %>%
    filter(type=='confirmed') %>%
    arrange(-total_cases)
  # filter by minimum number of cases
  countries_list = summary_df %>%
    filter(total_cases >= num_cases_min) %>%
    arrange(-total_cases)
  # collect ISO3 number
  countries_training = merge(countries_list, country_codes, by.x='Country.Region', by.y='Country')
  countries_training <- countries_training %>%
    arrange(-total_cases)
  
  # create the time series data and lag factors
  df_ts_lag_train <- data.frame()
  for(i in 1:length(countries_training$ISO3)){
    df.ts <- country_timeseries(raw_data, countries_training$ISO3[i], plot = F)
    df.ts.lag <- create_lag(df.ts, num_lag)
    df.ts.lag$ISO3 <- countries_training$ISO3[i]
    df.ts.lag$Country <- countries_training$Country.Region[i]
    df_ts_lag_train <- rbind(df_ts_lag_train,df.ts.lag)
  }
  df_ts_lag_train <- select(df_ts_lag_train, date, Country, ISO3, everything())
  print(paste0("Total number of countries included in analysis are: ", n_distinct(df_ts_lag_train$Country)))
  # print(paste0("Countries time histories included are: ", distinct(df_ts_lag_train, Country)))
  print(unique(df_ts_lag_train$Country))
  return(df_ts_lag_train)
}

## Looking at a single country -----

country_ts <- country_timeseries(raw_data, 'ITA', plot = T) #create time series and plot
country_ts_lag <- create_lag(country_ts, num=10)


## Creaint the full dataframe and saving the .csv file -----

output_df <- create_COVID_ML_df(coronavirus, num_cases_min = 1000, num_lag = 10)

<<<<<<< HEAD
write.csv(output_df, file="InputData/data_COVID_2020_03_24.csv")
=======
write.csv(output_df, file="InputData/data_COVID_2020_03_23.csv")
>>>>>>> 0123a27d7c26f931572a48a0ffdaf58e9db70073

