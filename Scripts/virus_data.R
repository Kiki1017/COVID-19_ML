# Collecting virus-related data

# Written by:  Jay H Arehart
# Written on: March 20th, 2020

# Install package for daily update of data:
# devtools::install_github("RamiKrispin/coronavirus")

# TRUE if you want to scale by population
incidence_flag <- T
HubeiFlag <- T

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(coronavirus)
library(ggpubr)
library(readxl)

# Update data?
update_datasets()

# Explore the data
data("coronavirus")
head(coronavirus)

# Load in csv files
country_codes <- read.csv('./InputData/CountryCodes.csv')

# Load in population data
population <- read_excel("./InputData/pop.xlsx", sheet = "pop_1000s", col_names = T)

population_countrycodes <- merge(country_codes,population, by='ISO3')
population_countrycodes <- population_countrycodes %>%
  mutate(Country = Country.x) %>%
  select(-Country.x,-Country.y,-PopTotal_1000s)
glimpse(population_countrycodes)

# Add ISO country codes to project
raw_data = tbl_df(merge(coronavirus, population_countrycodes, by.x='Country.Region', by.y='Country'))
if(HubeiFlag==T){
  raw_data = mutate(raw_data, Country.Region = replace(Country.Region,Country.Region == 'China', 'Hubei')) %>%
    # filter(Country.Region =='Hubei' | Province.State != 'Hubei')
    filter(!Province.State %in% c('Shandong','Guizhou','Hainan',
           'Macau','Ningxia','Inner Mongolia','Yunnan','Sichuan','Zhejiang','Guangxi','Guangdong','Hong Kong','Hunan',
           'Shaanxi','Shanxi','Tibet','Heilongjiang','Qinghai','Hebei','Chongqing','Liaoning','Xinjiang','Gansu',
           'Jiangsu','Jiangxi','Anhui','Henan','Shanghai','Fujian','Tianjin','Beijing','Jilin')) %>%
    mutate(ISO3 = replace(ISO3, ISO3=='CHN','HUB'))
    
}

## Functions to apply to a single country  ----
# Function to create a collapsed time series of data with cummulative sums for each country (if multiple regions exist)
country_timeseries <- function(df, country, incidence=T, plot=F){
  pop <- df %>%
    filter(ISO3==country) %>%
    summarise(pop = mean(PopTotal))
  pop <- as.numeric(pop)
  output <- df %>% filter(ISO3==country) %>%
    group_by(date, type) %>%
    summarise(daily_cases = sum(cases)) %>%
    pivot_wider(names_from = type,
                values_from = daily_cases) %>%
    ungroup()
  if(incidence==T){
    output <- output %>%
      mutate(confirmed_cum_per_million = cumsum(confirmed) / (pop/1000000)) %>%
      mutate(death_cum_per_million = cumsum(death) / (pop/1000000)) %>%
      mutate(confirmed_cum = cumsum(confirmed)) %>%
      mutate(death_cum = cumsum(death))
  }else{
    output <- output %>%
      mutate(confirmed_cum = cumsum(confirmed) ) %>%
      mutate(death_cum = cumsum(death) )
  }
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
create_lag <- function(country_ts, num=10, incidence=T){
  # creating lag variables for number of reported cases, deaths, and recovered
  lags <- seq(num)   # set the number of lag factors here
  lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                     sep = "_")
  lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
  
  if(dim(country_ts)[2] > 7){
    # country_df <- country_ts %>% mutate_at(vars(confirmed, death, recovered), funs_(lag_functions)) %>%
    if(incidence==T){
      country_df <- country_ts %>% mutate_at(vars(confirmed_cum_per_million, death_cum_per_million), funs_(lag_functions)) #%>%
        # select(-values1_lag1, -values1_lag2, -values2_lag1, -values2_lag2)
    }else{
      country_df <- country_ts %>% mutate_at(vars(confirmed_cum, death_cum), funs_(lag_functions)) #%>%
        # select(-values1_lag1, -values1_lag2, -values2_lag1, -values2_lag2)
    }
  }else{
    # country_df <- country_ts %>% mutate_at(vars(confirmed, death, recovered), funs_(lag_functions))
    if(incidence==T){
      country_df <- country_ts %>% mutate_at(vars(confirmed_cum_per_million, death_cum_per_million), funs_(lag_functions))
    }else{
      country_df <- country_ts %>% mutate_at(vars(confirmed_cum, death_cum), funs_(lag_functions))
    }
  }
  return(country_df)
}

# function for creating full dataframe 
create_COVID_ML_df <- function(coronavirus, num_cases_min = 4000, num_lag=10, incidence_flag=T){
  # aggregate the raw data
  summary_df <- coronavirus %>%
    filter(Country.Region != 'Cruise Ship') %>%
    group_by(Country.Region, type) %>%
    summarise(total_cases = sum(cases)) %>%
    filter(type=='confirmed') %>%
    arrange(-total_cases)
<<<<<<< HEAD
  if(HubeiFlag == T){
    HUB = coronavirus %>%
      filter(Country.Region != 'Cruise Ship') %>%
      group_by(Country.Region, type) %>%
      filter(Province.State == 'Hubei') %>%
      summarise(total_cases = sum(cases)) %>%
      filter(type=='confirmed')
    HUB[1,1] <- 'Hubei'
    summary_df = summary_df %>%
      filter(Country.Region != 'China') %>%
      bind_rows(HUB) %>%
      arrange(-total_cases)
  }
=======
  # if(HubeiFlag == T){
  #   HUB = coronavirus %>%
  #     filter(Country.Region != 'Cruise Ship') %>%
  #     group_by(Country.Region, type) %>%
  #     filter(Province.State == 'Hubei') %>%
  #     summarise(total_cases = sum(cases)) %>%
  #     filter(type=='confirmed')
  #   HUB[1,1] <- 'Hubei'
  #   summary_df = summary_df %>%
  #     filter(Country.Region != 'China') %>%
  #     bind_rows(HUB) %>%
  #     arrange(-total_cases)
  # }
>>>>>>> 6da032bb24eb9f9b8dd237cda4d9a66d62203169
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
    df.ts <- country_timeseries(raw_data, countries_training$ISO3[i], incidence=incidence_flag, plot = F)
    df.ts.lag <- create_lag(df.ts, num_lag, incidence=incidence_flag)
    df.ts.lag$ISO3 <- countries_training$ISO3[i]
    df.ts.lag$Country <- countries_training$Country.Region[i]
    df_ts_lag_train <- rbind(df_ts_lag_train,df.ts.lag)
  }
  df_ts_lag_train <- select(df_ts_lag_train, date, Country, ISO3, everything())
  print(paste0("Total number of countries included in analysis are: ", n_distinct(df_ts_lag_train$Country)))
  # print(paste0("Countries time histories included are: ", distinct(df_ts_lag_train, Country)))
  print(unique(df_ts_lag_train$Country))
  print('Most recent date of data is:  ')
  print(max(df_ts_lag_train$date))
  return(df_ts_lag_train)
}

## Looking at a single country -----

country_ts <- country_timeseries(raw_data, 'CHN', incidence = incidence_flag, plot = T) #create time series and plot
country_ts_lag <- create_lag(country_ts, num=10, incidence = incidence_flag)


## Creaint the full dataframe and saving the .csv file -----

output_df <- create_COVID_ML_df(coronavirus, num_cases_min = 1000, num_lag = 20, incidence_flag = incidence_flag)   # to change from cases per million to total cases, change default value in function defined above (country_timeseries)

<<<<<<< HEAD
# if(HubeiFlag == T){
#   output_df$Country <- as.character(output_df$Country)
#   output_df$Country[output_df$Country == "China"] <- "Hubei"
#   output_df$ISO3 <- as.character(output_df$ISO3)
#   output_df$ISO3[output_df$ISO3 == "CHN"] <- "HUB"
#   
# }
=======
if(HubeiFlag == T){
  output_df$Country <- as.character(output_df$Country)
  output_df$Country[output_df$Country == "China"] <- "Hubei"
  output_df$ISO3 <- as.character(output_df$ISO3)
  output_df$ISO3[output_df$ISO3 == "CHN"] <- "HUB"
  
}
>>>>>>> 6da032bb24eb9f9b8dd237cda4d9a66d62203169

write.csv(output_df, file="InputData/data_COVID_2020_04_02.csv")


