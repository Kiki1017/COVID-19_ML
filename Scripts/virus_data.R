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

# Explore the data
data("coronavirus")
head(coronavirus)

# Load in csv files
country_codes <- read.csv('./InputData/CountryCodes.csv')

# Add ISO country codes to project
df = tbl_df(merge(coronavirus, country_codes, by.x='Country.Region', by.y='Country'))

## Looking at China's confirmed cases  -----
CHN_ts <- df %>% filter(ISO3=='CHN') %>%
  group_by(date, type) %>%
  summarise(daily_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = daily_cases) %>%
  ungroup() %>%
  mutate(confirmed_cum = cumsum(confirmed)) %>%
  mutate(death_cum = cumsum(death)) %>%
  mutate(recovered_cum = cumsum(recovered))

# Plot cummulative sum of cases
p1 <- ggplot(CHN_ts, aes(x=date, y=confirmed_cum)) +
  geom_line() +
  ggtitle('Cummulative cases in China')

# Plot daily number of reported cases
p2 <- ggplot(CHN_ts, aes(x=date, y=confirmed)) +
  geom_line() +
  ggtitle('Reported number of cases in China')

multiplot(p1, p2, cols=1)

## Creating variables  -----

# creating lag variables for number of reported cases, deaths, and recovered
lags <- seq(10)   # set the number of lag factors here
lag_names <- paste("confirmed_cum_lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                   sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

CHN <- CHN_ts %>% mutate_at(vars(confirmed, death, recovered), funs_(lag_functions)) %>%
  select(-values1_lag1, -values1_lag2, -values2_lag1, -values2_lag2)




