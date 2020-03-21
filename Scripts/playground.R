# Playground script to explore the 'coronavirus' R package

# Written by:  Jay H Arehart
# Written on: March 20th, 2020

# Install package for daily update of data:
devtools::install_github("RamiKrispin/coronavirus")

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(coronavirus)

# Explore the data
data("coronavirus")
head(coronavirus)
tail(coronavirus)

# Countries with the most number of cases
summary_df <- coronavirus %>%
  group_by(Country.Region, type) %>%
  summarise(total_cases = sum(cases)) %>%
  filter(type=='confirmed') %>%
  arrange(-total_cases)

summary_df %>% head(20) 

# Summary of new cases in the past 24 hours
coronavirus %>% 
  filter(date == max(date)) %>%
  select(country = Country.Region, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)

# Look at only China
coronavirus %>% 
  filter(Country.Region == 'China') %>%
  select(country = Country.Region, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)




