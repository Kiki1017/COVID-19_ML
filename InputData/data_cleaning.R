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

# Load in raw data
raw_data_COVID <- read.csv('./InputData/data_COVID_2020_03_22.csv')
raw_data_static <- read.csv('./InputData/data_static_vars.csv')

# Manipulate original datasets (e.g., normalizing)
data_COVID <- raw_data_COVID %>%
  select(-X)

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

# Merge together country and 
data_features <- left_join(data_COVID, data_static, by = 'ISO3')

# Check merged dataframe
sapply(data_features, function(x) sum(is.na(x)))

summary(data_features)

write.csv(data_features, "./InputData/ML_features.csv", row.names = F)

