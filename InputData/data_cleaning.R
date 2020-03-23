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
#   .csv file for implementation in ML workflow

# Import packages
library(dplyr)
library(tidyr)

data_COVID <- read.csv('./InputData/data_COVID_2020_03_21.csv')
data_static <- read.csv('./InputData/data_static_vars.csv')
