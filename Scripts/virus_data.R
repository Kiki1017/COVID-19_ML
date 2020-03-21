# Collecting virus-related data

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

# Load in csv files
country_codes <- read.csv('InputData/CountryCodes.csv')

# Add ISO country codes to project
df = merge(coronavirus, country_codes, by.x='Country.Region', by.y='Country')
