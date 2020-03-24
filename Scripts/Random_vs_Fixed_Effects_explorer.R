library(tidyverse)
library(lme4)
library(scales)

data_clean <- read.csv("./InputData/ML_features.csv")
data_clean$date <- as.Date(data_clean$date)

# Looking at the data
glimpse(data_clean)
summary(data_clean)

# Drop variables not considered 
# training_countries <- c("CHN","ITA","KOR")
training_countries <- c("CHN","ITA")

data_clean$confirmed_cum <- as.numeric(rescale(data_clean$confirmed_cum, to = c(0, 1)))
data_clean$confirmed_cum_lag_01 <- as.numeric(rescale(data_clean$confirmed_cum_lag_01, to = c(0, 1)))
data_clean$confirmed_cum_lag_02 <- as.numeric(rescale(data_clean$confirmed_cum_lag_02, to = c(0, 1)))
data_clean$confirmed_cum_lag_03 <- as.numeric(rescale(data_clean$confirmed_cum_lag_03, to = c(0, 1)))
data_clean$confirmed_cum_lag_04 <- as.numeric(rescale(data_clean$confirmed_cum_lag_04, to = c(0, 1)))
data_clean$confirmed_cum_lag_05 <- as.numeric(rescale(data_clean$confirmed_cum_lag_05, to = c(0, 1)))
data_clean$GHS_Prevent <- as.numeric(rescale(data_clean$GHS_Prevent, to = c(0, 1)))
data_clean$GHS_Detect <- as.numeric(rescale(data_clean$GHS_Detect, to = c(0, 1)))
data_clean$GHS_Respond <- as.numeric(rescale(data_clean$GHS_Respond, to = c(0, 1)))
data_clean$GHS_Health <- as.numeric(rescale(data_clean$GHS_Health, to = c(0, 1)))
data_clean$GHS_Norms <- as.numeric(rescale(data_clean$GHS_Norms, to = c(0, 1)))
data_clean$GHS_Risk <- as.numeric(rescale(data_clean$GHS_Risk, to = c(0, 1)))
data_clean$GDP_percapita <- as.numeric(rescale(data_clean$GDP_percapita, to = c(0, 1)))

data_sub <- subset(data_clean,ISO3 %in% training_countries)

lmfitsimple <- lm(confirmed_cum ~ 
                       confirmed_cum_lag_01 +
                       confirmed_cum_lag_02 +
                       confirmed_cum_lag_03 +
                       confirmed_cum_lag_04 +
                       confirmed_cum_lag_05 +
                       GHS_Prevent +
                       GHS_Detect +          
                       GHS_Respond +
                       GHS_Health +
                       GHS_Norms +                  
                       GHS_Risk +
                       GDP_percapita
                        , data=data_sub)

summary(lmfitsimple)

lmfitsimple <- glmer(confirmed ~ (1 | ISO3) + (1 | date) +
                       # confirmed_cum_lag_01 + 
                       # confirmed_cum_lag_02 + 
                       # confirmed_cum_lag_03 + 
                       # confirmed_cum_lag_04 + 
                       # confirmed_cum_lag_05 + 
                      GHS_Prevent +
                      GHS_Detect +          
                      GHS_Respond +
                      GHS_Health +
                      GHS_Norms +                  
                      GHS_Risk +
                      GDP_percapita
                       , data=data_sub, family=poisson(), verbose = TRUE)


summary(lmfitsimple)

