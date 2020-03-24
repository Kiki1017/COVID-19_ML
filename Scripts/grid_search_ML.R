# COVID-19 Machine Learning
#
# Written by: Jay H Arehart
# Written on: Oct 24, 2019
# Last Updated: Mar 22, 2020
#
#
# Script Description:
#   Train a machine learning model on the COVID-19 data
#
# Output
#   Output

# Import packages
library(ggplot2)
# library(reshape2)
# library(egg)
library(tidyverse)
library(dplyr)
library(tidyr)
# library(rgdal)
library(caret)
library(GGally)
library(randomForest)
library(PerformanceAnalytics)
library(ggthemes)
library(corrplot)
library(car)
library(psych)
library(caret)
library(caretEnsemble)
library(doParallel)
library(RPushbullet)

start_time = Sys.time()

# Import data -------------------------------------------------------------------
set.seed(40)

data_clean <- read.csv("./InputData/ML_features.csv")
data_clean$date <- as.Date(data_clean$date)


# Looking at the data
glimpse(data_clean)
summary(data_clean)

# Drop variables not considered 
data_thin <- data_clean %>%
  select(-ISO3, -Country,-FullName) %>%
  # remove number of confirmed cases
  select(-contains("recovered")) %>%
  select(-contains("lag")) %>%
  select(-FullName)
  


# # Look at correlation between predictor variables
# cormat(data_thin, type="upper")
# corrplot(cor(data_thin), method="color")
# featurePlot(x=select(data_thin, -death),
#             y=data_thin$death,
#             plot = "scatter")

# Machine Learning Workflow with Caret ------------------------------------

# Input number of days for training / testing split
days <- 7
split_date <- max(data_clean$date) - days

#### creating sampling seeds ####
set.seed(123)
seeds <- vector(mode = "list", length = 432)
for(i in 1:431) seeds[[i]] <- sample.int(1000, 5)

## For the last model:
seeds[[432]] <- sample.int(1000, 1)

registerDoParallel(8)
getDoParWorkers()
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 7,
                              horizon = 5,
                              fixedWindow = FALSE,
                              allowParallel = TRUE)
                              # seeds = seeds)
tuneLength.num <- 5

glmnet.mod <- train(death ~ . -date,
                    data = data_thin,
                    method = "glmnet",
                    family = "gaussian",
                    trControl = myTimeControl,
                    tuneLength=tuneLength.num,
                    na.action=na.exclude)
pois.mod <- train(death ~ . - date,
                  data = data_thin,
                  method = "glmnet",
                  family = "poisson",
                  trControl = myTimeControl,
                  tuneLength=tuneLength.num,
                  na.action=na.exclude)
lm.mod <- train(death ~ . - date,
                data = data_thin,
                method = "lm",
                trControl = myTimeControl,
                tuneLength=tuneLength.num,
                na.action=na.exclude)
rf.mod <- train(death ~ . - date,
                data = data_thin,
                method = "rf",
                trControl = myTimeControl,
                tuneLength=tuneLength.num,
                na.action=na.exclude)


resamps <- resamples(list(glmnet = glmnet.mod,
                          glmnet.pois = pois.mod,
                          lm = lm.mod,
                          rf=rf.mod))
resamps

ss <- summary(resamps)


trellis.par.set(caretTheme())
dotplot(resamps, metric = "Rsquared")



