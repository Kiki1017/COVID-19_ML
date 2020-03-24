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
  select(-contains("recovered"))
  


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


# Select training and test data
X = data_thin %>%
  select(-death,-confirmed,-confirmed_cum,-death_cum)
  # select(-contains("recovered"))
y = data_thin %>%
  select(date,death)

# Check data
str(X)
str(y)

# Create training and test data
X_train <- filter(X, date <= split_date)
X_test  <- filter(X, date >  split_date)
y_train <- filter(y, date <= split_date)
y_test  <- filter(y, date >  split_date)

# Remove date as a predictor variable
X_train <- select(X_train, -date)
X_test  <- select(X_test, -date)
y_train <- select(y_train, -date)
y_test  <- select(y_test, -date)

str(X_train)
str(X_test)
str(y_train)
str(y_test)

# Set up parallel processing with number of cross validations (cv)
registerDoParallel(4)
getDoParWorkers()
set.seed(123)
my_control <- trainControl(method = "cv",
                           number = 3,
                           savePredictions = "final",
                           allowParallel = TRUE)

# Train models!
set.seed(222)
model_list <- caretList(X_train,
                        y_train,
                        trControl = my_control,
                        methodList = c("lm", "rf"),
                        tuneList = NULL,
                        continue_on_fail = FALSE, 
                        preProcess = c("center","scale"))
pbPost("note", "Model Training completed!", "")


# Print model results (RMSE)
options(digits = 3)
model_results <- data.frame(
  LM = min(model_list$lm$results$RMSE),
  SVM = min(model_list$svmRadial$results$RMSE)
  # RF = min(model_list$rf$results$RMSE)
  # XGBT = min(model_list$xgbTree$results$RMSE)
  # XGBL = min(model_list$xgbLinear$results$RMSE)
)
print(model_results)


# Resample model to evaluate confidence levels for each
resamples <- resamples(model_list)
dotplot(resamples, metric = "RMSE")

# What is the corellation of models?
modelCor(resamples)


# # Combine models (using ensemble) to create another model
# set.seed(222)
# ensemble_1 <- caretEnsemble(model_list, 
#                             metric = "RMSE", 
#                             trControl = my_control)
# summary(ensemble_1)
# plot(ensemble_1)
# 
# 
# # Stack models to create another
# set.seed(222)
# ensemble_2 <- caretStack(model_list, 
#                          method = "glm", 
#                          metric = "RMSE", 
#                          trControl = my_control)
# print(ensemble_2)


# Test data performance with models
# PREDICTIONS
pred_lm <- predict.train(model_list$lm, newdata = X_test)
pred_svm <- predict.train(model_list$svmRadial, newdata = X_test)
# pred_rf <- predict.train(model_list$rf, newdata = X_test)
# pred_xgbT <- predict.train(model_list$xgbTree, newdata = X_test)
# pred_xgbL <- predict.train(model_list$xgbLinear, newdata = X_test)
# predict_ens1 <- predict(ensemble_1, newdata = X_test)
# predict_ens2 <- predict(ensemble_2, newdata = X_test)

# RMSE
pred_RMSE <- data.frame(
  # ensemble_1 = RMSE(predict_ens1, y_test),
  # ensemble_2 = RMSE(predict_ens2, y_test),
  LM = RMSE(pred_lm, y_test),
  SVM = RMSE(pred_svm, y_test)
  # RF = RMSE(pred_rf, y_test),
  # XGBT = RMSE(pred_xgbT, y_test),
  # XGBL = RMSE(pred_xgbL, y_test)
)
print(pred_RMSE)

# xgbTree_model <- train(X_train,
#                        y_train,
#                        trControl = my_control,
#                        method = "xgbLinear",
#                        metric = "RMSE",
#                        preProcess = c("center","scale"),
#                        importance = TRUE)
# plot(varImp(xgbTree_model))

# save.image(file='Jan07_2020_Workspace')

end_time <- Sys.time()

total_time = end_time - start_time
print(total_time)
pbPost("note", "AWS R session completed!", "Check dat thang out boiiii.")
