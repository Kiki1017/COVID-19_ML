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
library(reshape2)
library(egg)
library(tidyverse)
library(dplyr)
library(tidyr)
library(rgdal)
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

# Data from python data wrangling. Different datasets have different number of data points.
data_raw <- read.csv("~/Input Data/Footprint_Training_Clean.csv")
# data_raw <- read.csv("~/Input Data/Footprint_Training_Clean_medium.csv")
# data_raw <- read.csv("~/Input Data/Footprint_Training_Clean_all.csv")
data_clean = sample_n(data_raw, size=10000)
data_clean$X = NULL
data_clean$OBJECTID = NULL
data_clean$centroid_lat = NULL
data_clean$centroid_long = NULL
data_clean$perimeter = NULL
data_clean$area = NULL
data_clean$mbr_area = NULL
data_clean$mbr_perimeter = NULL
data_clean$n_vertices = as.numeric(data_clean$n_vertices)
colnames(data_clean)[1] <- ('perimeter')
colnames(data_clean)[2] <- c('area')
colnames(data_clean)[12] <- c('mbr_perimeter')
colnames(data_clean)[11] <- c('mbr_area')
colnames(data_clean)[5] <- c('radius_mean')



# Create New Variables -------------------------------------------------------------------
data_clean = data_clean %>%
  mutate(Cooke_JC = perimeter / (4*sqrt(area)) - 1) %>%
  mutate(POP_index = 2*sqrt(pi*area) / (perimeter)) %>%
  mutate(Compactness = (4*pi*area) / perimeter^2) %>%
  mutate(fractality = 1 - log(area)/(2*log(perimeter))) %>%
  mutate(elongation = pmax(mbr_length, mbr_width) / pmin(mbr_length, mbr_width)) %>%
  mutate(concavity = area / convex_hull_area) %>%
  select(-height,height)


# Looking at the data
glimpse(data_clean)
summary(data_clean)

# Height to Num Stories -------------------------------------------------------------------
interstory_height = 3.5 #meters
data_clean = data_clean %>%
  mutate(no_floors = round(height/interstory_height))

plt1 <- ggplot(data=data_clean) +
  geom_histogram(mapping=aes(x=no_floors), bins=100, fill = "lightblue", color = "black") +
  ylab("Count") +
  xlim(0,10) +
  ggtitle("Interstory Height = 3.5 meters") +
  theme_classic()

plt2 <- ggplot(data=data_clean, aes(x="",y=no_floors )) +
  geom_boxplot(fill = "lightblue", color = "black") +
  coord_flip() +
  theme_classic() +
  xlab("") +
  ylim(0,10) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

egg::ggarrange(plt1, plt2, heights = 2:1)




# Variable to plot:

# Plotting
# plt1 <- ggplot(data=data_clean) +
#   geom_histogram(mapping=aes(x=perimeter), bins=100, fill = "lightblue", color = "black") +
#   ylab("Count") +
#   # xlim(0,20) +
#   theme_classic()
# 
# plt2 <- ggplot(data=data_clean, aes(x="",y=perimeter )) +
#   geom_boxplot(fill = "lightblue", color = "black") +
#   coord_flip() +
#   theme_classic() +
#   xlab("") +
#   # ylim(0,20) +
#   theme(axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# egg::ggarrange(plt1, plt2, heights = 2:1)


# # Look at correlation between predictor variables
chart.Correlation(data_clean)
corrplot(cor(data_clean), method="circle", type ="upper")
featurePlot(x=data_clean[, 1:22],
            y=data_clean$height,
            plot = "scatter",
)

# Identifying Correlated Predictors ---------------------------------------
data.frame(table(data_clean$height))


# Look at multicolinearity between variables
simple_lm <- lm(height ~., data=data_clean)
vif(simple_lm)


# Machine Learning Workflow with Caret -------------------------------------------------------------------

# Randomize data before
data_rand <- data_clean[sample(1:nrow(data_clean)), ]

# Select training and test data
X = data_rand[, -which( colnames(data_rand)=="height")]
y = data_clean[, which( colnames(data_rand)=="height")]

# Check data
str(X)
str(y)

# Create training and test data
set.seed(31)
part.index <- createDataPartition(data_rand$height, 
                                  p = 0.75,                         
                                  list = FALSE)

X_train <- X[part.index, ]
X_test <- X[-part.index, ]
y_train <- y[part.index]
y_test <- y[-part.index]

str(X_train)
str(X_test)
str(y_train)
str(y_test)

# Set up parallel processing with number of cross validations (cv)
registerDoParallel(8)
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
                        methodList = c("lm", "svmRadial"),
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
