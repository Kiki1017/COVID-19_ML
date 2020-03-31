library(tidyverse)
library(lme4)
library(scales)
library(tidyverse)
library(readxl)
library(reshape2)
library(rpart)
library(ggplot2)
library(rpart.plot)

data_clean <- read.csv("./InputData/ML_features.csv")
data_clean$date <- as.Date(data_clean$date)

# Looking at the data
glimpse(data_clean)
summary(data_clean)

# testing_countries <- c("USA")
# testing_countries <- c("DEU")
testing_countries <- c("BRA")


# make country lists
training_countries_all <- c("CHN","KOR","USA","GBR","ESP","IRN","FRA","ANT","CHE","AUT","BRA","DEU")
training_countries <- training_countries_all[which(training_countries_all != testing_countries)]
# training_countries <- c("CHN","KOR","ITA")

# make country lists
training_countries_all <- c("CHN","KOR","USA","GBR","ESP","IRN","FRA","ANT","CHE","AUT","BRA","DEU")
training_countries <- training_countries_all[which(training_countries_all != testing_countries)]
# training_countries <- c("CHN","KOR","ITA")

# subset to 100 cumulative cases as starting time threshold and add time column
for(i in 1:length(training_countries)){
  training_subset <- subset(data_clean,ISO3 %in% training_countries[i])
  start <- which(training_subset$confirmed_cum_per_million >= .3)[1]
  training_subset_aligned <- training_subset[start:nrow(training_subset),]
  training_subset_aligned$time <- c(1:nrow(training_subset_aligned))
  if(i==1){
    training_ready <- training_subset_aligned
  }else{
    training_ready <- as.data.frame(rbind(training_ready,training_subset_aligned))
  }
}

projectionTime <- 14

for(i in 1:length(testing_countries)){
  testing_subset <- subset(data_clean,ISO3 %in% testing_countries[i])
  start <- which(testing_subset$confirmed_cum_per_million >= 0.3)[1]
  testing_subset_aligned <- testing_subset[start:nrow(testing_subset),]
  tmp <- testing_subset_aligned[1:projectionTime,]
  tmp[,grep("cum", colnames(tmp))] <- NA
  tmp$Social_Distancing <- NA
  tmp$Quaranting_Cases <- NA
  tmp$Close_Border <- NA
  testing_subset_aligned_predictNA <- rbind(testing_subset_aligned,tmp)
  testing_subset_aligned_predictNA$time <- c(1:nrow(testing_subset_aligned_predictNA))
  if(i==1){
    testing_ready <- testing_subset_aligned_predictNA
  }else{
    testing_ready <- as.data.frame(rbind(testing_ready,testing_subset_aligned_predictNA))
  }
}

#---first plot---#########################################################################################################################################################################
# lineColors <- c("firebrick","darkgoldenrod1", "darkviolet", "limegreen", "dodgerblue")
ggplot() +
  geom_line(data=training_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size=0.8,alpha=.7)+
  geom_line(data=testing_ready, aes(x = time, y = confirmed_cum_per_million, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
  labs(x="Days Since 100 Cumulative Counts", y = "Confirmed Cumulative Cases per Million", title="") +
  guides(color=guide_legend(title="")) +
  theme(legend.title=element_text(size=18))+
  theme(axis.text.x = element_text(color="black",size = 16, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text(color="black",size = 16, angle = 0),
        axis.title.x = element_text(color="black",size = 18, angle = 0),
        axis.title.y = element_text(color="black",size = 18, angle = 90)
  )+
  # scale_x_continuous(breaks=seq(1, 10, 1))+
  theme(legend.text=element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  # scale_colour_manual(values=c(lineColors))
ggplot() +
  geom_line(data=training_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size=0.8,alpha=.7)+
  geom_line(data=testing_ready, aes(x = time, y = confirmed_cum, group = FullName, color = FullName), size=1, linetype = "3313",alpha=1)+
  labs(x="Days Since 100 Cumulative Counts", y = "Confirmed Cumulative Cases", title="") +
  guides(color=guide_legend(title="")) +
  theme(legend.title=element_text(size=18))+
  theme(axis.text.x = element_text(color="black",size = 16, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text(color="black",size = 16, angle = 0),
        axis.title.x = element_text(color="black",size = 18, angle = 0),
        axis.title.y = element_text(color="black",size = 18, angle = 90)
  )+
  # scale_x_continuous(breaks=seq(1, 10, 1))+
  theme(legend.text=element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
# scale_colour_manual(values=c(lineColors))
#---training tree---#########################################################################################################################################################################
str(training_ready)
str(testing_ready)

fit <- rpart(confirmed_cum_per_million ~ time+
               confirmed_cum_per_million_lag_01+
               confirmed_cum_per_million_lag_02+
               confirmed_cum_per_million_lag_03+
               confirmed_cum_per_million_lag_04+
               confirmed_cum_per_million_lag_05+
               confirmed_cum_per_million_lag_06+
               confirmed_cum_per_million_lag_07+
               confirmed_cum_per_million_lag_08+
               confirmed_cum_per_million_lag_09+
               confirmed_cum_per_million_lag_10+
               confirmed_cum_per_million_lag_11+
               confirmed_cum_per_million_lag_12+
               confirmed_cum_per_million_lag_13+
               confirmed_cum_per_million_lag_14+
               Social_Distancing +
               Quaranting_Cases +
               Close_Border +
               GHS_Prevent+
               GHS_Detect+
               GHS_Respond+
               GHS_Health+
               GHS_Norms+
               GHS_Risk+
               GDP_bill+
               GDP_percapita+
               Population_mill+
               HumanDevelopmentIndex_2018+
               EIUDemocracyIndexScore_2019+
               UNOnlineServicesIndexScore_2018+
               GlobalPeaceIndex+
               CorruptionsPerceptionIndex_2018+
               HumanCapitalIndex_2017+
               SDGIndexScore_2018+
               age_0_14_Percent+
               age_15_24_Percent+
               age_25_54_Percent+
               age_55_64_Percent+
               age_65_plus_Percent+
               PopulationGrowthRate+
               PopulationSmoking_male+
               PopulationSmoking_female+
               GINIindex+
               PercentUrban+
               RateUrbanization+
               Ave_household_size+
               Percent_house_Nuclear+
               Percent_house_Multi_generation+
               Percent_house_Three_generation+
               Percent_house_Skip_generation+
               EFindex,
             data=training_ready, 
             method="anova", #"anova", "poisson", "class" or "exp"
             control=rpart.control(minsplit=2, cp=0.0001))
summary(fit)
# rpart.plot(fit, main="Tree")

nLags <- 14
# setting the NPIflag to "lastNPI" is our method of saying that we want to fill all the NAs in the forecasting period with the last empirical time points' NPI values
NPIflag <- "lastNPI"
testing_ready_pred <- testing_ready
breaker <- nrow(testing_ready_pred)-projectionTime+1
# testing_ready_pred[(breaker-1):(breaker+1),grep("confirmed_cum_per_million", colnames(testing_ready_pred))]

# Note this code assumes that there are no NAs present in the NPI data
if(NPIflag == "lastNPI"){
  testing_ready_pred$Social_Distancing[head(which(!is.na(testing_ready_pred$Social_Distancing)),n=1):tail(which(is.na(testing_ready_pred$Social_Distancing)),n=1)] <- tail(testing_ready_pred$Social_Distancing[which(!is.na(testing_ready_pred$Social_Distancing))],n=1)
  testing_ready_pred$Quaranting_Cases[head(which(!is.na(testing_ready_pred$Quaranting_Cases)),n=1):tail(which(is.na(testing_ready_pred$Quaranting_Cases)),n=1)] <- tail(testing_ready_pred$Quaranting_Cases[which(!is.na(testing_ready_pred$Quaranting_Cases))],n=1)
  testing_ready_pred$Close_Border[head(which(!is.na(testing_ready_pred$Close_Border)),n=1):tail(which(is.na(testing_ready_pred$Close_Border)),n=1)] <- tail(testing_ready_pred$Close_Border[which(!is.na(testing_ready_pred$Close_Border))],n=1)
}

# Check before and after if you so desire, for the filling in of NPI data in the forecasting period.
# testing_ready$Social_Distancing
# testing_ready_pred$Social_Distancing
# testing_ready$Quaranting_Cases
# testing_ready_pred$Quaranting_Cases
# testing_ready$Close_Border
# testing_ready_pred$Close_Border

p1 <- predict(fit, testing_ready_pred[1:breaker-1,], na.action = na.pass)
for(i in breaker:nrow(testing_ready_pred)){
  for(l in 1:nLags){
    if(l==1){
      testing_ready_pred[i,c(paste0("confirmed_cum_per_million_lag_01"))] <- testing_ready_pred[i-1,c(paste0("confirmed_cum_per_million"))]
      if(NPIflag != "lastNPI"){
        # Some other forecasting of the NPI data
      }
    }else{
      testing_ready_pred[i,c(paste0(sprintf("confirmed_cum_per_million_lag_%02d", l)))] <- testing_ready_pred[i-1,c(paste0(sprintf("confirmed_cum_per_million_lag_%02d", l-1)))]
    }
  }
  testing_ready_pred[i,c(paste0("confirmed_cum_per_million"))] <- predict(fit, testing_ready_pred[i,], na.action = na.pass)
  # testing_ready_pred[(breaker-5):(i),grep("confirmed_cum_per_million", colnames(testing_ready_pred))]
  if(i==breaker){
    pN <- predict(fit, testing_ready_pred[i,], na.action = na.pass)
    pAll <- c(p1,pN)
  }else{
    pN <- predict(fit, testing_ready_pred[i,], na.action = na.pass)
    pAll <- c(pAll,pN)
  }
}

pAll <- as.data.frame(pAll)
pAll$time <- testing_ready_pred$time

plot1Data_tmp <- testing_ready[,c("FullName","time","confirmed_cum_per_million")]
plot1Data <- merge(pAll,plot1Data_tmp,by="time")
colnames(plot1Data) <- c("time","prediction","country","actual")
plot1Data <- plot1Data[order(plot1Data$time),]
plot1Data$prediction <- as.numeric(plot1Data$prediction)
plot1Data$actual <- as.numeric(plot1Data$actual)
plot1Data$country <- as.character(plot1Data$country)
str(plot1Data)

m1 <- melt(plot1Data,id=c("country","time"))
m1$time <- as.numeric(m1$time)
m1$value <- as.numeric(m1$value)
m1$variable <- as.factor(m1$variable)
m1$country <- as.factor(m1$country)
str(m1)
m1 <- m1[order(m1$time),]

# lineColors <- c("firebrick","darkgoldenrod1", "darkviolet", "dodgerblue", "forestgreen")
ggplot() +
  geom_line(data=subset(m1, variable == "actual"), aes(x = time, y = value, group = country, color = country), size=0.8,alpha=.7)+
  geom_line(data=subset(m1, variable == "prediction"), aes(x = time, y = value, group = country, color = country), size=0.85, linetype = "3313",alpha=.7)+
  labs(x="Days Since 100 Cumulative Counts", y = "Confirmed Cumulative Cases per Million", title="") +
  guides(color=guide_legend(title="")) +
  theme(legend.title=element_text(size=18))+
  theme(axis.text.x = element_text(color="black",size = 16, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text(color="black",size = 16, angle = 0),
        axis.title.x = element_text(color="black",size = 18, angle = 0),
        axis.title.y = element_text(color="black",size = 18, angle = 90)
  )+
  # scale_x_continuous(breaks=seq(1, 10, 1))+
  theme(legend.text=element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  # scale_colour_manual(values=c(lineColors))

# Plot variable importance
df <- data.frame(imp = fit$variable.importance)
df2 <- df %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))
ggplot2::ggplot(df2) +
  geom_segment(aes(x = variable, y = 0, xend = variable, yend = imp), 
               size = 1.5, alpha = 0.7) +
  geom_point(aes(x = variable, y = imp, col = variable), 
             size = 4, show.legend = F) +
  coord_flip() +
  theme_bw()


###################################


