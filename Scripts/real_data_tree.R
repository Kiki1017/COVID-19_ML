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

# make country lists
training_countries <- c("CHN","KOR","ITA","ESP")
testing_countries <- c("USA")
# testing_countries <- c("GBR")
# testing_countries <- c("AUS")
# testing_countries <- c("BRA")


# subset to 100 cumulative cases as starting time threshold and add time column
for(i in 1:length(training_countries)){
  training_subset <- subset(data_clean,ISO3 %in% training_countries[i])
  start <- which(training_subset$confirmed_cum >= 100)[1]
  training_subset_aligned <- training_subset[start:nrow(training_subset),]
  training_subset_aligned$time <- c(1:nrow(training_subset_aligned))
  if(i==1){
    training_ready <- training_subset_aligned
  }else{
    training_ready <- as.data.frame(rbind(training_ready,training_subset_aligned))
  }
}

projectionTime <- 0

for(i in 1:length(testing_countries)){
  testing_subset <- subset(data_clean,ISO3 %in% testing_countries[i])
  start <- which(testing_subset$confirmed_cum >= 100)[1]
  testing_subset_aligned <- testing_subset[start:nrow(testing_subset),]
  tmp <- testing_subset_aligned[1:projectionTime,]
  tmp[,c("confirmed_cum",
         "confirmed_cum_lag_01",
         "confirmed_cum_lag_02",
         "confirmed_cum_lag_03",
         "confirmed_cum_lag_04",
         "confirmed_cum_lag_05")] <- NA
  testing_subset_aligned_predictNA <- rbind(testing_subset_aligned,tmp)
  testing_subset_aligned_predictNA$time <- c(1:nrow(testing_subset_aligned_predictNA))
  if(i==1){
    testing_ready <- testing_subset_aligned_predictNA
  }else{
    testing_ready <- as.data.frame(rbind(testing_ready,testing_subset_aligned_predictNA))
  }
}

##########################################################################################################################################################################
lineColors <- c("firebrick","darkgoldenrod1", "darkviolet", "limegreen", "dodgerblue")
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
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values=c(lineColors))
##########################################################################################################################################################################


str(training_ready)
str(testing_ready)

fit <- rpart(confirmed_cum ~ time+
               confirmed_cum_lag_01+
               confirmed_cum_lag_02+
               confirmed_cum_lag_03+
               confirmed_cum_lag_04+
               confirmed_cum_lag_05+
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
               Ave_household_size+
               EFindex,
             data=training_ready, 
             method="poisson", #"anova", "poisson", "class" or "exp"
             control=rpart.control(minsplit=20, cp=0.001))
summary(fit)
rpart.plot(fit, main="Tree")

p1 <- predict(fit, testing_ready, na.action = na.pass)
p1 <- as.data.frame(p1)
p1$time <- testing_ready$time

plot1Data_tmp <- testing_ready[,c("FullName","time","confirmed_cum")]
plot1Data <- merge(p1,plot1Data_tmp,by="time")
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

lineColors <- c("firebrick","darkgoldenrod1", "darkviolet", "dodgerblue", "forestgreen")
ggplot() +
  geom_line(data=subset(m1, variable == "actual"), aes(x = time, y = value, group = country, color = country), size=0.8,alpha=.7)+
  geom_line(data=subset(m1, variable == "prediction"), aes(x = time, y = value, group = country, color = country), size=0.85, linetype = "3313",alpha=.7)+
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
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(values=c(lineColors))


###################################


