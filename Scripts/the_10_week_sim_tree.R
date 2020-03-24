library(tidyverse)
library(readxl)
library(reshape2)
library(rpart)
library(ggplot2)


training <- read_excel("./InputData/the_10_week_sim.xlsx", sheet = "training", col_names = T)
str(training)
training$N_cases_lag1 <- as.numeric(training$N_cases_lag1)
training$N_cases_lag2 <- as.numeric(training$N_cases_lag2)
str(training)

testing_half <- read_excel("./InputData/the_10_week_sim.xlsx", sheet = "testing_half", col_names = T)
str(testing_half)
testing_half$N_cases_lag1 <- as.numeric(testing_half$N_cases_lag1)
testing_half$N_cases_lag2 <- as.numeric(testing_half$N_cases_lag2)
str(testing_half)

testing_full <- read_excel("./InputData/the_10_week_sim.xlsx", sheet = "testing_full", col_names = T)
str(testing_full)
testing_full$N_cases_lag1 <- as.numeric(testing_full$N_cases_lag1)
testing_full$N_cases_lag2 <- as.numeric(testing_full$N_cases_lag2)
str(testing_full)

fit <- rpart(N_cases ~ time+N_cases_lag1+N_cases_lag2+aged_less_65+degree_isolation+health_preparedness1+health_preparedness2, data=training, method="class",control=rpart.control(minsplit=2, cp=0.001)) 
summary(fit)
plot(fit)
text(fit)

cList <- c(rep("C1",10),rep("C2",10),rep("C3",10))
p1 <- predict(fit, testing_half,
        type = c("vector"),
        na.action = na.pass)
plot1Data <- as.data.frame(cbind(cList,as.numeric(testing_half$time),as.numeric(p1),as.numeric(testing_half$N_cases)))
colnames(plot1Data) <- c("country","time","prediction","actual")
plot1Data$time <- as.character(plot1Data$time)
plot1Data$time <- as.numeric(plot1Data$time)
plot1Data <- plot1Data[order(plot1Data$time),]
plot1Data$prediction <- as.numeric(plot1Data$prediction)
plot1Data$actual <- as.numeric(plot1Data$actual)
plot1Data$country <- as.factor(plot1Data$country)
str(plot1Data)
m1 <- melt(plot1Data,id=c("country","time"))
m1$time <- as.numeric(m1$time)
m1$value <- as.numeric(m1$value)
m1$variable <- as.factor(m1$variable)
m1$country <- as.factor(m1$country)
str(m1)
m1 <- m1[order(m1$time),]

lineColors <- c("firebrick", "darkgoldenrod1", "forestgreen", "darkgoldenrod1", "firebrick")
ggplot() +
  geom_line(data=subset(m1, variable == "actual"), aes(x = time, y = value, group = country, color = country), size=0.8,alpha=.7)+
  geom_line(data=subset(m1, variable == "prediction"), aes(x = time, y = value, group = country, color = country), size=0.85, linetype = "3313",alpha=.7)+
  labs(x="time", y = "N Cases", title="") +
  guides(color=guide_legend(title="Country")) +
  theme(legend.title=element_text(size=18))+
  theme(axis.text.x = element_text(color="black",size = 16, angle = 30, hjust = .5, vjust = .5),
        axis.text.y = element_text(color="black",size = 16, angle = 0),
        axis.title.x = element_text(color="black",size = 20, angle = 0),
        axis.title.y = element_text(color="black",size = 20, angle = 90)
  )+
  scale_x_continuous(breaks=seq(1, 10, 1))+
  theme(legend.text=element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(" ", values=c("C1" = lineColors[1], "C2" = lineColors[2], "C3" = lineColors[3]))


###################################

cList <- c(rep("C1",10),rep("C2",10),rep("C3",10))
p2 <- predict(fit, testing_full,
              type = c("vector"),
              na.action = na.pass)
plot2Data <- as.data.frame(cbind(cList,as.numeric(testing_full$time),as.numeric(p2),as.numeric(testing_full$N_cases)))
colnames(plot2Data) <- c("country","time","prediction","actual")
plot2Data$time <- as.character(plot2Data$time)
plot2Data$time <- as.numeric(plot2Data$time)
plot2Data <- plot2Data[order(plot2Data$time),]
plot2Data$prediction <- as.numeric(plot2Data$prediction)
plot2Data$actual <- as.numeric(plot2Data$actual)
plot2Data$country <- as.factor(plot2Data$country)
str(plot2Data)
m2 <- melt(plot2Data,id=c("country","time"))
m2$time <- as.numeric(m2$time)
m2$value <- as.numeric(m2$value)
m2$variable <- as.factor(m2$variable)
m2$country <- as.factor(m2$country)
str(m2)
m2 <- m2[order(m2$time),]

lineColors <- c("firebrick", "darkgoldenrod1", "forestgreen", "darkgoldenrod1", "firebrick")
ggplot() +
  geom_line(data=subset(m2, variable == "actual"), aes(x = time, y = value, group = country, color = country), size=0.8,alpha=.7)+
  geom_line(data=subset(m2, variable == "prediction"), aes(x = time, y = value, group = country, color = country), size=0.85, linetype = "3313",alpha=.7)+
  labs(x="time", y = "N Cases", title="") +
  guides(color=guide_legend(title="Country")) +
  theme(legend.title=element_text(size=18))+
  theme(axis.text.x = element_text(color="black",size = 16, angle = 30, hjust = .5, vjust = .5),
        axis.text.y = element_text(color="black",size = 16, angle = 0),
        axis.title.x = element_text(color="black",size = 20, angle = 0),
        axis.title.y = element_text(color="black",size = 20, angle = 90)
  )+
  scale_x_continuous(breaks=seq(1, 10, 1))+
  theme(legend.text=element_text(size=16))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_colour_manual(" ", values=c("C1" = lineColors[1], "C2" = lineColors[2], "C3" = lineColors[3]))


