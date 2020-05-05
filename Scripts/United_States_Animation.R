library(tidyverse)
library(EpiEstim)
library(zoo)
library(VSURF)
library(caret)
library(doParallel)
library(ggplot2)
library(randomForest)
library(ranger)
library(viridis)
library(RColorBrewer)
library(randomcoloR)
library(reshape2)
# devtools::install_github("delabj/ggCyberPunk")
library(ggCyberPunk)
library(corrplot)
#
library(gridExtra)
library(grid)
library(ggpubr)
library(ggridges)
library(scales)
'%ni%' <- Negate('%in%')

'%ni%' <- Negate('%in%')
download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv","./InputData/Global_Mobility_Report.csv")
GoogData <- read.csv("./InputData/Global_Mobility_Report.csv", header=T, stringsAsFactors = F)
# Get just the USA country mobility stats
GoogData_sub <- subset(GoogData, country_region_code == "US" & sub_region_2 == "")
GoogData_sub$sub_region_1[GoogData_sub$sub_region_1==""] <- "USA"

names(GoogData_sub)
GoogData_sub2 <- GoogData_sub[,c("sub_region_1","date","retail_and_recreation_percent_change_from_baseline",
                                 "grocery_and_pharmacy_percent_change_from_baseline", 
                                 "parks_percent_change_from_baseline", 
                                 "transit_stations_percent_change_from_baseline",
                                 "workplaces_percent_change_from_baseline",
                                 "residential_percent_change_from_baseline")]
colnames(GoogData_sub2) <- c("Country.x","date","Google_Retail_recreation",
                             "Google_Grocery_pharmacy",
                             "Google_Parks",
                             "Google_Transit_stations",
                             "Google_Workplaces",
                             "Google_Residential")

# Merge the two dataframes by country name and date
GoogData_sub2$date <- as.Date(GoogData_sub2$date)
GoogData_sub2$Country.x <- as.character(GoogData_sub2$Country.x)
GoogData_sub2 <- as_tibble(GoogData_sub2)

# library(coronavirus)
# Update data?
# update_datasets()

# Explore the data
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv","./InputData/time_series_covid19_confirmed_US.csv")
CaseData <- read.csv("./InputData/time_series_covid19_confirmed_US.csv", header=T, stringsAsFactors = F)
CaseDataSub <- CaseData %>% select(Province_State,starts_with("X"))
for(j in 2:ncol(CaseDataSub)){
  d <- names(CaseDataSub)[j]
  d1 <- unlist(strsplit(d,"X"))[2]
  # dates in the format of: 2020-04-05
  cName <- format(as.Date(d1, format = "%m.%d.%y"),format = "%Y-%m-%d")
  jFrame <- aggregate(CaseDataSub[[d]], by=list(Country.x=CaseDataSub$Province_State), FUN=sum)
  colnames(jFrame) <- c("Country.x",cName)
  if(j == 2){
    CaseDataSubAgg <- jFrame
  }else{
    CaseDataSubAgg <- merge(CaseDataSubAgg, jFrame, by="Country.x")
  }
}
head(CaseDataSubAgg)
CaseDataSubAggDiff <- CaseDataSubAgg
for(j in 3:ncol(CaseDataSubAgg)){
  CaseDataSubAggDiff[,j] <- CaseDataSubAgg[,(j)] - CaseDataSubAgg[,(j-1)]
}
for(i in 1:nrow(CaseDataSubAggDiff)){
  window <- 7
  ts <- unlist(CaseDataSubAggDiff[i,2:ncol(CaseDataSubAggDiff)])
  CaseDataSubAggDiff[i,2:ncol(CaseDataSubAggDiff)] <- c(ts[1:((window-1)/2)],rollmean(ts, k=window, align = "center"),ts[(length(ts)-((window-1)/2)+1):length(ts)])
}
CaseDataSubAggMelt <- melt(CaseDataSubAggDiff, id="Country.x")
colnames(CaseDataSubAggMelt) <- c("Country.x", "date", "confirmed")
CaseDataSubAggMelt$date <- as.Date(CaseDataSubAggMelt$date)
str(CaseDataSubAggMelt)
str(GoogData_sub2)
merge1 <- GoogData_sub2 %>% full_join(CaseDataSubAggMelt, by = c("Country.x" = "Country.x", "date" = "date"))

download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv","./InputData/time_series_covid19_deaths_US.csv")
DeathData <- read.csv("./InputData/time_series_covid19_deaths_US.csv", header=T, stringsAsFactors = F)
DeathDataSub <- DeathData %>% select(Province_State,starts_with("X"))
for(j in 2:ncol(DeathDataSub)){
  d <- names(DeathDataSub)[j]
  d1 <- unlist(strsplit(d,"X"))[2]
  # dates in the format of: 2020-04-05
  cName <- format(as.Date(d1, format = "%m.%d.%y"),format = "%Y-%m-%d")
  jFrame <- aggregate(DeathDataSub[[d]], by=list(Country.x=DeathDataSub$Province_State), FUN=sum)
  colnames(jFrame) <- c("Country.x",cName)
  if(j == 2){
    DeathDataSubAgg <- jFrame
  }else{
    DeathDataSubAgg <- merge(DeathDataSubAgg, jFrame, by="Country.x")
  }
}
head(DeathDataSubAgg)
DeathDataSubAggDiff <- DeathDataSubAgg
for(j in 3:ncol(DeathDataSubAgg)){
  DeathDataSubAggDiff[,j] <- DeathDataSubAgg[,(j)] - DeathDataSubAgg[,(j-1)]
}
for(i in 1:nrow(DeathDataSubAggDiff)){
  window <- 7
  ts <- unlist(DeathDataSubAggDiff[i,2:ncol(DeathDataSubAggDiff)])
  DeathDataSubAggDiff[i,2:ncol(DeathDataSubAggDiff)] <- c(ts[1:((window-1)/2)],rollmean(ts, k=window, align = "center"),ts[(length(ts)-((window-1)/2)+1):length(ts)])
}
DeathDataSubAggMelt <- melt(DeathDataSubAggDiff, id="Country.x")
colnames(DeathDataSubAggMelt) <- c("Country.x", "date", "deaths")
DeathDataSubAggMelt$date <- as.Date(DeathDataSubAggMelt$date)
str(DeathDataSubAggMelt)
str(GoogData_sub2)
merge2 <- merge1 %>% full_join(DeathDataSubAggMelt, by = c("Country.x" = "Country.x", "date" = "date"))




#---NPI Density Animation---#########################################################################################################################################################################
# both <- subset(both, Country.x != "US")

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
unlink(paste0("./Output_CaseIncidence/npidensAnimation_","CaseIncidence.pdf"))
pdf(paste0("./Output_CaseIncidence/npidensAnimation_","CaseIncidence.pdf"),width = 8, height = 14)



dateRange <- seq(from=min(merge2$date,na.rm=T), to=max(merge2$date,na.rm=T), length.out = 23)
dateSplits <- seq(from=11, to=23, length.out = 4)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
NPIplotAnimation <- function(myNPI = npiList[1], myDate = as.Date("2020-03-28"), title = NA, xx = NA, merge2 = merge2){
  # startDate <- dateRange[1]
  # endDate <- dateRange[dateSplits[1]]
  # bothSub <- subset(both,date<=endDate & date>=startDate)
  # npiDensityPlotData <- bothSub[c("date", "Country.x",myNPI)]
  # npiDensityPlotDataMelted1 <- melt(npiDensityPlotData, id = c("date", "Country.x"))
  # npi1 <- ggplot(npiDensityPlotDataMelted1, aes(x = `value`, y = `Country.x`, fill = ..x..)) +
  #   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 3.) +
  #   scale_y_discrete(expand = c(0.0005, 0)) +
  #   scale_fill_viridis(name = "", option = "C") +
  #   labs(title = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")),
  #        subtitle = '')+
  #   xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))+
  #   theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
  # 
  # startDate <- dateRange[dateSplits[1]]
  # endDate <- dateRange[dateSplits[2]]
  # bothSub <- subset(both,date<=endDate & date>startDate)
  # npiDensityPlotData <- bothSub[c("date", "Country.x",myNPI)]
  # npiDensityPlotDataMelted2 <- melt(npiDensityPlotData, id = c("date", "Country.x"))
  # npi2 <- ggplot(npiDensityPlotDataMelted2, aes(x = `value`, y = `Country.x`, fill = ..x..)) +
  #   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 3.) +
  #   scale_y_discrete(expand = c(0.0005, 0)) +
  #   scale_fill_viridis(name = "", option = "C") +
  #   labs(title = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")),
  #        subtitle = '')+
  #   xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))+
  #   theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
  # 
  # startDate <- dateRange[dateSplits[2]]
  # endDate <- dateRange[dateSplits[3]]
  # bothSub <- subset(both,date<=endDate & date>startDate)
  # npiDensityPlotData <- bothSub[c("date", "Country.x",myNPI)]
  # npiDensityPlotDataMelted3 <- melt(npiDensityPlotData, id = c("date", "Country.x"))
  # npi3 <- ggplot(npiDensityPlotDataMelted3, aes(x = `value`, y = `Country.x`, fill = ..x..)) +
  #   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 3.) +
  #   scale_y_discrete(expand = c(0.0005, 0)) +
  #   scale_fill_viridis(name = "", option = "C") +
  #   labs(title = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")),
  #        subtitle = '')+
  #   xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))+
  #   theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
  # 
  # startDate <- dateRange[dateSplits[3]]
  # endDate <- dateRange[dateSplits[4]]
  # bothSub <- subset(both,date<=endDate & date>=startDate)
  # npiDensityPlotData <- bothSub[c("date", "Country.x",myNPI)]
  # npiDensityPlotDataMelted4 <- melt(npiDensityPlotData, id = c("date", "Country.x"))
  # npi4 <- ggplot(npiDensityPlotDataMelted4, aes(x = `value`, y = `Country.x`, fill = ..x..)) +
  #   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 3.) +
  #   scale_y_discrete(expand = c(0.0005, 0)) +
  #   scale_fill_viridis(name = "", option = "C") +
  #   labs(title = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")),
  #        subtitle = '')+
  #   xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))+
  #   theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
  if(myNPI %in% c("confirmed","deaths")){
    tmpy <- subset(merge2, Country.x != "USA" )
    both <- tmpy
  }else{
    both <- merge2
  }
  
  startDate <- myDate-5
  endDate <- myDate+5
  bothSub <- subset(both,date<=endDate & date>=startDate)
  npiDensityPlotData <- bothSub[c("date", "Country.x",myNPI)]
  # tmpy <- subset(npiDensityPlotData, Country.x == "US"); tmpy$Country.x <- "ZZZ"; tmpy[[myNPI]] <- 0
  # npiDensityPlotData <- rbind(npiDensityPlotData,tmpy)
  npiDensityPlotDataMelted5 <- melt(npiDensityPlotData, id = c("date", "Country.x"))
  npiDensityPlotDataMelted5$Country.x <- as.character(npiDensityPlotDataMelted5$Country.x)
  Clist1 <- unique(sort(npiDensityPlotDataMelted5$Country.x))
  if(myNPI %in% c("confirmed","deaths")){
    Clist <- Clist1
  }else{
    Clist <- c(Clist1[which(Clist1!="USA")], "USA")
  }
  npiDensityPlotDataMelted5$Country.x.num <- NA
  for(c in 1:nrow(npiDensityPlotDataMelted5)){
    myNum <- which(Clist == npiDensityPlotDataMelted5$Country.x[c])
    npiDensityPlotDataMelted5$Country.x.num[c] <- myNum
  }
  npiDensityPlotDataMelted5 <- npiDensityPlotDataMelted5[order(npiDensityPlotDataMelted5$Country.x.num),] 
  npi5 <- ggplot(npiDensityPlotDataMelted5, aes(x = `value`, y = `Country.x.num`, group = `Country.x.num`, fill = ..x.., point_color = ..x..)) +
    # npi5 <- ggplot(npiDensityPlotDataMelted5, aes(x = `value`, y = `Country.x`, group = `Country.x`, fill = ..x..)) +
    # geom_density_ridges_gradient() +
    # geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, gradient_lwd = 3., panel_scaling=F, bandwidth = 3) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001, gradient_lwd = 3,
      jittered_points = TRUE,
      point_shape = "|", point_size = 3, size = 0.25,
      position = position_points_jitter(height = 0)
    ) +
    # scale_y_discrete(expand = c(0, 0)) +
    # scale_y_continuous(breaks=c(1:length(Clist)),labels=c(Clist[1:(length(Clist)-1)],""),expand=c(0.0,2)) +
    scale_y_continuous(breaks=c(1:length(Clist)),labels=Clist,expand=c(0.0,.5)) +
    scale_fill_viridis(name = "", option = "C") +
    labs(title = paste0(format(myDate, format="%B %d")), subtitle = paste0(format(startDate, format="%B %d"), " - ",format(endDate, format="%B %d")))+
    theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank(), plot.title = element_text(margin = unit(c(0,0,0,0), "cm")))
  if(is.na(xx)){
    npi5 <- npi5 + xlab(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" ")))
  }else{
    npi5 <- npi5 + xlab(xx)
  }
  # npi5
  # Make commmon axis
  minx <- min(both[[myNPI]],na.rm=T)
  maxx <- max(both[[myNPI]],na.rm=T)
  lowerRange <- minx-(maxx-minx)*.15
  upperRange <- maxx+(maxx-minx)*.15
  outerBound <- c(abs(lowerRange),abs(upperRange))[which.max(c(abs(lowerRange),abs(upperRange)))]
  if((upperRange-lowerRange)>10){
    if(myNPI %in% c("confirmed","deaths")){
      minx <- min(both[[myNPI]],na.rm=T)
      maxx <- max(both[[myNPI]],na.rm=T)
      lowerRange <- minx-(maxx-minx)*.02
      upperRange <- maxx+(maxx-minx)*.02
      outerBound <- c(abs(lowerRange),abs(upperRange))[which.max(c(abs(lowerRange),abs(upperRange)))]
      # # npi1 <- npi1 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      # # npi2 <- npi2 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      # # npi3 <- npi3 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      # # npi4 <- npi4 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
      npi5 <- npi5 + scale_x_continuous(expand = c(0.01, 0), limits = c(0,upperRange)) + scale_fill_gradient2(limits = c(0, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
    }else{
      # npi1 <- npi1 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
      # npi2 <- npi2 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
      # npi3 <- npi3 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
      # npi4 <- npi4 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_viridis(alpha= 1, limits = c(-outerBound, outerBound), oob = scales::squish, name = "", option = "C")
      npi5 <- npi5 + scale_x_continuous(expand = c(0.01, 0), limits = c(lowerRange,upperRange)) + scale_fill_gradient2(limits = c(-outerBound, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))+
        scale_point_color_gradient(limits = c(-outerBound, outerBound), oob = scales::squish, name = "", low = muted("blue"), high = muted("red"))
    }
  }
  

  if(is.na(title)){
    if(myNPI == "movingAverage"){
      title_paste <- paste0("New Daily Cases Density")
    }else{
      title_paste <- paste0(simpleCap(paste(unlist(strsplit(myNPI,"_")), sep=" ", collapse=" "))," Density")
    }
  }else{
    title_paste <- title
  }

  
  gl <- list(npi5)
  final <- grid.arrange(grobs = gl, 
                        top = textGrob(title_paste, gp=gpar(fontsize=18)), 
                        layout_matrix = rbind( c(1)),
                        common.legend = TRUE, legend="bottom"
  )
  print(final)
}

# print(NPIplotter(myNPI = npiList[1]))


# for(d_d in 1:length(d_dRange)){
#   # for(n_n in 7){
#   print("Google_Workplaces")
#   print(d_d)
#   print(d_dRange[d_d])
#   print(NPIplotAnimation(myNPI = "Google_Workplaces", myDate = d_dRange[d_d]))
# }


# install.packages("magick")
# install.packages("animation")
library(animation)
library(magick)
d_dRange <- as.Date(c(as.Date(min(merge2$date,na.rm=T)):as.Date(max(merge2$date,na.rm=T))))
# d_dRange <- as.Date(c(as.Date("2020-04-05"):as.Date("2020-04-15")))

# d_dRange <- seq(from=as.Date("2020-04-05"), to=as.Date("2020-04-13"), length.out = 8)


npiList <- c("confirmed",
             "deaths",
             "Google_Residential",
             "Google_Workplaces",
             "Google_Transit_stations",
             "Google_Parks",
             "Google_Grocery_pharmacy",
             "Google_Retail_recreation"
            )

titleList <- c("11 Day Sliding Window of Confirmed New Daily Cases",
               "11 Day Sliding Window of Confirmed New Daily Deaths",
               "11 Day Sliding Window of Percent Change in Visits to Residential Areas", 
               "11 Day Sliding Window of Percent Change in Visits to Workplaces", 
               "11 Day Sliding Window of Percent Change in Visits to Transit Stations",
               "11 Day Sliding Window of Percent Change in Visits to Parks", 
               "11 Day Sliding Window of Percent Change in Visits to Grocery & Pharmacy", 
               "11 Day Sliding Window of Percent Change in Visits to Retail & Recreation"
              )

xxList <- c("Confirmed New Daily Cases",
            "Confirmed New Daily Deaths",
            "Percent Change in Visits to Residential Areas", 
            "Percent Change in Visits to Workplaces", 
            "Percent Change in Visits to Transit Stations",
            "Percent Change in Visits to Parks", 
            "Percent Change in Visits to Grocery & Pharmacy", 
            "Percent Change in Visits to Retail & Recreation"
            )

# for(n_n in 1:length(npiList)){
for(n_n in 1:1){
  print(n_n)
  print(npiList[n_n])
  saveGIF({
    for (d_d in 1:length(d_dRange)){
      NPIplotAnimation(myNPI = npiList[n_n], myDate = d_dRange[d_d], title = titleList[n_n], xx = xxList[n_n], merge2 = merge2)}
  }, interval = .2, movie.name=paste0("USA",npiList[n_n],".gif"),ani.width = 600, ani.height = 750)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

dev.off()

