#####################################
##  SEIR functions
##  Vanja Dukic, April 9, 2020
#####################################
library(scales)
library(ggrepel)
library(cowplot)
############ Basic SEIR ODE function
data_clean <- read.csv("./InputData/ML_features.csv")
data_clean$date <- as.Date(data_clean$date)
# Looking at the data
# glimpse(data_clean)
# summary(data_clean)
pdf(paste0("./Output_R0/finalPlot_SEIR.pdf"),width = 10, height = 14)
training_countries_all <- c("ITA","GBR","ZAF","BRA","ESP","MYS","KOR","USA","SWE","AUT","CHE","DEU","FRA","DZA","IRN","CAN","TUR","BEL","ANT","PRT","ISR","RUS","NOR","IRL","AUS","IND","DNK","CHL","CZE","JPN","UKR","MAR","ARG","SGP","ROU")
for(cc in 1:length(training_countries_all)){
# for(cc in 1:1){
  testing_country <- training_countries_all[cc]
  
  #---Estimating reproduction numbers, R0---#########################################################################################################################################################################
  # We are using the epiestim package to calculate the R0 for countries. This requires two things
  # 1. specific information about the serial intervals for COVID
  # 2. the timeseries incidence data.
  # "SI for Serial Intervals.
  # Determination of the serial interval, the time between the start of symptoms in the primary patient (infector)
  # and onset of symptoms in the patient receiving that infection from the infector (the infectee)"
  # Table 1 from https://www.medrxiv.org/content/10.1101/2020.04.13.20062760v1
  # "we calculate a weighted mean of the published parameters and inferred a serial interval described
  # by a gamma distribution, parameterised with mean SI of 4.56 days (credible interval: 2.54 - 7.36)
  # and standard deviation 4.53 days (credible interval 4.17 - 5.05)."
  
  serialIntervals = tibble(
    mean_si_estimate = c(3.96, 6.3, 4.22, 4.56, 3.95, 5.21, 4.7, 7.5,6.6),
    mean_si_estimate_low_ci = c(3.53, 5.2, 3.43, 2.69,-4.47, -3.35, 3.7, 5.3, 0.7),
    mean_si_estimate_high_ci = c(4.39, 7.6, 5.01, 6.42, 12.51,13.94, 6.0, 19.0, 19.0),
    std_si_estimate = c(4.75,4.2, 0.4, 0.95, 4.24, 4.32, 2.3, 3.4, NA),
    std_si_estimate_low_ci = c(4.46, 3.1, NA, NA, 4.03, 4.06, 1.6, NA, NA),
    std_si_estimate_high_ci = c(5.07, 5.3, NA, NA, 4.95, 5.58, 3.5, NA, NA),
    sample_size = c(468,48,135,93,45,54,28,16,90),
    population = c("China", "Shenzhen","Taijin","Singapore","Taijin","Singapore", "SE Asia", "Wuhan","Italy"),
    source = c(
      "Zhanwei Du et al. Serial Interval of COVID-19 among Publicly Reported Confirmed Cases. Emerging Infectious Disease journal 26, (2020)",
      "Bi, Q. et al. Epidemiology and Transmission of COVID-19 in Shenzhen China: Analysis of 391 cases and 1,286 of their close contacts. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.03.20028423",
      "Tindale, L. et al. Transmission interval estimates suggest pre-symptomatic spread of COVID-19. Epidemiology (2020) doi:10.1101/2020.03.03.20029983",
      "Tindale, L. et al. Transmission interval estimates suggest pre-symptomatic spread of COVID-19. Epidemiology (2020) doi:10.1101/2020.03.03.20029983",
      "Ganyani, T. et al. Estimating the generation interval for COVID-19 based on symptom onset data. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.05.20031815",
      "Ganyani, T. et al. Estimating the generation interval for COVID-19 based on symptom onset data. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.05.20031815",
      "Nishiura, H., Linton, N. M. & Akhmetzhanov, A. R. Serial interval of novel coronavirus (COVID-19) infections. Int. J. Infect. Dis. (2020) doi:10.1016/j.ijid.2020.02.060",
      "Li, Q. et al. Early Transmission Dynamics in Wuhan, China, of Novel Coronavirus-Infected Pneumonia. N. Engl. J. Med. (2020) doi:10.1056/NEJMoa2001316",
      "Cereda, D. et al. The early phase of the COVID-19 outbreak in Lombardy, Italy. arXiv [q-bio.PE] (2020)")
  )
  
  unk=function(x) ifelse(is.na(x),"unk",x)
  
  SItable1 = serialIntervals %>% mutate(
    `Mean SI\n(95% CrI) days`=paste0(mean_si_estimate,"\n(",unk(mean_si_estimate_low_ci),"-",
                                     unk(mean_si_estimate_high_ci),")"),
    `Std SI\n(95% CrI) days`=paste0(unk(std_si_estimate),"\n(",unk(std_si_estimate_low_ci),"-",unk(std_si_estimate_high_ci),")")
  ) %>% select(-contains("estimate")) %>% select(
    `Reference`=source,
    `Mean SI\n(95% CrI) days`,
    `Std SI\n(95% CrI) days`,
    `N`=sample_size,
    `Population`=population
  )
  
  wtSIs = serialIntervals %>% summarise(
    mean_si = weighted.mean(mean_si_estimate,sample_size,na.rm = TRUE),
    min_mean_si = weighted.mean(mean_si_estimate_low_ci,sample_size,na.rm = TRUE),
    max_mean_si = weighted.mean(mean_si_estimate_high_ci,sample_size,na.rm = TRUE),
    std_si  = weighted.mean(ifelse(is.na(std_si_estimate_low_ci),NA,1)*std_si_estimate,sample_size,na.rm = TRUE),
    min_std_si  = weighted.mean(std_si_estimate_low_ci,sample_size,na.rm = TRUE),
    max_std_si  = weighted.mean(std_si_estimate_high_ci,sample_size,na.rm = TRUE)
    #total = sum(sample_size)
  ) %>% mutate(
    std_mean_si = (max_mean_si - min_mean_si) / 3.92, # TODO: fit gamma
    std_std_si = (max_std_si - min_std_si) / 3.92
  )
  
  testing_subset <- subset(data_clean,ISO3 == testing_country)
  start <- which(testing_subset$confirmed_cum >= 12)[1]
  testing_subset_aligned <- testing_subset[start:nrow(testing_subset),]
  
  # Smooth out incidence using moving average with a centered window of 7 datapoints (3 to the left, 3 to the right)
  # make sure the window is an odd integer
  window <- 7
  # dim(testing_subset_aligned)
  # length(rollmean(testing_subset_aligned$confirmed, k=window))
  testing_subset_aligned$movingAverage <- c(testing_subset_aligned$confirmed[1:((window-1)/2)],rollmean(testing_subset_aligned$confirmed, k=window, align = "center"),testing_subset_aligned$confirmed[(nrow(testing_subset_aligned)-((window-1)/2)+1):nrow(testing_subset_aligned)])
  # Plot cases
  # gg <- ggplot(testing_subset_aligned) +
  #   geom_line(aes(x=date, y=confirmed),color="red") +
  #   geom_line(aes(x=date, y=movingAverage),color="blue") +
  #   ggtitle(paste0("COVID INCIDENCE IN ", testing_country))
  # print(gg)
  # Add moving average day lag and one day difference variables
  testing_subset_aligned[["movingAverage_Lag_1"]] <- lag(testing_subset_aligned[["movingAverage"]],1)
  testing_subset_aligned[["movingAverage_Lag_3"]] <- lag(testing_subset_aligned[["movingAverage"]],3)
  testing_subset_aligned[["movingAverage_Lag_7"]] <- lag(testing_subset_aligned[["movingAverage"]],7)
  testing_subset_aligned[["movingAverage_Lag_14"]] <- lag(testing_subset_aligned[["movingAverage"]],14)
  testing_subset_aligned[["movingAverage_diff_1_3"]] <- testing_subset_aligned[["movingAverage_Lag_1"]] - testing_subset_aligned[["movingAverage_Lag_3"]]
  testing_subset_aligned[["movingAverage_diff_1_7"]] <- testing_subset_aligned[["movingAverage_Lag_1"]] - testing_subset_aligned[["movingAverage_Lag_7"]]
  testing_subset_aligned[["movingAverage_diff_1_14"]] <- testing_subset_aligned[["movingAverage_Lag_1"]] - testing_subset_aligned[["movingAverage_Lag_14"]]
  testing_subset_aligned[["movingAverage_diff_3_7"]] <- testing_subset_aligned[["movingAverage_Lag_3"]] - testing_subset_aligned[["movingAverage_Lag_7"]]
  testing_subset_aligned[["movingAverage_diff_7_14"]] <- testing_subset_aligned[["movingAverage_Lag_7"]] - testing_subset_aligned[["movingAverage_Lag_14"]]
  
  # toCalcR0 <- testing_subset_aligned[,c("date","confirmed")]
  toCalcR0 <- testing_subset_aligned[,c("date","movingAverage")]
  colnames(toCalcR0) <- c("dates","I")
  toCalcR0$I[toCalcR0$I<0] <- NA
  #Get of erroneous negative counts... they sneak throught the API sometimes. 
  # But if thre is a negative at teh end... are the last one lets just make it equal to the n-1 one
  if(is.na(tail(toCalcR0$I,1))){
    toCalcR0$I[length(toCalcR0$I)] <- toCalcR0$I[length(toCalcR0$I)-1]
  }
  # If the NA is not at the end, Lets linearly interpolate them:
  toCalcR0$I <- na.approx(toCalcR0$I)
  toCalcR0$I <- as.integer(toCalcR0$I)
  rownames(toCalcR0) <- c(1:nrow(toCalcR0))
  # res_uncertain_si <- estimate_R(toCalcR0,
  #                                method = "uncertain_si",
  #                                config = config) 
  # res <- wallinga_teunis(toCalcR0, method="parametric_si",
  #                        config = list(si_parametric_distr = "G", 
  #                                      t_start = seq(2, nrow(toCalcR0)-6), t_end = seq(8, nrow(toCalcR0)),
  #                                      mean_si = 4.7, min_mean_si = 3.6, max_mean_si = 6.0, 
  #                                      std_si = 2.9, min_std_si = 1.9, max_std_si = 4.9,
  #                                      n_sim = 100))
  res_uncertain_si <- wallinga_teunis(toCalcR0, method="parametric_si",
                                      config = list(si_parametric_distr = "G", 
                                                    t_start = seq(1, nrow(toCalcR0)-6), t_end = seq(7, nrow(toCalcR0)),
                                                    mean_si = wtSIs$mean_si, 
                                                    std_mean_si = wtSIs$std_mean_si,
                                                    min_mean_si = wtSIs$min_mean_si, 
                                                    max_mean_si = wtSIs$max_mean_si,
                                                    std_si = wtSIs$std_si, 
                                                    std_std_si = wtSIs$std_si,
                                                    min_std_si = wtSIs$min_std_si, 
                                                    max_std_si = wtSIs$max_std_si,
                                                    n_sim = 100))
  
  seir <- function(t,x,parms)
  {
    S <- x["S"]
    E <- x["E"]
    I <- x["I"]
    R <- x["R"]
    with(as.list(c(parms)),{
      ds <- -b*I*S
      de <- b*I*S - a*E
      di <- a*E - r*I
      dr <- r*I
      der <- c(ds,de,di,dr)
      list(der)
    })
  }
  
  ###################### SEIR Solver function
  
  SEIR.determ <- function(parms, initial, time.window, ntime){
    require(deSolve)
    times <- seq(time.window[1], time.window[2], length=ntime)
    as.data.frame(lsoda(initial, times, seir, parms))
  }
  
  ### example: run the above model as follows
  #
  # γ [(95% confidence interval (CI)] to be: 0.154 (95% CI=0.0721-0.238) http://jtd.amegroups.com/article/view/36385/html
  # Because the incubation period of the SARS-CoV-2 has been reported to be between 2 to 14 days (2,10,11,12), we chose the midpoint of 7 days
  # RctSeries <- c(rep(3,20),rep(2,40),rep(1,30),rep(0.8,10))
  # RctSeries <- c(rep(3,100))
  timeExtension = 100
  RctSeries <- c(res_uncertain_si[["R"]]$`Mean(R)`,rep(tail(res_uncertain_si[["R"]]$`Mean(R)`,1),timeExtension))
  initialInfectedSeq <- c(seq(from=1,to=1001,by=100), seq(from=1301,to=4901,by=300))
  for(II in 1:length(initialInfectedSeq)){
    initialInfected <- initialInfectedSeq[II]
    
    for(i in 1:length(RctSeries)){
      Rct = RctSeries[i]
      tmax = 1
      t.seq <- seq(0,25,.1);
      N <- testing_subset_aligned$Population_mill[1]*1000000
      if(i == 1){
        initial <- c(S=(N-initialInfected)/N, E=0/N, I=initialInfected/N, R=0/N);
      }else{
        initial <- c(S=seirFinal$S[nrow(seirFinal)], E=seirFinal$E[nrow(seirFinal)], I=seirFinal$I[nrow(seirFinal)], R=seirFinal$R[nrow(seirFinal)]);
      }
      parms <- c(b=0.154*Rct,a=1/7,r=0.154);
      time.window <- c(i-1, i);
      ntime = 100
      
      seir1 <- SEIR.determ(parms, initial, time.window, ntime)
      seir1[seir1<0]<-0
      names(seir1)
      # "time" "S"    "E"    "I"    "R"
      if(i == 1){
        seirFinal <- seir1
      }else{
        seirFinal <- rbind(seirFinal,seir1)
      }
    }
    IpeakDiff <- abs(max(seirFinal$I*N)-max(testing_subset_aligned$movingAverage))
    if(II == 1){
      chosen_initialInfected <- initialInfected
      chosen_IpeakDiff <- IpeakDiff
    }else if(IpeakDiff < chosen_IpeakDiff){
      chosen_initialInfected <- initialInfected
      chosen_IpeakDiff <- IpeakDiff
    }
  }
  
  print(chosen_initialInfected)
  print(chosen_IpeakDiff)
  
  ################################
  
  initialInfected <- chosen_initialInfected
  
  for(i in 1:length(RctSeries)){
    Rct = RctSeries[i]
    tmax = 1
    t.seq <- seq(0,25,.1);
    N <- testing_subset_aligned$Population_mill[1]*1000000
    if(i == 1){
      initial <- c(S=(N-initialInfected)/N, E=0/N, I=initialInfected/N, R=0/N);
    }else{
      initial <- c(S=seirFinal$S[nrow(seirFinal)], E=seirFinal$E[nrow(seirFinal)], I=seirFinal$I[nrow(seirFinal)], R=seirFinal$R[nrow(seirFinal)]);
    }
    parms <- c(b=0.154*Rct,a=1/7,r=0.154);
    time.window <- c(i-1, i);
    ntime = 100
    
    seir1 <- SEIR.determ(parms, initial, time.window, ntime)
    seir1[seir1<0]<-0
    names(seir1)
    # "time" "S"    "E"    "I"    "R"
    if(i == 1){
      seirFinal <- seir1
    }else{
      seirFinal <- rbind(seirFinal,seir1)
    }
  }
  
  
  minDate <- as.Date(testing_subset_aligned$date[1])-1
  maxDate <- as.Date("2020-05-14")+timeExtension
  
  
  testing_subset_aligned$R0 <- NA 
  testing_subset_aligned$R0[head(res_uncertain_si[["R"]]$`t_start`,1):tail(res_uncertain_si[["R"]]$`t_start`,1)] <- res_uncertain_si[["R"]]$`Mean(R)`
  RDF <- testing_subset_aligned[,c("date","R0")]
  colnames(RDF) <- c("Date", "R")
  R_gg <- plot(res_uncertain_si, "R")
  R_gg <- R_gg +
    scale_x_date(date_breaks = "2 week", date_labels =  "%b %d", limits=c(minDate, maxDate))+
    xlab(NULL)+
    theme(legend.position = "top")+
    theme(text = element_text(size=14))+
    ggtitle(NULL)+
    scale_y_continuous(label=comma,
                       # Features of the first axis
                       name = "R", 
                       # Add a second axis and specify its features
                       sec.axis = sec_axis( trans=~., name="R"))
  
  # seirPlotDF <- seirFinal
  # seirPlotDF$date <- seq(from=as.POSIXct(paste(res_uncertain_si$dates[1], "00:00:00"),tz = "America/Chicago"), to=as.POSIXct(paste(res_uncertain_si$dates[length(res_uncertain_si$dates)], "23:59:59"),tz = "America/Chicago"),length.out=nrow(seirFinal))
  # seirPlotDF_melt <- melt(seirPlotDF, id=c("date","time"))
  # seirPlotDF_melt$date <- as.Date(seirPlotDF_melt$date)
  # seirPlotDF_melt$value <- seirPlotDF_melt$value*100
  # seir_gg <- ggplot(seirPlotDF_melt,aes(x=date,y=value,color=variable))+geom_line(linetype = "twodash")+
  #   scale_color_manual(values=c("black", "red", "green", "blue"))+
  #   theme(legend.position = "top")+
  #   labs(colour = NULL)+
  #   ylab("% of Population")+
  #   xlab(NULL)+
  #   scale_x_date(date_breaks = "2 week", date_labels =  "%b %d", limits=c(minDate, maxDate))+
  #   # ggtitle(testing_country)+
  #   theme(text = element_text(size=14))+
  #   ylim(0,max(seirScaledPlotDF$R)*100)
  
  seirScaledPlotDF <- seirFinal
  seirScaledPlotDF$date <- seq(from=as.POSIXct(paste(res_uncertain_si$dates[1], "00:00:00"),tz = "America/Chicago"), to=as.POSIXct(paste(as.Date(res_uncertain_si$dates[length(res_uncertain_si$dates)]+timeExtension), "23:59:59"),tz = "America/Chicago"),length.out=nrow(seirFinal))
  labelMax <- max(seirScaledPlotDF$I)
  seirScaledPlotDF_melt <- melt(seirScaledPlotDF, id=c("date","time"))
  seirScaledPlotDF_melt$date <- as.Date(seirScaledPlotDF_melt$date)
  seirScaledPlotDF_melt$value <- seirScaledPlotDF_melt$value*N
  dataMax <- seirScaledPlotDF_melt %>% group_by(variable) %>% summarise(Max = max(value))
  dataMin <- seirScaledPlotDF_melt %>% group_by(variable) %>% summarise(Min = min(value))
  dataMax$Min <- dataMin$Min
  dataMax$text <- c(paste0(format.Date(seirScaledPlotDF$date[which(seirScaledPlotDF$S == dataMax$Min[1]/N)],"%b %d"), ", ", "Min S: ", formatC(dataMax$Min[1], format="f", big.mark=",", digits=0))[1],
                    paste0(format.Date(seirScaledPlotDF$date[which(seirScaledPlotDF$E == dataMax$Max[2]/N)],"%b %d"), ", ", "Max E: ", formatC(dataMax$Max[2], format="f", big.mark=",", digits=0))[1],
                    paste0(format.Date(seirScaledPlotDF$date[which(seirScaledPlotDF$I == dataMax$Max[3]/N)],"%b %d"), ", ", "Max I: ", formatC(dataMax$Max[3], format="f", big.mark=",", digits=0))[1],
                    paste0(format.Date(seirScaledPlotDF$date[which(seirScaledPlotDF$R == dataMax$Max[4]/N)],"%b %d"), ", ", "Max R: ", formatC(dataMax$Max[4], format="f", big.mark=",", digits=0))[1]
                    )
  seirScaledPlotDF_melt$label <- rep(NA,nrow(seirScaledPlotDF_melt))
  seirScaledPlotDF_melt$label[which(seirScaledPlotDF_melt$value == dataMax$Min[1] & seirScaledPlotDF_melt$variable == "S")[1]] <- dataMax$text[1]
  seirScaledPlotDF_melt$label[which(seirScaledPlotDF_melt$value == dataMax$Max[2] & seirScaledPlotDF_melt$variable == "E")[1]] <- dataMax$text[2]
  seirScaledPlotDF_melt$label[which(seirScaledPlotDF_melt$value == dataMax$Max[3] & seirScaledPlotDF_melt$variable == "I")[1]] <- dataMax$text[3]
  seirScaledPlotDF_melt$label[which(seirScaledPlotDF_melt$value == dataMax$Max[4] & seirScaledPlotDF_melt$variable == "R")[1]] <- dataMax$text[4]
  seirScaledPlotDF_melt$label[which(seirScaledPlotDF_melt$value == initialInfected & seirScaledPlotDF_melt$variable == "I")[1]] <- 
    paste0(format.Date(seirScaledPlotDF$date[which(seirScaledPlotDF$I == initialInfected/N)],"%b %d"), ", ", "Initial I: ", formatC(initialInfected, format="f", big.mark=",", digits=0))[1]
  seirScaledPlotDF_melt$label[tail(which(round(seirScaledPlotDF_melt$value) == round(initialInfected/1) & seirScaledPlotDF_melt$variable == "I"),1)] <- 
    tail(paste0(format.Date(seirScaledPlotDF$date[which(round(seirScaledPlotDF$I*N) == round(initialInfected/1))],"%b %d"), ", ", "I: ", formatC(initialInfected/1, format="f", big.mark=",", digits=0)),1)
  seirScaledPlotDF_melt$label[tail(which(round(seirScaledPlotDF_melt$value) == round(initialInfected/10) & seirScaledPlotDF_melt$variable == "I"),1)] <- 
    tail(paste0(format.Date(seirScaledPlotDF$date[which(round(seirScaledPlotDF$I*N) == round(initialInfected/10))],"%b %d"), ", ", "I: ", formatC(initialInfected/10, format="f", big.mark=",", digits=0)),1)
  
  unique(seirScaledPlotDF_melt$label)
  
  seirScaled_gg <- ggplot(seirScaledPlotDF_melt,aes(x=date,y=value,color=variable))+
    geom_line(linetype = "twodash")+
    scale_color_manual(values=c("black", "red", "green", "blue"))+
    scale_fill_manual(values=c("black", "red", "green", "blue"))+
    theme(legend.position = "top")+
    labs(colour = NULL)+
    xlab(NULL)+
    scale_x_date(date_breaks = "2 week", date_labels =  "%b %d", limits=c(minDate, maxDate))+
    # ggtitle(testing_country)+
    theme(text = element_text(size=14))+
    # ylim(0,max(seirScaledPlotDF$R)*N)+
    scale_y_continuous(label=comma,
      # Features of the first axis
      name = "Population", 
      limits = c(0,max(seirScaledPlotDF$R)*N*1.1),
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~./N*100, name="% Population")
    )+
    geom_label_repel(aes(label = seirScaledPlotDF_melt$label,
                         fill = factor(seirScaledPlotDF_melt$variable)), color = 'white',
                     size = 3.5, show.legend = F,
                     segment.color = "black", box.padding = 0.5
                     ) 
  
  
  incidenceDF <- testing_subset_aligned[,c("date","confirmed","movingAverage")]
  colnames(incidenceDF) <- c("Date", "Confirmed", "Moving Average")
  merge1 <- seirScaledPlotDF[,c("date","I")]
  colnames(merge1) <- c("date","SEIR I-Curve")
  merge1$date <- as.Date(merge1$date)
  incidenceDF_premelt <- merge(merge1,incidenceDF,seirScaledPlotDF,by.x="date",by.y="Date",all.x=T,all.y=T)
  incidenceDF_premelt$`SEIR I-Curve` <- incidenceDF_premelt$`SEIR I-Curve`*N
  incidenceDF_melt <- melt(incidenceDF_premelt, id=c("date"))
  incidenceDF_melt$date <- as.Date(incidenceDF_melt$date)
  incidence_gg <- ggplot(incidenceDF_melt,aes(x=date,y=value,color=variable))+geom_line(linetype = "twodash")+
    scale_color_manual(values=c("green","orange","brown4"))+
    theme(legend.position = "top")+
    labs(colour = NULL)+
    xlab(NULL)+
    scale_x_date(date_breaks = "2 week", date_labels =  "%b %d", limits=c(minDate, maxDate))+
    ggtitle(paste0("COVID-19 Cases in ", as.character(testing_subset_aligned$Country.x[1]))) +
    theme(text = element_text(size=14))+
    scale_y_continuous(label=comma,
                       # Features of the first axis
                       name = "New Daily Cases", 
                       # Add a second axis and specify its features
                       sec.axis = sec_axis( trans=~., name="New Daily Cases"))
  incidence_gg
  
  # incidenceDF <- testing_subset_aligned[,c("date","confirmed","movingAverage")]
  # colnames(incidenceDF) <- c("Date", "Confirmed", "Moving Average")
  # incidenceDF_melt <- melt(incidenceDF, id=c("Date"))
  # incidenceDF_melt$Date <- as.Date(incidenceDF_melt$Date)
  # incidence_gg <- ggplot(incidenceDF_melt,aes(x=Date,y=value,color=variable))+geom_line(linetype = "twodash")+
  #   scale_color_manual(values=c("red","blue"))+
  #   theme(legend.position = "top")+
  #   labs(colour = NULL)+
  #   xlab(NULL)+
  #   scale_x_date(date_breaks = "2 week", date_labels =  "%b %d", limits=c(minDate, maxDate))+
  #   ggtitle(paste0("Reported Cases in ", testing_country)) +
  #   theme(text = element_text(size=14))+
  #   scale_y_continuous(label=comma,
  #                      # Features of the first axis
  #                      name = "New Daily Cases", 
  #                      # Add a second axis and specify its features
  #                      sec.axis = sec_axis( trans=~., name="New Daily Cases"))
  
  
  print(plot_grid(incidence_gg, R_gg, seirScaled_gg, ncol=1, align="v"))

}

dev.off()

