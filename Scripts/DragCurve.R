library(plotly)
library(purrr)
library(shiny)
library(lubridate)



download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv","./InputData/Global_Mobility_Report.csv")
GoogData <- read.csv("./InputData/Global_Mobility_Report.csv", header=T)
GoogData <- subset(GoogData, sub_region_1 == "Colorado" & sub_region_2 == "")
GoogData <- GoogData[,5:ncol(GoogData)]
library(reshape2)
plott <- melt(GoogData,id="date")
plott$date <- as.Date(plott$date)
plott$variable <- as.factor(plott$variable)
plott$value <- as.numeric(plott$value)
ggplot(plott)+
  geom_line(aes(x=date, y=value, color=variable))+
  scale_color_manual(values=c("red","blue","green","black","brown","orange"))+
  ylim(-100,100)
###
GoogData <- read.csv("./InputData/Global_Mobility_Report.csv", header=T)
GoogData <- subset(GoogData, sub_region_1 == "Colorado" & sub_region_2 == "Boulder County")
GoogData <- GoogData[,5:ncol(GoogData)]
library(reshape2)
plott <- melt(GoogData,id="date")
plott$date <- as.Date(plott$date)
plott$variable <- as.factor(plott$variable)
plott$value <- as.numeric(plott$value)
ggplot(plott)+
  geom_line(aes(x=date, y=value, color=variable))+
  scale_color_manual(values=c("red","blue","green","black","brown","orange"))+
  ylim(-100,100)
###
GoogData <- read.csv("./InputData/Global_Mobility_Report.csv", header=T)
GoogData <- subset(GoogData, sub_region_1 == "Colorado" & sub_region_2 == "Denver County")
GoogData <- GoogData[,5:ncol(GoogData)]
library(reshape2)
plott <- melt(GoogData,id="date")
plott$date <- as.Date(plott$date)
plott$variable <- as.factor(plott$variable)
plott$value <- as.numeric(plott$value)
ggplot(plott)+
  geom_line(aes(x=date, y=value, color=variable))+
  scale_color_manual(values=c("red","blue","green","black","brown","orange"))+
  ylim(-100,100)


GoogData <- subset(GoogData, sub_region_1 == "" & sub_region_2 == "")
GoogData$country_region <- as.character(GoogData$country_region)
GoogData$country_region[GoogData$country_region=="United States"] <- "US"
GoogData$country_region[GoogData$country_region=="South Korea"] <- "Korea, South"
testingCountry <- "US"
dateBreak <- "2020-04-15"
forecastTime <- 14
dateBreak <- as.Date(dateBreak)
# Get just the overall country mobility stats
GoogData_subC <- subset(GoogData, country_region == testingCountry)
GoogData_subC$date <- as.Date(GoogData_subC$date)
latestBreak <- max(GoogData_subC$date)
GoogData_subC_subD <- subset(GoogData_subC, GoogData_subC$date >= dateBreak)
GoogData_subC_extend <- subset(GoogData_subC, GoogData_subC$date >= (latestBreak-6) & GoogData_subC$date <= (latestBreak))
forecastDF <- GoogData_subC[rep(1,forecastTime),]
forecastDF$date <- seq(from=as.Date(latestBreak+1), to=(as.Date(latestBreak)+forecastTime+1),length.out=forecastTime)
GoogleCM <- as.data.frame(t(colMeans(GoogData_subC_extend[,6:ncol(GoogData_subC_extend)])))
forecastDF[,names(GoogleCM)] <- GoogleCM[rep(1,forecastTime),]
GoogData_subC_rbind <- rbind(GoogData_subC_subD, forecastDF)
GoogData_subC_rbind$date <- as.numeric(GoogData_subC_rbind$date)

ui <- fluidPage(
  fluidRow(
    column(5, verbatimTextOutput("summary")),
    column(7, plotlyOutput("p"))
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    x = GoogData_subC_rbind$date[1:8],
    y = GoogData_subC_rbind$retail_and_recreation_percent_change_from_baseline[1:8]
  )
  grid <- reactive({
    data.frame(x = seq(min(rv$x), max(rv$x), length = 10))
  })
  model <- reactive({
    d <- data.frame(x = rv$x, y = rv$y)
    lm(y ~ poly(x,5), d)
  })
  
  output$p <- renderPlotly({
    # creates a list of circle shapes from x/y data
    circles <- map2(rv$x, rv$y, 
                    ~list(
                      type = "circle",
                      # anchor circles at (mpg, wt)
                      xanchor = .x,
                      yanchor = .y,
                      # give each circle a 2 pixel diameter
                      x0 = -4, x1 = 4,
                      y0 = -4, y1 = 4,
                      xsizemode = "pixel", 
                      ysizemode = "pixel",
                      # other visual properties
                      fillcolor = "blue",
                      line = list(color = "transparent")
                    )
    )
    
    # plot the shapes and fitted line
    plot_ly() %>%
      add_lines(x = grid()$x, y = predict(model(), grid()), color = I("red")) %>%
      layout(shapes = circles) %>%
      config(edits = list(shapePosition = TRUE))
  })
  # 
  output$summary <- renderPrint({a
    summary(model())
  })
  
  # update x/y reactive values in response to changes in shape anchors
  observe({
    ed <- event_data("plotly_relayout")
    shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed))]
    if (length(shape_anchors) != 2) return()
    row_index <- unique(readr::parse_number(names(shape_anchors)) + 1)
    pts <- as.numeric(shape_anchors)
    rv$x[row_index] <- pts[1]
    rv$y[row_index] <- pts[2]
    print(pts[2])
  })

}


shinyApp(ui, server)