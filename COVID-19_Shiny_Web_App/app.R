library(shiny)
# install.packages("shinythemes")
library(shinythemes)
library(ggplot2)

# Define UI for application that displays model
ui <- navbarPage("The COVID Model", theme = shinytheme("simplex"),
   # Application title
#   titlePanel("The COVID Model"),
   
   # Sidebar with a select box input for country 
#   sidebarLayout(
#      sidebarPanel(
#         selectInput(inputId = "country",
#                     label = "Country",
#                     choices = c("United States" = "USA"))
#         radioButtons(inputId = "casesDeaths", label = "", 
#                      choices = c("Cases" = "cases"))
#      ),
      
# Main panel for displaying outputs ----
        tabPanel("Plots",
                 fluidPage(
         fluidRow(wellPanel(
             selectInput(inputId = "country",
                                 label = "Country",
                                 choices = c("United States" = "USA"))),
             plotOutput("plot1"),
             plotOutput("plot2"),
         plotOutput("plot3"),
         plotOutput("plot4"),
         plotOutput("plot_predict")))),
        tabPanel("Model information", verbatimTextOutput("summary")))


# Define server logic required to display model
server <- function(input, output) {
  output$plot1 <- reactivePlot(function() {
    # check for the input country
    if (input$country == "USA") {
      # USA
      load('./COVID-19_Shiny_Web_App/Inputs/USA_Cases_data.RData')
    }

    print(plot1)
  })
  output$plot2 <- reactivePlot(function() {
    # check for the input country
    if (input$country == "USA") {
      # USA
      load('./COVID-19_Shiny_Web_App/Inputs/USA_Cases_data.RData')
    }
    
    print(plot2)
  })
  output$plot3 <- reactivePlot(function() {
    # check for the input country
    if (input$country == "USA") {
      # USA
      load('./COVID-19_Shiny_Web_App/Inputs/USA_Cases_data.RData')
    }
    
    print(plot3)
  })
  output$plot4 <- reactivePlot(function() {
    # check for the input country
    if (input$country == "USA") {
      # USA
      load('./COVID-19_Shiny_Web_App/Inputs/USA_Cases_data.RData')
    }
    
    print(plot4)
  })
  output$plot_predict <- reactivePlot(function() {
    # check for the input country
    if (input$country == "USA") {
      # USA
      load('./COVID-19_Shiny_Web_App/Inputs/USA_Cases_data.RData')
    }
    
    print(plot_predict)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

