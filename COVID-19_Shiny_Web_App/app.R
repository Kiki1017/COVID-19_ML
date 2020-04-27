library(shiny)
# install.packages("shinythemes")
library(shinythemes)
library(ggplot2)

# Define UI for application that displays model
ui <- fluidPage(theme = shinytheme("simplex"),
   
   # Application title
   titlePanel("The COVID Model"),
   
   # Sidebar with a select box input for country 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "country",
                     label = "Country",
                     choices = c("United States" = "USA"))
      ),
      
      # Show a plot of the confirmed cumulative cases per million
      mainPanel(
         plotOutput("plot1")
      )
   )
)

# Define server logic required to display model
server <- function(input, output) {
  output$plot1 <- reactivePlot(function() {
    # check for the input country
    if (input$country == "USA") {
      # USA
      load(./)
    }
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)

