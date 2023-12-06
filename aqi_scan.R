# Load the required packages
library(shiny)
library(openair)

# Define the user interface
ui <- fluidPage(
  titlePanel("Air Quality Index (AQI) App"),
  sidebarLayout(
    sidebarPanel(
      textInput("city", "Enter the city name", ""),
      actionButton("goButton", "Get AQI")
    ),
    mainPanel(
      textOutput("aqiOutput")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  observeEvent(input$goButton, {
    aqi_data <- importKCL(input$city, pollutant = "aqi", year = 2023)
    aqi_value <- aqi_data$Value[1]
    output$aqiOutput <- renderText({
      paste("The Air Quality Index (AQI) for", input$city, "is", aqi_value)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
