# Install and load necessary packages
if (!require("shiny")) install.packages("shiny")
if (!require("leaflet")) install.packages("leaflet")
if (!require("dplyr")) install.packages("dplyr")

library(shiny)
library(leaflet)
library(dplyr)

# Sample dataset with rental data
# Replace this with your actual dataset
set.seed(123)
data <- data.frame(
  State = rep(state.name, each = 3),
  County = rep(c("County A", "County B", "County C"), times = length(state.name)),
  Bedrooms = rep(c(1, 2, 3), times = length(state.name)),
  AvgRent = round(runif(length(state.name) * 3, min = 800, max = 2500), 2)
)

# Define UI
ui <- fluidPage(
  titlePanel("RentOFun: An easy way to check out the rental acorss the US"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select a State", choices = c("All", unique(data$State))),
      selectInput("bedrooms", "Select Number of Bedrooms", choices = c("All", unique(data$Bedrooms))),
      sliderInput("rent_range", "Select Rent Range", min = 800, max = 2500, value = c(800, 2500)),
      selectInput("county", "Select a County", choices = c("All", unique(data$County)))
    ),
    
    mainPanel(
      leafletOutput("map"),
      tableOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    filtered <- data
    
    if (input$state != "All") {
      filtered <- filtered %>% filter(State == input$state)
    }
    
    if (input$bedrooms != "All") {
      filtered <- filtered %>% filter(Bedrooms == as.numeric(input$bedrooms))
    }
    
    filtered <- filtered %>% 
      filter(AvgRent >= input$rent_range[1], AvgRent <= input$rent_range[2])
    
    if (input$county != "All") {
      filtered <- filtered %>% filter(County == input$county)
    }
    
    filtered
  })
  
  output$map <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addMarkers(
        lng = -95.7129,
        lat = 37.0902,
        popup = paste("State: ", filtered_data()$State, "<br> County: ", filtered_data()$County, "<br> Bedrooms: ", filtered_data()$Bedrooms, "<br> Avg Rent: $", filtered_data()$AvgRent)
      )
  })
  
  output$table <- renderTable({
    filtered_data()
  })
  
}

# Run the application
shinyApp(ui, server)
