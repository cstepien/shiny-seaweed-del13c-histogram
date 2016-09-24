library(shiny)
library(ggplot2)
library(dplyr)
rawdata <- read.csv("data.csv")

ui <- fluidPage(
  checkboxGroupInput(inputId = "taxa", label = "Select Taxa to Display", 
                     c("Green Seaweeds (Chlorophyta)" = "Chlorophyta", "Red Seaweeds (Rhodophyta)" = "Rhodophyta", 
                       "Brown Seaweeds (Ochrophyta)" = "Ochrophyta", "Surfgrasses (Tracheophyta)" = "Tracheophyta"),
                     selected = c("Chlorophyta", "Rhodophyta", 
                                  "Ochrophyta", "Tracheophyta")), 
  actionButton(inputId = "go",
               label = "Update"),
  plotOutput(outputId = "hist"),
  verbatimTextOutput(outputId = "stats")
)

server <- function(input, output) {
  data <- eventReactive(input$go, 
                        {rnorm(input$isotope)})
  output$hist <- renderPlot({
    title <- "Histogram of Seaweed Carbon-13 Isotope Values" 
    hist(rnorm(data()), main = title)})
  output$stats <- renderPrint({
    summary(rnorm(data()))})
}



shinyApp(ui = ui, server = server)