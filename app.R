# Add binwidth slider, and resulting value in ggplot binwidth - maybe from 1 to 10 for binwidth
# Edit fill - make it white, with a black outline for each bar
# Goint to need the ylimit to be dependent on the binwidth - if you have 10 widths, what's the max count of a single bin?

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
  #actionButton(inputId = "go",
               #label = "Plot histogram"),
  plotOutput(outputId = "hist"),
  verbatimTextOutput(outputId = "stats")
)

server <- function(input, output) {
  data <- reactive({filter(rawdata, division %in% input$taxa)})
  output$hist <- renderPlot({
    ggplot(data(), aes(x = del13c)) + xlim(-40,0) + ylim(0,110) +
      geom_histogram(binwidth = 1, fill = "gray", color = "black") +
      geom_vline(xintercept = -30, linetype = "dotted", size = 1) +
      xlab(expression(paste("\nMean species ", delta^{13}, "C ", "(\u2030)"))) +
      ylab("Count") +
      theme(legend.key = element_blank(), panel.grid.major.y = element_line(color = "gray"), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_text(size=20, color = "black"), 
            axis.text.y = element_text(size=20, color = "black"),
            axis.title.x = element_text(size=20), 
            axis.title.y = element_text(size=20))
    })
  #output$stats <- renderPrint({
   # summary(mode(data()))})
}

shinyApp(ui = ui, server = server)