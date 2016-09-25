# add Bicarbonate + CO2 users, versus CO2 only users for each selection
# Add a title overall, a description of the interactive feature
library(shiny)
library(ggplot2)
library(dplyr)
library(modeest)
rawdata <- read.csv("data.csv")
rawdata$del13c <- round(rawdata$del13c, digits = 0)
yaxis <- c(70, 110, 170, 200, 215)

ui <- fluidPage(
  checkboxGroupInput(inputId = "taxa", label = "Select Taxa to Display", 
                     c("Green Seaweeds (Chlorophyta)" = "Chlorophyta", "Red Seaweeds (Rhodophyta)" = "Rhodophyta", 
                       "Brown Seaweeds (Ochrophyta)" = "Ochrophyta", "Surfgrasses (Tracheophyta)" = "Tracheophyta"),
                     selected = c("Chlorophyta", "Rhodophyta", 
                                  "Ochrophyta", "Tracheophyta")), 
  #actionButton(inputId = "go",
               #label = "Plot histogram"),
  selectInput(inputId = "binwidth", 
              label = "Bin width (isotope units)", 
              choices = c(1, 2, 3, 4, 5), 
              selected = 1,
              width = '70px'),
  plotOutput(outputId = "hist"),
  verbatimTextOutput(outputId = "max"),
  verbatimTextOutput(outputId = "min"),
  verbatimTextOutput(outputId = "mo")
)

server <- function(input, output) {
  data <- reactive({filter(rawdata, division %in% input$taxa)})
  bin <- reactive({as.numeric(input$binwidth)})
  lim <- reactive({yaxis[as.numeric(input$binwidth)]})
  output$hist <- renderPlot({
    ggplot(data(), aes(x = del13c)) + xlim(-41,0) + ylim(0,lim()) +
      geom_histogram(binwidth = bin(), fill = "gray", color = "black") +
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
  output$max <- renderText({paste("Maximum species ", "\u03B413C", "(\u2030) =", max(data()$del13c))})
  output$min <- renderText({paste("Minimum species ", "\u03B413C", "(\u2030) =", min(data()$del13c))})
  output$mo <- renderText({paste("Most common species ", "\u03B413C", "(\u2030) =", 
                                 paste(mfv(data()$del13c), collapse = ", "))})
}

shinyApp(ui = ui, server = server)