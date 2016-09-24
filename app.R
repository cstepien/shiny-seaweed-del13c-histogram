library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "isotope",
              label = "Choose a Carbon-13 isotope value", 
              value = 5, min = 1, max = 100),
  plotOutput(outputId = "hist")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    title <- "Histogram of Seaweed Carbon-13 Isotope Values" 
    hist(rnorm(input$isotope), main = title)})
}

shinyApp(ui = ui, server = server)