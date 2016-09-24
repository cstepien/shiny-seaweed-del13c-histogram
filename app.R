library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "isotope",
              label = "Choose a Carbon-13 isotope value", 
              value = 5, min = 1, max = 100),
  plotOutput(outputId = "hist"),
  verbatimTextOutput(outputId = "stats")
)

server <- function(input, output) {
  data <- reactive({
    rnorm(input$isotope)
    })
  output$hist <- renderPlot({
    title <- "Histogram of Seaweed Carbon-13 Isotope Values" 
    hist(rnorm(data()), main = title)})
  output$stats <- renderPrint({
    summary(rnorm(data()))})
}



shinyApp(ui = ui, server = server)