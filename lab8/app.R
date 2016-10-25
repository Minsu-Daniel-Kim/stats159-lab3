library(shiny)

# data Loading...
Advertising <- read.csv("Advertising.csv")

Advertising <- Advertising[,2:5]

predictors <- names(Advertising)[1:3]

ui <- fluidPage(
  headerPanel('Exploratory Analysis on Advertising Dataset'),
  sidebarPanel(
    selectInput('xcol', 'Predictors', predictors)
  ),
  mainPanel(
    plotOutput('scatterplot')
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    Advertising[, c(input$xcol, Sales)]
  })
  
  output$scatterplot <- renderPlot({
    plot(Advertising[, input$xcol],
         Advertising$Sales,
         xlab = input$xcol,
         ylab = "Sales",
         col = "red",
         pch = 20, cex = 1)
  })
}

shinyApp(ui = ui, server = server)