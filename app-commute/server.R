#library(shiny)

shinyServer(function(input, output) {
  output$silly <- renderText(paste('I love', input$kernel.bandwidth.miles))
})