library(shiny)
source('logic.R')

dir.data <- paste0(Sys.getenv('UserProfile'),'/Google Drive/Skunkworks_OnePlus/') %T>% print()
commute.src <- src_sqlite(paste0(dir.data,'commute.sqlite')) %T>% print()

i.components <- GenerateComponents(commute.src)

shinyServer(function(input, output, session) {
  output$heatmap <- renderPlot(
    CreateHeatMap(
      i.components
      ,input$kernel.bandwidth.miles
    )
  )
})
