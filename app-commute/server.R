library(shiny)
source('logic.R')

#' Snag a fresh commute database if we can reach it
dir.data <- paste0(Sys.getenv('UserProfile'),'/Google Drive/Skunkworks_OnePlus/') %T>% print()
filepath.commute.db <- paste0(dir.data,'commute.sqlite') %>% print()
if(file.exists(filepath.commute.db)) {file.copy(filepath.commute.db, getwd())}

commute.src <- src_sqlite('commute.sqlite') %T>% print()

i.components <- GenerateComponents(commute.src)

shinyServer(function(input, output, session) {
  output$heatmap <- renderPlot(
    CreateHeatMap(
      i.components
      ,input$kernel.bandwidth.miles
    )
  )
})
