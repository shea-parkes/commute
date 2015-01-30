library(shiny)
print('Initializing Shiny App')

print('Loading logic file')
source('logic.R')
print('Finished loading logic file')

print('Attempting to grab fresh database')
#' Snag a fresh commute database if we can reach it
dir.data <- paste0(Sys.getenv('UserProfile'),'/Google Drive/Skunkworks_OnePlus/') %T>% print()
filepath.commute.db <- paste0(dir.data,'commute.sqlite') %>% print()
if(file.exists(filepath.commute.db)) {file.copy(filepath.commute.db, getwd())}
print('Finished attempting to grab fresh database')

print('Generating base objects')
commute.src <- src_sqlite('commute.sqlite') %T>% print()
i.components <- GenerateComponents(commute.src) %T>% {str(.) %>% print()}
print('Finished generating base objects')

print('Moving into reactive state')
shinyServer(function(input, output, session) {
  output$heatmap <- renderPlot(
    CreateHeatMap(
      i.components
      ,input$kernel.bandwidth.miles
    )
  )
})
