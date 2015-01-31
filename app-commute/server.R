library(shiny)
print('Initializing Shiny App')

print('Loading logic file')
source('logic.R')
print('Finished loading logic file')

print('Attempting to grab fresh database')
#' Snag a fresh commute database if we can reach it
dir.data <- paste0(Sys.getenv('UserProfile'),'/Google Drive/Skunkworks_OnePlus/') %T>% print()
filepath.commute.db <- paste0(dir.data,'commute.sqlite') %>% print()
if(file.exists(filepath.commute.db)) {file.copy(filepath.commute.db, getwd(), overwrite = TRUE)}
print('Finished attempting to grab fresh database')

print('Moving into reactive state')
shinyServer(function(input, output, session) {
  
  GetComponents <- reactive({
    print('Generating base components')
    commute.src <- src_sqlite('commute.sqlite') %T>% print()
    i.components <- GenerateComponents(commute.src)
    print('Finished generating base components')
    i.components
  })
  
  output$ui.direction <- renderUI({checkboxGroupInput(
    'active.directions'
    ,'Commuting Direction'
    ,choices = GetComponents()$tbl.commute$direction %>% levels() %>% as.list()
    ,selected = GetComponents()$tbl.commute$direction %>% levels()
  )})
  
  output$ui.dates <- renderUI({dateRangeInput(
    'active.date.range'
    ,'Commuting Dates'
    ,start = GetComponents()$tbl.commute$date.parse %>% min()
    ,end = GetComponents()$tbl.commute$date.parse %>% max()
    ,min = GetComponents()$tbl.commute$date.parse %>% min()
    ,max = GetComponents()$tbl.commute$date.parse %>% max()
  )})
  
  GetActive <- reactive({
    ApplyFilters(
      GetComponents()
      ,active.directions = input$active.directions
      ,active.date.range = input$active.date.range
    )
  })
  
  output$heatmap <- renderPlot(
    CreateHeatMap(
      src.list = GetComponents()
      ,active.points = GetActive()
      ,kernel.bandwidth.miles = input$kernel.bandwidth.miles
      ,kernel.function.power = input$kernel.function.power
      ,alpha.saturation.limit = input$alpha.saturation.limit
      ,alpha.transform.power = input$alpha.transform.power
      ,duration.winsor.percent = input$duration.winsor.percent
    )
  )
})
