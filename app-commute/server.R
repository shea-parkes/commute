library(shiny)
print('Initializing Shiny App')


print('Loading logic file')
source('logic.R')
print('Finished loading logic file')

print('Attempting to grab fresh database')
dir.data <- paste0(Sys.getenv('UserProfile'),'/Google Drive/Skunkworks_OnePlus/') %T>% print()
filepath.commute.db <- paste0(dir.data,'commute.sqlite') %>% print()
if(file.exists(filepath.commute.db)) {file.copy(filepath.commute.db, getwd(), overwrite = TRUE)}
print('Finished attempting to grab fresh database')

print('Moving into reactive state')
shinyServer(function(input, output, session) {
  
  GetComponents <- reactive({
    
    print('Generating base components')
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Initializing Mapping Assets", value = 0)
    
    progress$set(detail = 'Connecting to database', value = 0.1)
    commute.src <- src_sqlite('commute.sqlite') %T>% print()
    
    updateProgress <- function(detail = NULL, value.reduce.pct = 0.2) {
      value.current <- progress$getValue()
      value.new <- value.current + (progress$getMax() - value.current) * value.reduce.pct
      progress$set(value = value.new, detail = detail)
    }
    
    i.components <- GenerateComponents(
      commute.src
      ,updateProgress = updateProgress
      )
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
  
  output$ui.n.paths <- renderUI({sliderInput(
    'path.trace.n.max'
    ,HTML(paste0(
      'Max # of Paths to Trace'
      ,'<br>'
      ,'(<i>'
      ,GetActive()$date_direction %>% n_distinct
      ,' paths currently active</i>)'
    ))
    ,min = 0L
    ,max = 25L
    ,step = 1L
    ,value = 5L
  )})
  
  output$heatmap <- renderPlot(
    CreateHeatMap(
      src.list = GetComponents()
      ,active.points = GetActive()
      ,kernel.bandwidth.miles = input$kernel.bandwidth.miles
      ,kernel.function.power = input$kernel.function.power
      ,alpha.saturation.limit = input$alpha.saturation.limit
      ,alpha.transform.power = input$alpha.transform.power
      ,duration.winsor.percent = input$duration.winsor.percent
      ,path.trace.n.max = input$path.trace.n.max
    )
  )
})
