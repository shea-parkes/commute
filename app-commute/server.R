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
  
  ui.direction.initialized <- FALSE ## Indicator that we're still in the initialization phase
  output$ui.direction <- renderUI({
    
    options.active.cnt <- GetActive() %>%
      group_by(direction) %>%
      summarize(n.trips = n_distinct(date_direction))
    
    options.named <- GetComponents()$tbl.commute %>%
      select(direction) %>%
      distinct() %>%
      left_join(options.active.cnt, by = 'direction') %>%
      mutate(n.trips = ifelse(is.na(n.trips), 0, n.trips)) %>%
      arrange(direction) %$%
      setNames(
        as.character(direction)
        ,paste0(as.character(direction),' (',n.trips,' trips)')
      )
    
    if(ui.direction.initialized) {
      options.selected <- input$active.directions
    } else {
      options.selected <- options.named
      ui.direction.initialized <<- TRUE
    }
    
    checkboxGroupInput(
      'active.directions'
      ,'Commuting Direction'
      ,choices = options.named %>% as.list()
      ,selected = options.selected
    )
  })
  
  output$ui.dates <- renderUI({dateRangeInput(
    'active.date.range'
    ,'Commuting Dates'
    ,start = GetComponents()$tbl.commute$date.parse %>% min()
    ,end = GetComponents()$tbl.commute$date.parse %>% max()
    ,min = GetComponents()$tbl.commute$date.parse %>% min()
    ,max = GetComponents()$tbl.commute$date.parse %>% max()
  )})
  
  GetActive <- reactive({
    validate(need(
      diff(input$active.date.range) >= 0
      ,message=paste(
        'Date range is backwards:'
        ,input$active.date.range[1]
        ,'to'
        ,input$active.date.range[2]
      )
    ))
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
      ,GetActive()$date_direction %>% n_distinct()
      ,' paths currently active</i>)'
    ))
    ,min = 0L
    ,max = 25L
    ,step = 1L
    ,value = 5L
  )})
  
  output$heatmap <- renderPlot({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Generating Heat Map", value = 0)
    
    updateProgress <- function(detail = NULL, value.reduce.pct = 0.2) {
      value.current <- progress$getValue()
      value.new <- value.current + (progress$getMax() - value.current) * value.reduce.pct
      progress$set(value = value.new, detail = detail)
    }
    
    CreateHeatMap(
      src.list = GetComponents()
      ,active.points = GetActive()
      ,kernel.bandwidth.miles = input$kernel.bandwidth.miles
      ,kernel.function.power = input$kernel.function.power
      ,alpha.saturation.limit = input$alpha.saturation.limit
      ,alpha.transform.power = input$alpha.transform.power
      ,duration.winsor.percent = input$duration.winsor.percent
      ,path.trace.n.max = input$path.trace.n.max
      ,updateProgress = updateProgress
    )
  })
  
  output$tbl.commute <- renderDataTable({
    GetActive() %>%
      select(
        date.parse
        ,direction
        ,time_start
        ,duration
      ) %>%
      rename(
        date = date.parse
        ,time_start_hours = time_start
        ,duration_minutes = duration
      ) %>%
      mutate(
        time_start_hours = round(time_start_hours, 2)
      ) %>%
      distinct()
  }
  ,options = list(orderClasses = TRUE)
  )
})
