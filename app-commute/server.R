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
  
  ui_direction.initialized <- FALSE ## Indicator that we're still in the initialization phase
  output$ui_direction <- renderUI({
    
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
    
    if(ui_direction.initialized) {
      options.selected <- input$active_directions
    } else {
      options.selected <- options.named
      ui_direction.initialized <<- TRUE
    }
    
    checkboxGroupInput(
      'active_directions'
      ,'Commuting Direction'
      ,choices = options.named %>% as.list()
      ,selected = options.selected
    )
  })
  
  output$ui_dates <- renderUI({dateRangeInput(
    'active_date_range'
    ,'Commuting Dates'
    ,start = GetComponents()$tbl.commute$date.parse %>% min()
    ,end = GetComponents()$tbl.commute$date.parse %>% max()
    ,min = GetComponents()$tbl.commute$date.parse %>% min()
    ,max = GetComponents()$tbl.commute$date.parse %>% max()
  )})
  
  
  GetTimeRange <- reactive({
    GetComponents()$tbl.commute$time_start %>%
      range() %>%
      multiply_by(c(0.99,1.01)) %>% {
        c(
          floor(.[1]*20)/20
          ,ceiling(.[2]*20)/20
        )
      }
  })
  
  output$ui_time_departure <- renderUI({
    time_start.range <- GetTimeRange()
    sliderInput(
      'active_departure_range'
      ,label = NULL
      ,min = time_start.range[1]
      ,max = time_start.range[2]
      ,value = time_start.range
      ,step = 0.05
      ,round = -2
    )
  })
  
  output$hist_departure_time <- renderPlot({
    time_start.range <- GetTimeRange()
    op <- par(mar = rep(0, 4))
    hist(
      GetActiveTrips()$time_start_hours
      ,breaks = seq(time_start.range[1], time_start.range[2], length.out=142L)
      ,main = NULL, xlab = NULL, ylab = NULL, axes = FALSE
      ,col = 'skyblue'
      ,lty = 0
    )
    par(op)
  })
  
  GetActive <- reactive({
    validate(need(
      diff(input$active_date_range) >= 0
      ,message=paste(
        'Date range is backwards:'
        ,input$active_date_range[1]
        ,'to'
        ,input$active_date_range[2]
      )
    ))
    ApplyFilters(
      GetComponents()
      ,active_directions = input$active_directions
      ,active_date_range = input$active_date_range
      ,active_departure_range = input$active_departure_range
    )
  })
  
  output$ui_n_paths_max <- renderUI({sliderInput(
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
      ,kernel_bandwidth_miles = input$kernel_bandwidth_miles
      ,kernel_function_power = input$kernel_function_power
      ,alpha_saturation_limit = input$alpha_saturation_limit
      ,alpha_transform_power = input$alpha_transform_power
      ,duration_winsor_percent = input$duration_winsor_percent
      ,path.trace.n.max = input$path.trace.n.max
      ,updateProgress = updateProgress
    )
  })
  
  GetActiveTrips <- reactive({
    long.mean <- GetComponents()$tbl.commute$long %>% mean()
    
    GetActive() %>%
      select(
        date.parse
        ,direction
        ,time_start
        ,duration
        ,long
      ) %>%
      rename(
        date = date.parse
        ,time_start_hours = time_start
        ,duration_minutes = duration
      ) %>%
      mutate(
        time_start_hours = round(time_start_hours, 2)
        ,long.deviance = abs(long - long.mean)
      ) %>%
      group_by(
        date
        ,direction
        ,time_start_hours
        ,duration_minutes
      ) %>%
      summarize(
        longitude_widest = long[which.max(long.deviance)]
      ) %>%
      arrange(
        desc(date)
        ,desc(time_start_hours)
      )
  })
  
  output$tbl_trips <- renderDataTable(
    GetActiveTrips()
    ,options = list(orderClasses = TRUE)
  )
  
  output$download_trips <- downloadHandler(
    filename = function() {paste0(
      'commute.trips.selected.'
      ,format(Sys.time(), '%Y%m%d.%H%M%S')
      ,'.csv'
    )}
    ,content = function(file) {
      write.csv(
        GetActiveTrips()
        ,file
        ,row.names = FALSE
      )
    }
  )
})
