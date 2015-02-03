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
    src.commute <- src_sqlite('commute.sqlite') %T>% print()
    
    updateProgress <- function(detail = NULL, value.reduce.pct = 0.2) {
      value.current <- progress$getValue()
      value.new <- value.current + (progress$getMax() - value.current) * value.reduce.pct
      progress$set(value = value.new, detail = detail)
    }
    
    i.components <- MungeCommuteDB(
      src.commute
      ,updateProgress = updateProgress
    )
    print('Finished generating base components')
    i.components
  })
  
  
  GetActivePoints <- reactive({
    ApplyFilters(
      GetComponents()$tbl.points
      ,active_directions = input$active_directions
      ,active_date_range = input$active_date_range
      ,active_departure_range = input$active_departure_range
    ) %>%
      arrange(date, time)
  })
  
  
  GetActiveTrips <- reactive({
    ApplyFilters(
      GetComponents()$tbl.trips
      ,active_directions = input$active_directions
      ,active_date_range = input$active_date_range
      ,active_departure_range = input$active_departure_range
    ) %>%
      arrange(desc(date), desc(time_start_hours))
  })
  
  
  ui_direction.initialized <- FALSE ## Indicator that we're still in the initialization phase
  output$ui_direction <- renderUI({
    
    options.active.cnt <- GetActiveTrips() %>%
      group_by(direction) %>%
      summarize(n.trips = n())
    
    options.named <- GetComponents()$tbl.trips %>%
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
    ,start = GetComponents()$tbl.points$date %>% min()
    ,end = GetComponents()$tbl.points$date %>% max()
    ,min = GetComponents()$tbl.points$date %>% min()
    ,max = GetComponents()$tbl.points$date %>% max()
  )})
  
  
  GetTimeRange <- reactive({
    GetComponents()$tbl.trips$time_start_hours %>%
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
  
  
  output$ui_n_paths_max <- renderUI({sliderInput(
    'path.trace.n.max'
    ,HTML(paste0(
      'Max # of Paths to Trace'
      ,'<br>'
      ,'(<i>'
      ,GetActiveTrips() %>% nrow()
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
      ,active.points = GetActivePoints()
      ,kernel_bandwidth_miles = input$kernel_bandwidth_miles
      ,kernel_function_power = input$kernel_function_power
      ,alpha_saturation_limit = input$alpha_saturation_limit
      ,alpha_transform_power = input$alpha_transform_power
      ,duration_winsor_percent = input$duration_winsor_percent
      ,path.trace.n.max = input$path.trace.n.max
      ,updateProgress = updateProgress
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
