library(shiny)

fluidPage(
  titlePanel('Carmel Commuters')
  ,sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          'Basic'
          ,uiOutput('ui_dates')
          ,uiOutput('ui_direction')
          ,HTML('<b>Departure Times (<i>in fractional military hours</i>)</b>')
          ,plotOutput(
            'hist_departure_time'
            ,height = '42px'
          )
          ,uiOutput('ui_time_departure')
          ,conditionalPanel(
            condition = "input.tabs_main == 'heatmap'"
            ,sliderInput(
              'kernel_bandwidth_miles'
              ,HTML('Kernel Bandwidth (<i>in miles</i>)')
              ,min = 0.1
              ,max = 5
              ,step = 0.1
              ,value = 1
              ,round = -1
            )
            ,uiOutput('ui_n_paths_max')
          )
          ,conditionalPanel(
            condition = "input.tabs_main == 'triptable'"
            ,downloadButton(
              'download_trips'
              ,'Download Trip Level Data'
            )
          )
        )
        ,tabPanel(
          'Advanced'
          ,fileInput(
            'db_upload'
            ,HTML('Upload new database<br>(<i>not guaranteed to persist</i>)')
          )
          ,conditionalPanel(
            condition = "input.tabs_main == 'heatmap'"
            ,sliderInput(
              'kernel_function_power'
              ,HTML('Kernel Power Function<br>(<i>1=Epanechnikov, 2=Quartic, 3=Triweight</i>)')
              ,min = 1
              ,max = 3
              ,step = 0.1
              ,value = 3
              ,round = -1
            )
            ,sliderInput(
              'alpha_saturation_limit'
              ,HTML('Transparency Saturation Limit<br>(<i>1 = No Capping</i>)')
              ,min = 0.75
              ,max = 1
              ,step = 0.01
              ,value = 0.95
              ,round = -2
            )
            ,sliderInput(
              'alpha_transform_power'
              ,HTML('Transparency Dampening Power<br>(<i>1 = No Dampening</i>)')
              ,min = 0.2
              ,max = 1
              ,step = 0.1
              ,value = 0.5
              ,round = -1
            )
            ,sliderInput(
              'duration_winsor_percent'
              ,HTML('Duration Winsoring Percent<br>(<i>0 = No Winsoring</i>)')
              ,min = 0
              ,max = 0.1
              ,step = 0.01
              ,value = 0.05
              ,round = -2
            )
          )
        )
      )
    )
    ,mainPanel(
      tabsetPanel(
        id = 'tabs_main'
        ,tabPanel(
          'Geo Heatmap'
          ,plotOutput('heatmap', height='600px')
          ,value = 'heatmap'
        )
        ,tabPanel(
          'Trip Table'
          ,dataTableOutput('tbl_trips')
          ,value = 'triptable'
        )
        ,tabPanel(
          'Widest Longitude'
          ,plotOutput('plt_longitude_widest')
          ,value = 'scatter_longitude'
        )
        ,tabPanel(
          'Departure Time'
          ,plotOutput('plt_departure_time')
          ,value = 'scatter_longitude'
        )
      )
    )
  )
)
