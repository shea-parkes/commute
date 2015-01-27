#library(shiny)

fluidPage(
  titlePanel('Milliman Carmel Commuters')
  ,sidebarLayout(
    sidebarPanel(
      h3('Visual Parameters')
      ,sliderInput(
        'kernel.bandwidth.miles'
        ,'Kernel Bandwidth\n(in miles)'
        ,min = 0.1
        ,max = 5
        ,step = 0.1
        ,value = 1
        ,round = -1
      )
    )
    ,mainPanel(
      'Commuting Heatmap'
      ,plotOutput(
        'heatmap'
         # ,width = 'auto'
         ,height = '600px'
        )
    )
  )
)
