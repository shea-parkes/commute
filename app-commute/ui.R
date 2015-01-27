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
        ,max = 10
        ,step = 0.1
        ,value = 1
        ,round = -1
      )
    )
    ,mainPanel(
      'Pretty Visuals'
      ,textOutput('silly')
    )
  )
)
