library(shiny)

fluidPage(
  titlePanel('Carmel Commuters')
  ,sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          'Basic'
          ,sliderInput(
            'kernel.bandwidth.miles'
            ,HTML('Kernel Bandwidth<br>(<i>in miles</i>)')
            ,min = 0.1
            ,max = 5
            ,step = 0.1
            ,value = 1
            ,round = -1
          )
          ,uiOutput('ui.direction')
          ,uiOutput('ui.dates')
          ,uiOutput('ui.n.paths')
        )
        ,tabPanel(
          'Advanced'
          ,sliderInput(
            'kernel.function.power'
            ,HTML('Kernel Power Function<br>(<i>1=Epanechnikov, 2=Quartic, 3=Triweight</i>)')
            ,min = 1
            ,max = 3
            ,step = 0.1
            ,value = 3
            ,round = -1
          )
          ,sliderInput(
            'alpha.saturation.limit'
            ,HTML('Transparency Saturation Limit<br>(<i>1 = No Capping</i>)')
            ,min = 0.75
            ,max = 1
            ,step = 0.01
            ,value = 0.95
            ,round = -2
          )
          ,sliderInput(
            'alpha.transform.power'
            ,HTML('Transparency Dampening Power<br>(<i>1 = No Dampening</i>)')
            ,min = 0.2
            ,max = 1
            ,step = 0.1
            ,value = 0.5
            ,round = -1
          )
          ,sliderInput(
            'duration.winsor.percent'
            ,HTML('Duration Winsoring Percent<br>(<i>0 = No Winsoring</i>)')
            ,min = 0
            ,max = 0.1
            ,step = 0.01
            ,value = 0.01
            ,round = -2
          )
        )
      )
    )
    ,mainPanel(
      'Commuting Heatmap'
      ,plotOutput('heatmap', height='600px')
    )
  )
)
