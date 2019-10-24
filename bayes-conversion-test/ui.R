# bayes-conversion-test

library(shiny)
library(plotly)

shinyUI(
  fluidPage(
    titlePanel('Bayesian A/B test for conversion'),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          'success_A',
          'successes A:',
          min = 0,
          max = 1e6,
          value = 200
        ),
        numericInput(
          'total_A',
          'total A:',
          min = 0,
          max = 1e6,
          value = 1000
        ),
        numericInput(
          'success_B',
          'successes B:',
          min = 0,
          max = 1e6,
          value = 230
        ),
        numericInput(
          'total_B',
          'total B:',
          min = 0,
          max = 1e6,
          value = 1000
        ),
        textInput(
          'name_A',
          'name A:',
          value = 'control'
        ),
        textInput(
          'name_B',
          'name B:',
          value = 'test'
        ),
        actionButton(
          'button',
          'Calculate'
        ),
        hr(),
        tags$div(
          class='header', checked = NA,
          # tags$a(
          #   href = 'http://confluence.pixelfederation.com/pages/viewpage.action?pageId=4607020',
          #   'Click here to read how to use this calculator'
          # ),
          tags$br(),
          tags$a(
            href = 'https://github.com/Vidogreg/bayes-ab-testing',
            'Get the code here'
          ),
          tags$p('support: vidogreg@gmail.com')
        )
      ),
      mainPanel(
        tableOutput('table'),
        htmlOutput('prob'),
        htmlOutput('exp_loss_A'),
        htmlOutput('exp_loss_B'),
        hr(),
        plotlyOutput('posterior_plot_B_minus_A', width = '590', height = '260px'),
        hr(),
        plotlyOutput('posterior_plot', width = '590px', height = '260px')
      )
    )
  )
)
