# bayes-arpu-test

library(shiny)
library(plotly)

shinyUI(
  fluidPage(
    titlePanel('Bayesian A/B test for ARPU'),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          'rev_A',
          'total revenue A:',
          min = 0,
          max = 1e6,
          value = 5000
        ),
        numericInput(
          'success_A',
          'converted payers A:',
          min = 0,
          max = 1e6,
          value = 200
        ),
        numericInput(
          'total_A',
          'all players A:',
          min = 0,
          max = 1e6,
          value = 10000
        ),
        numericInput(
          'rev_B',
          'total revenue B:',
          min = 0,
          max = 1e6,
          value = 6600
        ),
        numericInput(
          'success_B',
          'converted payers B:',
          min = 0,
          max = 1e6,
          value = 240
        ),
        numericInput(
          'total_B',
          'all players B:',
          min = 0,
          max = 1e6,
          value = 10000
        ),
        numericInput(
          'sim_sample',
          'sample size used in simulations:',
          min = 2,
          max = 1e7,
          value = 1e5
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
          #   href = 'http://confluence.pixelfederation.com/pages/viewpage.action?pageId=4607085',
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
        tableOutput('table1'),
        tableOutput('table2'),
        hr(),
        plotlyOutput('posterior_plot_B_minus_A', width = '580px', height = '250px'),
        hr(),
        plotlyOutput('posterior_plot', width = '580px', height = '250px')
      )
    )
  )
)