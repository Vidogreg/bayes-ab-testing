library(shiny)
library(plotly)
source('utilities.R')

shinyServer(function(input, output) {

  observeEvent(input$button, {
    req(
      input$success_A,
      input$total_A,
      input$success_B,
      input$total_B
    )
    if(
      input$success_A >= 0 &&
      input$total_A > 0 &&
      input$success_B >= 0 &&
      input$total_B > 0 &&
      input$success_A <= input$total_A &&
      input$success_B <= input$total_B
    ) {
      sample_A <- isolate({input$total_A})
      sample_B <- isolate({input$total_B})
      conv_A <- isolate({input$success_A/input$total_A})
      conv_B <- isolate({input$success_B/input$total_B})
      alpha_A <- isolate({input$success_A + 1})
      alpha_B <- isolate({input$success_B + 1})
      beta_A <- isolate({input$total_A - input$success_A + 1})
      beta_B <- isolate({input$total_B - input$success_B + 1})
      hdi_A <- isolate({hdi_of_icdf(qbeta, shape1 = alpha_A, shape2 = beta_A)})
      hdi_B <- isolate({hdi_of_icdf(qbeta, shape1 = alpha_B, shape2 = beta_B)})
      diff_post_sample <- isolate({
        create_sample_beta_diff(alpha_A, beta_A, alpha_B, beta_B, size = 1e5)
      })
      hdi_diff <- hdi_of_sample(diff_post_sample)
      x_lim <- {
        a <- min(hdi_A, hdi_B)
        b <- max(hdi_A, hdi_B)
        c(1.2*a - 0.2*b, 1.2*b - 0.2*a)
      }
      x_lim_diff <- {
        a <- hdi_diff[1]
        b <- hdi_diff[2]
        c(1.2*a - 0.2*b, 1.2*b - 0.2*a)
      }
      printPlot <- isolate({TRUE})
    } else {
      sample_A <- isolate({0})
      sample_B <- isolate({0})
      conv_A <- isolate({NaN})
      conv_B <- isolate({NaN})
      alpha_A <- isolate({1})
      alpha_B <- isolate({1})
      beta_A <- isolate({1})
      beta_B <- isolate({1})
      hdi_A <- isolate({c(NaN, NaN)})
      hdi_B <- isolate({c(NaN, NaN)})
      hdi_diff <- isolate({c(NaN, NaN)})
      x_lim <- isolate({c(NaN, NaN)})
      x_lim_diff <- isolate({c(NaN, NaN)})
      printPlot <- isolate({FALSE})
    }
    output$prob <- renderText({
      sprintf(
        'Probability that B is better than A: <b>\n%.1f%%</b>.',
        prob_B_beats_A(alpha_A, beta_A, alpha_B, beta_B)*100
      )
    })
    output$exp_loss_B <- renderText({
      sprintf(
        'Expected loss in conversion if B is actually worse: <b>\n%.2g%%</b>.',
        expected_loss_B_over_A(alpha_A, beta_A, alpha_B, beta_B)*100
      )
    })
    output$exp_loss_A <- renderText({
      sprintf(
        'Expected uplift in conversion if B is actually better: <b>\n%.2g%%</b>.',
        expected_loss_B_over_A(alpha_B, beta_B, alpha_A, beta_A)*100
      )
    })
    output$table <- renderTable({
      tab <- data.frame(
        metric = c('sample size', 'conversion', '95% HDI'),
        A = c(
          sprintf('\n%.d', sample_A),
          sprintf('\n%.3g%%', conv_A*100),
          sprintf('[ %.3g%%, \n%.3g%% ]', hdi_A[1]*100, hdi_A[2]*100)
        ),
        B = c(
          sprintf('\n%.d', sample_B),
          sprintf('\n%.3g%%', conv_B*100),
          sprintf('[ %.3g%%, \n%.3g%% ]', hdi_B[1]*100, hdi_B[2]*100)
        ),
        BA = c(
          sprintf(' '),
          sprintf('\n%.3g%%', conv_B*100 - conv_A*100),
          sprintf('[ %.3g%%, \n%.3g%% ]', hdi_diff[1]*100, hdi_diff[2]*100)
        )
      )
      colnames(tab) <- c(' ', 'A', 'B', 'B - A')
      tab
    })
    output$posterior_plot <- renderPlotly({
      plot_post_beta(
        alphaA = alpha_A, betaA = beta_A, alphaB = alpha_B, betaB = beta_B,
        hdiA = hdi_A, hdiB = hdi_B, xlim = x_lim, printPlot = printPlot
      )
    })
    output$posterior_plot_B_minus_A <- renderPlotly({
      plot_sample_density(
        sample = diff_post_sample,
        hdi = hdi_diff,
        printPlot = printPlot
      )
    })
  })

})