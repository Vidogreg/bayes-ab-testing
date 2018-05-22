library(shiny)
library(plotly)
source('utilities.R')

shinyServer(function(input, output) {
  
  observeEvent(input$button, {
    req(
      input$success_A,
      input$total_A,
      input$success_B,
      input$total_B,
      input$rev_A,
      input$rev_B,
      input$sim_sample
    )
    if(
      input$success_A >= 0 &&
      input$total_A > 0 &&
      input$success_B >= 0 &&
      input$total_B > 0 &&
      input$success_A <= input$total_A &&
      input$success_B <= input$total_B &&
      input$rev_A >= 0 &&
      input$rev_B >= 0 &&
      input$sim_sample >= 1
    ) {
      sample_A <- isolate({input$total_A})
      sample_B <- isolate({input$total_B})
      conv_A <- isolate({input$success_A/input$total_A})
      conv_B <- isolate({input$success_B/input$total_B})
      arppu_A <- isolate({input$rev_A/input$success_A})
      arppu_B <- isolate({input$rev_B/input$success_B})
      arpu_A <- isolate({input$rev_A/input$total_A})
      arpu_B <- isolate({input$rev_B/input$total_B})
      alpha_A <- isolate({input$success_A + 1})
      alpha_B <- isolate({input$success_B + 1})
      beta_A <- isolate({input$total_A - input$success_A + 1})
      beta_B <- isolate({input$total_B - input$success_B + 1})
      k_A <- isolate({input$success_A + 1})
      k_B <- isolate({input$success_B + 1})
      theta_A <- isolate({1/(1 + input$rev_A)})
      theta_B <- isolate({1/(1 + input$rev_B)})
      res <- isolate({
        bayes_arpu(
          alphaA = alpha_A, betaA = beta_A,
          kA = k_A, thetaA = theta_A,
          alphaB = alpha_B, betaB = beta_B,
          kB = k_B, thetaB = theta_B,
          MSamples = input$sim_sample
        )
      })
      post_sample_A <- res$sampleLambdaA/res$sampleOmegaA
      post_sample_B <- res$sampleLambdaB/res$sampleOmegaB
      diff_post_sample <- post_sample_B - post_sample_A
      hdi_A <- hdi_of_sample(post_sample_A)
      hdi_B <- hdi_of_sample(post_sample_B)
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
      # message <- isolate({'Done'})
    } else {
      conv_A <- isolate({NaN})
      conv_B <- isolate({NaN})
      arppu_A <- isolate({NaN})
      arppu_B <- isolate({NaN})
      arpu_A <- isolate({NaN})
      arpu_B <- isolate({NaN})
      alpha_A <- isolate({1})
      alpha_B <- isolate({1})
      beta_A <- isolate({1})
      beta_B <- isolate({1})
      k_A <- isolate({1})
      k_B <- isolate({1})
      theta_A <- isolate({1})
      theta_B <- isolate({1})
      hdi_A <- isolate({c(NaN, NaN)})
      hdi_B <- isolate({c(NaN, NaN)})
      hdi_diff <- isolate({c(NaN, NaN)})
      x_lim <- isolate({c(NaN, NaN)})
      x_lim_diff <- isolate({c(NaN, NaN)})
      res <- isolate({list(probBbeatsA = NaN, expLossA = NaN, expLossB = NaN)})
      printPlot <- isolate({FALSE})
      # message <- isolate({'Check inputs!'})
    }
    # output$prob <- renderText({
    #   sprintf(
    #     'Probability that B is better than A: <b>\n%.1f%%</b>.',
    #     res$arpuProbBbeatsA*100
    #   )
    # })
    # output$exp_loss_A <- renderText({
    #   sprintf(
    #     'Expected uplift in ARPU if B is actually better: <b>\n%.2g €</b>.',
    #     res$arpuExpLossA
    #   )
    # })
    # output$exp_loss_B <- renderText({
    #   sprintf(
    #     'Expected loss in ARPU if B is actually worse: <b>\n%.2g €</b>.',
    #     res$arpuExpLossB
    #   )
    # })
    output$table1 <- renderTable({
      tab <- data.frame(
        metric = c('sample size', 'conversion', 'ARPPU', 'ARPU', '95% HDI'),
        A = c(
          sprintf('\n%.d', sample_A),
          sprintf('\n%.3g%%', conv_A*100),
          sprintf('\n%.4g €', arppu_A),
          sprintf('\n%.4g €', arpu_A),
          sprintf('[ %.3g €, \n%.3g € ]', hdi_A[1], hdi_A[2])
        ),
        B = c(
          sprintf('\n%.d', sample_B),
          sprintf('\n%.3g%%', conv_B*100),
          sprintf('\n%.4g €', arppu_B),
          sprintf('\n%.4g €', arpu_B),
          sprintf('[ %.3g €, \n%.3g € ]', hdi_B[1], hdi_B[2])
        ),
        BA = c(
          sprintf(' '),
          sprintf('\n%.3g%%', conv_B*100 - conv_A*100),
          sprintf('\n%.4g €', arppu_B - arppu_A),
          sprintf('\n%.4g €', arpu_B - arpu_A),
          sprintf('[ %.3g €, \n%.3g € ]', hdi_diff[1], hdi_diff[2])
        )
      )
      colnames(tab) <- c(' ', 'A', 'B', 'B - A')
      tab
    }, spacing = 'xs')
    output$table2 <- renderTable({
      tab <- data.frame(
        column1 = c(
          'Probability that B is better than A',
          'Expected uplift if B is actually better',
          'Expected loss if B is actually worse'
        ),
        conversion = c(
          sprintf('\n%.1f%%', res$convProbBbeatsA*100),
          sprintf('\n%.2g%%', res$convExpLossA*100),
          sprintf('\n%.2g%%', res$convExpLossB*100)
        ),
        ARPPU = c(
          sprintf('\n%.1f%%', res$revProbBbeatsA*100),
          sprintf('\n%.2g €', res$revExpLossA),
          sprintf('\n%.2g €', res$revExpLossB)
        ),
        ARPU = c(
          sprintf('\n%.1f%%', res$arpuProbBbeatsA*100),
          sprintf('\n%.2g €', res$arpuExpLossA),
          sprintf('\n%.2g €', res$arpuExpLossB)
        )
      )
      colnames(tab) <- c(' ', 'conversion', 'ARPPU', 'ARPU')
      tab
    }, spacing = 'xs')
    output$posterior_plot_B_minus_A <- renderPlotly({
      plot_sample_density(
        sample = diff_post_sample,
        hdi = hdi_diff,
        printPlot = printPlot
      )
    })
    output$posterior_plot <- renderPlotly({
      plot_sample_densities(
        sampleDataA = post_sample_A,
        sampleDataB = post_sample_B,
        hdiA = hdi_A,
        hdiB = hdi_B,
        printPlot = printPlot
      )
    })
  })
  
})