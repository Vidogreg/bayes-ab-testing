# Bayesian AB test for ARPU
# Calculates the probability that B is better than A and expected loss for A and B
# Also returns samples used for calculations
{
  # Arguments:
  #   Alpha and beta parameters of beta distributions for A and B
  #   k and theta parameters of gamma distributions for A and B
  #   alpha = number of successes + 1
  #   beta = number of failures + 1
  #   k = payer count + 1
  #   theta = 1/(1 + revenue)
  # Assumptions:
  #   payer ~ Bernoulli(lambda)
  #   lambda ~ Beta(alpha, beta)
  #   revenue ~ Exp(omega)
  #   omega ~ Gamma(k, theta)
  #   ARPU = E(payer)*E(revenue) = lambda*1/omega
  #   all varianles are calculated for A and B group
  # Return value:
  #   probBbeatsA = probability that the B version has higher ARPU than the A version
  #   expLossA = expected loss on condition that we select A but B is better
  #   expLossB = expected loss on condition that we select B but A is better
  # Example of use:
  #   If version A converted 180 out of 10000 users with revenue 5000 euros and
  #     version B converted 200 out of 11000 users with revenue 6000 euros then use
  #     bayes_arpu(
  #       180 + 1, 10000 - 180 + 1, 180 + 1, 1/(1 + 5000),
  #       200 + 1, 11000 - 200 + 1, 200 + 1, 1/(1 + 6000)
  #     )
  # Implemented based on VWO white paper
  # https://cdn2.hubspot.net/hubfs/310840/VWO_SmartStats_technical_whitepaper.pdf
}
bayes_arpu <- function(
  alphaA, betaA,
  kA, thetaA,
  alphaB, betaB,
  kB, thetaB,
  MSamples
) {
  if(alphaA <= 0 || betaA <= 0 || alphaB <= 0 || betaB <= 0 ||
     kA <= 0 || thetaA <= 0 || kB <= 0 || thetaB <= 0) {
    probBbeatsA <- 0
    expLossA <- 0
    expLossB <- 0
    lambdaA <- 0
    lambdaB <- 0
    omegaA <- 0
    omegaB <- 0
  } else {
    lambdaA <- rbeta(MSamples, alphaA, betaA)
    lambdaB <- rbeta(MSamples, alphaB, betaB)
    omegaA <- rgamma(MSamples, shape = kA, scale = thetaA)
    omegaB <- rgamma(MSamples, shape = kB, scale = thetaB)
    
    convProbBbeatsA <- sum(lambdaB > lambdaA)/MSamples
    diffTemp <- lambdaB - lambdaA
    convExpLossA <- sum(diffTemp*(diffTemp > 0))/MSamples
    convExpLossB <- sum(-diffTemp*(-diffTemp > 0))/MSamples
    
    revProbBbeatsA <- sum(1/omegaB > 1/omegaA)/MSamples
    diffTemp <- 1/omegaB - 1/omegaA
    revExpLossA <- sum(diffTemp*(diffTemp > 0))/MSamples
    revExpLossB <- sum(-diffTemp*(-diffTemp > 0))/MSamples
    
    arpuProbBbeatsA <- sum(lambdaB/omegaB > lambdaA/omegaA)/MSamples
    diffTemp <- lambdaB/omegaB - lambdaA/omegaA
    arpuExpLossA <- sum(diffTemp*(diffTemp > 0))/MSamples
    arpuExpLossB <- sum(-diffTemp*(-diffTemp > 0))/MSamples
  }
  list(
    convProbBbeatsA = convProbBbeatsA,
    convExpLossA = convExpLossA,
    convExpLossB = convExpLossB,
    revProbBbeatsA = revProbBbeatsA,
    revExpLossA = revExpLossA,
    revExpLossB = revExpLossB,
    arpuProbBbeatsA = arpuProbBbeatsA,
    arpuExpLossA = arpuExpLossA,
    arpuExpLossB = arpuExpLossB,
    sampleLambdaA = lambdaA,
    sampleLambdaB = lambdaB,
    sampleOmegaA = omegaA,
    sampleOmegaB = omegaB
  )
}

# Calculates the HDI interval from a sample of representative values
# estimated as shortest credible interval
{
  # Arguments:
  #   sampleVec is a vector of representative values from a probability
  #     distribution.
  #   credMass is a scalar between 0 and 1, indicating the mass within
  #     the credible interval that is to be estimated.
  # Return value:
  #   Highest density iterval (HDI) limits in a vector
}
hdi_of_sample <- function(sampleVec, credMass = 0.95) {
  sortedPts <- sort(sampleVec)
  sortedPtsLength <- length(sortedPts)
  if(sortedPtsLength >= 3) {
    ciIdxInc <- min(ceiling(credMass*sortedPtsLength), sortedPtsLength - 1)
    nCIs <- sortedPtsLength - ciIdxInc
    ciWidth <- rep(0, nCIs)
    for (i in 1:nCIs) {
      ciWidth[i] <- sortedPts[i + ciIdxInc] - sortedPts[i]
    }
    HDImin <- sortedPts[which.min(ciWidth)]
    HDImax <- sortedPts[which.min(ciWidth) + ciIdxInc]
    HDIlim <- c(HDImin, HDImax)
  } else {
    HDIlim <- c(min(sortedPts), max(sortedPts))
  }
  return(HDIlim)
}

# Creates plotly chart of approximate density based on a sample data
plot_sample_density <- function(
  sampleData, hdi = c(-0.1, 0.1), printPlot = TRUE, ...
) {
  if(printPlot) {
    dens <- density(sampleData, bw = 'nrd')
    plotData <- data.frame(x = dens$x, y = dens$y)
    x <- plotData$x
    y <- plotData$y
    xArea <- plotData[plotData$x >= hdi[1] & plotData$x <= hdi[2], ]$x
    yArea <- plotData[plotData$x >= hdi[1] & plotData$x <= hdi[2], ]$y
    
    plot_ly(
      x = x,
      y = y,
      type = 'scatter',
      mode = 'lines',
      name = 'ARPU B - A',
      line = list(color = '#F27B0C'),
      hoverinfo = 'none',
      # text = ~paste(sprintf('%.3g%%', x*100)),
      showlegend = FALSE
    ) %>% add_trace(
      x = xArea,
      y = yArea,
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      fillcolor = 'rgba(242, 123, 12, 0.2)',
      line = list(color = '#F27B0C'),
      hoverinfo = 'none',
      # text = ~paste(sprintf('%.3g%%', xArea*100)),
      showlegend = FALSE
    ) %>% layout(
      title = 'Approximate distribution of difference of ARPU B - A',
      title = list(font = 14),
      xaxis = list(showgrid = FALSE, fixedrange = TRUE),
      yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange = TRUE),
      legend = list(x = 0.9, y = 0.9)
    ) %>% config(displayModeBar = FALSE)
    
  } else {
    plot_ly(type = 'scatter', mode = 'markers')
  }
}

# Creates plotly chart of approximate densities based on a sample data
plot_sample_densities <- function(
  sampleDataA, hdiA = c(-0.1, 0.1),
  sampleDataB, hdiB = c(-0.1, 0.1),
  nameA, nameB, printPlot = TRUE, ...
) {
  if(printPlot) {
    densA <- density(sampleDataA, bw = 'nrd')
    plotDataA <- data.frame(x = densA$x, y = densA$y)
    xA <- plotDataA$x
    yA <- plotDataA$y
    xAreaA <- plotDataA[plotDataA$x >= hdiA[1] & plotDataA$x <= hdiA[2], ]$x
    yAreaA <- plotDataA[plotDataA$x >= hdiA[1] & plotDataA$x <= hdiA[2], ]$y
    densB <- density(sampleDataB, bw = 'nrd')
    plotDataB <- data.frame(x = densB$x, y = densB$y)
    xB <- plotDataB$x
    yB <- plotDataB$y
    xAreaB <- plotDataB[plotDataB$x >= hdiB[1] & plotDataB$x <= hdiB[2], ]$x
    yAreaB <- plotDataB[plotDataB$x >= hdiB[1] & plotDataB$x <= hdiB[2], ]$y
    plot_ly(
      x = xA,
      y = yA,
      type = 'scatter',
      mode = 'lines',
      name = nameA,
      line = list(color = '#5BC0DE'),
      hoverinfo = 'none'
    ) %>% add_trace(
      x = xAreaA,
      y = yAreaA,
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      fillcolor = 'rgba(91, 192, 222, 0.2)',
      line = list(color = '#5BC0DE'),
      hoverinfo = 'none',
      showlegend = FALSE
    ) %>% add_trace(
      x = xB,
      y = yB,
      type = 'scatter',
      mode = 'lines',
      name = nameB,
      line = list(color = '#5CB85C'),
      hoverinfo = 'none'
    ) %>% add_trace(
      x = xAreaB,
      y = yAreaB,
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      fillcolor = 'rgba(92, 184, 92, 0.2)',
      line = list(color = '#5CB85C'),
      hoverinfo = 'none',
      showlegend = FALSE
    ) %>% layout(
      title = 'Distribution of ARPU A and B',
      title = list(font = 14),
      xaxis = list(showgrid = FALSE, fixedrange = TRUE),
      yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange = TRUE),
      legend = list(x = 0.9, y = 0.9)
    ) %>% config(displayModeBar = FALSE)
  } else {
    plot_ly(type = 'scatter', mode = 'markers')
  }
}