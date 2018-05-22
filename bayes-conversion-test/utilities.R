# Calculates the probability that B is better than A
{
  # Arguments:
  #   Alpha and beta parameters of beta distributions for A and B
  #   alpha = number of successes + 1
  #   beta = number of failures + 1
  # Assumptions:
  #   p_A ~ Beta(alpha_A, beta_A)
  #   p_B ~ Beta(alpha_B, beta_B)
  # Return value:
  #   Pr(p_B > p_A) = probability that the B version is better than the A version
  # Example of use:
  #   If version A converted 180 out of 1000 users and
  #     version B converted 200 out of 950 users type
  #     prob_B_beats_A(180 + 1, 1000 - 180 + 1, 200 + 1, 950 - 200 + 1)
  # Implemented based on Evan Miller`s blog post
  # http://www.evanmiller.org/bayesian-ab-testing.html#binary_ab
}
prob_B_beats_A <- function(alphaA, betaA, alphaB, betaB) {
  if(alphaA <= 0 || betaA <= 0 || alphaB <= 0 || betaB <= 0)
    result <- 0
  else {
    result <- 1
    for (i in 0:(alphaA - 1)) {
      result <- result - 
        exp(
          lbeta(alphaB + i, betaB + betaA) -
            log(betaA + i) -
            lbeta(1 + i, betaA) -
            lbeta(alphaB, betaB)
        )
    }
  }
  result
}

# Calculates the loss function when choosing B over A
{
  # Arguments:
  #   Alpha and beta parameters of beta distributions for A and B
  #   alpha = number of successes + 1
  #   beta = number of failures + 1
  # Assumptions:
  #   p_A ~ Beta(alpha_A, beta_A)
  #   p_B ~ Beta(alpha_B, beta_B)
  # Return value:
  #   Expected loss when choosing B over A
  #   The amount of uplift that one can expect to loose by choosing B over A
  # Example of use:
  #   If version A converted 180 out of 1000 users and
  #     version B converted 200 out of 950 users type
  #     expected_loss_B_over_A(180 + 1, 1000 - 180 + 1, 200 + 1, 950 - 200 + 1)
  # Implemented based on Chris Stucchio`s blog post and whitepaper
  # https://www.chrisstucchio.com/blog/2014/bayesian_ab_decision_rule.html
  # https://cdn2.hubspot.net/hubfs/310840/VWO_SmartStats_technical_whitepaper.pdf
}
expected_loss_B_over_A <- function(alphaA, betaA, alphaB, betaB) {
  if(alphaA <= 0 || betaA <= 0 || alphaB <= 0 || betaB <= 0)
    result <- 0
  else {
    exp(
      lbeta(alphaA + 1, betaA) - lbeta(alphaA, betaA) +
        log(1 - prob_B_beats_A(alphaA + 1, betaA, alphaB, betaB))
    ) -
      exp(
        lbeta(alphaB + 1, betaB) - lbeta(alphaB, betaB) +
          log(1 - prob_B_beats_A(alphaA, betaA, alphaB + 1, betaB))
      )
  }
}

# Calculates the HDI interval from ICDF
{
  # Arguments:
  #   ICDFname is R's name for the inverse cumulative density function
  #     of the distribution
  #   credMass is the desired mass of the HDI region
  #   tol is passed to R's optimize function
  # Return value:
  #   Highest density iterval (HDI) limits in a vector
  # Example of use:
  #   For determining HDI of a beta(30,12) distribution, type
  #     hdi_of_icdf(qbeta, shape1 = 30, shape2 = 12)
  #   Notice that the parameters of the ICDFname must be explicitly named
  #     e.g., hdi_of_icdf(qbeta, 30, 12) does not work
  # Copied from the package to Kruschke`s book DBDA 2
  # https://sites.google.com/site/doingbayesiandataanalysis/software-installation
  # which was adapted and corrected from Greg Snow's TeachingDemos package
}
hdi_of_icdf <- function(ICDFname, credMass = 0.95, tol = 1e-8, ...) {
  incredMass <-  1 - credMass
  intervalWidth <- function(lowTailPr, ICDFname, credMass, ...) {
    ICDFname(credMass + lowTailPr, ...) - ICDFname(lowTailPr, ...)
  }
  optInfo <- optimize(
    intervalWidth,
    c(0, incredMass),
    ICDFname = ICDFname,
    credMass = credMass,
    tol = tol, ...
  )
  HDIlowTailPr <- optInfo$minimum
  return(
    c(ICDFname(HDIlowTailPr, ...), ICDFname(credMass + HDIlowTailPr, ...))
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

# Creates sample of difference of two beta distributions
create_sample_beta_diff <- function(alpha_A, beta_A, alpha_B, beta_B, size = 1e5) {
  if(
    alpha_A >= 0 &&
    beta_A >= 0 &&
    alpha_B >= 0 &&
    beta_B >= 0
  ) {
    rbeta(size, alpha_B, beta_B) - rbeta(size, alpha_A, beta_A)
  } else 0
}

# Creates plotly chart of two posterior beta distributions
plot_post_beta <- function(
    alphaA, betaA, alphaB, betaB, hdiA, hdiB, xlim = c(0, 1), printPlot = TRUE, ...
  ) {
  if(printPlot) {
    x <- seq(xlim[1], xlim[2], by = (xlim[2] - xlim[1])/1000)
    xAreaA <- seq(hdiA[1], hdiA[2], by = (hdiA[2] - hdiA[1])/1000)
    xAreaB <- seq(hdiB[1], hdiB[2], by = (hdiB[2] - hdiB[1])/1000)
    yA <- dbeta(x, alphaA, betaA)
    yB <- dbeta(x, alphaB, betaB)
    yAreaA <- dbeta(xAreaA, alphaA, betaA)
    yAreaB <- dbeta(xAreaB, alphaB, betaB)
    plot_ly(
      x = x,
      y = yA,
      type = 'scatter',
      mode = 'lines',
      name = 'sample A',
      line = list(color = '#5BC0DE'),
      hoverinfo = 'none'
      # text = ~paste(sprintf('%.3g%%', x*100))
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
      x = x,
      y = yB,
      type = 'scatter',
      mode = 'lines',
      name = 'sample B',
      line = list(color = '#5CB85C'),
      hoverinfo = 'none'
      # text = ~paste(sprintf('%.3g%%', x*100))
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
      title = 'Distribution of conversions A and B',
      titlefont = list(size = 14),
      xaxis = list(range = xlim, showgrid = FALSE, tickformat = '.1%', fixedrange = TRUE),
      yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange = TRUE),
      legend = list(x = 0.9, y = 0.9)
    ) %>% config(displayModeBar = FALSE)
  } else {
    plot_ly(type = 'scatter', mode = 'markers')
  }
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
      name = 'conversion B - A',
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
      title = 'Approximate distribution of difference of conversions B - A',
      titlefont = list(size = 14),
      xaxis = list(showgrid = FALSE, tickformat = '.1%', fixedrange = TRUE),
      yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange = TRUE),
      legend = list(x = 0.9, y = 0.9)
    ) %>% config(displayModeBar = FALSE)
    
  } else {
    plot_ly(type = 'scatter', mode = 'markers')
  }
}