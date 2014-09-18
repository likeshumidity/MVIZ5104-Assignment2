# Normal Plot Assignment for MVIZ5104
# Generate plots for normal distributions with given parameters
# Created by: Jacob Joseph
# Due: 9/16/2014
#
# Based on Rcmdr example:
# .x <- seq(6.709, 13.291, length.out=1000)
# plotDistr(.x, dnorm(.x, mean=10, sd=1), cdf=FALSE, xlab="x", ylab="Density",
#           main=paste("Normal Distribution:  Mean=10, Standard deviation=1"))
# remove(.x)
# -----------------------------------------


# Assignment data in (mu, sigma) pairs
summaryDataParameters <- data.frame(c(0.0, 4.0, 0.0, -2.0),
                                    c(1.0, 1.0, 0.5, 0.5),
                                    c("red", "blue", "darkgreen", "magenta"),
                                    c(2,2,2,2),
                                    c(1,2,3,4))

names(summaryDataParameters) <- c("Mean", "StdDev", "col", "lwd", "lty")


CreateLegendLabels <- function(summaryDataParameters) {
  .labels <- c()
  for (row in 1:nrow(summaryDataParameters)) {
    .labels <- c(.labels, paste("mean=", format(summaryDataParameters[row,][1,1], nsmall=1),
                                "; sd=", format(summaryDataParameters[row,][1,2], nsmall=1)))
#    append(.labels, row)
#    print(.labels)
#    append(.labels, bquote(paste(mu, "=", .(summaryDataParameters[row,][1,1]),
#                                 "; ", sigma, "=", .(summaryDataParameters[row,][1,2]))))
  }

return(.labels)
}

PlotNormalDistributionDensity <- function(.x, .xd, .mu, .sigma, .xlim, .ylim, row,
                                          plotTogether = FALSE, .col = "black",
                                          .lwd = 1, .lty = 1) {
  if (plotTogether) {
    if (row == 1) {
      # For the first value pair, plot a graph
      plot(.x, .xd, type="l",
           xlab="x",
           ylab="Density",
           main=paste("Normal Population Distribution Densities"),
           xlim=.xlim,
           ylim=.ylim,
           col=.col,
           lwd=.lwd,
           lty=.lty)
    } else {
      # For subsequent value pairs, plot a line
      lines(.x, .xd, col=.col, lwd=.lwd, lty=.lty)
    }
  } else {
    # For each value pair, plot graph
    plot(.x, .xd, type="l",
         xlab="x",
         ylab="Density",
         main=paste("Normal Population Distribution Density"),
         sub=bquote(paste(mu, "=", .(format(.mu,nsmall=1)), "; ", sigma, "=", .(format(.sigma,nsmall=1)))),
         xlim=.xlim,
         ylim=.ylim)
  }
}

CalculateDistributionAxesLimits <- function(summaryDataParameters) {
  # Calculate low and high values for axes
  xLow <- min(summaryDataParameters$Mean - summaryDataParameters$StdDev * 4)
  xHigh <- max(summaryDataParameters$Mean + summaryDataParameters$StdDev * 4)

  for (row in 1:nrow(summaryDataParameters)) {
    .mu <- summaryDataParameters[row,][1,1]
    .sigma <- summaryDataParameters[row,][1,2]

    # Generate sequence of values between -4 and 4 standard deviations from mean
    .x <- seq(xLow, xHigh, length.out=1000)
    
    # Generate normalized distribution
    .xd <- dnorm(.x, .mu, .sigma)

    if (row > 1) {
      if (min(.xd) < yLow) {
        yLow <- min(.xd)
      }
      if (max(.xd) > yHigh) {
        yHigh <- max(.xd)
      }
    } else {
      yLow <- min(.xd)
      yHigh <- max(.xd)
    }
  }

  return(c(xLow, xHigh, yLow, yHigh))
}

PlotDistributions <- function(summaryDataParameters, sameScale = FALSE, plotTogether = FALSE) {
  if (sameScale || plotTogether) {
    axesLimits <- CalculateDistributionAxesLimits(summaryDataParameters)
    .xlim <- c(axesLimits[1], axesLimits[2])
    .ylim <- c(axesLimits[3], axesLimits[4])
  }

  # Iterate through (mu, sigma) value pairs
  for (row in 1:nrow(summaryDataParameters)) {
    .mu <- summaryDataParameters[row,][1,1]
    .sigma <- summaryDataParameters[row,][1,2]
    
    if (!sameScale && !plotTogether) {
      .xlim <- c(.mu - .sigma * 4.0, .mu + .sigma * 4.0)
    }

    # For each value pair, generate sequence of values between -4 and 4 standard deviations from mean
    .x <- seq(.xlim[1], .xlim[2], length.out=1000)

    # For each value, generate normalized distribution
    .xd <- dnorm(.x, .mu, .sigma)

    if (!sameScale && !plotTogether) {
      .ylim <- c(min(.xd), max(.xd))
    }

    if (plotTogether) {
      .col <- summaryDataParameters[row,][1,3]
      .lwd <- summaryDataParameters[row,][1,4]
      .lty <- summaryDataParameters[row,][1,5]
      
      PlotNormalDistributionDensity(.x, .xd, .mu, .sigma, .xlim, .ylim, row,
                                    plotTogether, .col, .lwd, .lty)
    } else {
      PlotNormalDistributionDensity(.x, .xd, .mu, .sigma, .xlim, .ylim, row)
    }
  }

  if (plotTogether) {
    # Add Legend
    .labels = CreateLegendLabels(summaryDataParameters)
    legend("topright", inset=.05, title="Distributions",
           legend=.labels,
           col=summaryDataParameters$col,
           lwd=summaryDataParameters$lwd,
           lty=summaryDataParameters$lty)
  }
}

PlotDistributions(summaryDataParameters)
PlotDistributions(summaryDataParameters, sameScale=TRUE)
PlotDistributions(summaryDataParameters, plotTogether=TRUE)


# Clean up
remove(summaryDataParameters)
