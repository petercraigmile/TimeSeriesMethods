
plot.ts.residuals <- function (x, y=NULL, lag.max=NULL, mean.line=TRUE,
			    acf.ylim=c(-0.25,1), mfrow=c(2,2),
                            lags=NULL, ...) {
  ## ======================================================================
  ## Define a function which we will use to summarize time series
  ## residuals
  ## ======================================================================

  ## we want a 'mfrow[1] x mfrow[2]' display for the
  ## plots, only if mfrow is not NULL
  if (!is.null(mfrow))
    par(mfrow=mfrow)
  
  if (is.null(y)) ## if we do not supply a time variable.
  {
    y <- as.numeric(x)
    x <- seq(length(y))
  } else { ## make sure we get no trouble from time series objects!
    x <- as.numeric(x)
    y <- as.numeric(y)
  }

  if (is.null(lag.max)) {
    lag.max <-  floor(10 * log10(length(x)))
  }

  ## Produce a time series plot of the residual,
  ## adding a line at 'y'=0 if 'mean.line' is TRUE.  
  plot(x, y, type="l", ...)
  if (mean.line) abline(h=0, lty=2)

  ## Produce a normal Q-Q plot.
  qqnorm(y, main="")
  qqline(y)

  ## Plot the sample ACF and PACF
  if (is.null(lags)) {
    acf(y, main="", lag.max=lag.max, xlim=c(0, lag.max), ylim=acf.ylim,
        ylab="sample ACF")

    pacf(y, main="", lag.max=lag.max, xlim=c(0, lag.max), ylim=acf.ylim,
         ylab="sample PACF")
  }  
  else {
    acf(y, main="", lag.max=lag.max, xlim=c(0, lag.max), ylim=acf.ylim,
        ylab="sample ACF", xaxt="n")
    axis(side=1, at=lags)

    pacf(y, main="", lag.max=lag.max, xlim=c(0, lag.max), ylim=acf.ylim,
         ylab="sample PACF", xaxt="n")
    axis(side=1, at=lags)
  }

  ## Finally carry out the Box-Pierce test
  ## We will use the same number of lags as used in the ACF and PACF plot.
  Box.test(y, lag.max, type="Ljung-Box")
}

