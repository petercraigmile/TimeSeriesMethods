
MA.acvs <- function (lag.max=NULL, theta, sigma2=1, lags) {
  ## ======================================================================
  ## Calculate the autocovariance sequence (ACVS) for the MA(q)
  ## process with coefficients theta and innovation variance sigma2.
  ## Calculate the ACVS at lags 0...lag.max unless lag.max is NULL;
  ## in that case evaluate at lags 'lags'.  
  ## ======================================================================

  if (!is.null(lag.max)) {

    the.acf <- ARMAacf(ma=theta, lag.max=lag.max)
  } else {

    the.acf <- ARMAacf(ma=theta, lag.max=max(lags))[abs(lags)+1]
  }

  sigma2 * (1 + sum(theta^2)) * the.acf
}

