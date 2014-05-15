
AR.acvs.to.lag.p <- function (ar, sigma2=1) {
  ## ======================================================================
  ## Calculate lags 0 to p of the autocovariance sequence (ACVS) of an
  ## autoregressive, AR(p) process with coefficients 'ar' (of length
  ## p), and innovation variance sigma2.
  ##
  ## Based on an argument for calculating the ACVS of an AR(p) process
  ## from Chapter 3 of Brockwell and Davis (2002).
  ##
  ## pfc@stat.osu.edu
  ## ======================================================================
  
  p  <- length(ar)
  mm <- diag(p+1)

  ## CAREFUL: This code is written to overwrite the matrix 'mm' 
  ##          many times per iteration!
  for (h in 0:p) {
    for (j in 1:p) {
      k <- abs(h-j)+1
      mm[h+1, k] <- mm[h+1, k] - ar[j]
    }
  }

  solve(mm, c(sigma2, rep(0, p)))
}



AR.acvs.to.lag <- function (lag.max, ar, sigma2=1) {
  ## ======================================================================
  ## Calculate lags 0 to lag.max of the autocovariance sequence (ACVS) of an
  ## AR(p) process with coefficients 'ar', and innovation variance sigma2.
  ## (Assumes lag.max is positive)
  ##
  ## pfc@stat.osu.edu
  ## ======================================================================

  acvs.p <- AR.acvs.to.lag.p(ar, sigma2)
  ARMAacf(ar, lag.max=lag.max) * acvs.p[1]
}


