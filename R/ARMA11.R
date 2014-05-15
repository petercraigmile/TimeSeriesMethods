

ARMA11.acvs <- function (lag.max=NULL, phi, theta, sigma2=1, lags) {
  ## ======================================================================
  ## Calculate the autocovariance sequence (ACVS) for the ARMA(1,1)
  ## process from lags 0 to lag.max If lag.max is NULL instead
  ## calculate the ACVS at lags 'lags'.
  ## The AR parameter is 'phi', the MA parameter is 'theta'
  ## and the innovation variance is 'sigma2'.
  ##
  ## Assumes: lag.max>=0.
  ## pfc@stat.osu.edu
  ## ======================================================================

  w   <- phi + theta
  eta <- 1-phi^2

  if (!is.null(lag.max)) {

    if (lag.max==0) {
      
      sigma2 * (1 + w^2/eta)
    } else {
      
      sigma2 * c(1 + w^2/eta, (w + w^2*phi/eta) * phi^(0:(lag.max-1)))
    }
  } else {
    
    sigma2 * ifelse(lags==0,
                    1 + w^2/eta,
                    (w + w^2*phi/eta) * phi^(abs(lags)-1))
  }
}

