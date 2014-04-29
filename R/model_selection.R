
FPE <- function (model) {
  ## ======================================================================
  ## Purpose : Calculates the Akaike's Final Prediction Error (FPE)
  ##           criterion for the AR 'model' fitted using the 'arima' function.
  ## Updated : Peter F. Craigmile, November 2002.
  ## Contact : pfc@stat.osu.edu
  ## ======================================================================
  
  n <- length(model$resid)
  p <- model$arma[1]
  
  (n+p)/(n-p) * model$sigma2
}




AIC.to.AICC <- function (aic, n, npars) {  
  ## ======================================================================
  ## Purpose : Transforms the AIC value to the AICC value.    
  ## Updated : Peter F. Craigmile, November 2002.
  ## Contact : pfc@stat.osu.edu
  ## ======================================================================
  
  aic - 2 * npars * ( 1 - n/(n-1-npars))  
}



AICC <- function (model) {
  ## ======================================================================
  ## Purpose : Calculate the AICC for the arima 'model'
  ##           fitted using the 'arima' function.
  ## Updated : Peter F. Craigmile, November 2002.
  ## Contact : pfc@stat.osu.edu
  ## ======================================================================
  
  AIC.to.AICC(AIC(model), length(model$resid), length(model$coef)+1)
}

