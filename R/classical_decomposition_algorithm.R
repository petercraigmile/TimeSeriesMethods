
## ======================================================================
## The classical decomposition algorithm, as described in
## Brockwell and Davis [2002, Section 1.5]
## Written for teaching 'The Statistical Analysis of Time Series'.
##
## 'x' is the time series, and 'd' is the period of the seasonality.
##
## pfc@stat.osu.edu.
## ======================================================================

cda.step1 <- function (x, d=12) {
  
  ## create the filter
  if (d%%2==1) ## d is odd
    our.filter <- rep(1,d)/d
  else ## d is even
    our.filter <- c(0.5, rep(1,d-1), 0.5)/d

  n        <- length(x)
  half.d   <- d/2
  longer.x <- c(rep(x[1],half.d), x, rep(x[n],half.d))

  ## return the filtered sequence
  filter(longer.x, our.filter)[(half.d+1):(n+half.d)]
}



cda.step2 <- function (x, d=12)
{
  s.hat.stars <- apply(matrix(x, nrow=d), 1, mean)
  s.hat.stars - mean(s.hat.stars)
}
