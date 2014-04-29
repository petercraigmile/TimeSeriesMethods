
tent.map.sim <- function (n, a) {
  ## ======================================================================
  ## Simulate a tent map of length 'n' and parameter 'a', starting from
  ## a random uniform RV at time 1.
  ## Reference: Brockwell and Davis, 2002.
  ## Updated : Peter F. Craigmile, November 2002.
  ## ======================================================================
  
  x <- c(runif(1), rep(0,n-1))
  for (t in 2:n)
    x[t] <- ifelse(x[t-1]<=a, x[t-1]/a, (1-x[t-1])/(1-a))
  x
}
