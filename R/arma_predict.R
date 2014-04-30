

arma.predict <- function (m, ar, ma, h=1) {
  ## ======================================================================
  ## arma.predict(m, ar, ma, h=1)
  ##
  ## predict X_{m+h} based on X_{1}, ..., X_{m}, where {X_t} is an 
  ## ARMA(p,q) process with AR coefficients 'ar' (of length p) and MA 
  ## coefficients 'ma' (of length q).
  ##
  ## pfc@stat.osu.edu
  ## ======================================================================

  the.acf <- ARMAacf(ar=ar, ma=ma, lag.max=h+m-1)
  c(1, -solve(toeplitz(the.acf[1:m]), the.acf[(h+1):(m+h)]))
}

