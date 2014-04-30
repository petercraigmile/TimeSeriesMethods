
days.per.month <- function (leap=FALSE) {
  ## ======================================================================
  ## Returns a vector of the number of days in each month of the year
  ## pfc@stat.osu.edu
  ## ======================================================================

  if (!leap) {
    
    c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  } else {
    
    c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }
}


months.axis.on.days.of.year <- function (every=3, leap=FALSE, num.chars=3) {
  ## ======================================================================
  ## Put the month names on an x axis of a plot of days of the year
  ## pfc@stat.osu.edu
  ## ======================================================================
  
  ks <- seq(1 ,12, every)
  
  axis(at=cumsum(c(1, days.per.month(leap)))[ks],
       labels=substr(month.name, 1, num.chars)[ks],
       side=1, cex.axis=0.85)
}



## An example

## plot(1:365, rnorm(365), type="l", xaxt="n", xlab="day of year", ylab="time series value")

## months.axis.on.days.of.year(1)

