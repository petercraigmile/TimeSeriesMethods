\name{plot.ts.residuals}
\alias{plot.ts.residuals}
\title{
Produces one possible set of plots for model diagnostics using time series residuals
}
\description{
Produces one possible set of plots for model diagnostics using time series residuals
}
\usage{
plot.ts.residuals(x, y = NULL, lag.max = NULL, mean.line = TRUE, 
		     acf.ylim = c(-0.25, 1), mfrow = c(2, 2), 
		     lags = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
If y is NULL the time series of residuals; otherwise the time variable
}
  \item{y}{
The time series of residuals when x is supplied as the time variable
}
  \item{lag.max}{
The maxiumium lag to show for sample autocorrelation and sample partial autocorrelation plots.
}
  \item{mean.line}{
Add a mean line for the time series plot of the residuals if TRUE.
}
  \item{acf.ylim}{
%%     ~~Describe \code{acf.ylim} here~~
}
  \item{mfrow}{
%%     ~~Describe \code{mfrow} here~~
}
  \item{lags}{
%%     ~~Describe \code{lags} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
}
\references{
}
\author{
Peter F. Craigmile
}
\note{
}
