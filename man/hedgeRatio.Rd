\name{hedgeRatio}
\alias{hedgeRatio}
\title{Compute the hedge ratio}
\usage{
  hedgeRatio(P1, P2, method = c("tls", "ols"),
    no_int = TRUE)
}
\arguments{
  \item{P1}{Time series of close prices for symbol 1}

  \item{P2}{Time series of close prices for symbol 2}

  \item{method}{method to estimate the hedge ratio, 'tls'
  or 'ols': tls = total least squares using principal
  components; ols = ordinary least squares using linear
  regression}

  \item{no_int}{omit intercept from the linear model when
  method='ols'}
}
\description{
  Computes the hedge ratio for a spread of two time series
}
\author{
  Ross Bennett
}

