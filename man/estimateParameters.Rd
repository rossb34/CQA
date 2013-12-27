\name{estimateParameters}
\alias{estimateParameters}
\title{Estimate spread parameters}
\usage{
  estimateParameters(P1, P2, method = c("ols", "tls"))
}
\arguments{
  \item{P1}{Time series of close prices for symbol 1}

  \item{P2}{Time series of close prices for symbol 2}

  \item{method}{method to estimate the hedge ratio, 'tls'
  or 'ols': tls = total least squares using principal
  components; ols = ordinary least squares using linear
  regression}
}
\description{
  Estimate the hedge ratio and spread for two time series
}
\author{
  Ross Bennett
}

