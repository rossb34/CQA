\name{estimateParametersHistorically}
\alias{estimateParametersHistorically}
\title{Estimate spread parameters}
\usage{
  estimateParametersHistorically(P1, P2, window = 20,
    method = c("ols", "tls"))
}
\arguments{
  \item{P1}{Time series of close prices for symbol 1}

  \item{P2}{Time series of close prices for symbol 2}

  \item{method}{method to estimate the hedge ratio, 'tls'
  or 'ols': tls = total least squares using principal
  components; ols = ordinary least squares using linear
  regression}

  \item{window}{number of periods}
}
\description{
  Estimate the hedge ratio and spread for two time series
  over a moving window using historical data
}
\author{
  Ross Bennett
}

