\name{emaMomentum}
\alias{emaMomentum}
\title{Smoothed Momentum}
\usage{
  emaMomentum(prices, nROC = 252, nEMA = 50)
}
\arguments{
  \item{xts}{object of prices}

  \item{nROC}{number of lookback periods for ROC
  calculation}

  \item{nEMA}{number of lookback periods for EMA
  calculation}
}
\value{
  Only returns the most recent observation.
}
\description{
  Momentum metric based on rolling n-period lookback rate
  of change and smoothed using an EMA.
}

