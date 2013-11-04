\name{emaStrength}
\alias{emaStrength}
\title{Smoothed Momentum Strength}
\usage{
  emaStrength(prices, nROC = 252, nEMA = 50, nSD = 252)
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
  Momentum strength metric based on rolling n-period
  lookback rate of change and n-period rolling standard
  deviation, both are smoothed using an EMA.
}

