\name{priceToHILO}
\alias{priceToHILO}
\title{Price To n-period High and Low}
\usage{
  priceToHILO(prices, n = 252, lag = 1)
}
\arguments{
  \item{xts}{object of prices}

  \item{n}{number of lookback periods}

  \item{lag}{number of periods to lag for the n-period high
  and low. A value of lag=1 means the high and low are
  calculated as of the previous period.}
}
\value{
  a list with two elements; HI (percent from n-period high)
  and LO (percent from n-period low). Only returns the most
  recent observation.
}
\description{
  Momentum metric of current close price percent from
  n-period high and low.
}

