\name{priceToHILO}
\alias{priceToHILO}
\title{Price To n-period High and Low}
\usage{
  priceToHILO(prices, n = 252, lag = 1, HI.only = FALSE)
}
\arguments{
  \item{xts}{object of prices}

  \item{n}{number of lookback periods}

  \item{lag}{number of periods to lag for the n-period high
  and low. A value of lag=1 means the high and low are
  calculated as of the previous period.}

  \item{HI.only}{default FALSE. Optionally, only return the
  current price relative to the n-period high close}
}
\value{
  a list with two elements; HI (percent from n-period high)
  and LO (percent from n-period low). If HI.only=TRUE, only
  the current price relative to the n-period high close
  will be returned. The most recent observation is
  returned.
}
\description{
  Momentum metric of current close price percent from
  n-period high and low.
}

