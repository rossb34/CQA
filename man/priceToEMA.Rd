\name{priceToEMA}
\alias{priceToEMA}
\title{Price To EMA}
\usage{
  priceToEMA(prices, n = 252)
}
\arguments{
  \item{prices}{xts object of prices}

  \item{n}{number of periods to use for moving average}
}
\value{
  Current close price percentage above or below the EMA.
  Only returns the most recent observation.
}
\description{
  Momentum metric based on current close price relative to
  n-period EMA. A positive (negative value indicates that
  the current close price is above (below) the n-period
  EMA.
}

