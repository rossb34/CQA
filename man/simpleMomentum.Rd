\name{simpleMomentum}
\alias{simpleMomentum}
\title{Momentum}
\usage{
  simpleMomentum(prices, n = 252)
}
\arguments{
  \item{xts}{object of prices}

  \item{n}{number of lookback periods}
}
\value{
  Only returns the most recent observation
}
\description{
  Simple momentum metric just based on n-period lookback
  rate of change
}

