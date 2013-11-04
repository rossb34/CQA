\name{simpleStrength}
\alias{simpleStrength}
\title{Smoothed Momentum Strength}
\usage{
  simpleStrength(prices, n = 252)
}
\arguments{
  \item{xts}{object of prices}

  \item{n}{number of lookback periods}
}
\value{
  Only returns the most recent observation
}
\description{
  Simple momentum strength metric based on n-period
  lookback rate of change and n-period standard deviation.
}

