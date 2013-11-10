\name{backtestMomentum}
\alias{backtestMomentum}
\title{Backtest a momentum strategy}
\usage{
  backtestMomentum(prices, trainingPeriods = 252,
    trailingPeriods = 252, rebalanceFrequency = "months",
    N = 20, FUN, ..., multiplier = 1, verbose = FALSE)
}
\arguments{
  \item{prices}{xts object of prices}

  \item{trainingPeriods}{number of initial training periods
  to use}

  \item{tralingPeriods}{number of trailing periods to use.}

  \item{rebalanceFrequency}{frequency to rebalance:
  "weeks", "months", "quarters", or "years"}

  \item{N}{number of assets to include in portfolio. see
  details}

  \item{FUN}{name of function to use for calculating the
  momentum metric}

  \item{\dots}{passthru parameters to FUN}
}
\value{
  a list with the weights and portfolio returns
}
\description{
  This function is designed to backtest a momentum strategy
  that goes long the top N/2 assets and short the bottom
  N/2 assets based on a momentum metric.
}

