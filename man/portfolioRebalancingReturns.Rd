\name{portfolioRebalancingReturns}
\alias{portfolioRebalancingReturns}
\title{Calculate returns of a rebalanced portfolio}
\usage{
  portfolioRebalancingReturns(R, weights,
    show.weights = FALSE)
}
\arguments{
  \item{R}{xts object of asset returns}

  \item{weights}{named list of weights where the names of
  the list are the rebalancing dates}

  \item{show.weights}{TRUE/FALSE to show the weights at
  each period}
}
\value{
  xts object of portfolio returns
}
\description{
  This function calculates returns of a rebalanced
  portfolio based on a list of weights at each rebalance
  period. This function is primarily to work with the
  dynamic rebalancing strategy. A different function may
  need to be used with a standard
  optimize.portfolio.rebalancing object from
  PortfolioAnalytics.
}

