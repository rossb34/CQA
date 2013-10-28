\name{calculateReturns}
\alias{calculateReturns}
\title{Calculate returns for several symbols}
\usage{
  calculateReturns(symbols, type = "discrete", ...)
}
\arguments{
  \item{symbols}{character vector of symbols}

  \item{type}{discrete or continuous compounding type}

  \item{\dots}{any other passthru parameters to ROC}
}
\value{
  an xts object of the returns of each asset in symbols
}
\description{
  This function calculates the returns for a character
  vector of symbols. The xts objects for the symbols must
  be loaded in the global environment. This function is a
  wrapper around ROC to calculate the 1-period returns.
}
\author{
  Ross Bennett
}

