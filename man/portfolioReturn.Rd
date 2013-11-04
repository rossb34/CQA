\name{portfolioReturn}
\alias{portfolioReturn}
\title{Calculate portfolio returns}
\usage{
  portfolioReturn(R, weights, show.weights = FALSE)
}
\arguments{
  \item{R}{xts object of returns}

  \item{weights}{vector of weights}

  \item{show.weights}{TRUE/FALSE to show the weights at
  each period}
}
\value{
  xts object of portfolio returns
}
\description{
  This function calculates portfolio returns based on a
  single set of weights.
}

