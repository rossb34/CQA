\name{isStationaryDF}
\alias{isStationaryDF}
\title{Stationary test}
\usage{
  isStationaryDF(x, conf_level = 0.05)
}
\arguments{
  \item{x}{spread}

  \item{conf_level}{confidence level to reject H0}
}
\description{
  Test if a price series is stationary using the standard
  Dickey-Fuller Test. Wrapper around tseries::adf.test to
  return TRUE if the spread is likely stationary, FALSE
  otherwise.
}
\details{
  H0: x has a unit root If the spread has a unit root, it
  is not mean reverting
}
\author{
  Ross Bennett
}

