\name{isStationaryURDF}
\alias{isStationaryURDF}
\title{Stationary test}
\usage{
  isStationaryURDF(x,
    conf_level = c("10pct", "5pct", "1pct"),
    type = c("none", "drift", "trend"), lags = 1,
    selectlags = c("Fixed", "AIC", "BIC"))
}
\arguments{
  \item{x}{spread}

  \item{conf_level}{confidence level to reject H0}

  \item{type}{Test type, either "none", "drift" or "trend"}

  \item{lags}{Number of lags for endogenous variable to be
  included.}

  \item{selectlags}{Lag selection can be achieved according
  to the Akaike "AIC" or the Bayes "BIC" information
  criteria. The maximum number of lags considered is set by
  lags. The default is to use a "fixed" lag length set by
  lags.}
}
\description{
  Test if a price series is stationary using the Augmented
  Dickey-Fuller Test. Wrapper around urca::ur.df to return
  TRUE if the spread is likely stationary, FALSE otherwise
}
\details{
  H0: x has a unit root If the spread has a unit root, it
  is not mean reverting
}
\author{
  Ross Bennett
}

