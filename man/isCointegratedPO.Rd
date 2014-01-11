\name{isCointegratedPO}
\alias{isCointegratedPO}
\title{Phillips & Ouliaris Cointegration Test}
\usage{
  isCointegratedPO(x,
    conf_level = c("10pct", "5pct", "1pct"),
    demean = c("none", "constant", "trend"),
    lag = c("short", "long"), type = c("Pz", "Pu"))
}
\arguments{
  \item{x}{Time series of prices}

  \item{conf_level}{confidence level to reject H0}

  \item{demen}{either 'none', 'constant', or 'trend'}

  \item{type}{Test type, either 'Pz' or 'Pu'}

  \item{lag}{Lag type, either 'short' or 'long'}
}
\description{
  Test if a price series is cointegrated using the Phillips
  & Ouliaris Cointegration Test. Wrapper around urca::ca.po
  to return TRUE if the time series is likely cointegrated,
  FALSE otherwise.
}
\details{
  H0: The time series are not cointegrated
}
\author{
  Ross Bennett
}

