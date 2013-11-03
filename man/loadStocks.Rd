\name{loadStocks}
\alias{loadStocks}
\title{Load stock data from a local directory}
\usage{
  loadStocks(stocks, data.dir, ...)
}
\arguments{
  \item{stocks}{character vector of stock symbols}

  \item{data.dir}{directory of stock data}

  \item{\dots}{passthru parameters to \code{read.table}}
}
\description{
  This is a temporary hack to load stock data until we get
  the stock data loaded in a database. \code{parent.frame}
  is for where to assign the data.
}
\author{
  Ross Bennett
}

