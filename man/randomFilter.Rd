\name{randomFilter}
\alias{randomFilter}
\title{Filter stocks at random}
\usage{
  randomFilter(symbols, min.stocks = 50, max.stocks = 100)
}
\arguments{
  \item{symbols}{character vector of stock symbols}

  \item{min.stocks}{minimum number of stocks to select}

  \item{max.stocks}{maximum number of stocks to select}
}
\value{
  character of randomly select stock symbols
}
\description{
  This function randomly selects a number of stocks. This
  function can be used to test if our screening and
  filtering methods can outperform a screening method that
  selects stocks at random.
}
\author{
  Ross Bennett
}

