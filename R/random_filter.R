
#' Filter stocks at random
#' 
#' This function randomly selects a number of stocks. This function can be used
#' to test if our screening and filtering methods can outperform a screening
#' method that selects stocks at random.
#' 
#' @param symbols character vector of stock symbols
#' @param min.stocks minimum number of stocks to select
#' @param max.stocks maximum number of stocks to select
#' @return character of randomly select stock symbols
#' @author Ross Bennett
#' @export
randomFilter <- function(symbols, min.stocks=50, max.stocks=100){
  # select a number between min.stocks and max.stocks
  n.select <- sample(x=min.stocks:max.stocks, size=1)
  
  # total number of stocks in our universe
  n.stocks <- length(symbols)
  
  # index of stocks to select
  tmp.idx <- sample(n.stocks, n.select)
  
  # character vector of randomly selected symbols (duplicates are removed)
  selected.stocks <- symbols[tmp.idx[!duplicated(tmp.idx)]]
  
  return(selected.stocks)
}

