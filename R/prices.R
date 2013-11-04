
#' Combine the Adjust close column of multiple xts objects
#' 
#' This function combines the Adjusted closing prices of multiple xts objects.
#' 
#' @param symbols character vector of symbols in your current workspace
#' @return xts object of adjusted closing prices
#' @export
combinePrices <- function(symbols) {
  # The function takes a character vector of symbols loaded into
  # the environment and returns an xts object of Adjusted close prices
  # Currently this is only for prepping
  
  # symbols         : character vector of symbols
  # list.sym        : list of symbols with market data
  
  list.sym <- list()
  for(i in 1:length(symbols)) {
    list.sym[[symbols[i]]] <- get(symbols[i])
  }
  do.call(merge, lapply(list.sym, Ad))
}
