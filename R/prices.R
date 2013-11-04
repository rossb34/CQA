
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
  
  # list.sym <- list()
  # for(i in 1:length(symbols)) {
  #   list.sym[[symbols[i]]] <- get(symbols[i])
  # }
  # do.call(merge, lapply(list.sym, Ad))
  n.symbols <- length(symbols)
  for(i in 1:n.symbols){
    x <- try(get(symbols[i]), silent=TRUE)
    if(inherits(x, "try-error")) stop(paste(symbols[i], "not detected."))
    tmp.x <- try(Ad(x), silent=TRUE)
    if(!inherits(tmp.x, "try-error")) {
      # x.ret <- ROC(x=tmp.x, n=1, type=type, ...)
      # colnames(x.ret) <- symbols[i]
      if(i == 1){
        out <- tmp.x
      } else {
        out <- cbind(out, tmp.x)
      }
    } else {
      warning(tmp.x)
      print(symbols[i])
    }
  }
  return(out)
}
