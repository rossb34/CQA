
#' Calculate returns for several symbols
#' 
#' This function calculates the returns for a character vector of symbols. The
#' xts objects for the symbols must be loaded in the global environment. This
#' function is a wrapper around ROC to calculate the 1-period returns.
#' 
#' @param symbols character vector of symbols
#' @param type discrete or continuous compounding type
#' @param \dots any other passthru parameters to ROC
#' @return an xts object of the returns of each asset in symbols
#' @author Ross Bennett
#' @export
calculateReturns <- function(symbols, type="discrete", ...){
  n.symbols <- length(symbols)
  for(i in 1:n.symbols){
    x <- try(get(symbols[i]), silent=TRUE)
    if(inherits(x, "try-error")) stop(paste(symbols[i], "not detected."))
    x.ret <- ROC(x=Ad(x), n=1, type=type, ...)
    colnames(x.ret) <- symbols[i]
    if(i == 1){
      out.ret <- x.ret
    } else {
      out.ret <- cbind(out.ret, x.ret)
    }
  }
  return(out.ret)
}












