
#' Load stock data from a local directory
#' 
#' This is a temporary hack to load stock data until we get the stock data
#' loaded in a database. \code{parent.frame} is for where to assign the data.
#' 
#' @param stocks character vector of stock symbols
#' @param data.dir directory of stock data
#' @param \dots passthru parameters to \code{read.table}
#' @author Ross Bennett
#' @export
loadStocks <- function(stocks, data.dir, ...){
  # load the symbols
  for(sym in stocks){
    tmp <- try(as.xts(read.zoo(paste(data.dir, sym, ".csv", sep=""), ...=...)))
    assign(sym, tmp, pos=parent.frame())
    rm(tmp)
  }
}