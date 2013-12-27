
#' Estimate spread parameters
#' 
#' Estimate the hedge ratio and spread for two time series
#' 
#' @param P1 Time series of close prices for symbol 1
#' @param P2 Time series of close prices for symbol 2
#' @param method method to estimate the hedge ratio, 'tls' or 'ols': 
#' tls = total least squares using principal components; 
#' ols = ordinary least squares using linear regression
#' @author Ross Bennett
#' @export
estimateParameters <- function(P1, P2, method=c("ols", "tls")){
  method <- match.arg(method)
  hedge_ratio <- hedgeRatio(P1, P2, method)
  spread <- P1 - hedge_ratio * P2
  return(list(spread=as.numeric(last(spread)), hedge_ratio=hedge_ratio))
}

#' Estimate spread parameters
#' 
#' Estimate the hedge ratio and spread for two time series over a moving window 
#' using historical data
#' 
#' @param P1 Time series of close prices for symbol 1
#' @param P2 Time series of close prices for symbol 2
#' @param method method to estimate the hedge ratio, 'tls' or 'ols': 
#' tls = total least squares using principal components; 
#' ols = ordinary least squares using linear regression
#' @param window number of periods
#' @author Ross Bennett
#' @export
estimateParametersHistorically <- function(P1, P2, window=20, method=c("ols", "tls")){
  method <- match.arg(method)
  # Combine P1 and P2 into a single object
  data <- cbind(P1, P2)
  if(method == "ols"){
    fun_ols <- function(x){
      out <- estimateParameters(P1=x[,1], P2=x[,2], method="ols")
      c(spread=out$spread, hedge_ratio=out$hedge_ratio)
    }
    fun <- match.fun(fun_ols)
  } else if(method == "tls"){
    fun_tls <- function(x){
      out <- estimateParameters(P1=x[,1], P2=x[,2], method="tls")
      c(spread=out$spread, hedge_ratio=out$hedge_ratio)
    }
    fun <- match.fun(fun_tls)
  }
  # xts(rollapplyr(data, window, fun, by.column = FALSE), index(data))
  rollapplyr(data, window, fun, by.column = FALSE)
}
