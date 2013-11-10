
#' Backtest a momentum strategy
#' 
#' This function is designed to backtest a momentum strategy that goes long
#' the top N/2 assets and short the bottom N/2 assets based on a momentum
#' metric. 
#' 
#' @param prices xts object of prices
#' @param trainingPeriods number of initial training periods to use
#' @param tralingPeriods number of trailing periods to use.
#' @param rebalanceFrequency frequency to rebalance: "weeks", "months", "quarters", or "years"
#' @param N number of assets to include in portfolio. see details
#' @param FUN name of function to use for calculating the momentum metric
#' @param \dots passthru parameters to FUN
#' @return a list with the weights and portfolio returns
#' @export
backtestMomentum <- function(prices, trainingPeriods=252, trailingPeriods=252, rebalanceFrequency="months", N=20, FUN, ..., verbose=FALSE){
  nAssets <- ncol(prices)
  nObs <- nrow(prices)
  
  if(trainingPeriods >= nObs) stop("trainingPeriods greater than number of observations in prices")
  if(trailingPeriods >= nObs) stop("trailingPeriods greater than number of observations in prices")
  
  if(N > nAssets) stop("N greater than number of assets")
  
  # Calculate the rebalance dates for the endpoints index
  ep.i <- endpoints(prices, on=rebalanceFrequency)[which(endpoints(prices, on=rebalanceFrequency) >= trainingPeriods)]
  optList <- foreach(ep=iter(ep.i), .errorhandling='pass', .packages='CQA') %dopar% {
    # subset the returns data to the periods I want
    # R[(ifelse(ep - trailingPeriods >= 1, ep - trailingPeriods, 1)):ep, ] from PortfolioAnalytics
    # tmpPrices <- prices[(ifelse(ep - trailingPeriods >= 1, ep - trailingPeriods, 1)):ep, ]
    
    fun <- try(match.fun(FUN), silent=TRUE)
    if(inherits(x=fun, "try-error")) stop("could not match function:", fun)
    
    dargs <- list(...)
    dargs$prices <- prices[(ifelse(ep - trailingPeriods >= 1, ep - trailingPeriods, 1)):ep, ]
    
    tmpMeasure <- try(do.call(fun, dargs), silent=TRUE)
    if(inherits(x=tmpMeasure, "try-error")) stop(paste("Failed calling", fun, ";", tmpMeasure))
    
    # tmpMeasure <- emaStrength(prices=prices[(ifelse(ep - trailingPeriods >= 1, ep - trailingPeriods, 1)):ep, ], nROC=200, nEMA=50, nSD=200)
    tmpMeasure <- tmpMeasure[order(tmpMeasure, decreasing=TRUE)]
    
    p <- length(tmpMeasure)
    topN <- tmpMeasure[1:(N/2)]
    bottomN <- tmpMeasure[(1+p-N/2):p]
    longWeights <- rep(1/(N/2), (N/2))
    names(longWeights) <- names(topN)
    shortWeights <- rep(-1/(N/2), (N/2))
    names(shortWeights) <- names(bottomN)
    
    if(verbose) print(paste("Completed backtest for rebalance period", index(prices[ep])))
    
    c(longWeights, shortWeights)
  }
  names(optList) <- index(prices[ep.i])
  # Portfolio returns through time with rebalancing
  # ret <- portfolioRebalancingReturns(R=ROC(x=prices, n=1, type="discrete"), weights=optList)
  return(optList)
}