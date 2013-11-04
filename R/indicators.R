
#' Momentum
#' 
#' Simple momentum metric just based on n-period lookback rate of change
#' 
#' @param xts object of prices
#' @param n number of lookback periods
#' @return Only returns the most recent observation
#' @export
simpleMomentum <- function(prices, n=252){
  # number of periods of data
  nPrices <- nrow(prices)
  # n must be less than the total number of periods of data
  n <- min(nPrices, n)
  return(prices[nPrices] / as.numeric(prices[(nPrices-n),]) - 1)
}

#' Smoothed Momentum Strength
#' 
#' Simple momentum strength metric based on n-period lookback rate of change
#' and n-period standard deviation.
#' 
#' @param xts object of prices
#' @param n number of lookback periods
#' @return Only returns the most recent observation
#' @export
simpleStrength <- function(prices, n=252){
  # This is similar to the sharpe ratio
  # number of periods of data
  nPrices <- nrow(prices)
  # n must be less than the total number of periods of data
  n <- min(nPrices, n)
  nROC <- prices[nPrices] / as.numeric(prices[(nPrices-n),]) - 1
  # calculate the one-period returns used for sd 
  tmpROC <- ROC(x=tail(prices, n+1), n=1, type="discrete")
  tmpSD <- apply(tmpROC, MARGIN=2, FUN=sd, na.rm=TRUE)
  return(nROC / tmpSD)
}

#' Smoothed Momentum
#' 
#' Momentum metric based on rolling n-period lookback rate of change and smoothed
#' using an EMA. 
#' 
#' @param xts object of prices
#' @param nROC number of lookback periods for ROC calculation
#' @param nEMA number of lookback periods for EMA calculation
#' @return Only returns the most recent observation.
#' @export
emaMomentum <- function(prices, nROC=252, nEMA=50){
  tmpROC <- ROC(x=prices, n=nROC, type="discrete")
  emaROC2 <- vector("numeric", length=ncol(tmpROC))
  for(i in 1:ncol(tmpROC)){
    # only calculate based on the nROC most recent observations of tmpROC
    emaROC2[i] <- as.numeric(last(EMA(x=tail(tmpROC[,i], nROC), n=nEMA)))
  }
  names(emaROC2) <- colnames(prices)
  return(emaROC2)
}

# emaMomentum <- function(prices, nROC=252, nEMA=50){
#   tmpROC <- ROC(x=prices, n=nROC, type="discrete")
#   emaROC <- apply(X=tmpROC, MARGIN=2, FUN=EMA, n=nEMA)
#   out <- as.numeric(tail(emaROC, 1))
#   names(out) <- colnames(prices)
#   return(out)
# }



#' Smoothed Momentum Strength
#' 
#' Momentum strength metric based on rolling n-period lookback rate of change 
#' and n-period rolling standard deviation, both are smoothed using an EMA. 
#' 
#' @param xts object of prices
#' @param nROC number of lookback periods for ROC calculation
#' @param nEMA number of lookback periods for EMA calculation
#' @return Only returns the most recent observation.
#' @export
emaStrength <- function(prices, nROC=252, nEMA=50, nSD=252){
  tmpROC <- ROC(x=prices, n=nROC, type="discrete")
  emaROC <- apply(X=tmpROC, MARGIN=2, FUN=EMA, n=nEMA)
  n1ROC <- ROC(x=prices, n=1, type="discrete")
  tmpSD <- apply(X=n1ROC, MARGIN=2, FUN=runSD, n=nSD)
  emaSD <- apply(X=tmpSD, MARGIN=2, FUN=EMA, n=50)
  out <- as.numeric(tail(emaROC, 1)) / as.numeric(tail(emaSD, 1))
  names(out) <- names(prices)
  return(out)
}

#' Price To n-period High and Low
#' 
#' Momentum metric of current close price percent from n-period high and low.
#' 
#' @param xts object of prices
#' @param n number of lookback periods
#' @param lag number of periods to lag for the n-period high and low. A value
#' of lag=1 means the high and low are calculated as of the previous period.
#' @param HI.only default FALSE. Optionally, only return the current price 
#' relative to the n-period high close
#' @return a list with two elements; HI (percent from n-period high) and 
#' LO (percent from n-period low). If HI.only=TRUE, only the current price 
#' relative to the n-period high close will be returned. The most 
#' recent observation is returned.
#' @export
priceToHILO <- function(prices, n=252, lag=1, HI.only=FALSE){
  nPrices <- nrow(prices)
  # calculate the n day high and low as of the lagged period
  tmpP <- prices[(nPrices-n-lag):(nPrices-lag)]
  tmpMax <- apply(X=tmpP, MARGIN=2, FUN=max)
  HI <- prices[nPrices] / tmpMax - 1
  if(HI.only){
    return(HI)
  } else {
    tmpMin <- apply(X=tmpP, MARGIN=2, FUN=min)
    LO <- prices[nPrices] / tmpMin - 1
    return(list(HI=HI, LO=LO))
  }
}

#' Price To EMA
#' 
#' Momentum metric based on current close price relative to n-period EMA. A 
#' positive (negative value indicates that the current close price is above (below)
#' the n-period EMA.
#' 
#' @param prices xts object of prices
#' @param n number of periods to use for moving average
#' @return Current close price percentage above or below the EMA. Only returns 
#' the most recent observation.
#' @export
priceToEMA <- function(prices, n=252){
  # tmpEMA <- apply(X=tail(prices, n), MARGIN=2, FUN=EMA, n=n)
  out <- vector("numeric", ncol(prices))
  for(i in 1:ncol(prices)){
    tmpEMA <- last(EMA(x=tail(prices[,i], n*2), n=n))
    out[i] <- as.numeric(last(prices[,i])) / as.numeric(tmpEMA) - 1
  }
  names(out) <- colnames(prices)
  return(out)
}
