
#' Calculate portfolio returns 
#' 
#' This function calculates portfolio returns based on a single set of weights.
#' 
#' @param R xts object of returns
#' @param weights vector of weights
#' @param show.weights TRUE/FALSE to show the weights at each period
#' @return xts object of portfolio returns
#' @export
portfolioReturn <- function(R, weights, show.weights=FALSE){
  # equivalent to geometric return for positive and negative weights
  
  # equation for w_i,_(t+1) from
  # http://www.slideshare.net/andsteinconsult/correct-calculation-of-ex-post-
  # contributions-to-return-volatility-and-tracking-error
  
  if(isTRUE(nrow(weights) > 1)) stop("This function is only used for calculating
                             portfolio returns given a single period 
                             weights vector")
  
  # weights <- checkData(weights, method = "xts")
  R <- checkData(R, method = "xts")
  
  port.ret <- vector(mode="numeric", length=nrow(R))
  
  tmpWeights <- matrix(0, nrow=nrow(R), ncol=ncol(R))
  colnames(tmpWeights) <- paste("w", colnames(R), sep=".")
  tmpWeights[1,] <- weights
  # Calculate the first period portfolio return because we know the first period weights
  port.ret[1] <- sum(as.numeric(R[1,]) * tmpWeights[1,])
  for(ii in 2:nrow(tmpWeights)){
    for(j in 1:ncol(tmpWeights)){
      tmpWeights[ii,j] <- tmpWeights[(ii-1),j] * (1 + R[(ii-1),j]) / (1 + port.ret[(ii-1)])
    }
    port.ret[(ii)] <- sum(as.numeric(R[ii,]) * tmpWeights[ii,])
  }
  out <- xts(port.ret, index(R))
  colnames(out) <- "portfolio.returns"
  if(show.weights){
    return(cbind(out, tmpWeights))
  } else {
    return(out)
  }
}

#' Calculate returns of a rebalanced portfolio
#' 
#' This function calculates returns of a rebalanced portfolio based on a list
#' of weights at each rebalance period. This function is primarily to work with
#' the dynamic rebalancing strategy. A different function may need to be used
#' with a standard optimize.portfolio.rebalancing object from PortfolioAnalytics.
#' 
#' @param R xts object of asset returns
#' @param weights named list of weights where the names of the list are the
#' rebalancing dates
#' @param show.weights TRUE/FALSE to show the weights at each period
#' @return xts object of portfolio returns
#' @export
portfolioRebalancingReturns <- function(R, weights, show.weights=FALSE){
  # equation for w_i,_(t+1) from
  # http://www.slideshare.net/andsteinconsult/correct-calculation-of-ex-post-
  # contributions-to-return-volatility-and-tracking-error
  
  # weights <- checkData(weights, method = "xts")
  R <- checkData(R, method = "xts")
  if(!inherits(weights, "list")) stop("weights must be a list")
  
  # Loop through the weights of each rebalance date.
  # Subset the returns object based on the rebalance dates of the weights.
  for(i in 1:(length(weights)-1)){
    from <- as.Date(names(weights)[i]) + 1
    if(i == length(weights)){
      to <- last(index(R))
    } else {
      to <- as.Date(names(weights)[i+1])
    }
    # subset R based on the dates as well as the asset names of the weights
    tmpR <- R[paste(from, to, sep="/"), names(weights[[i]])]
    
    if(nrow(tmpR) < 1) stop("No data in the subsetted returns object")
    
    # Pass tmpR and the weights at each rebalance date to portfolio.return
    if (nrow(tmpR) >= 1) {
      resultreturns <- portfolioReturn(R=tmpR, weights=as.numeric(weights[[i]]), show.weights=show.weights)
      if (i == 1) {
        result <- resultreturns
      } else {
        result <- rbind(result, resultreturns)
      }
    }
  }
  return(result)
}


# 
# BAD.rebalance.weights <- function(R, weights){
#   
#   weights <- checkData(weights, method = "xts")
#   R <- checkData(R, method = "xts")
#   # loop through the weights
#   # subset the returns object based on the rebalance dates of the weights
#   # compute the updated weights in between rebalance dates
#   for(i in 1:nrow(weights)){
#     from <- as.Date(index(weights[i])) + 1
#     if(i == nrow(weights)){
#       to <- last(index(R))
#     } else {
#       to <- as.Date(index(weights[i+1]))
#     }
#     # subset the returns object based on the weights
#     tmpR <- R[paste(from, to, sep="/")]
#     tmpWeights <- matrix(0, nrow=nrow(tmpR), ncol=ncol(tmpR))
#     tmpWeights[1,] <- weights[i, ]
#     for(ii in 2:nrow(tmpWeights)){
#       tmpSum <- sum(abs(tmpWeights[(ii-1),]) * (1 + as.numeric(tmpR[(ii-1),])))
#       for(j in 1:ncol(tmpWeights)){
#         tmpW <- tmpWeights[(ii-1), j]
#         rj <- as.numeric(tmpR[(ii-1), j])
#         tmpWeights[ii, j] <- (abs(tmpW) * (1 + rj)) / tmpSum * sign(tmpW)
#       }
#     }
#     if(i == 1){
#       out <- tmpWeights
#     } else {
#       out <- rbind(out, tmpWeights)
#     }
#   } # end outer loop
#   return(reclass(out, R))
# }
# 
