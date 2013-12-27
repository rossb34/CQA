olsHedgeRatio <- function(P1, P2, no_int=TRUE){
  if(no_int){
    m <- lm(P1 ~ P2 - 1)
    beta <- as.numeric(coef(m))
  } else {
    m <- lm(P1 ~ P2)
    beta <- as.numeric(coef(m)[2])
  }
  return(beta)
}

tlsHedgeRatio <- function(P1, P2){
  m <- princomp(~ P1 + P2)
  beta <- as.numeric(m$loadings[1,1] / m$loadings[2,1])
  return(beta)
}

#' Compute the hedge ratio
#' 
#' Computes the hedge ratio for a spread of two time series
#' 
#' @param P1 Time series of close prices for symbol 1
#' @param P2 Time series of close prices for symbol 2
#' @param method method to estimate the hedge ratio, 'tls' or 'ols': 
#' tls = total least squares using principal components; 
#' ols = ordinary least squares using linear regression
#' @param no_int omit intercept from the linear model when method='ols'
#' @author Ross Bennett
#' @export
hedgeRatio <- function(P1, P2, method=c("tls", "ols"), no_int=TRUE){
  method <- match.arg(method)
  switch(method,
         tls = {
           out <- tlsHedgeRatio(P1=P1, P2=P2)
         },
         ols = {
           out <- olsHedgeRatio(P1=P1, P2=P2, no_int=no_int)
         }
  )# end switch
  return(out)
}
