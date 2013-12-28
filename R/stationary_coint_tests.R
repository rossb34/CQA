
#' Stationary test
#' 
#' Test if a price series is stationary using the standard Dickey-Fuller Test.
#' Wrapper around tseries::adf.test to return TRUE if the spread is likely
#' stationary, FALSE otherwise.
#' 
#' H0: x has a unit root
#' If the spread has a unit root, it is not mean reverting
#' 
#' @param x spread
#' @param conf_level confidence level to reject H0
#' @author Ross Bennett
#' @export
isStationaryDF <- function(x, conf_level=0.05){
  # Dickey-Fuller test for a unit root
  # H0: x has a unit root
  # If the spread has a unit root, it is not mean reverting
  # Setting alternative="stationary" chooses the appropriate test.
  # Setting k=0 forces a basic (not augmented) test.
  x <- na.omit(x)
  ht <- tseries::adf.test(x=x, alternative="stationary", k=0)
  if (ht$p.value < conf_level) {
    # cat("Reject H0\n")
    # cat("The spread is likely mean-reverting.\n")
    out <- TRUE
  } else {
    # cat("Do not reject H0\n")
    # cat("The spread is likely not mean-reverting.\n")
    out <- FALSE
  }
  return(out)
}

#' Stationary test
#' 
#' Test if a price series is stationary using the Augmented Dickey-Fuller Test.
#' Wrapper around urca::ur.df to return TRUE if the spread is likely  
#' stationary, FALSE otherwise
#' 
#' H0: x has a unit root
#' If the spread has a unit root, it is not mean reverting
#' 
#' @param x spread
#' @param conf_level confidence level to reject H0
#' @param type Test type, either "none", "drift" or "trend"
#' @param lags Number of lags for endogenous variable to be included.
#' @param selectlags Lag selection can be achieved according to the Akaike 
#' "AIC" or the Bayes "BIC" information criteria. The maximum number of lags 
#' considered is set by lags. The default is to use a "fixed" lag length set 
#' by lags.
#' @author Ross Bennett
#' @export
isStationaryURDF <- function(x, 
                             conf_level=c("10pct", "5pct", "1pct"),
                             type = c("none", "drift", "trend"), 
                             lags=1, 
                             selectlags = c("Fixed", "AIC", "BIC")){
  # The null hypothesis is that a unit root is present
  # Match the args
  conf_level <- match.arg(conf_level)
  type <- match.arg(type)
  selectlags <- match.arg(selectlags)
  
  x <- na.omit(x)
  
  # Run the test
  ur_ht <- urca::ur.df(y=x, type=type, lags=lags, selectlags=selectlags)
  test_stat <- ur_ht@teststat
  cval <- ur_ht@cval[,conf_level]
  
  if(test_stat < cval){
    # The test statistic is more extreme than the critical value
    # cat("Reject H0\n")
    # cat("There is likely not a unit root\n")
    # cat("The spread is likely mean-reverting.\n")
    out <- TRUE
  } else {
    # cat("Do not reject H0\n")
    # cat("There is likely not a unit root\n")
    # cat("The spread is likely not mean-reverting.\n")
    out <- FALSE
  }
  return(out)
}

#' Phillips & Ouliaris Cointegration Test
#' 
#' Test if a price series is cointegrated using the Phillips & Ouliaris Cointegration Test.
#' Wrapper around urca::ca.po to return TRUE if the time series is likely  
#' cointegrated, FALSE otherwise.
#' 
#' H0: The time series are not cointegrated
#' 
#' @param x Time series of prices
#' @param conf_level confidence level to reject H0
#' @param demen either 'none', 'constant', or 'trend'
#' @param type Test type, either 'Pz' or 'Pu'
#' @param lag Lag type, either 'short' or 'long'
#' @author Ross Bennett
#' @export
isCointegratedPO <- function(x, 
                             conf_level=c("10pct", "5pct", "1pct"), 
                             demean=c("none", "constant", "trend"), 
                             lag=c("short", "long"), 
                             type=c("Pz", "Pu")){
  if(ncol(x) > 2) stop("Only test pairs of assets")
  # Match the arguments
  conf_level <- match.arg(conf_level)
  demean <- match.arg(demean)
  lag <- match.arg(lag)
  type <- match.arg(type)
  
  # Run the test
  po <- urca::ca.po(z=x, demean=demean, lag=lag, type=type)
  test_stat <- po@teststat
  cval <- po@cval[,conf_level]
  if(test_stat > cval){
    # The time series are cointegrated
    # cat("Reject H0\n")
    # cat("The time series are cointegrated\n")
    out <- TRUE
  } else {
    # The time series are not cointegrated
    # cat("Do not reject H0\n")
    # cat("The time series are not cointegrated\n")
    out <- FALSE
  }
  return(out)
}

# Johansen Procedure
isCointegratedJO <- function(x, 
                             conf_level=c("10pct", "5pct", "1pct"),
                             type=c("eigen", "trace"), 
                             ecdet=c("none", "const", "trend"), 
                             K=2, 
                             spec=c("longrun", "transitory")){
  # Match the arguments
  conf_level <- match.arg(conf_level)
  type <- match.arg(type)
  ecdet <- match.arg(ecdet)
  spec <- match.arg(spec)
  
  # Run the Johansen procedure test
  jo <- urca::ca.jo(x=x, type=type, ecdet=ecdet, K=K, spec=spec)
  
  # Get the critical values for each hypothesis
  cval <- jo@cval
  rnames <- rownames(cval)
  cval0 <- cval[grep("r = 0", rnames), conf_level]
  cval1 <- cval[grep("r <= 1", rnames), conf_level]
  
  # Get the test statistics for each hypothesis
  test_stat <- jo@teststat
  names(test_stat) <- rnames
  test_stat0 <- test_stat[grep("r = 0", rnames)]
  test_stat1 <- test_stat[grep("r <= 1", rnames)]
  
  # First, test the H0 that r = 0
  if(test_stat0 < cval0){
    # cat("Do not reject H0\n")
    # cat("There are 0 cointegrating vectors\n")
    pass <- FALSE
    out <- FALSE
  } else {
    # cat("Reject H0\n")
    # cat("There are cointegrating vectors\n")
    pass <- TRUE
  }
  
  if(pass){
    if(test_stat1 < cval1){
      # cat("Do not reject H0\n")
      # cat("There is 1 cointegrating vector\n")
      out <- TRUE
    } else {
      # cat("Reject H0\n")
      # cat("There is not 1 cointegrating vector\n")
      out <- FALSE
    }
  }
  return(out)
}
