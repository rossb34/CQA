# Buy Ideas

# 1, 2, and 3-month performance of worst performing sectors from previous year

library(CQA)

# Sector Performance
data <- read.csv(file="data/ishares.csv", header=TRUE, as.is=TRUE)
data <- data[-1,]

data(BetaValues2013)
symbols <- BetaValues2013[, "Symbol"]
sector.etfs <- data[, "Symbol"]

# Load the ETFs to use as a proxy for sector performance
getSymbols(sector.etfs)
getSymbols("SPY")
SPY.ret <- ROC(Ad(SPY), 1, "discrete")

# Calculate the returns of the sector ETFs
sector.returns <- calculateReturns(sector.etfs)
sector.returns <- sector.returns[-1,]

year.vec <- c("2007", "2008", "2009", "2010", "2011", "2012", "2013")
out <- list()
for(year in year.vec){
  out[[year]] <- apply(X=sector.returns[year], MARGIN=2, FUN=function(x) {prod(1 + na.omit(x)) - 1})
}
annual.ret <- do.call(rbind, out)
annual.ret

foo <- function(R, annual_returns, N=5, method=c("momentum", "reversal")){
  method <- match.arg(method)
  if(method == "momentum"){
    mult <- -1
  } else {
    mult <- 1
  }
  date_idx <- index(R)
  # Weights will go on the years
  years <- date_idx[endpoints(date_idx, on="years")]
  months <- date_idx[endpoints(date_idx, on="months")]
  quarters <- date_idx[endpoints(date_idx, on="quarters")]
  
  weight_mat <- xts(matrix(0, nrow=length(quarters), ncol=ncol(annual_returns)), quarters)
  rand_weight_mat <- xts(matrix(0, nrow=length(quarters), ncol=ncol(annual_returns)), quarters)
  
  for(i in 1:nrow(annual_returns)){
    tmp_weight <- rep(0, ncol(annual_returns))
    rand_tmp_weight <- rep(0, ncol(annual_returns))
    
    tmp_weight[order(mult * annual_returns[i,])[1:N]] <- 1 / N
    rand_tmp_weight[sample.int(n=ncol(annual_returns), size=N)] <- 1 / N
    
    weight_mat[years[i],] <- tmp_weight
    rand_weight_mat[years[i],] <- rand_tmp_weight
  }
  ret_out <- Return.rebalancing(R, weight_mat)
  ret_rand <- Return.rebalancing(R, rand_weight_mat)
  ret_out[is.na(ret_out)] <- 0
  ret_rand[is.na(ret_rand)] <- 0
  out <- cbind(ret_out, ret_rand)
  colnames(out) <- c("ret", "random")
  return(out)
}

# Filter out the assets without returns for the full period
idx <- which(apply(sector.returns, 2, function(x) !any(is.na(x))))

for(N in 1:10){
  ret <- foo(sector.returns[,idx], annual.ret[,idx], N, "reversal")
  charts.PerformanceSummary(cbind(ret, SPY.ret), wealth.index=TRUE, 
                            main=paste("Performance: N =", N))
  print(paste("Performance: N =", N))
  print(table.AnnualizedReturns(cbind(ret, SPY.ret)))
}

