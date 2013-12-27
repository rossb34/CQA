# Buy Ideas

# 1, 2, and 3-month performance of worst performing stocks from previous year

library(CQA)

##### Data Prep #####
data(BetaValues2013)
data(startDates)

# Remove all rows that have an NA for beta values
data <- BetaValues2013[!is.na(BetaValues2013[, "Value"]), ]
rm(BetaValues2013)

# list of all symbols in the data
symbols <- data[, "Symbol"]

# This returns a character vector of symbols with start dates earlier than
# the specified date
tmpSymbols <- names(startDates[startDates <= as.Date("2007-01-01")])

dir <- "~/Documents/tmp/data/"
loadStocks(stocks=tmpSymbols, data.dir=dir, format="%Y-%m-%d", sep=",", header=TRUE)

# Get the adjusted close prices
prices <- na.omit(combinePrices(tmpSymbols))
colnames(prices) <- gsub(pattern=".Adjusted", replacement="", x=colnames(prices))
head(prices[,1:5])
tail(prices[,1:5])

# Calculate the one period returns
returns <- na.omit(ROC(prices, n=1, "discrete"))
# returns <- calculateReturns(tmpSymbols)
head(returns[,1:5])
tail(returns[,1:5])

##### End Data Prep #####

# Load SPY as a proxy for the market
getSymbols("SPY")
SPY.ret <- ROC(Ad(SPY), 1, "discrete")

# year.vec <- c("2007", "2008", "2009", "2010", "2011", "2012", "2013")
# out <- list()
# for(year in year.vec){
#   out[[year]] <- apply(X=returns[year], MARGIN=2, FUN=function(x) {prod(1 + na.omit(x)) - 1})
# }
# annual.ret <- do.call(rbind, out)
# annual.ret

annual.ret <- ROC(prices[endpoints(prices, "years")], 1, "discrete", FALSE)
annual.ret[,which.max(annual.ret[1,])]

tmp_ret <- returns[,1:5]
tmp_ann.ret <- annual.ret[, 1:5]

head(tmp_ret)
head(tmp_ann.ret)

foo <- function(R, annual_ret, N=1, method=c("momentum", "reversal")){
  stopifnot(all.equal(colnames(R), colnames(annual_ret)))
  
  method <- match.arg(method)
  if(method == "momentum"){
    #print("momentum")
    mult <- -1
  } else {
    #print("reversal")
    mult <- 1
  }
  
  date_idx <- index(R)
  quarters <- date_idx[endpoints(date_idx, on="quarters")]
  years <- date_idx[endpoints(date_idx, on="years")]
  
  weight_mat <- xts(matrix(0, nrow=length(quarters), ncol=ncol(R)), quarters)
  colnames(weight_mat) <- colnames(R)
  
  # This just calculates an equal weight portfolio
  # I could use portfolio optimization to compute the weights
  # idx <- order(mult * annual_ret[i,])[1:N]
  # tmp_R <- R[, idx]
  #opt_weights <- optimize.portfolio()
  #tmp_weight[idx] <- opt_weights
  
  for(i in 1:nrow(annual_ret)){
    tmp_weight <- rep(0, ncol(R))
    tmp_weight[order(mult * annual_ret[i,])[1:N]] <- 1 / N
    weight_mat[index(annual_ret)[i],] <- tmp_weight
  }
  #print(weight_mat[endpoints(weight_mat, "years")])
  ret <- Return.rebalancing(R, weight_mat)
  ret[is.na(ret)] <- 0
  return(ret)
}

tmp_ann.ret
rebal.ret <- foo(R=tmp_ret, annual_ret=tmp_ann.ret, N=1, method="momentum")
charts.PerformanceSummary(cbind(rebal.ret, SPY.ret))
rebal.ret <- foo(R=tmp_ret, annual_ret=tmp_ann.ret, N=1, method="reversal")
charts.PerformanceSummary(cbind(rebal.ret, SPY.ret))

##
ret <- foo(R=returns, annual_ret=annual.ret, N=100, method="momentum")
charts.PerformanceSummary(cbind(ret, SPY.ret))


for(N in 1:50){
  ret <- foo(returns, annual.ret, N, "reversal")
  charts.PerformanceSummary(cbind(ret, SPY.ret), main=paste("Performance: N =", N))
  print(paste("Performance: N =", N))
  tmp_perf <- table.AnnualizedReturns(ret)
  colnames(tmp_perf) <- paste("N =", N)
  if(N == 1){
    out_perf <- tmp_perf
  } else {
    out_perf <- cbind(out_perf, tmp_perf)
  }
  print(table.AnnualizedReturns(cbind(ret, SPY.ret)))
}

for(N in 1:50){
  ret <- foo(returns, annual.ret, N, "momentum")
  charts.PerformanceSummary(cbind(ret, SPY.ret), main=paste("Performance: N =", N))
  print(paste("Performance: N =", N))
  tmp_perf <- table.AnnualizedReturns(ret)
  colnames(tmp_perf) <- paste("N =", N)
  if(N == 1){
    out_perf_mom <- tmp_perf
  } else {
    out_perf_mom <- cbind(out_perf_mom, tmp_perf)
  }
  print(table.AnnualizedReturns(cbind(ret, SPY.ret)))
}

rev_sr <- as.numeric(out_perf[3,])
mom_sr <- as.numeric(out_perf_mom[3,])
tmp <- rbind(rev_sr, mom_sr)
barplot(tmp, beside=TRUE, names.arg=1:50, cex.names=0.8,
        legend.text=c("Reversal", "Momentum"), 
        main="Sharpe Ratio of Q1 Strategy")
barplot(rev_sr, main="Reversal Strategy Sharpe Ratio")
barplot(mom_sr, main="Momentum Strategy Sharpe Ratio")

rev_4 <- foo(returns, annual.ret, 4, "reversal")
rev_5 <- foo(returns, annual.ret, 5, "reversal")
rev_6 <- foo(returns, annual.ret, 6, "reversal")
mom_4 <- foo(returns, annual.ret, 4, "momentum")
mom_5 <- foo(returns, annual.ret, 5, "momentum")
mom_6 <- foo(returns, annual.ret, 6, "momentum")

ret_all <- cbind(rev_4, rev_5, rev_6, mom_4, mom_5, mom_6, SPY.ret)
colnames(ret_all) <- c("rev_4", "rev_5", "rev_6", "mom_4", "mom_5", "mom_6", "SPY")
charts.PerformanceSummary(ret_all, main="Q1 Reversal & Momentum Performance")
table.AnnualizedReturns(ret_all)

# foo <- function(R, annual_returns, N=5, method=c("momentum", "reversal")){
#   method <- match.arg(method)
#   if(method == "momentum"){
#     mult <- -1
#   } else {
#     mult <- 1
#   }
#   date_idx <- index(R)
#   # Weights will go on the years
#   years <- date_idx[endpoints(date_idx, on="years")]
#   months <- date_idx[endpoints(date_idx, on="months")]
#   quarters <- date_idx[endpoints(date_idx, on="quarters")]
#   
#   weight_mat <- xts(matrix(0, nrow=length(quarters), ncol=ncol(annual_returns)), quarters)
#   colnames(weight_mat) <- colnames(R)
#   rand_weight_mat <- xts(matrix(0, nrow=length(quarters), ncol=ncol(annual_returns)), quarters)
#   colnames(rand_weight_mat) <- colnames(R)
#   
#   for(i in 1:nrow(annual_returns)){
#     tmp_weight <- rep(0, ncol(annual_returns))
#     rand_tmp_weight <- rep(0, ncol(annual_returns))
#     
#     tmp_weight[order(mult * annual_returns[i,])[1:N]] <- 1 / N
#     rand_tmp_weight[sample.int(n=ncol(annual_returns), size=N)] <- 1 / N
#     
#     weight_mat[years[i],] <- tmp_weight
#     rand_weight_mat[years[i],] <- rand_tmp_weight
#   }
#   ret_out <- Return.rebalancing(R, weight_mat)
#   ret_rand <- Return.rebalancing(R, rand_weight_mat)
#   ret_out[is.na(ret_out)] <- 0
#   ret_rand[is.na(ret_rand)] <- 0
#   out <- cbind(ret_out, ret_rand)
#   colnames(out) <- c("ret", "random")
#   return(list(ret=out, weights=weight_mat))
# }
