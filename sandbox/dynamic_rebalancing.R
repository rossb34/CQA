# set up a custom optimization for backtesting with stock filtering

# Just use a small number of stocks initially for testing

library(CQA)
library(foreach)
library(iterators)
library(PortfolioAnalytics)

# register a parallel backend
# The multicore package, and therefore registerDoMC, should not be used in a
# GUI environment, because multiple processes then share the same GUI. Only use
# when running from the command line.
require(doMC)
registerDoMC(3)

data(BetaValues2013)
data(startDates)

# Remove all rows that have an NA for beta values
data <- BetaValues2013[!is.na(BetaValues2013[, "Value"]), ]
rm(BetaValues2013)

# list of all symbols in the data
symbols <- data[, "Symbol"]

# This returns a character vector of symbols with start dates earlier than
# the specified date
tmpSymbols <- names(startDates[startDates <= as.Date("2000-01-01")])

dir <- "~/Documents/tmp/data/"
loadStocks(stocks=tmpSymbols, data.dir=dir, format="%Y-%m-%d", sep=",", header=TRUE)


# This calculates the single period (i.e. daily) returns
retAll <- na.omit(calculateReturns(tmpSymbols))
# head(retAll)

##### Parameters for dynamic rebalancing optimization #####
# asset returns object to use
returns <- retAll

# Define the training period
trainingPeriods <- 756

# Define the rebalance freqency
rebalanceFrequency <- "months"

# Number of stocks to use for optimization
N <- 100

# Define the trailing periods
# 1260 is approximately 5 years
trailingPeriods <- 1260

# arguments to functionalize this
# returns
# trainingPeriods
# trailingPeriods
# rebalanceFrequency
# N
# FUN
# ... to FUN
# betas


#####
# Calculate the rebalance dates for the endpoints index
ep.i <- endpoints(returns, on=rebalanceFrequency)[which(endpoints(returns, on=rebalanceFrequency) >= trainingPeriods)]

optList <- foreach(ep=iter(ep.i), .errorhandling='pass', .packages='PortfolioAnalytics') %dopar% {
  # subset the returns data to the periods I want
  # R[(ifelse(ep - trailingPeriods >= 1, ep - trailingPeriods, 1)):ep, ] from PortfolioAnalytics
  tmpR <- returns[(ifelse(ep - trailingPeriods >= 1, ep - trailingPeriods, 1)):ep, ]
  #   print(start(tmpR))
  #   print(end(tmpR))
  #   print("*****")
  # This function is slow, possibly rewrite
  tmpES <- apply(X=tmpR, MARGIN=2, FUN=ES, method="historical", p=0.95, invert=FALSE)
  #   print(head(tmpR[, order(tmpES)[1:4]]))
  #   print(tail(tmpR[, order(tmpES)[1:4]]))
  #   print("*****")
  # Use the N assets with the lowest ES
  R <- tmpR[, order(tmpES)[1:N]]
  funds <- colnames(R)
  
  # match the betas based on the selected funds
  betas <- data[match(funds, data[, "Symbol"]), "Value"]
  names(betas) <- funds
  
  # Create a portfolio with constraints and objectives
  # Set up the portfolio with basic constraints
  # some of this should be taken out of the loop
  init.portf <- portfolio.spec(assets=funds)
  init.portf <- add.constraint(portfolio=init.portf, type="weight_sum", min_sum=-0.01, max=0.01)
  init.portf <- add.constraint(portfolio=init.portf, type="box", min=-0.05, max=0.05)
  init.portf <- add.constraint(portfolio=init.portf, type="leverage_factor", leverage=2)
  init.portf <- add.constraint(portfolio=init.portf, type="factor_exposure", 
                               B=betas, lower=-0.5, upper=0.5)
  
  # Add objectives
  # Maybe use random portfolios or DEoptim for a forecast mean function
  init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
  # meanES.portf <- add.objective(portfolio=meanES.portf, type="risk", name="ES")
  
  # Run the optimization
  optList[[i]] <- optimize.portfolio(R=R, portfolio=init.portf, optimize_method="ROI", trace=TRUE)
  print(paste("Completed optimization for rebalance period", i))
}
names(optList) <- index(returns[ep.i])

save(optList, file="optList.rda")

# Each rebalance period may have different assets so we need to consider this
# when calculating the returns
weights <- lapply(optList, function(x) x$weights)
# weights

# Portfolio returns through time with rebalancing
ret <- portfolioRebalancingReturns(R=returns, weights=weights)
# charts.PerformanceSummary(ret)


#


# Loop without a trailing period
# without trailing periods, start is the beginning of the data
# for(i in 1:length(ep.i)){
#   # subset the returns data to the periods I want
#   tmpR <- tmp[1:ep.i[i], ]
#   print(start(tmpR))
#   print(end(tmpR))
#   print("*****")
#   tmpES <- apply(X=tmpR, MARGIN=2, FUN=ES, method="historical", p=0.95, invert=FALSE)
#   print(head(tmpR[, order(tmpES)[1:4]]))
#   print(tail(tmpR[, order(tmpES)[1:4]]))
#   print("*****")
# }