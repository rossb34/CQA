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
# require(doMC)
# registerDoMC(3)

data(BetaValues2013)

# Remove all rows that have an NA for beta values
data <- BetaValues2013[!is.na(BetaValues2013[, "Value"]), ]
rm(BetaValues2013)

symbols <- data[, "Symbol"][1:10]

# We should have a table with symbol data that keeps track of, for example,
# the first available date of history

dir <- "~/Documents/tmp/data/"
loadStocks(stocks=symbols, data.dir=dir, format="%Y-%m-%d", sep=",", header=TRUE)

head(na.omit(combinePrices(symbols)))

# Get the start date of each symbol
startDates <- rep(0, length(symbols))
for(i in 1:length(symbols)){
  startDates[i] <- start(get(symbols[i]))
}
startDates <- as.Date(startDates)
names(startDates) <- symbols

# This returns a character vector of symbols with start dates earlier than
# the specified date
tmpSymbols <- names(startDates[startDates <= as.Date("2000-01-01")])

retAll <- na.omit(calculateReturns(tmpSymbols))
head(retAll)

tmp <- retAll["2010/2013", 1:15]
head(tmp)


# define a training period
trainingPeriods <- 150

# define the endpoints
rebalanceInterval <- "quarters"
# endpoints(tmp, on=rebalanceInterval)
ep.i <- endpoints(tmp, on=rebalanceInterval)[which(endpoints(tmp, on=rebalanceInterval) >= trainingPeriods)]

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

optList <- list()
trailingPeriods <- 100
# R[(ifelse(ep - trailingPeriods >= 1, ep - trailingPeriods, 1)):ep, ]
for(i in 1:length(ep.i)){
  # subset the returns data to the periods I want
  tmpR <- tmp[(ifelse(ep.i[i] - trailingPeriods >= 1, ep.i[i] - trailingPeriods, 1)):ep.i[i], ]
#   print(start(tmpR))
#   print(end(tmpR))
#   print("*****")
  tmpES <- apply(X=tmpR, MARGIN=2, FUN=ES, method="historical", p=0.95, invert=FALSE)
#   print(head(tmpR[, order(tmpES)[1:4]]))
#   print(tail(tmpR[, order(tmpES)[1:4]]))
#   print("*****")
  # Use the returns with the lowest ES
  R <- tmpR[, order(tmpES)[1:4]]
  funds <- colnames(R)
  
  # match the betas based on the selected funds
  betas <- data[match(funds, data[, "Symbol"]), "Value"]
  names(betas) <- funds
  # Create a portfolio with constraints and objectives
  
  # Set up the portfolio with basic constraints
  # some of this should be taken out of the loop
  init.portf <- portfolio.spec(assets=funds)
  init.portf <- add.constraint(portfolio=init.portf, type="long_only")
  init.portf <- add.constraint(portfolio=init.portf, type="box", min=0.05, max=0.65)
  #init.portf <- add.constraint(portfolio=init.portf, type="factor_exposure", 
  #                             B=betas, lower=-0.5, upper=0.5)
  
  # Add objectives
  init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
  # meanES.portf <- add.objective(portfolio=meanES.portf, type="risk", name="ES")
  
  # Run the optimization
  opt <- optimize.portfolio(R=R, portfolio=init.portf, 
                                   optimize_method="ROI", trace=TRUE)
  # print(paste("Completed optimization", i, "at", Sys.time(), sep=" "))
  
  # Add the optimization to the list
  optList[[i]] <- opt
}
names(optList) <- index(tmp[ep.i])

# Each rebalance period may have different assets so we need to consider this
# when calculating the returns
weights <- lapply(optList, function(x) x$weights)
weights

# Portfolio returns through time with rebalancing
ret <- portfolioRebalancingReturns(R=tmp, weights=weights)
charts.PerformanceSummary(ret)
