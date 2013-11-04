# set up a custom optimization for backtesting with stock filtering

# Just use a small number of stocks initially for testing

library(CQA)
library(foreach)
library(iterators)

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
# retAll <- na.omit(calculateReturns(tmpSymbols))
# head(retAll)

prices <- na.omit(combinePrices(tmpSymbols))
# prices <- prices["2010/2013"]

# returns <- na.omit(ROC(prices, 1, "discrete"))


##### Parameters for dynamic rebalancing optimization #####

# Define the training period
trainingPeriods <- 1512

# Define the rebalance freqency
rebalanceFrequency <- "months"

# Number of stocks to use for optimization
N <- 80

# Define the trailing periods
# 1260 is approximately 5 years. This means I always use the most recent 5 
# years of data for calculating the momentum indicators
trailingPeriods <- 1260

btPriceToEMA <- backtestMomentum(prices=prices, 
                                 trainingPeriods=trainingPeriods, 
                                 trailingPeriods=trailingPeriods, 
                                 rebalanceFrequency=rebalanceFrequency, 
                                 N=N, 
                                 FUN=priceToEMA, 
                                 n=252)

save(btPriceToEMA, file=paste("btPriceToEMA.", Sys.Date(), ".rda", sep=""))

print(paste("Completed riceToEMA backtest at", Sys.time()))

btPriceToHILO <- backtestMomentum(prices=prices, 
                                  trainingPeriods=trainingPeriods, 
                                  trailingPeriods=trailingPeriods, 
                                  rebalanceFrequency=rebalanceFrequency, 
                                  N=N, 
                                  FUN=priceToHILO, 
                                  n=252,
                                  HI.only=TRUE)

save(btPriceToHILO, file=paste("btPriceToHILO", Sys.Date(), ".rda", sep=""))

print(paste("Completed riceToHILO backtest at", Sys.time()))

btemaMom <- backtestMomentum(prices=prices, 
                             trainingPeriods=trainingPeriods, 
                             trailingPeriods=trailingPeriods, 
                             rebalanceFrequency=rebalanceFrequency, 
                             N=N, 
                             FUN=emaMomentum, 
                             nROC=252,
                             nEMA=50)

save(btemaMom, file=paste("btemaMom", Sys.Date(), ".rda", sep=""))

print(paste("Completed emaMomentum backtest at", Sys.time()))

btsimpleStrength <- backtestMomentum(prices=prices, 
                                     trainingPeriods=trainingPeriods, 
                                     trailingPeriods=trailingPeriods, 
                                     rebalanceFrequency=rebalanceFrequency, 
                                     N=N, 
                                     FUN=simpleStrength, 
                                     n=252)

save(btsimpleStrength, file=paste("btsimpleStrength", Sys.Date(), ".rda", sep=""))

print(paste("Completed simpleStrength backtest at", Sys.time()))

print("All optimizations complete")

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