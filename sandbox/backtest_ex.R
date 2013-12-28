# set up a custom optimization for backtesting with stock filtering

# Just use a small number of stocks initially for testing

library(CQA)
library(foreach)
library(iterators)

# register a parallel backend
# The multicore package, and therefore registerDoMC, should not be used in a
# GUI environment, because multiple processes then share the same GUI. Only use
# when running from the command line.
# require(doMC)
# registerDoMC(3)


##### Parameters for dynamic rebalancing optimization #####

# Define the training period
trainingPeriods <- 1512

# Define the rebalance freqency
rebalanceFrequency <- "months"

# Number of stocks to use for optimization
N <- 140

# Define the trailing periods
# 1260 is approximately 5 years. This means I always use the most recent 5 
# years of data for calculating the momentum indicators
trailingPeriods <- 1260

btPriceToEMA <- backtestMomentumLS(prices=prices, 
                                 trainingPeriods=trainingPeriods, 
                                 trailingPeriods=trailingPeriods, 
                                 rebalanceFrequency=rebalanceFrequency, 
                                 N=N, 
                                 FUN=priceToEMA, 
                                 n=126,
                                 multiplier=-1)

retPriceToEMA <- portfolioRebalancingReturns(R=returns, weights=btPriceToEMA)

# save(btPriceToEMA, file=paste("btPriceToEMA.", Sys.Date(), ".rda", sep=""))

print(paste("Completed riceToEMA backtest at", Sys.time()))


btPriceToHILO <- backtestMomentumLS(prices=prices, 
                                  trainingPeriods=trainingPeriods, 
                                  trailingPeriods=trailingPeriods, 
                                  rebalanceFrequency=rebalanceFrequency, 
                                  N=N, 
                                  FUN=priceToHILO, 
                                  n=126,
                                  HI.only=TRUE,
                                  multiplier=-1)

retPriceToHILO <- portfolioRebalancingReturns(R=returns, weights=btPriceToHILO)

# save(btPriceToHILO, file=paste("btPriceToHILO", Sys.Date(), ".rda", sep=""))

print(paste("Completed riceToHILO backtest at", Sys.time()))

btemaMom <- backtestMomentumLS(prices=prices, 
                             trainingPeriods=trainingPeriods, 
                             trailingPeriods=trailingPeriods, 
                             rebalanceFrequency=rebalanceFrequency, 
                             N=N, 
                             FUN=emaMomentum, 
                             nROC=126,
                             nEMA=50,
                             multiplier=-1)

retemaMom <- portfolioRebalancingReturns(R=returns, weights=btemaMom)

# save(btemaMom, file=paste("btemaMom", Sys.Date(), ".rda", sep=""))

print(paste("Completed emaMomentum backtest at", Sys.time()))

btsimpleStrength <- backtestMomentumLS(prices=prices, 
                                     trainingPeriods=trainingPeriods, 
                                     trailingPeriods=trailingPeriods, 
                                     rebalanceFrequency=rebalanceFrequency, 
                                     N=N, 
                                     FUN=simpleStrength, 
                                     n=126,
                                     multiplier=-1)

retsimpleStrength <- portfolioRebalancingReturns(R=returns, weights=btsimpleStrength)

# save(btsimpleStrength, file=paste("btsimpleStrength", Sys.Date(), ".rda", sep=""))

print(paste("Completed simpleStrength backtest at", Sys.time()))

print("All optimizations complete")

ret <- cbind(retPriceToEMA, retPriceToHILO, retemaMom, retsimpleStrength)
colnames(ret) <- c("PriceToEMA", "PriceToHILO", "emaMom", "SimpleStrength")

charts.PerformanceSummary(ret, main="Technical Indicators Backtest")

