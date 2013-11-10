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

# Use data to 2006 to calculate :
# annualized geometric return
# annualized standard deviation
# annualized sharpe ratio
# historical ES

tmpReturns <- returns["/2006"]
# Select N assets
out <- foreach(i=1:ncol(tmpReturns)) %do% {
  x <- na.omit(tmpReturns[, i])
  n <- length(x)
  # annualized standard deviation
  tmpSD <- sqrt(252) * sd(x)
  # annualized geometric mean return
  tmpMean <- prod(1 + x)^(252/n) - 1
  # expected Shortfall
  tmpES <- as.numeric(ES(x, p=0.95, method="historical"))
  # annualized sharpe ratio
  tmpSR <- tmpMean / tmpSD
  # structure the object that is returned
  matrix(c(tmpSD, tmpMean, tmpES, tmpSR), nrow=1, dimnames=list(colnames(x), c("sd", "mean", "ES", "SR")))
}
# length(out)
historicalMetrics <- do.call(rbind, out)

minES <- historicalMetrics[historicalMetrics[, "ES"] >= quantile(historicalMetrics[, "ES"], probs=0.75),]
maxSR <- historicalMetrics[historicalMetrics[, "SR"] >= quantile(historicalMetrics[, "SR"], probs=0.75),]


# Number of stocks to use for optimization
N <- 10

# Get the top N SR stocks in the minES object
topN <- names(minES[, "SR"][order(minES[,"SR"], decreasing=TRUE)][1:(N/2)])
bottomN <- names(minES[, "SR"][order(minES[,"SR"], decreasing=FALSE)][1:(N/2)])

# Select N stocks ranked by Sharpe Ratio that are in the bottom quartile of ES
R <- returns[, c(topN, bottomN)]
funds <- colnames(R)

# match the betas based on the selected funds
betas <- data[match(funds, data[, "Symbol"]), "Value"]
names(betas) <- funds

# Define the training period
trainingPeriods <- 1260

# Define the rebalance freqency
rebalanceFrequency <- "months"

# Define the trailing periods
# 1260 is approximately 5 years
trailingPeriods <- 1260

# Create a portfolio with constraints and objectives
# Set up the portfolio with basic constraints
# some of this should be taken out of the loop
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="weight_sum", 
                             min_sum=-0.01, max=0.01)
init.portf <- add.constraint(portfolio=init.portf, type="box", 
                             min=-0.05, max=0.05)
init.portf <- add.constraint(portfolio=init.portf, type="leverage_exposure", 
                             leverage=2)
init.portf <- add.constraint(portfolio=init.portf, type="factor_exposure", 
                             B=betas, lower=-0.25, upper=0.25)

# Add objectives
# Maybe use random portfolios or DEoptim for a forecast mean function
# init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
# ES is kind of slow, potentially rewrite for a custom function passed in here
# or use StdDev because it is significantly faster
init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean", 
                            multiplier=-1)

rp <- random_portfolios(portfolio=init.portf, permutations=2500, 
                        rp_method="sample")

# Run the optimization with DEoptim
optDE <- optimize.portfolio(R=R, portfolio=init.portf, 
                            optimize_method="DEoptim", 
                            search_size=2500, trace=TRUE, 
                            rp=rp)
optDE
# beta
t(betas) %*% optDE$weights 
chart.Weights(optDE)
chart.RiskReward(optDE, risk.col="StdDev")

# Run the optimization with random portfolios
# optRP <- optimize.portfolio(R=R, portfolio=init.portf, 
#                             optimize_method="random", 
#                             search_size=2500, trace=TRUE, 
#                             rp=rp)


