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

symbols <- data[, "Symbol"][1:150]

# We should have a table with symbol data that keeps track of, for example,
# the first available date of history

dir <- "~/Documents/tmp/data/"
loadStocks(stocks=symbols, data.dir=dir, format="%Y-%m-%d", sep=",", header=TRUE)

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

funds <- colnames(retAll)

init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="active")
init.portf <- add.constraint(portfolio=init.portf, type="box", min=-0.05, max=0.05)

# Add objectives
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")

# Run the optimization
opt <- optimize.portfolio(R=retAll, portfolio=init.portf, 
                          optimize_method="ROI", trace=TRUE)
opt
sum(abs(opt$weights))
chart.Weights(opt)

lev.portf <- add.constraint(portfolio=init.portf, type="leverage_exposure", leverage=2)

# Run the optimization
lev.opt <- optimize.portfolio(R=retAll, portfolio=lev.portf, 
                          optimize_method="ROI", trace=TRUE)
lev.opt
sum(abs(lev.opt$weights))
chart.Weights(lev.opt)