

library(CQA)

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

# This function is really slow!!!
# need to speed up
system.time({
  emaStrength(prices["/2005"], nROC=252, nEMA=50, nSD=252)
})

system.time({
  simpleMomentum(prices["/2005"], n=252)
})

system.time({
  simpleStrength(prices["/2005"], n=252)
})

system.time({
  emaMomentum(prices["/2005"], nROC=252, nEMA=50)
})

# system.time({
#   emaMomentum2(prices["/2005",, nROC=252, nEMA=50)
# })

system.time({
  priceToHILO(prices["/2005"], n=252)
})

system.time({
  priceToEMA(prices["/2005"], n=252)
})

