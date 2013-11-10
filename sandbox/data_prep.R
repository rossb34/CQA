# Data preparation

library(CQA)

jason <- read.csv("sandbox/jason.csv", header=TRUE, as.is=TRUE)
linda <- read.csv("sandbox/linda.csv", header=TRUE, as.is=TRUE)

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

# Get the adjusted close prices
prices <- na.omit(combinePrices(tmpSymbols))
colnames(prices) <- gsub(pattern=".Adjusted", replacement="", x=colnames(prices))
head(prices[,1:5])
tail(prices[,1:5])
                         
# Calculate the one period returns
returns <- na.omit(ROC(prices, n=1, "discrete"))
head(returns[,1:5])
tail(returns[,1:5])
