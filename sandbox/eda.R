
library(CQA)
library(foreach)
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

# Get a vector of the beta values
betas <- data[, "Value"]

##### Overall Beta Analysis #####

summary(betas)

# Plot the distribution of
hist(betas)
boxplot(betas, main="Boxplot of betas")

##### Beta Analysis by Industry #####

par(mar=c(9, 4, 4, 2)+0.1)
# mar=c(bottom, left, top, right)
boxplot(Value~Industry, data=data, 
        cex.axis=0.8, las=3,
        ylab="Beta", main="Beta values by Industry")
par(mar=c(5, 4, 4, 2)+0.1)

# Statistical summary of the betas by industry
aggregate(Value~Industry, data=data, FUN=summary)

# Number of stocks in each industry
aggregate(Value~Industry, data=data, FUN=length)

##### Return Analysis #####

# This tries to load 986 symbols
# Somewhat slow, but once they are loaded in memory it is fine
symbols <- data[, "Symbol"]

# just load the first 50 symbols for testing puposes
# symbols <- data[, "Security.Symbol"][1:50]

dir <- "~/Documents/tmp/data/"
loadStocks(stocks=symbols, data.dir=dir, format="%Y-%m-%d", sep=",", header=TRUE)

# Get the start date of each symbol
startDates <- rep(0, length(symbols))
for(i in 1:length(symbols)){
  startDates[i] <- start(get(symbols[i]))
}
startDates <- as.Date(startDates)
names(startDates) <- symbols

# Should I filter here based on available data?
# This returns a character vector of symbols with start dates earlier than
# the specified date
# names(startDates[startDates <= as.Date("2000-01-01")])

# Any stock that can't have returns calculated will be printed out
# Issues with T and F stocks, this is likely due to how R handles T=TRUE and F=FALSE

# Calculate the daily returns of all the symbols
retAll <- calculateReturns(symbols)
# ncol(retAll)

# Calculate annualized standard deviation, annualized geometric mean returns, 
# and annualized sharpe ratio

# daily = {scale = 252}
# weekly = {scale = 52}
# monthly = {scale = 12}
# quarterly = {scale = 4}
# yearly = {scale = 1}

# annualized standard deviation
# sqrt(scale) * sd(x, na.rm=TRUE)

# annualized geometric return
# R = na.omit(R)
# n = length(R)
# prod(1 + R)^(scale/n) - 1

# hardcoded for daily returns

out <- foreach(i=1:ncol(retAll)) %dopar% {
  x <- na.omit(retAll[, i])
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
length(out)
tmp <- do.call(rbind, out)
head(tmp)

# Now match these metrics to industry category
xxdf <- data.frame(tmp, data[match(rownames(tmp), data[, "Symbol"]), c("Symbol", "Industry")])
stopifnot(all.equal(xxdf[, "Symbol"], rownames(xxdf)))
# rm(tmp)

# boxplots of various risk and return metrics by industry
par(mar=c(9, 4, 4, 2)+0.1)
# mar=c(bottom, left, top, right)
boxplot(SR~Industry, data=xxdf, 
        cex.axis=0.8, las=3,
        ylab="Annualized Sharpe Ratio", main="Annualix=zed Sharpe Ratio by Industry")

boxplot(sd~Industry, data=xxdf, 
        cex.axis=0.8, las=3,
        ylab="Annualized StdDev", main="Annualized StdDev by Industry")

boxplot(mean~Industry, data=xxdf, 
        cex.axis=0.8, las=3,
        ylab="Annualized Geometric Mean", main="Annualized Geometric Mean by Industry")

boxplot(ES~Industry, data=xxdf, 
        cex.axis=0.8, las=3,
        ylab="ES", main="Expected Shortfall by Industry")
par(mar=c(5, 4, 4, 2)+0.1)

##### Industry Specific #####

# the xxdf objec already has various risk return metrics and the industry the
# stock belongs to.

xxdfTech <- xxdf[xxdf[, "Industry"] == "TECHNOLOGY", ]
boxplot(xxdfTech[, "mean"], main="Annualized Geometric Mean")
# one outlier, possible a data error?

xxdfTech[which.max(xxdfTech[, "mean"]),]
# Plot annualized geometric mean return vs annualized standard deviation
plot(x=xxdfTech[-35, "sd"], y=xxdfTech[-35, "mean"], xlab="Annualized StdDev", ylab="Annualized Geometric Mean")
text(x=xxdfTech[-35, "sd"], y=xxdfTech[-35, "mean"], labels=xxdfTech[-35, "Symbol"], pos=4, cex=0.6)

# get the names of the stocks in each industry
industries <- unique(data[, "Industry"])

industryMembership <- list()
for(i in 1:length(industries)){
  industryMembership[[industries[i]]] <- data[data[, "Industry"] == industries[i], "Symbol"]
}
# industryMembership

# # Risk return analysis
# # This function is ridiculously slow
# chart.RiskReturnScatter(retAll[, industryMembership$TECH[1:50]])

#####

# Technical
# RSI for oversold/undersold
# % from 52 week high/low 
# % from moving average (EMA or SMA)
# 6, 9, 12 month returns
# others

# various risk measures
# StdDev
# ES
# VaR
# others

# momentum or mean reversion bias
# Momentum: buy the stocks with the best return and short the stocks with the worst return
# Mean reversion: buy oversold stocks and sell overbought stocks


