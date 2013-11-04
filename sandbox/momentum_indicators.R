
data(stocks)

symbols <- c("AAPL", "MSFT", "KO", "PEP")

# xts object of combined prices
pricesAll <- na.omit(combinePrices(symbols))
head(pricesAll)

# Only use the most recent 4 years for example purposes
tmpPrices <- pricesAll["2010/2013", ]
head(tmpPrices)

# N <- nrow(tmpPrices)
# tmpPrices[N] / as.numeric(tmpPrices[(N-252),]) - 1
ROC(x=tail(x=tmpPrices, n=253), n=252, type="discrete", na.pad=FALSE)
tmpROC <- ROC(x=tail(tmpPrices, 253), n=1, type="discrete")
apply(tail(tmpROC, 252), MARGIN=2, FUN=sd)


# Momentum and relative strength metrics
# 252 period return
# 375 period return
# 500 period return

# strength
# 252 period return / sd
# 375 period return / sd
# 500 period return / sd

# price relative to n-period high and low
# price relative to n-period EMA

simpleMomentum(prices=tmpPrices, n=252)
ROC(x=tail(x=tmpPrices, n=253), n=252, type="discrete", na.pad=FALSE)

simpleStrength(tmpPrices)

emaMomentum(tmpPrices)

tmpROC <- ROC(x=tmpPrices, n=252, type="discrete")
emaROC <- as.xts(apply(X=tmpROC, MARGIN=2, FUN=EMA, n=50), order.by=index(tmpROC))
tail(emaROC)

n1ROC <- ROC(x=tmpPrices, n=1, type="discrete")
tmpSD <- as.xts(apply(X=n1ROC, MARGIN=2, FUN=runSD, n=252), order.by=index(n1ROC))
emaSD <- as.xts(apply(X=tmpSD, MARGIN=2, FUN=EMA, n=50), order.by=index(tmpSD))

plot(tmpPrices[, 1])
plot(tmpROC[, 1])
lines(emaROC[, 1], col="red")

emaStrength(tmpPrices)

priceToHILO(tmpPrices)

priceToEMA(tmpPrices)
