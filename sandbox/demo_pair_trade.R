
# Kindly contributed to quantstrat by Garrett See
# Code borrowed heavily from existing quantstrat demos

# This is a simple pairs trading example intended to illustrate how you can 
# extend existing quantstrat functionality.  It uses addPosLimits to specify 
# levels and position limits, and shows how to pass a custom order sizing 
# function to osFUN 

# Note that it would be easier to build a spread first and treat it as a single 
# instrument instead of dealing with a portfolio of stocks.

# Given 2 stocks, calculate the ratio of their notional values.  If the ratio 
# falls below it's 2 stdev band, then when it crosses back above it, buy stock 1 
# and sell stock 2.  If the ratio rises above it's 2 stdev band, then when it 
# crosses back below it, sell stock 1 and buy stock 2.  If the ratio crosses 
# its moving average, then flatten any open positions.

# The Qty of Stock A that it buys (sells) = MaxPos / lvls
# The Qty of Stock B that is sells (buys) = MaxPos * Ratio / lvls  

require(quantstrat)

suppressWarnings(rm("order_book.pair1",pos=.strategy))
suppressWarnings(rm("account.pairs", "portfolio.pair1", pos=.blotter))
suppressWarnings(rm("initDate", "endDate", "startDate", "initEq", "SD", "N", 
                    "portfolio1.st", "account.st", "pairStrat", "out1"))

##### Load Data #####

symb1 <- 'BAC'
symb2 <- 'TCB'

startDate <- '2009-01-02'
endDate <- Sys.Date()

getSymbols(c(symb1, symb2), from=startDate, to=endDate, adjust=TRUE)

# The following function is used to make sure the timestamps of all symbols are 
# the same deletes rows where one of the stocks is missing data
alignSymbols <- function(symbols, env=.GlobalEnv) {
  # This is a simplified version of qmao::alignSymbols()
  if (length(symbols) < 2) 
    stop("Must provide at least 2 symbols")
  if (any(!is.character(symbols))) 
    stop("Symbols must be vector of character strings.")
  ff <- get(symbols[1],env=env)
  for (sym in symbols[-1]) {
    tmp.sym <- get(sym,env=env)
    ff <- merge(ff, tmp.sym, all=FALSE)
  }
  for (sym in symbols) {
    assign(sym,ff[,grep(sym, colnames(ff))], env=env)
  }
  symbols
}

alignSymbols(c(symb1, symb2))

# Define Instruments
currency("USD")
stock(symb1, currency="USD", multiplier=1)
stock(symb2, currency="USD", multiplier=1)

##### Initial Parameters #####

# Initial Dates
initDate <- '2009-01-01'

# Initial Equity
initEq <- 100000

# Parameters for bollinger bands
SD <- 2
N <- 60

window <- 120

# Position limits
MaxPos <- 1500  #max position in symb1; 
# max position in symb2 will be max * ratio, i.e. no hard position limit in symb2
lvls <- 1  #how many times to fade; Each order's qty will = MaxPos/lvls

##### Initialize Portfolio, Account, and Orders ####
portfolio1.st <- 'pair1'
account.st <- 'pairs'

initPortf(name=portfolio1.st, symbols=c(symb1, symb2), initDate=initDate)
initAcct(name=account.st, portfolios=portfolio1.st, initDate=initDate, initEq=initEq)
initOrders(portfolio=portfolio1.st, initDate=initDate)

# osFUN will need to know which symbol is leg 1 and which is leg 2 as well as 
# what the values are for MaxPos and lvls.  So, create a slot in portfolio to 
# hold this info.
pair <- c(1, 2, MaxPos, lvls)
names(pair) <- c(symb1, symb2, "MaxPos", "lvls")
.blotter[[paste('portfolio', portfolio1.st, sep='.')]]$pair <- pair

# Create initial position limits and levels by symbol
addPosLimit(portfolio=portfolio1.st, timestamp=initDate, symbol=symb1, 
            maxpos=MaxPos, longlevels=lvls, minpos=-MaxPos, shortlevels=lvls)

addPosLimit(portfolio=portfolio1.st, timestamp=initDate, symbol=symb2, 
            maxpos=MaxPos, longlevels=lvls, minpos=-MaxPos, shortlevels=lvls)

# Create a strategy object 
pairStrat <- strategy('pairStrat')

##### Spread Function #####
# Indicator function
# This is a function to calculate the spread
calcSpread <- function(x) { 
  # returns the ratio of notional close prices for 2 symbols
  x1 <- Cl(get(x[1]))
  x2 <- Cl(get(x[2]))
  mult1 <- getInstrument(x[1])$multiplier
  mult2 <- getInstrument(x[2])$multiplier
  # rat <- (mult1 * Cl(x1)) / (mult2 * Cl(x2))
  # colnames(rat) <- 'Ratio'
  out <- estimateParametersHistorically(P1=x1, P2=x2, window=window, method="ols")$spread
  colnames(out) <- "Spread"
  out
}

calcHR <- function(x) { 
  # returns the ratio for 2 symbols
  x1 <- Cl(get(x[1]))
  x2 <- Cl(get(x[2]))
  mult1 <- getInstrument(x[1])$multiplier
  mult2 <- getInstrument(x[2])$multiplier
  # rat <- (mult1 * Cl(x1)) / (mult2 * Cl(x2))
  # colnames(rat) <- 'Ratio'
  out <- estimateParametersHistorically(P1=x1, P2=x2, window=window, method="ols")$hedge_ratio
  colnames(out) <- "hedge_ratio"
  out
}

# Indicator used for determining entry/exits
Spread <- calcSpread(c(symb1[1], symb2[1])) 

# Calculate the hedge ratio
HedgeRatio <- calcHR(c(symb1[1], symb2[1]))

# Store hedge ratio in portfolio so that it's available for order sizing 
# function. In this example, the hedge ratio happens to be the same as the 
# Ratio indicator.
.blotter[[paste('portfolio',portfolio1.st, sep='.')]]$HedgeRatio <- HedgeRatio

# Make a function to get the most recent HedgeRatio
getHedgeRatio <- function(portfolio, timestamp) {
  portf <- getPortfolio(portfolio)
  timestamp <- format(timestamp,"%Y-%m-%d %H:%M:%S")
  # above line ensures you don't get last value of next day if using intraday 
  # data and timestamp=midnight
  toDate <- paste("::", timestamp, sep="")
  Ratio <- last(portf$HedgeRatio[toDate])
  as.numeric(Ratio)
}

# Create an indicator - BBands on the Ratio
# The name arguments must be the name of the indicator function
pairStrat <- add.indicator(strategy=pairStrat, name="calcSpread", 
                           arguments=list(x=c(symb1, symb2)))

pairStrat <- add.indicator(strategy=pairStrat, name = "BBands", 
                           arguments=list(HLC=quote(Spread), sd=SD, n=N, 
                                          maType='SMA'))

# tmp <- applyIndicators(strategy=pairStrat, mktdata=get(symb1[1])) #for debugging
# head(tmp, 121)

# Create signals - buy when crossing lower band from below, sell when crossing 
# upper band from above, flatten when crossing mavg from above or from below

# Signal for when the spread crosses below the upper band
pairStrat <- add.signal(strategy=pairStrat, name="sigCrossover", 
                        arguments=list(columns=c("Spread","up"), 
                                       relationship="lt"),
                        label="cross.up")

# Signal for when the spread crosses above the lower band
pairStrat <- add.signal(strategy=pairStrat, name="sigCrossover", 
                        arguments=list(columns=c("Spread","dn"), 
                                       relationship="gt"), 
                        label="cross.dn")

# Signal for when the spread crosses below the moving average
pairStrat <- add.signal(strategy=pairStrat, name="sigCrossover", 
                        arguments=list(columns=c("Spread","mavg"), 
                                       relationship="lt"), 
                        label="cross.mid.fa")

# Signal for when the spread crosses above the moving average
pairStrat <- add.signal(strategy=pairStrat, name="sigCrossover", 
                        arguments=list(columns=c("Spread","mavg"), 
                                       relationship="gt"), 
                        label="cross.mid.fb")

# Order Sizing Function
# Check to see which stock it is. If it is the second stock, reverse orderqty
# and orderside
osSpreadMaxPos <- function (data, timestamp, orderqty, ordertype, orderside, 
                            portfolio, symbol, ruletype, ..., orderprice) {
  portf <- getPortfolio(portfolio)
  #check to make sure pair slot has the things needed for this function
  if (!any(portf$pair == 1) && !(any(portf$pair == 2))) 
    stop('pair must contain both values 1 and 2')
  if (!any(names(portf$pair) == "MaxPos") || !any(names(portf$pair) == "lvls")) 
    stop('pair must contain MaxPos and lvls')  
  
  if (portf$pair[symbol] == 1) legside <- "long"
  if (portf$pair[symbol] == 2) legside <- "short"  
  MaxPos <- portf$pair["MaxPos"]
  lvls <- portf$pair["lvls"]
  ratio <- getHedgeRatio(portfolio, timestamp)
  pos <- getPosQty(portfolio, symbol, timestamp)       
  PosLimit <- getPosLimit(portfolio, symbol, timestamp) 
  qty <- orderqty
  if (legside == "short") {#symbol is 2nd leg
    ## Comment out next line to use equal ordersizes for each stock. 
    addPosLimit(portfolio=portfolio, timestamp=timestamp, symbol=symbol, 
                maxpos=round(MaxPos*ratio,0), longlevels=lvls, 
                minpos=round(-MaxPos*ratio,0), shortlevels=lvls)
    ## 
    qty <- -orderqty #switch orderqty for Stock B
  }
  
  if (qty > 0) orderside = 'long'
  if (qty < 0) orderside = 'short'
  
  orderqty <- osMaxPos(data=data,timestamp=timestamp, orderqty=qty,
                       ordertype=ordertype, orderside=orderside,
                       portfolio=portfolio, symbol=symbol, ruletype=ruletype, 
                       ...)
  
  #Add the order here instead of in the ruleSignal function
  if (!is.null(orderqty) & !orderqty == 0 & !is.null(orderprice)) {
    addOrder(portfolio=portfolio, symbol=symbol, 
             timestamp=timestamp, qty=orderqty, price=as.numeric(orderprice), 
             ordertype=ordertype, side=orderside, replace=FALSE,
             status="open", ...=...)
  }
  return(0) #so that ruleSignal function doesn't also try to place an order
}

# Create entry and exit rules for longs  and for shorts. Both symbols will get 
# the same buy/sell signals, but osMaxPos will reverse those for the second 
# symbol.
# orderqty's are bigger than PosLimits allow. osMaxPos will adjust the orderqty 
# down to 1/3 the max allowed. (1/3 is because we are using 3 levels in 
# PosLimit)
pairStrat <- add.rule(strategy=pairStrat, name='ruleSignal', 
                      arguments=list(sigcol="cross.dn", sigval=TRUE, 
                                     orderqty=1e6, ordertype='market', 
                                     orderside=NULL, osFUN='osSpreadMaxPos'), 
                      type='enter')

pairStrat <- add.rule(strategy=pairStrat, name='ruleSignal', 
                      arguments=list(sigcol="cross.up", sigval=TRUE, 
                                     orderqty=-1e6, ordertype='market', 
                                     orderside=NULL, osFUN='osSpreadMaxPos'), 
                      type='enter')

pairStrat <- add.rule(strategy=pairStrat, name='ruleSignal', 
                      arguments=list(sigcol="cross.mid.fb", sigval=TRUE, 
                                     orderqty='all', ordertype='market', 
                                     orderside=NULL), 
                      type='exit')

pairStrat <- add.rule(strategy=pairStrat, name='ruleSignal', 
                      arguments=list(sigcol="cross.mid.fa", sigval=TRUE, 
                                     orderqty='all', ordertype='market', 
                                     orderside=NULL), 
                      type='exit')

# for debugging
# tmp_sig <- applySignals(strategy=pairStrat, mktdata=applyIndicators(strategy=pairStrat, mktdata=get(symb1)))
# head(tmp_sig)

out1 <- applyStrategy(strategy=pairStrat, portfolios=portfolio1.st, prefer='Adjusted')

updatePortf(Portfolio=portfolio1.st, Dates=paste("::", as.Date(Sys.time()), sep=''))
updateAcct(account.st, Dates=paste(startDate, endDate, sep="::")) 
updateEndEq(account.st, Dates=paste(startDate, endDate, sep="::"))
getEndEq(account.st, Sys.time())

chart.Posn(Portfolio=portfolio1.st, Symbol=symb1)
chart.Posn(Portfolio=portfolio1.st, Symbol=symb2)
chartSeries(Spread, TA="addBBands(n=N, sd=SD)")

ret1 <- PortfReturns(account.st)
ret1$total <- rowSums(ret1)

book <- getOrderBook(portfolio1.st)
txns_symb1 <- getTxns(portfolio1.st, symb1)
txns_symb2 <- getTxns(portfolio1.st, symb2)
txns <- list(symb1=txns_symb1, symb2=txns_symb2)
stats <- tradeStats(portfolio1.st)

charts.PerformanceSummary(ret1$total, geometric=FALSE, wealth.index=TRUE)


###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Package Copyright (c) 2009-2012
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: pair_trade.R 1515 2013-10-03 08:11:20Z erastus $
#
###############################################################################
