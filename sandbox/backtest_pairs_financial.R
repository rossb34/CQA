
library(quantstrat)
library(CQA)

data(BetaValues2013)
data(fundamental10252013)

# Beta values
betas <- BetaValues2013
rm(BetaValues2013)

# Filter to the Sector = Financial
financial_sector <- fundamental_10252013[fundamental_10252013[, "Sector"] == "Financial",]

# Filter to Industry = Money Center Banks
# These are the large banks like Bank of America, Citi, etc.
mcb <- financial_sector[financial_sector[,"Industry"] == "Money Center Banks",]

# Filter to assets that are the investable universe
mcb <- mcb[mcb[,"Ticker"] %in% betas[,"Symbol"],]

# symbols <- as.character(mcb[,"Ticker"])

symbols <- c("BAC", "C", "JPM", "KEY", "PNC", "STI", "TCB", "WFC")
startDate <- '2009-01-02'

# Load the symbols
getSymbols(symbols, from=startDate, adjust=TRUE)

# Align symbols
# TODO

# Test the combinations of pairs for for mean reverting spreads
out <- list()
for(i in 1:length(symbols)){
  j <- i + 1
  while(j <= length(symbols)){
    if(j != i){
      print(paste("Testing", symbols[i], symbols[j]))
      sym1 <- get(symbols[i])
      sym1 <- Cl(sym1)
      
      sym2 <- get(symbols[j])
      sym2 <- Cl(sym2)
      
      tmp <- estimateParametersHistorically(sym1, sym2, 120, "ols")
      # Test if the spread is stationary
      if(isStationaryDF(tmp$spread)){
        print(paste(symbols[i], symbols[j], "spread is stationary"))
        out[[paste0(symbols[i], symbols[j])]] <- list()
        out[[paste0(symbols[i], symbols[j])]]$pairs <- c(symbols[i], symbols[j])
        out[[paste0(symbols[i], symbols[j])]]$data <- tmp
      }
      j <- j + 1
    } else {
      j <- j + 1
    }
  }
}

# Plot the spreads of mean reverting pairs
for(i in 1:length(out)){
  plot(out[[i]]$data$spread, main=paste(names(out)[i]))
}

pairs <- lapply(out, function(x) x$pairs)
pairs_names <- names(pairs)

backtest_out <- list()

for(i in 1:length(pairs)){
  backtest_out[[i]] <- list()
  
  suppressWarnings(rm("order_book.pair1", pos=.strategy))
  suppressWarnings(rm("account.pairs", "portfolio.pair1", pos=.blotter))
  suppressWarnings(rm("initDate", "endDate", "startDate", "initEq", "SD", "N", 
                      "portfolio1.st", "account.st", "pairStrat", "out1"))
  
  ##### Load Data #####
  
  tmp_name <- pairs_names[i]
  tmp_pairs <- pairs[[i]]
  symb1 <- tmp_pairs[1]
  symb2 <- tmp_pairs[2]
  
  print(paste("Testing", symb1, "and", symb2))
  
  # symb1 <- 'BAC'
  # symb2 <- 'TCB'
  
  startDate <- '2009-01-02'
  endDate <- Sys.Date()
  
  # getSymbols(c(symb1, symb2), from=startDate, to=endDate, adjust=TRUE)
  
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
  
  #chart.Posn(Portfolio=portfolio1.st, Symbol=symb1)
  #chart.Posn(Portfolio=portfolio1.st, Symbol=symb2)
  #chartSeries(Spread, TA="addBBands(n=N, sd=SD)")
  
  ret1 <- PortfReturns(account.st)
  ret1$total <- rowSums(ret1)
  
  book <- getOrderBook(portfolio1.st)
  txns_symb1 <- getTxns(portfolio1.st, symb1)
  txns_symb2 <- getTxns(portfolio1.st, symb2)
  txns <- list(symb1=txns_symb1, symb2=txns_symb2)
  #stats <- tradeStats(portfolio1.st)
  
  backtest_out[[i]]$ret <- ret1
  backtest_out[[i]]$book <- book
  backtest_out[[i]]$txns <- txns
  
  charts.PerformanceSummary(ret1$total, geometric=FALSE, wealth.index=TRUE, main=paste("Performance", tmp_name))
}

names(backtest_out) <- pairs_names

table.ret <- lapply(backtest_out, function(x) table.AnnualizedReturns(x$ret$total))
table.ret

spreadSignals <- function(spread, hedge_ratio, n=60, sd=2){
  tmpTA <- BBands(spread, n=n, maType="EMA", sd=sd)
  tmp <- cbind(spread, hedge_ratio, tmpTA)
  sig <- vector("numeric", nrow(tmp))
  for(i in 1:length(sig)){
    if(!is.na(tmp$dn[i])){
      if(tmp$spread[i] > tmp$up[i]){
        sig[i] <- 1
      }
      if(tmp$spread[i] < tmp$dn[i]){
        sig[i] <- -1
      }
    }
  }
  cbind(tmp, sig)
}

plotSpread <- function(object, main="Spread"){
  plot(object$spread, main=main)
  lines(object$dn, col="red")
  lines(object$up, col="red")
  lines(object$mavg)
}

spreadOrders <- function(P1, P2, hedge_ratio, market_value=500000, order_side=c("long", "short")){
  order_side <- match.arg(order_side)
  tmp_sym1_shares <- floor(min(market_value / P1, market_value / (hedge_ratio * P2)))
  tmp_sym2_shares <- floor(tmp_sym1_shares * hedge_ratio)
  
  if(order_side == "long"){
    # we are buying symbol 1 and shorting symbol 2
    sym1_shares <- tmp_sym1_shares
    sym2_shares <- - tmp_sym2_shares
  }
  if(order_side == "short"){
    # we are selling symbol 1 and buying symbol 2
    sym1_shares <- - tmp_sym1_shares
    sym2_shares <- tmp_sym2_shares
  }
  # market value
  sym1_mv <- sym1_shares * P1
  sym2_mv <- sym2_shares * P2
  
  # put in list
  sym1 <- list()
  sym1$shares <- sym1_shares
  sym1$market_value <- sym1_mv
  
  sym2 <- list()
  sym2$shares <- sym2_shares
  sym2$market_value <- sym2_mv
  list(symbol1=sym1, symbol2=sym2)
}

# JPM-STI
# PNC-TCB
# PNC-WFC
# STI-TCB

# chartSeries(out$JPMSTI$data$spread)
# addBBands(n=60, maType="EMA", sd=2)

head(out$JPMSTI$data)

# Sell signal: spread is above upper BBand and crosses below.
# sell x shares of symbol 1 and buy hedge_ratio * x_shares of symbol 2

# Buy signal: spread is below lower BBand and crosses above
# buy x shares of symbol 1 and sell hedge_ratio * x_shares of symbol 2

JPMSTI <- spreadSignals(out$JPMSTI$data$spread, out$JPMSTI$data$hedge_ratio)
plotSpread(tail(JPMSTI, 10), main="JPMSTI")
# buy the JPM-STI spread
# buy JPM, sell STI
as.numeric(last(JPMSTI[, "hedge_ratio"]))

JPM.Price <- 55.73
STI.Price <- 38.04

spreadOrders(JPM.Price, STI.Price, as.numeric(last(JPMSTI[, "hedge_ratio"])), 350000)

PNCTCB <- spreadSignals(out$PNCTCB$data$spread, out$PNCTCB$data$hedge_ratio)
plotSpread(tail(PNCTCB, 10), main="PNCTCB")
# sell the PNC-TCB spread
# sell PNC, buy TCB
as.numeric(last(PNCTCB[, "hedge_ratio"]))

PNC.Price <- 81.23
TCB.Price <- 16.45

spreadOrders(PNC.Price, TCB.Price, as.numeric(last(PNCTCB[, "hedge_ratio"])), 350000, "short")

PNCWFC <- spreadSignals(out$PNCWFC$data$spread, out$PNCWFC$data$hedge_ratio)
plotSpread(tail(PNCWFC, 10), main="PNCWFC")
# sell the PNC-WFC spread
# sell PNC, buy WFC
as.numeric(last(PNCWFC[, "hedge_ratio"]))

WFC.Price <- 45.96

spreadOrders(PNC.Price, WFC.Price, as.numeric(last(PNCWFC[, "hedge_ratio"])), 250000, "short")


STITCB <- spreadSignals(out$STITCB$data$spread, out$STITCB$data$hedge_ratio)
plotSpread(tail(STITCB, 10), main="STITCB")
