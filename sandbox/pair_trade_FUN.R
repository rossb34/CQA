
calcSpread <- function(x, window=20) { 
  # returns the ratio of notional close prices for 2 symbols
  x1 <- Cl(get(x[1]))
  x2 <- Cl(get(x[2]))
  mult1 <- getInstrument(x[1])$multiplier
  mult2 <- getInstrument(x[2])$multiplier
  out <- estimateParametersHistorically(P1=x1, P2=x2, window=window, method="ols")$spread
  colnames(out) <- "Spread"
  out
}

calcHR <- function(x, window=20) { 
  # returns the ratio for 2 symbols
  x1 <- Cl(get(x[1]))
  x2 <- Cl(get(x[2]))
  mult1 <- getInstrument(x[1])$multiplier
  mult2 <- getInstrument(x[2])$multiplier
  out <- estimateParametersHistorically(P1=x1, P2=x2, window=window, method="ols")$hedge_ratio
  colnames(out) <- "hedge_ratio"
  out
}

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

backtestPairTrade <- function(symbols, start_date, end_date, SD=2, N=20, window=60){
  
  suppressWarnings(rm("order_book.pair1",pos=.strategy))
  suppressWarnings(rm("account.pairs", "portfolio.pair1", pos=.blotter))
  # suppressWarnings(rm("initDate", "endDate", "startDate", "initEq", "SD", "N", 
  #                     "portfolio1.st", "account.st", "pairStrat", "out1"))
  
  ##### Load Data #####
  symb1 <- symbols[1]
  symb2 <- symbols[2]
  
  ##### Initial Parameters #####
  # Initial Dates
  init_date <- as.Date(start_date) - 1
  
  # Initial Equity
  init_eq <- 100000
  
  # Position limits
  MaxPos <- 1500  #max position in symb1; 
  # max position in symb2 will be max * ratio, i.e. no hard position limit in symb2
  lvls <- 1  #how many times to fade; Each order's qty will = MaxPos/lvls
  
  ##### Initialize Portfolio, Account, and Orders ####
  portfolio1.st <- 'pair1'
  account.st <- 'pairs'
  
  initPortf(name=portfolio1.st, symbols=c(symb1, symb2), initDate=init_date)
  initAcct(name=account.st, portfolios=portfolio1.st, initDate=init_date, initEq=init_eq)
  initOrders(portfolio=portfolio1.st, initDate=init_date)
  
  # osFUN will need to know which symbol is leg 1 and which is leg 2 as well as 
  # what the values are for MaxPos and lvls.  So, create a slot in portfolio to 
  # hold this info.
  pair <- c(1, 2, MaxPos, lvls)
  names(pair) <- c(symb1, symb2, "MaxPos", "lvls")
  .blotter[[paste('portfolio', portfolio1.st, sep='.')]]$pair <- pair
  
  # Create initial position limits and levels by symbol
  addPosLimit(portfolio=portfolio1.st, timestamp=init_date, symbol=symb1, 
              maxpos=MaxPos, longlevels=lvls, minpos=-MaxPos, shortlevels=lvls)
  
  addPosLimit(portfolio=portfolio1.st, timestamp=init_date, symbol=symb2, 
              maxpos=MaxPos, longlevels=lvls, minpos=-MaxPos, shortlevels=lvls)
  
  # Create a strategy object 
  pairStrat <- strategy('pairStrat')
  
  # Indicator used for determining entry/exits
  Spread <- calcSpread(c(symb1[1], symb2[1]), window=window) 
  
  # Calculate the hedge ratio
  HedgeRatio <- calcHR(c(symb1[1], symb2[1]), window=window)
  
  # Store hedge ratio in portfolio so that it's available for order sizing 
  # function. In this example, the hedge ratio happens to be the same as the 
  # Ratio indicator.
  .blotter[[paste('portfolio',portfolio1.st, sep='.')]]$HedgeRatio <- HedgeRatio
  
  # Create an indicator - BBands on the spread
  # The name arguments must be the name of the indicator function
  pairStrat <- add.indicator(strategy=pairStrat, name="calcSpread", 
                             arguments=list(x=c(symb1, symb2), window=window))
  
  pairStrat <- add.indicator(strategy=pairStrat, name = "BBands", 
                             arguments=list(HLC=quote(Spread), sd=SD, n=N, 
                                            maType='SMA'))
  
  # tmp <- applyIndicators(strategy=pairStrat, mktdata=get(symb1[1])) #for debugging
  # print(head(tmp, 120))
  
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
  
  out1 <- applyStrategy(strategy=pairStrat, portfolios=portfolio1.st)
  
  updatePortf(Portfolio=portfolio1.st, Dates=paste("::", as.Date(Sys.time()), sep=''))
  updateAcct(account.st, Dates=paste(start_date, end_date, sep="::")) 
  updateEndEq(account.st, Dates=paste(start_date, end_date, sep="::"))
  cat("Ending Equity:", getEndEq(account.st, Sys.time()), "\n")
  
  ret <- PortfReturns(account.st)
  ret$total <- rowSums(ret)
  
  book <- getOrderBook(portfolio1.st)
  txns_symb1 <- getTxns(portfolio1.st, symb1)
  txns_symb2 <- getTxns(portfolio1.st, symb2)
  txns <- list(symb1=txns_symb1, symb2=txns_symb2)
  stats <- tradeStats(portfolio1.st)
  out <- list(ret=ret, book=book, transactions=txns, stats=stats)
  return(out)
}
