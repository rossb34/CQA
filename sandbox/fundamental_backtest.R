# set up a custom optimization for backtesting with stock filtering

# Just use a small number of stocks initially for testing

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

fundamentalList <- list()

fundamentalFilter <- jason #linda

it <- 1
for(i in 1:ncol(fundamentalFilter)){
  ##### Parameters for dynamic rebalancing optimization #####
  # asset returns object to use
  # returns
  
  # Define the training period
  # start approximatley 10 years later from the start date
  trainingPeriods <- 2500
  
  # Define the rebalance freqency
  rebalanceFrequency <- "months"
  
  # Number of stocks to use for optimization
  N <- p[i]
  
  # Define the trailing periods
  # 1260 is approximately 5 years
  trailingPeriods <- 90
  
  # arguments to functionalize this
  # returns
  # trainingPeriods
  # trailingPeriods
  # rebalanceFrequency
  # N
  # FUN
  # ... to FUN
  # betas
  
  
  ##### min ES filtered max mean ROI #####
  # Calculate the rebalance dates for the endpoints index
  ep.i <- endpoints(returns, on=rebalanceFrequency)[which(endpoints(returns, on=rebalanceFrequency) >= trainingPeriods)]
  
  opt.mean.ROI <- foreach(ep=iter(ep.i), .errorhandling='pass', .packages='PortfolioAnalytics') %dopar% {
    # subset the returns data to the periods I want
    # R[(ifelse(ep - trailingPeriods >= 1, ep - trailingPeriods, 1)):ep, ] from PortfolioAnalytics
    tmpR <- returns[(ifelse(ep - trailingPeriods >= 1, ep - trailingPeriods, 1)):ep, ]
    
    # Filter based on expected shortfall
    # tmpES <- ES(R=tmpR, method="historical", p=0.95, invert=FALSE)
    
    # Use the N assets with the lowest ES
    R <- tmpR[, fundamentalFilter[,i][ fundamentalFilter[,i] != ""]]
    funds <- colnames(R)
    
    # target return
    # target <- mean(apply(X=R, MARGIN=2, FUN=median))
    # target <- 0.0005
    
    # match the betas based on the selected funds
    betas <- data[match(funds, data[, "Symbol"]), "Value"]
    names(betas) <- funds
    
    # Create a portfolio with constraints and objectives
    # Set up the portfolio with basic constraints
    # some of this should be taken out of the loop
    init.portf <- portfolio.spec(assets=funds)
    init.portf <- add.constraint(portfolio=init.portf, type="weight_sum", min_sum=-0.01, max=0.01)
    init.portf <- add.constraint(portfolio=init.portf, type="box", min=-0.05, max=0.05)
    # init.portf <- add.constraint(portfolio=init.portf, type="return", return_target=0.00005)
    init.portf <- add.constraint(portfolio=init.portf, type="leverage_exposure", leverage=2)
    init.portf <- add.constraint(portfolio=init.portf, type="factor_exposure", 
                                 B=betas, lower=-0.5, upper=0.5)
    
    # Add objectives
    # Maybe use random portfolios or DEoptim for a forecast mean function
    init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
    # meanES.portf <- add.objective(portfolio=meanES.portf, type="risk", name="ES")
    
    # Run the optimization
    optimize.portfolio(R=R, portfolio=init.portf, optimize_method="ROI", trace=TRUE)
  }
  names(opt.mean.ROI) <- index(returns[ep.i])
  
  # save(optList, file="optList.rda")
  
  # Each rebalance period may have different assets so we need to consider this
  # when calculating the returns
  opt.mean.ROI.weights <- lapply(opt.mean.ROI, function(x) x$weights)
  # weights
  
  # Portfolio returns through time with rebalancing
  opt.mean.ret <- portfolioRebalancingReturns(R=returns, weights=opt.mean.ROI.weights)
  fundamentalList[[i]] <- opt.mean.ret
  charts.PerformanceSummary(opt.mean.ret, main=paste("Max Mean ROI:\n", p[i], "assets\n", periods[j], "trailing periods"))
  it <- it + 1
}


