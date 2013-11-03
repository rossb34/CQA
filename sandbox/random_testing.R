library(CQA)
library(PortfolioAnalytics)

data(BetaValues2013)

# symbols of our universe of stocks
symbols <- BetaValues2013[, "Security.Symbol"]

# directory where my data lies
dir <- "~/Documents/tmp/data/"

opt.list <- list()

# Test N different portfolios for mean-ES
N <- 10

for(i in 1:N){
  opt.list[[i]] <- list()
  
  # Step 1:
  # Select some symbols at random and load them
  tmp.symbols <- randomFilter(symbols, 50, 100)
  loadStocks(stocks=tmp.symbols, data.dir=dir, format="%Y-%m-%d", sep=",", header=TRUE)
  opt.list[[i]]$stocks <- tmp.symbols
  
  # Step 2:
  # Calculate the returns of the selected symbols
  ret <- calculateReturns(symbols=tmp.symbols)
  # remove rows with NA
  ret <- na.omit(ret)
  
  # Step 3:
  # Get the beta values
  beta.dat <- BetaValues2013[which(symbols %in% tmp.symbols), c("Security.Symbol", "Value")]
  tmp.betas <- beta.dat[, "Value"]
  names(tmp.betas) <- beta.dat[, "Security.Symbol"]
  
  # Step 5:
  # Align the betas based on tmp.symbols
  betas <- tmp.betas[tmp.symbols]
  opt.list[[i]]$betas <- betas
  
  # Make sure the returns and betas are aligned with tmp.symbols
  stopifnot(all.equal(tmp.symbols, colnames(ret)))
  stopifnot(all.equal(tmp.symbols, names(betas)))
  
  # Step 6:
  # Create a portfolio with constraints and objectives
  
  # Set up the portfolio with basic constraints
  init.portf <- portfolio.spec(assets=tmp.symbols)
  init.portf <- add.constraint(portfolio=init.portf, type="dollar_neutral")
  init.portf <- add.constraint(portfolio=init.portf, type="box", min=-0.05, max=0.05)
  init.portf <- add.constraint(portfolio=init.portf, type="factor_exposure", 
                               B=betas, lower=-0.5, upper=0.5)
  
  # Add objectives
  meanES.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
  meanES.portf <- add.objective(portfolio=meanES.portf, type="risk", name="ES")
  
  # Step 7:
  # Run the optimization
  meanES.opt <- optimize.portfolio(R=ret, portfolio=meanES.portf, 
                                   optimize_method="ROI", trace=TRUE)
  print(paste("Completed optimization", i, "at", Sys.time(), sep=" "))
  
  # Step 8:
  # Add the optimization to the list
  opt.list[[i]]$optimization <- meanES.opt
}

opt.list

