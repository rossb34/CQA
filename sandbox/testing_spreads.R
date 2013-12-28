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
from <- "2009-12-31"

# Load the symbols
getSymbols(symbols, from=from)

# Align the symbols
# TODO

# Test the combinations of pairs for for mean reverting spreads
out <- list()
for(i in 1:length(symbols)){
  j <- i + 1
  while(j <= length(symbols)){
    if(j != i){
      print(paste("Testing", symbols[i], symbols[j]))
      sym1 <- Cl(get(symbols[i]))
      sym2 <- Cl(get(symbols[j]))
      
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
