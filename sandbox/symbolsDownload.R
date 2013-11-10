library(quantmod)
library(CQA)

# load the data for the beta values
data(BetaValues2013)

# This is a character vector of the ticker symbols from the BetaValues2013 file
tickers <- BetaValues2013[, "Security.Symbol"]

# This will get data from as far back as yahoo has
from <- "1900-01-01"
to <- Sys.Date()

# Set the directory to write the data to
dir <- "~/Documents/tmp/data/"

nTickers <- length(tickers)

# Loop through tickers and load each symbol
for(i in 1:nTickers){
  x <- try(getSymbols(Symbols=tickers[i], from=from, to=to, env=NULL), silent=TRUE)
  if(inherits(x, "try-error")){
    message(paste("Could not download", tickers[i]))
  } else {
    cat("Successfully downloaded", tickers[i], "\n")
    cat(i, "out of", nTickers, "\n")
    write.zoo(x, file=paste(dir, tickers[i], ".csv", sep=""), sep=",", col.names=TRUE)
    rm(x)
  }
  # Pause for 5 seconds
  Sys.sleep(5)
}


