# # Example for subsetting returns of unequal histories
# loadStocks(stocks=c("COH", "LGF"), data.dir=dir, format="%Y-%m-%d", sep=",", header=TRUE)
# tmp <- calculateReturns(c("COH", "LGF"))
# start(COH)
# start(LGF)
# tmp.subset <- tmp["2000-10"]
# # remove all rows that contain NA
# head(na.omit(tmp.subset))