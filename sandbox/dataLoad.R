library(quantMod)

BetaValues2013 <- read.csv("/Users/rossbennett/devel/R/CQA/rawData/BetaValues2013.csv", header=TRUE, as.is=TRUE)
colnames(BetaValues2013) <- c("Symbol", "Name", "Type", "Value", "Market", "Industry", "Unit")
save(BetaValues2013, file="data/BetaValues2013.Rda")
# load("data/BetaValues2013.Rda")

mergedData <- read.csv("/Users/rossbennett/devel/R/CQA/rawData/mergedData.txt", header=TRUE,sep="\t",as.is=TRUE,fill=TRUE,quote="")
save(mergedData, file="data/mergedData.Rda")
# load("data/mergedData.Rda")

cqaMatchedData <- read.csv("/Users/rossbennett/devel/R/CQA/rawData/cqaMatchedData.txt", header=TRUE,sep="\t",as.is=TRUE,fill=TRUE,quote="")
save(cqaMatchedData, file="data/cqaMatchedData.Rda")
# load("data/cqaMatchedData.Rda")
