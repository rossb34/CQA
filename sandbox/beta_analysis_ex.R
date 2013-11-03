# eda.R

# exploratory data analysis on the universe of our returns

library(CQA)
data(BetaValues2013)

# Remove all rows that have an NA for beta values
data <- BetaValues2013[!is.na(BetaValues2013[, "Value"]), ]

# Get a vector of the beta values
betas <- data[, "Value"]

##### Overall Beta Analysis #####

summary(betas)

# Plot the distribution of
hist(betas)
boxplot(betas, main="Boxplot of betas")

##### Beta Analysis by Industry #####

par(mar=c(9, 4, 4, 2)+0.1)
# mar=c(bottom, left, top, right)
boxplot(Value~Industry, data=BetaValues2013, 
        cex.axis=0.8, las=3,
        ylab="Beta", main="Beta values by Industry")
par(mar=c(5, 4, 4, 2)+0.1)

# Statistical summary of the betas by industry
aggregate(Value~Industry, data=data, FUN=summary)

# Number of stocks in each industry
aggregate(Value~Industry, data=data, FUN=length)


