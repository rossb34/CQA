library(CQA)

# Sector Performance
data <- read.csv(file="data/ishares.csv", header=TRUE, as.is=TRUE)
data <- data[-1,]

data(BetaValues2013)
symbols <- BetaValues2013[, "Symbol"]

sector.etfs <- data[, "Symbol"]

# Load the ETFs to use as a proxy for sector performance
getSymbols(sector.etfs)
getSymbols("SPY")
SPY.ret <- ROC(Ad(SPY), 1, "discrete")

# Calculate the returns of the sector ETFs
sector.returns <- calculateReturns(sector.etfs)
sector.returns <- cbind(sector.returns, SPY.ret)

# The first taper announcement came on 5/22/2013
# Evaluate performance for the following month
# tmp.ret <- sector.returns['2013-05-21/2013-06-21']
tmp.ret <- sector.returns['2013']
# head(tmp.ret)



# Calendar Returns for each sector ETF
# monthly.ret <- list()
# tmp_names <- colnames(sector.returns)
# for(i in 1:ncol(sector.returns)){
#   monthly.ret[[tmp_names[i]]] <- table.CalendarReturns(sector.returns[, i], 2, TRUE, TRUE)
# }

# Cumulative return over this period
ret.cum <- apply(tmp.ret, 2, function(x) prod(1 + x) - 1)
sort(ret.cum)
ret.cum[ret.cum >= 0]

which(ret.cum <= 0.02)

barcol <- rep("gray", length(ret.cum))

date.range <- "2013"

barplot(ret.cum, las=3, cex.names=0.6, 
        main=paste("Sector Performance\n", date.range),
        col=barcol)

worst_names <- names(sort(ret.cum[ret.cum <= -0.1]))

# RTL - iShares Retail Real Estate Capped
# FNIO - iShares Indl/Office Rel Est Capped
# FTY - iShares Real Estate 50
# ICF - iShares Cohen & Steers REIT
# IYR - iShares US Real Estate
# REZ - iShares Residential Rel Est Capped
# ITB - iShares US Home Construction
# REM - iShares Mortgage Real Estate Capped

# These sectors fell much more than SPY for the following month after the first
# taper announcement
charts.PerformanceSummary(tmp.ret[, c(worst_names, "SPY.Adjusted")], 
                          legend.loc=NULL,
                          main="Sector Performance\n2013-05-21 to 2013-06-21")

# Top 10 holdings of each ETF
RTL.h <- c("SPG", "GGP", "KIM", "MAC", "COLE", "O", "DDR", "REG", "FRT", "TCO")
FNIO.h <- c("PLD", "BXP", "SLG", "LRY", "DRE", "ARE", "KRC", "BMR", "HIW", "DEI")
FTY.h <- c("SPG", "AMT", "PSA", "PLD", "VTR", "HCP", "EQR", "HCN", "WY", "AVB")
ICF.h <- c("SPG", "PSA", "PLD", "VTR", "HCP", "HCN", "EQR", "BXP", "AVB", "VNO")
IYR.h <- c("SPG", "AMT", "PSA", "PLD", "VTR", "HCP", "HCN", "WY", "EQR", "BXP")
REZ.h <- c("PSA", "VTR", "HCP", "EQR", "HCN", "UDR", "AVB", "ESS", "CPT", "MAA")
ITB.h <- c("PHM", "LEN", "DHI", "TOL", "NVR", "HD", "RYL", "LOW", "MTH", "KBH")
REM.h <- c("NLY", "AGNC", "STWD", "TWO", "MFA", "CIM", "NRF", "IVR", "HTS", "NRZ")

# Holdings of the ETFs
all.h <- unique(c(RTL.h, FNIO.h, FTY.h, ICF.h, IYR.h, REZ.h, ITB.h, REM.h))
taper_names <- all.h[all.h %in% symbols]
taper_names


