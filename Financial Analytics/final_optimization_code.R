library(graphics)
library(quantmod)
library(TTR)
library(ks)
library(scales)
library(forecast)
library(aTSA)
library(ccgarch)
library(fGarch)
library(rugarch)


# Load Stock Data & Calculate Returns#
tickers = c("IBM","JNJ","NKE","PG","WMT")

getSymbols(tickers)

stocks_opt <- cbind(WMT[,4], 
                  JNJ[,4], 
                  PG[,4], 
                  IBM[,4],
                  NKE[,4])

stocks_opt$wmt_r <- periodReturn(stocks_opt$WMT.Close, period = "daily")
stocks_opt$jnj_r <- periodReturn(stocks_opt$JNJ.Close, period = "daily")
stocks_opt$pg_r <- periodReturn(stocks_opt$PG.Close, period = "daily")
stocks_opt$ibm_r <- periodReturn(stocks_opt$IBM.Close, period = "daily")
stocks_opt$nke_r <- periodReturn(stocks_opt$NKE.Close, period = "daily")

stocks_opt <- stocks_opt["2017-02-01/2019-02-08"]

# Calcualte Historical Mean and Variance #
cov_h <- cov(stocks_opt[,6:10])

means_h <- colMeans(stocks_opt[,6:10])

##final models:
# why -1
WMT.Skew.GARCH.t <- garchFit(formula= ~ garch(1,1), data=stocks_opt$wmt_r[-1], cond.dist="sstd", include.mean = FALSE)
JNJ.Skew.GARCH.t <- garchFit(formula= ~ garch(1,1), data=stocks_opt$jnj_r[-1], cond.dist="sstd", include.mean = FALSE)
PG.GARCH.t <- garchFit(formula= ~ garch(1,1), data=stocks_opt$pg_r[-1], cond.dist="std", include.mean = FALSE)
IBM.GARCH.t <- garchFit(formula= ~ garch(1,1), data=stocks_opt$ibm_r[-1], cond.dist="std", include.mean = FALSE)
NKE.Skew.GARCH.t <- garchFit(formula= ~ garch(1,1), data=stocks_opt$nke_r[-1], cond.dist="sstd", include.mean = FALSE)

# 5-day forecast of volatility
wmt_for <- median(head(predict(WMT.Skew.GARCH.t),5)[,3])^2
jnj_for <- median(head(predict(JNJ.Skew.GARCH.t),5)[,3])^2
pg_for <- median(head(predict(PG.GARCH.t),5)[,3])^2
ibm_for <- median(head(predict(IBM.GARCH.t),5)[,3])^2
nke_for <- median(head(predict(NKE.Skew.GARCH.t),5)[,3])^2


# Optimize the Portfolio - GARCH # forecasted varaince and historical covariance
f <- function(x) x[1]*wmt_for*x[1]   + x[1]*cov_h[1,2]*x[2]  + x[1]*cov_h[1,3]*x[3] + x[1]*cov_h[1,4]*x[4] + x[1]*cov_h[1,5]*x[5] + 
  x[2]*cov_h[2,1]*x[1] + x[2]*jnj_for*x[2]   + x[2]*cov_h[2,3]*x[3] + x[2]*cov_h[2,4]*x[4] + x[2]*cov_h[2,5]*x[5]  + 
  x[3]*cov_h[3,1]*x[1] + x[3]*cov_h[3,2]*x[2] + x[3]*pg_for*x[3]   + x[3]*cov_h[3,4]*x[4] + x[3]*cov_h[3,5]*x[5] + 
  x[4]*cov_h[4,1]*x[1] + x[4]*cov_h[4,2]*x[2] + x[4]*cov_h[4,3]*x[3] + x[4]*ibm_for*x[4] + x[4]*cov_h[4,5]*x[5] + 
  x[5]*cov_h[5,1]*x[1] + x[5]*cov_h[5,2]*x[2] + x[5]*cov_h[5,3]*x[3] + x[5]*cov_h[5,4]*x[4] + x[5]*nke_for*x[5] 
  

theta <- c(0.48,0.01,0.015,0.09,0.40)

ui <- rbind(c(1,0,0,0,0),
            c(0,1,0,0,0),
            c(0,0,1,0,0),
            c(0,0,0,1,0),
            c(0,0,0,0,1),
            c(-1,-1,-1,-1,-1),
            c(1,1,1,1,1),
            c(means_h))
ci <- c(0,
        0,
        0,
        0,
        0,
        -1,
        0.99,
        0.0005) # 5.04% Annual Return Spread to Daily #

port_opt <- constrOptim(theta = theta, f = f, ui = ui, ci = ci, grad = NULL)

port_weights_g <- port_opt$par
port_var_g <- port_opt$value
names(port_weights_g) <- names(means_h)
final_g <- round(port_weights_g*100,2)

# optimized portfolio:
# wmt_r: 30.86 jnj_r: 22.17  pg_r: 20.10 ibm_r: 6.37 nke_r: 19.54
