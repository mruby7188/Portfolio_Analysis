require(tseries)
require(zoo)
require(IntroCompFinR)
require(lubridate)
require(PerformanceAnalytics)
require(corrplot)
require(quantmod)
require(boot)
require(dygraphs)
require(quadprog)
require(linprog)
require(boot)

## load data

start.date = "2015-04-01"
end.date = "2018-08-27"



# Pull out NFLX, AYI, NVDA, NVR ------------------------------------------

# AAXN

aaxn <- get.hist.quote("AAXN", start=start.date,end=end.date, quote="AdjClose",
                       provider="yahoo", origin = "1970-01-01",
                       compression="d", retclass="zoo")


# NFLX

nflx <- get.hist.quote("NFLX", start=start.date,end=end.date, quote="AdjClose",
                        provider="yahoo", origin = "1970-01-01",
                        compression="m", retclass="zoo")

# AYI

ayi <- get.hist.quote("AYI", start=start.date,end=end.date, quote="AdjClose",
                           provider="yahoo", origin = "1970-01-01",
                           compression="m", retclass="zoo")

# NVDA

nvda <- get.hist.quote("NVDA", start=start.date,end=end.date, quote="AdjClose",
                       provider="yahoo", origin = "1970-01-01",
                       compression="m", retclass="zoo")

# NVR

nvr  <- get.hist.quote("NVR", start=start.date,end=end.date, quote="AdjClose",
                       provider="yahoo", origin = "1970-01-01",
                       compression="m", retclass="zoo")

# merge assets

proj.prices <- merge(nflx, nvda, ayi, nvr)
names(proj.prices) <- c("NFLX", "NVDA", "AYI", "NVR")

# Calculate Returns -------------------------------------------------------

ret.cc <- na.omit(Return.calculate(proj.prices, "log"))

ret.simp <- exp(ret.cc-1)

assets.ec <- cumprod(1+ret.simp)


# Descriptive Statistics --------------------------------------------------

## 2. Plot prices, returns and equity curves ---------------------------------------------------------------

my.panel <- function(...) {
  lines(...)
  abline(h=0)
}

plot(proj.prices, main="Monthly Prices", col="blue", lwd=2)
plot(ret.simp, main="Monthly Simple Returns", panel=my.panel, col="blue", lwd=2)
plot(ret.cc, main="Monthly CC Returns", panel=my.panel, col="blue", lwd=2)


chart.CumReturns(ret.cc, wealth.index=TRUE, legend.loc="topleft", 
                 lwd=2, main="Growth of $1") 

ret.mat = coredata(ret.cc)

## 3. Diagnostic plots ---------------------------------------------------------------

fourPanelPlot(ret.cc[, "NFLX", drop=FALSE])
fourPanelPlot(ret.cc[, "NVDA", drop=FALSE])
fourPanelPlot(ret.cc[, "AYI", drop=FALSE])
fourPanelPlot(ret.cc[, "NVR", drop=FALSE])

boxplot(ret.mat, col="cornflowerblue", main="Returns")
abline(h=0)

## 4,5,6 Descriptive Statistics ---------------------------------------------------------------

muvals <- colMeans(ret.cc)
sd.vals <- apply(ret.cc, 2, sd)
skew.vals <- apply(ret.cc, 2, skewness)
ekurt.vals <- apply(ret.cc, 2, kurtosis)
cov.mat <- var(ret.cc)
cor.mat <- cov2cor(cov.mat)

covhat.vals <- cov.mat[lower.tri(cov.mat)]
rhohat.vals <- cor.mat[lower.tri(cor.mat)]
names(covhat.vals) <- names(rhohat.vals) <- 
  c("NFLX,NVDA", "NFLX,AYI", "NFLX,NVR", 
    "NVDA,AYI", "NVDA,NVR", 
    "AYI,MVR")

# empirical quantiles for VaR calculations
q.vals <- apply(ret.cc, 2, quantile, prob=c(0.01,0.05))

# display results in a table
stats.mat <- rbind(muvals, 
                  sd.vals,
                  skew.vals,
                  ekurt.vals,
                  q.vals)
rownames(stats.mat) <- c("Mean", "Std Dev", "Skewness", 
                        "Excess Kurtosis", "1% Quantile", 
                        "5% Quantile")
# print statistics
xtable(stats.mat)
xtable(cov.mat)
xtable(cor.mat)


corrplot.mixed(cor.mat)

# Model Estimation ---------------------------------------------------------------


## 7. Mean and SD SE's and 95% CI ---------------------------------------------------------------

# Mean

n.obs <- nrow(ret.cc)

muhat.se <- sd.vals/sqrt(n.obs)
muhat.se

mu.lwr <- muhat.vals-2*muhat.se
mu.upr <- muhat.vals+2*muhat.se

muhat.mat <- cbind(muhat.vals, muhat.se, mu.lwr, mu.upr)
colnames(muhat.mat) <- c("Mean", "SE", "LCL (0.95)", "UCL (0.95)")

muhat.mat

# SD

sd.se <- sd.vals/sqrt(2*n.obs)
sd.se

sd.lwr <- sd.vals-2*sd.se
sd.upr <- sd.vals+2*sd.se

sd.mat <- cbind(sd.vals, sd.se, sd.lwr, sd.upr)
colnames(sd.mat) <- c("Std Dev", "SE", "LCL (0.95)", "UCL (0.95)")

sd.mat

## 8. Bootstrap SE ---------------------------------------------------------------

bootstraped.boot.mean <- function(x, idx) {
  muhat = mean(x[idx])
}

bootstraped.boot.sd = function(x, idx) {
  sigmahat = sd(x[idx])
}


set.seed(1)

nvr.boot.1000.mean <- boot(ret.mat[, "NVR"], 
                     statistic=bootstraped.boot.mean, R=1000)
nvr.boot.1000.mean

set.seed(1)

nvr.boot.1000.sd <- boot(ret.mat[, "NVR"], 
                           statistic=bootstraped.boot.sd, R=1000)
nvr.boot.1000.sd

set.seed(1)

nvr.boot.10000.mean <- boot(ret.mat[, "NVR"], 
                      statistic=bootstraped.boot.mean, R=10000)
nvr.boot.10000.mean

set.seed(1)

nvr.boot.10000.sd <- boot(ret.mat[, "NVR"], 
                         statistic=bootstraped.boot.sd, R=1000)
nvr.boot.10000.sd

boot.ci(nvr.boot.1000.mean, conf = 0.95, type = c("norm","perc"))

boot.ci(nvr.boot.1000.sd, conf = 0.95, type = c("norm","perc"))

boot.mean.1000 <- cbind(nvr.boot.1000.mean$t0, sd(nvr.boot.1000.mean$t), muhat.se["NVR"])

boot.sd.1000 <- cbind(nvr.boot.1000.sd$t0, sd(nvr.boot.1000.sd$t), sd.se["NVR"])

rownames(boot.mean.1000) <- "R=1000"

rownames(boot.sd.1000) <- "R=1000"

boot.mean.10000 <- cbind(nvr.boot.10000.mean$t0, sd(nvr.boot.10000.mean$t), muhat.se["NVR"])

boot.sd.10000 <- cbind(nvr.boot.10000.sd$t0, sd(nvr.boot.10000.sd$t), sd.se["NVR"])

rownames(boot.mean.10000) <- "R=10000"

rownames(boot.sd.10000) <- "R=10000"

boot.mean <- rbind(boot.mean.1000, boot.mean.10000)

boot.sd <- rbind(boot.sd.1000, boot.sd.10000)

colnames(boot.mean) <- c("Estimate", "Boot S.E.", "Comp S.E.")

colnames(boot.sd) <- c("Estimate", "Boot S.E.", "Comp S.E.")
sd.vals
sd.se
mu
## 9. plot, R=1000

plot(nvr.boot.1000.mean)

plot(nvr.boot.1000.sd)


# 10. SE and 95% confidence values for estimated correlations -------------



n.obs <- nrow(ret.cc)
SE.rhohat <- (1 - rhohat.vals^2)/sqrt(n.obs)
rho.lower <- rhohat.vals - 2*SE.rhohat
rho.upper <- rhohat.vals + 2*SE.rhohat

rhohat.mat <- cbind(rhohat.vals, SE.rhohat, rho.lower, rho.upper)
colnames(rhohat.mat) <- c("rho.hat", "SE", "LCL (0.95)", "UCL (0.95)")
rhohat.mat

## 11. VAR --------------------------------------------------------

# function to compute normal and empirical VaR for a matrix of cc returns

Value.at.Risk <- function(x, p=0.05, w=100000, 
                         method=c("normal", "empirical"),
                         return.type=c("cc", "simple")) {
  method=method[1]
  return.type=return.type[1]
  x = as.matrix(x)
  if (method == "normal") {
    q = apply(x, 2, mean) + apply(x, 2, sd)*qnorm(p)
  } else {    
    q = apply(x, 2, quantile, p)
  }
  if (return.type == "simple") {
    VaR = q*w
  } else {
    VaR = (exp(q) - 1)*w
  }
  VaR
}

# compute 5% and 1% normal VaR for all assets

VaR.normal.05 <- Value.at.Risk(ret.mat, p=0.05, 
                              method="normal",
                              return.type="cc")

VaR.normal.01 <- Value.at.Risk(ret.mat, p=0.01, 
                               method="normal",
                               return.type="cc")

VaR.normal.01

VaR.normal.05

# empirical VaR

VaR.empirical.05 <- Value.at.Risk(ret.mat, p=0.05, 
                                 method="empirical",
                                 return.type="cc")

VaR.empirical.01 <- Value.at.Risk(ret.mat, p=0.01, 
                                 method="empirical",
                                 return.type="cc")

VaR.normal.mat <- cbind(VaR.normal.05, VaR.normal.01)

VaR.empirical.mat <- cbind(VaR.empirical.05, VaR.empirical.01)


# function to bootstrap VaR for a single asset

ValueAtRisk.boot <- function(x, idx, p=0.05, w=100000,
                            method=c("normal", "empirical"),
                            return.type=c("cc", "simple")) {
  method = method[1]
  return.type = return.type[1]
  if (method == "normal") {
    q = mean(x[idx]) + sd(x[idx])*qnorm(p)
  } else {
    q = quantile(x[idx], p)
  }
  if (return.type == "cc") {
    VaR = (exp(q) - 1)*w
  } else {
    VaR = q*w
  }
  VaR
}

computeSEconfintVaR = function(x, p=0.05, w=100000,
                               method=c("normal", "empirical"),
                               return.type=c("cc", "simple")) {
  VaR.boot = boot(x, statistic=ValueAtRisk.boot, R=1000)
  VaR.hat = VaR.boot$t0
  SE.VaR = sd(VaR.boot$t)
  CI.VaR = boot.ci(VaR.boot, conf = 0.95, type="norm")$normal
  CI.VaR = CI.VaR[-1]
  ans = c(VaR.hat, SE.VaR, CI.VaR)
  names(ans) = c("VaR.05", "SE", "LCL (0.95)", "UCL (0.95)")
  return(ans)
}


# bootstrap SE and 95% CI for assets

VaR.boot.nflx <- computeSEconfintVaR(ret.mat[, "NFLX", drop=FALSE])

VaR.boot.nvda <- computeSEconfintVaR(ret.mat[, "NVDA", drop=FALSE])

VaR.boot.ayi <- computeSEconfintVaR(ret.mat[, "AYI", drop=FALSE])

VaR.boot.nvr <- computeSEconfintVaR(ret.mat[, "NVR", drop=FALSE])

VaR.boot.mat <- rbind(VaR.boot.nflx,
                     VaR.boot.nvda,
                     VaR.boot.ayi,
                     VaR.boot.nvr)
rownames(VaR.boot.mat) <- colnames(ret.cc)
VaR.boot.mat

# annualized VaR numbers

Value.at.Risk.annual = function(x, p=0.05, w=100000, 
                                method=c("normal", "empirical"),
                                return.type=c("cc", "simple")) {
  method=method[1]
  return.type=return.type[1]
  x = as.matrix(x)
  if (method == "normal") {
    q = 12*apply(x, 2, mean) + sqrt(12)*apply(x, 2, sd)*qnorm(p)
  } else {    
    # compute standardized returns and empirical quantile 
    zhat = apply(x, 2, scale)
    qzhat = apply(zhat, 2, quantile, p)
    q = 12*apply(x, 2, mean) + sqrt(12)*apply(x, 2, sd)*qzhat
  }
  if (return.type == "simple") {
    VaR = q*w
  } else {
    VaR = (exp(q) - 1)*w
  }
  VaR
}

VaR.normal.05.annual <- Value.at.Risk.annual(ret.mat, p=0.05, 
                                            method="normal",
                                            return.type="cc")

VaR.normal.01.annual <- Value.at.Risk.annual(ret.mat, p=0.01, 
                                            method="normal",
                                            return.type="cc")

# empirical VaR

VaR.empirical.05.annual <- Value.at.Risk.annual(ret.mat, p=0.05, 
                                               method="empirical",
                                               return.type="cc")

VaR.empirical.01.annual <- Value.at.Risk.annual(ret.mat, p=0.01, 
                                               method="empirical",
                                               return.type="cc")

VaR.normal.annual.mat <- cbind(VaR.normal.05.annual, VaR.normal.01.annual)

VaR.empirical.annual.mat <- cbind(VaR.empirical.05.annual, VaR.empirical.01.annual)


# Portfolio Theory --------------------------------------------------------

## 14. Sharpe Ratio --------------------------------------------------------

rf <- 0.005/12

sr <- as.matrix((muvals-rf)/sd.vals, ncol=1)

colnames(sr) = "Sharpe"

sr

# compute bootstrap standard error and 95% CI for sharpe ratio
sharpeRatio.boot = function(x, idx, risk.free) {
  muhat = mean(x[idx])
  sigmahat = sd(x[idx])
  sharpeRatio = (muhat - risk.free)/sigmahat
  sharpeRatio
}

sharpe.nflx.boot = boot(ret.mat[, "NVR"], 
                        statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.nflx.boot
boot.ci(sharpe.nflx.boot, conf = 0.95, type = c("norm","perc"))
plot(sharpe.nflx.boot)

computeSEconfintSharpe = function(x, risk.free) {
  Sharpe.boot = boot(x, statistic=sharpeRatio.boot, R=999, risk.free=risk.free)
  Sharpe.hat = Sharpe.boot$t0
  SE.Sharpe = sd(Sharpe.boot$t)
  CI.Sharpe = boot.ci(Sharpe.boot, conf = 0.95, type="norm")$normal
  CI.Sharpe = CI.Sharpe[-1]
  ans = c(Sharpe.hat, SE.Sharpe, CI.Sharpe)
  names(ans) = c("Sharpe", "SE", "LCL (0.95)", "UCL (0.95)")
  return(ans)
}

# bootstrap SE and 95% CI for assets Sharpe Ratio
set.seed(1)
Sharpe.boot.nflx = computeSEconfintSharpe(ret.mat[, "NFLX", drop=FALSE], risk.free=rf)
set.seed(1)
Sharpe.boot.nvda = computeSEconfintSharpe(ret.mat[, "NVDA", drop=FALSE], risk.free=rf)
set.seed(1)
Sharpe.boot.ayi = computeSEconfintSharpe(ret.mat[, "AYI", drop=FALSE], risk.free=rf)
set.seed(1)
Sharpe.boot.nvr = computeSEconfintSharpe(ret.mat[, "NVR", drop=FALSE], risk.free=rf)

Sharpe.boot.mat = rbind(Sharpe.boot.nflx,
                        Sharpe.boot.nvda,
                        Sharpe.boot.ayi,
                        Sharpe.boot.nvr)

rownames(Sharpe.boot.mat) = colnames(ret.cc)
Sharpe.boot.mat 

## 15. plot return-risk tradeoff and compute Sharpe ratios --------------------------------------------------------

# S & P 500

sp500 <- get.hist.quote("^gspc", start=start.date,end=end.date, quote="AdjClose",
                        provider="yahoo", origin = "1970-01-01",
                        compression="m", retclass="zoo")

ret.sp500.s <- na.omit(Return.calculate(sp500, "simple"))

ret.sp500.cc <- na.omit(Return.calculate(sp500, "log"))

sd.sp500 <- sd(ret.sp500.cc)
er.sp500 <- mean(ret.sp500.cc)

# Plot

plot(sd.vals, muvals, xlim=c(0, 0.15), ylim=c(0, 0.050),
     ylab="Expected Return", xlab="Standard Deviation",
     cex=1.25, pch=16, col=c("forest green","cornflowerblue","red","black"))
  points(sd.sp500, er.sp500, lwd=2, col="gold", pch=16)
  points(0, rf, cex=2, pch=16, col="green")
  text(0, rf, labels="Rf", pos=3)
  text(sd.vals, muvals, labels=colnames(ret.cc),
       pos=c(1:4))
  text(sd.sp500, er.sp500, labels="S&P 500", pos=1)

## 16. Compute the GMV --------------------------------------------------------

gmv <- globalMin.portfolio(muvals, cov.mat)

gmv

plot(gmv, col="cornflowerblue")

## 17 VaR --------------------------------------------------------

w0 <- 100000

q.05 <- (gmv$er + gmv$sd*qnorm(0.05))

gmv.VaR.05 <- (exp(q.05) - 1)*w0

q.01 <- (gmv$er + gmv$sd*qnorm(0.01))

gmv.VaR.01 <- (exp(q.01) - 1)*w0



## 18. Compute global minimum variance portfolio with no short sales --------------------------------------------------------

gmin.port.ns <- globalMin.portfolio(muvals, cov.mat, shorts=FALSE)

attributes(gmin.port.ns)

print(gmin.port.ns)

summary(gmin.port.ns, risk.free=rf)

plot(gmin.port.ns, col="cornflowerblue")

# compare weights

wts.mat = cbind(gmv$weights,
                gmin.port.ns$weights)

colnames(wts.mat) = c("gmin", "gmin.ns")

wts.mat

# 19. 1% and 5% VaR --------------------------------------------------------

w0=100000

q.05 = (gmin.port.ns$er + gmin.port.ns$sd*qnorm(0.05))

VaR.gmin.ns.05 = (exp(q.05) - 1)*w0

q.01 = (gmin.port.ns$er + gmin.port.ns$sd*qnorm(0.01))

VaR.gmin.ns.01 = (exp(q.01) - 1)*w0

VaR.gmin.ns.05

VaR.gmin.ns.01

VaR.gmin.mat = cbind(c(gmv$er*12, gmv$sd*sqrt(12), 
                       gmv.VaR.05, gmv.VaR.01),
                     c(gmin.port.ns$er*12, gmin.port.ns$sd*sqrt(12), 
                       VaR.gmin.ns.05, VaR.gmin.ns.01))

rownames(VaR.gmin.mat) = c("Annual Mean", "Annual SD", "VaR.05", "VaR.01")

colnames(VaR.gmin.mat) = c("Shorts", "No Shorts")

VaR.gmin.mat



# 20. Efficient portfolio with target return equal to max returns --------------------------------------------------------

target.return <- max(muvals)

e.port.max<- efficient.portfolio(muvals, cov.mat, target.return)

e.port.max

summary(e.port.max, risk.free=rf)

plot(e.port.max, col="cornflowerblue")

## compute efficient frontier

ef <- efficient.frontier(muvals, cov.mat, alpha.min=-1, alpha.max=1.5, nport=20)

plot(ef, plot.assets=F, col="blue", lwd=2)
  abline(v=0)
  abline(h=0)
  points(gmv$sd, gmv$er, col="orange", lwd=2, pch=16) ## gmv
  text(sd.vals, muvals, labels=colnames(ret.cc), col=c("forest green","cornflowerblue","red","black"))
  text(gmv$sd, gmv$er, labels = "GMV", pos=2)



# 21. Compute tangency portfolio--------------------------------------------------------
  
tan.port <- tangency.portfolio(muvals, cov.mat, rf)
  
tan.port

summary(tan.port, risk.free=rf)

plot(tan.port, col="cornflowerblue")

# Efficient Frontier of Tangency

tan.wts <- ret.cc*tan.port$weights

alpha <- seq(-0,2,0.1)

tan.ef.er <- rf + (tan.port$er-rf)*alpha
tan.ef.sd <- (alpha*tan.port$sd)


plot(ef, plot.assets = F, col="blue",lwd=2)
  points(0, rf, col="green", pch=16)
  points(gmv$sd, gmv$er, col="orange", lwd=2, pch=16) ## gmv
  points(sd.vals, muvals, col=c("forest green","cornflowerblue","red","black"), pch=16)
  points(tan.ef.sd, tan.ef.er, lwd=2, col="purple", pch=20)
  points(tan.port$sd, tan.port$er, col="navyblue", pch=20, lwd=2)
  points(0, rf, col="green", pch=16)
  text(sd.vals, muvals, labels=colnames(ret.cc), col=c("forest green","cornflowerblue","red","black"), pos=c(4,1,4,4))
  text(0, rf, "RF", pos=4)
  text(gmv$sd, gmv$er, labels = "GMV", pos=4, col="orange")
  text(tan.port$sd, tan.port$er+0.0008, "Tan Port", pos=2, col="magenta")
  

# 22. Compute Efficiency portfolio with No-Short Sales --------------------------------------------------------

ef.ns <- efficient.frontier(muvals, cov.mat, alpha.min=-1, 
                            alpha.max=1.5, nport=20, shorts = F)

plot(ef, plot.assets=F, col=c("blue"), lwd=2)
  points(sd.vals, muvals, col=c("forest green","cornflowerblue","red","black"), pch=16)
  points(ef.ns$sd, ef.ns$er, col="red", pch=16)
  points(gmv$sd, gmv$er, col="orange", lwd=2, pch=16) ## gmv
  text(sd.vals, muvals, labels=colnames(ret.cc), col=c("forest green","cornflowerblue","red","black"), pos=c(4,1,4,3))
  text(gmv$sd, gmv$er, labels = "GMV", pos=2, col="orange")
  
## Compare volatility of short/no short portfolios with E[R]=0.02
  
ef.02 <- efficient.portfolio(muvals, cov.mat, target.return = .02)

ns.ef.02 <- efficient.portfolio(muvals, cov.mat, target.return = .02, shorts = F)

ns.ef.02$sd

ef.02$sd

plot(ef, plot.assets=F, col=c("blue"), lwd=2)
  points(sd.vals, muvals, col=c("forest green","cornflowerblue","red","black"), pch=16)
  points(ef.ns$sd, ef.ns$er, col="red", pch=16)
  points(gmv$sd, gmv$er, col="orange", lwd=2, pch=16) ## gmv
  points(ns.ef.02$sd, ns.ef.02$er, col="green")
  text(sd.vals, muvals, labels=colnames(ret.cc), col=c("forest green","cornflowerblue","red","black"), pos=c(4,1,4,3))
  text(gmv$sd, gmv$er, labels = "GMV", pos=2, col="orange")

# 23. Compute tangency portfolio with no shorts --------------------------------------------------------

tan.port.ns <- tangency.portfolio(muvals, cov.mat, rf, shorts = F)

tan.port.ns

summary(tan.port.ns, risk.free = rf)

plot(tan.port.ns, col = "cornflowerblue")


# annualized mean and sd
12*tan.port.ns$er
sqrt(12)*tan.port.ns$sd

# compare weights
wts.mat = cbind(tan.port$weights,
                tan.port.ns$weights)
colnames(tan.wts.mat) = c("tan", "tan.ns")

stats.tan.mat = cbind(c(tan.port$er*12, tan.port$sd*sqrt(12)), 
                      c(tan.port.ns$er*12, tan.port.ns$sd*sqrt(12)))
rownames(stats.tan.mat) = c("Annual Mean", "Annual SD")
colnames(stats.tan.mat) = c("Shorts", "No Shorts")

# plot portfolio weights
par(mfrow=c(2,1))
plot(gmv, col="cornflowerblue")
plot(tan.port, col="cornflowerblue")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(gmin.port.ns, col="cornflowerblue")
plot(tan.port.ns, col="cornflowerblue")
par(mfrow=c(1,1))

# 24. Plot Equity curve of S & P 500 and tangency portfolio --------------------------------------------------------

port_return <- zoo(ret.cc%*%tan.port$weights)

names(ret.sp500.cc) <- "S & P 500"

names(port_return) <- "Tangent Portfolio"

ret.sp500.cc <- zoo(as.data.frame(ret.sp500.cc))

index(port_return) <- as.yearmon(index(ret.cc))

index(ret.sp500.cc) <- as.yearmon(index(ret.cc))

sp500.tan <- merge(port_return, ret.sp500.cc)

chart.CumReturns(sp500.tan, wealth.index=TRUE, legend.loc="topleft", 
                 lwd=2, main="Growth of $1")


# 25. Efficient portfolio with SD[rp]=SD[S&P 500] --------------------------------------------------------

port.wts <- sd(ret.sp500.cc)/tan.port$sd

tan.sp.ret <- zoo(rf+(port_return%*%port.wts-rf))

names(tan.sp.ret) <- "SD = SD[S & P 500]"

tan.sp.ret.er <- mean(tan.sp.ret)
tan.sp.ret.sd <- sd(tan.sp.ret)

sr.tan.sp <- (tan.sp.ret.er-rf)/tan.sp.ret.sd

# Equity Curve

index(tan.sp.ret) <- as.yearmon(index(ret.cc))

portsd.sp <- merge(ret.sp500.cc, tan.sp.ret)

chart.CumReturns(portsd.sp, wealth.index=TRUE, legend.loc="topleft", 
                 lwd=2, main="Growth of $1")


rmarkdown::render("project analysis.R", output_format = "pdf_document", clean = F)
