# ECON424 Final Project

---
title: "Final Write-up"
author: "Michael Ruby"
date: "5/28/2018"
output:
  pdf_document:
    keep_tex: true
subparagraph: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(message = F, warning = F, echo=F, tidy = T)

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
require(knitr)
require(xtable)
```


```{r code, include=F}

## load data

start.date = "2013-04-01"
end.date = "2018-04-30"


# Pull out NFLX, AYI, NVDA, NVR ------------------------------------------

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


## set plot panel

my.panel <- function(...) {
  lines(...)
  abline(h=0)
}

# Calculate Returns -------------------------------------------------------

ret.cc <- na.omit(Return.calculate(proj.prices, "log"))

ret.simp <- exp(ret.cc-1)

assets.ec <- cumprod(1+ret.simp)


# Descriptive Statistics --------------------------------------------------

## 2. Plot prices, returns and equity curves ---------------------------------------------------------------

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
    "AYI,NVR")

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
stats.mat
cov.mat
cor.mat
#

corrplot.mixed(cor.mat)

# Model Estimation ---------------------------------------------------------------


## 7. Mean and SD SE's and 95% CI ---------------------------------------------------------------

# Mean

n.obs <- nrow(ret.cc)

muhat.se <- sd.vals/sqrt(n.obs)
muhat.se

mu.lwr <- muvals-2*muhat.se
mu.upr <- muvals+2*muhat.se

muhat.mat <- cbind(muvals, muhat.se, mu.lwr, mu.upr)
colnames(muhat.mat) <- c("Mean", "SE", "LCL (0.95)", "UCL (0.95)")

# SD

sd.se <- sd.vals/sqrt(2*n.obs)
sd.se

sd.lwr <- sd.vals-2*sd.se
sd.upr <- sd.vals+2*sd.se

sd.mat <- cbind(sd.vals, sd.se, sd.lwr, sd.upr)
colnames(sd.mat) <- c("Standard Deviation", "SE", "LCL (0.95)", "UCL (0.95)")

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

## 9. plot, R=1000

plot(nvr.boot.1000.mean)

plot(nvr.boot.1000.sd)


# 10. SE and 95% confidence values for estimated correlations -------------

SE.rhohat <- (1 - rhohat.vals^2)/sqrt(n.obs)
rho.lower <- rhohat.vals - 2*SE.rhohat
rho.upper <- rhohat.vals + 2*SE.rhohat

rhohat.mat <- cbind(rhohat.vals, SE.rhohat, rho.lower, rho.upper)
colnames(rhohat.mat) <- c("rho.hat", "SE", "LCL (0.95)", "UCL (0.95)")
rhohat.mat

## 11. VAR --------------------------------------------------------

# function to compute normal and empirical VaR for a matrix of cc returns

Value.at.Risk <- function(x, p, w=100000,
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

VaR.norm <- cbind(VaR.normal.01,VaR.normal.05)
colnames(VaR.norm) <- c("1%", "5%")

# empirical VaR

VaR.empirical.05 <- Value.at.Risk(ret.mat, p=0.05,
                                 method="empirical",
                                 return.type="cc")

VaR.empirical.01 <- Value.at.Risk(ret.mat, p=0.01,
                                 method="empirical",
                                 return.type="cc")

VaR.normal.mat <- cbind(VaR.normal.05, VaR.normal.01)

colnames(VaR.normal.mat) <- c("5% VaR","1% VaR")

VaR.empirical.mat <- cbind(VaR.empirical.05, VaR.empirical.01)

colnames(VaR.empirical.mat) <- c("5% VaR","1% VaR")

# annualized VaR numbers

Value.at.Risk.annual = function(x, p, w=100000,
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
## 12 bootstrap normal VaR

norm.ValueAtRisk.boot05 = function(x, idx, p=0.05, w=100000,
                            method=c("normal"),
                            return.type="cc") {
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

computeSEconfintVaR05 = function(x, p=0.05, w=100000,
                               method=c("normal", "empirical"),
                               return.type=c("cc", "simple")) {
  VaR.boot = boot(x, statistic=norm.ValueAtRisk.boot05, R=1000)
  VaR.hat = VaR.boot$t0
  SE.VaR = sd(VaR.boot$t)
  CI.VaR = boot.ci(VaR.boot, conf = 0.95, type="norm")$normal
  CI.VaR = CI.VaR[-1]
  ans = c(VaR.hat, SE.VaR, CI.VaR)
  names(ans) = c("5% VaR", "SE", "LCL (0.95)", "UCL (0.95)")
  return(ans)
}

set.seed(1)
VaR.boot.nflx.norm.05 <- computeSEconfintVaR05(ret.mat[, "NFLX", drop=FALSE])
set.seed(1)
VaR.boot.nvda.norm.05 <- computeSEconfintVaR05(ret.mat[, "NVDA", drop=FALSE])
set.seed(1)
VaR.boot.ayi.norm.05 <- computeSEconfintVaR05(ret.mat[, "AYI", drop=FALSE])
set.seed(1)
VaR.boot.nvr.norm.05 <- computeSEconfintVaR05(ret.mat[, "NVR", drop=FALSE])
set.seed(1)
VaR.boot.mat.norm.05 <- rbind(VaR.boot.nflx.norm.05,
                     VaR.boot.nvda.norm.05,
                     VaR.boot.ayi.norm.05,
                     VaR.boot.nvr.norm.05)
rownames(VaR.boot.mat.norm.05) <- colnames(ret.cc)
VaR.boot.mat.norm.05


### 1% VaR

norm.ValueAtRisk.boot01 = function(x, idx, p=0.01, w=100000,
                            method=c("normal"),
                            return.type="cc") {
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

computeSEconfintVaR01 = function(x, p=0.01, w=100000,
                               method=c("normal", "empirical"),
                               return.type=c("cc", "simple")) {
  VaR.boot = boot(x, statistic=norm.ValueAtRisk.boot01, R=1000)
  VaR.hat = VaR.boot$t0
  SE.VaR = sd(VaR.boot$t)
  CI.VaR = boot.ci(VaR.boot, conf = 0.95, type="norm")$normal
  CI.VaR = CI.VaR[-1]
  ans = c(VaR.hat, SE.VaR, CI.VaR)
  names(ans) = c("1% VaR", "SE", "LCL (0.95)", "UCL (0.95)")
  return(ans)
}

set.seed(1)
VaR.boot.nflx.norm.01 <- computeSEconfintVaR01(ret.mat[, "NFLX", drop=FALSE])
set.seed(1)
VaR.boot.nvda.norm.01 <- computeSEconfintVaR01(ret.mat[, "NVDA", drop=FALSE])
set.seed(1)
VaR.boot.ayi.norm.01 <- computeSEconfintVaR01(ret.mat[, "AYI", drop=FALSE])
set.seed(1)
VaR.boot.nvr.norm.01 <- computeSEconfintVaR01(ret.mat[, "NVR", drop=FALSE])
set.seed(1)
VaR.boot.mat.norm.01 <- rbind(VaR.boot.nflx.norm.01,
                     VaR.boot.nvda.norm.01,
                     VaR.boot.ayi.norm.01,
                     VaR.boot.nvr.norm.01)
rownames(VaR.boot.mat.norm.01) <- colnames(ret.cc)
VaR.boot.mat.norm.01

## 13. Empirical VaR

emp.ValueAtRisk.boot05 = function(x, idx, p=0.05, w=100000,
                            method=c("empirical"),
                            return.type="cc") {
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

computeSEconfintVaR.emp05 = function(x, p=0.05, w=100000,
                               method="empirical",
                               return.type=c("cc", "simple")) {
  VaR.boot = boot(x, statistic=emp.ValueAtRisk.boot05, R=1000)
  VaR.hat = VaR.boot$t0
  SE.VaR = sd(VaR.boot$t)
  CI.VaR = boot.ci(VaR.boot, conf = 0.95, type="norm")$normal
  CI.VaR = CI.VaR[-1]
  ans = c(VaR.hat, SE.VaR, CI.VaR)
  names(ans) = c("5% VaR", "SE", "LCL (0.95)", "UCL (0.95)")
  return(ans)
}

set.seed(1)
VaR.boot.nflx.emp.05 <- computeSEconfintVaR.emp05(ret.mat[, "NFLX", drop=FALSE])
set.seed(1)
VaR.boot.nvda.emp.05 <- computeSEconfintVaR.emp05(ret.mat[, "NVDA", drop=FALSE])
set.seed(1)
VaR.boot.ayi.emp.05 <- computeSEconfintVaR.emp05(ret.mat[, "AYI", drop=FALSE])
set.seed(1)
VaR.boot.nvr.emp.05 <- computeSEconfintVaR.emp05(ret.mat[, "NVR", drop=FALSE])
set.seed(1)
VaR.boot.mat.emp.05 <- rbind(VaR.boot.nflx.emp.05,
                     VaR.boot.nvda.emp.05,
                     VaR.boot.ayi.emp.05,
                     VaR.boot.nvr.emp.05)
rownames(VaR.boot.mat.emp.05) <- colnames(ret.cc)
VaR.boot.mat.emp.05

emp.ValueAtRisk.boot01 = function(x, idx, p=0.01, w=100000,
                            method=c("empirical"),
                            return.type="cc") {
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

computeSEconfintVaR.emp01 = function(x, p=0.01, w=100000,
                               method="empirical",
                               return.type=c("cc", "simple")) {
  VaR.boot = boot(x, statistic=emp.ValueAtRisk.boot01, R=1000)
  VaR.hat = VaR.boot$t0
  SE.VaR = sd(VaR.boot$t)
  CI.VaR = boot.ci(VaR.boot, conf = 0.95, type="norm")$normal
  CI.VaR = CI.VaR[-1]
  ans = c(VaR.hat, SE.VaR, CI.VaR)
  names(ans) = c("1% VaR", "SE", "LCL (0.95)", "UCL (0.95)")
  return(ans)
}

set.seed(1)
VaR.boot.nflx.emp.01 <- computeSEconfintVaR.emp01(ret.mat[, "NFLX", drop=FALSE])
set.seed(1)
VaR.boot.nvda.emp.01 <- computeSEconfintVaR.emp01(ret.mat[, "NVDA", drop=FALSE])
set.seed(1)
VaR.boot.ayi.emp.01 <- computeSEconfintVaR.emp01(ret.mat[, "AYI", drop=FALSE])
set.seed(1)
VaR.boot.nvr.emp.01 <- computeSEconfintVaR.emp01(ret.mat[, "NVR", drop=FALSE])
set.seed(1)
VaR.boot.mat.emp.01 <- rbind(VaR.boot.nflx.emp.01,
                     VaR.boot.nvda.emp.01,
                     VaR.boot.ayi.emp.01,
                     VaR.boot.nvr.emp.01)
rownames(VaR.boot.mat.emp.01) <- colnames(ret.cc)
VaR.boot.mat.emp.01

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

sharpe.nflx.boot = boot(ret.mat[, "NFLX"],
                        statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.nvda.boot = boot(ret.mat[, "NVDA"],
                        statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.ayi.boot = boot(ret.mat[, "AYI"],
                        statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.nvr.boot = boot(ret.mat[, "NVR"],
                        statistic=sharpeRatio.boot, R=999, risk.free=rf)

sharpe.boot <- rbind(sharpe.nflx.boot$t0, sharpe.nvda.boot$t0, sharpe.ayi.boot$t0, sharpe.nflx.boot$t0)
sr.se <- rbind(sd(sharpe.nflx.boot$t),sd(sharpe.nvda.boot$t),sd(sharpe.ayi.boot$t),sd(sharpe.nvr.boot$t))

colnames(sharpe.boot) <- "Boot"
colnames(sr.se) <- "SE"
sharpe <- cbind(sr, sharpe.boot, sr.se)

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

Sharpe.boot.nflx = computeSEconfintSharpe(ret.mat[, "NFLX", drop=FALSE], risk.free=rf)
Sharpe.boot.nvda = computeSEconfintSharpe(ret.mat[, "NVDA", drop=FALSE], risk.free=rf)
Sharpe.boot.ayi = computeSEconfintSharpe(ret.mat[, "AYI", drop=FALSE], risk.free=rf)
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

gmv.VaR <- cbind(gmv.VaR.05, gmv.VaR.01)

colnames(gmv.VaR) <- c("5%", "1%")

rownames(gmv.VaR) <- "GMV"

## 18. Compute global minimum variance portfolio with no short sales --------------------------------------------------------

gmin.port.ns <- globalMin.portfolio(muvals, cov.mat, shorts=FALSE)

attributes(gmin.port.ns)

print(gmin.port.ns)

summary(gmin.port.ns, risk.free=rf)

plot(gmin.port.ns, col="cornflowerblue")

# compare weights

wts.mat = cbind(gmv$weights,
                gmin.port.ns$weights)

colnames(wts.mat) = c("GMV", "No Short GMV")

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

target.return <- max(muvals)

# 20. Efficient portfolio with target return equal to max returns --------------------------------------------------------

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


tan.port <- tangency.portfolio(muvals, cov.mat, rf)

# 21. Compute tangency portfolio--------------------------------------------------------

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
  points(tan.port$sd, tan.port$er, col="purple", pch=16)
  points(sd.vals, muvals, col=c("forest green","cornflowerblue","red","black"), pch=16)
  points(tan.ef.sd, tan.ef.er, lwd=2, col="purple", pch=20)
  points(0, rf, col="green", pch=16)
  text(sd.vals, muvals, labels=colnames(ret.cc), col=c("forest green","cornflowerblue","red","black"), pos=c(4,1,4,3))
  text(0, rf, "RF", pos=4)
  text(gmv$sd, gmv$er, labels = "GMV", pos=4, offset = 1, col="orange")
  text(tan.port$sd, tan.port$er+0.0008, "Tan Port", pos=2, col="purple")


# 22. Compute Efficiency portfolio with No-Short Sales --------------------------------------------------------

ef.ns <- efficient.frontier(muvals, cov.mat, alpha.min=0,
                            alpha.max=1, nport=20, shorts = F)

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

summary(tan.port, risk.free = rf)

plot(tan.port.ns, col = "cornflowerblue")


# annualized mean and sd
12*tan.port.ns$er
sqrt(12)*tan.port.ns$sd

# compare weights
tan.wts.mat = cbind(tan.port$weights,
                tan.port.ns$weights)
colnames(tan.wts.mat) = c("Tan", "Tan N.S.")

stats.tan.mat = cbind(c(tan.port$er, tan.port$sd),
                      c(tan.port.ns$er, tan.port.ns$sd))
rownames(stats.tan.mat) = c("Annual Mean", "Annual SD")
colnames(stats.tan.mat) = c("Shorts", "No Shorts")

# 24. Plot Equity curve of S & P 500 and tangency portfolio --------------------------------------------------------

# plot portfolio weights
par(mfrow=c(2,1))
plot(gmv, col="cornflowerblue")
plot(tan.port, col="cornflowerblue")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(gmin.port.ns, col="cornflowerblue")
plot(tan.port.ns, col="cornflowerblue")
par(mfrow=c(1,1))

port_return <- zoo(ret.cc%*%tan.port$weights)

ret.sp500.cc <- as.data.frame(ret.sp500.cc)

names(ret.sp500.cc) <- "S & P 500"

names(port_return) <- "Tangent Portfolio"

ret.sp500.cc <- zoo(as.data.frame(ret.sp500.cc))

index(port_return) <- as.yearmon(index(ret.cc))

index(ret.sp500.cc) <- as.yearmon(index(ret.cc))

sp500.tan <- merge(port_return, ret.sp500.cc)

chart.CumReturns(sp500.tan, wealth.index=TRUE, legend.loc="topleft",
                 lwd=2, main="Growth of $1")

# 25. Efficient portfolio with SD[rp]=SD[S&P 500] --------------------------------------------------------

sp.mean <- mean(ret.sp500.cc)

sp.sd <- sd(ret.sp500.cc)

sp.sr <- (sp.mean-rf)/sp.sd

port.wts <- sd(ret.sp500.cc)/tan.port$sd

tan.sp.ret <- zoo(rf+(port_return%*%port.wts-rf))

names(tan.sp.ret) <- "SD = SD[S & P 500]"

sp.var.port.wts <- rbind(port.wts, 1-port.wts)

rownames(sp.var.port.wts) <- c("Tangency Portfolio", "Risk-Free")

colnames(sp.var.port.wts) <- "Portfolio Weights"

tan.sp.ret.er <- mean(tan.sp.ret)
tan.sp.ret.sd <- sd(tan.sp.ret)

sr.tan.sp <- (tan.sp.ret.er-rf)/tan.sp.ret.sd

sp.tan.mat <- rbind(tan.sp.ret.er, tan.sp.ret.sd, sr.tan.sp)

sp.tan.mat <- cbind(sp.tan.mat, rbind(sp.mean, sp.sd, sp.sr))

rownames(sp.tan.mat) <- c("Expected Returns", "Std Dev", "Sharpe Ratio")

colnames(sp.tan.mat) <- c("SD[R]=SD[S&P 500]", "S&P 500")

# Equity Curve

index(tan.sp.ret) <- as.yearmon(index(ret.cc))

portsd.sp <- merge(ret.sp500.cc, tan.sp.ret)

chart.CumReturns(portsd.sp, wealth.index=TRUE, legend.loc="topleft",
                 lwd=2, main="Growth of $1")

```

## Introduction

  This report consists of a report on the returns of four assets; Netflix, Inc. (NFLX), Nvidia Corporation (NVDA), Acuity Brands, Inc. (AYI), and NVR, Inc. (NVR). These companies each enjoyed positive returns between April 2013 and April 2018, with AYI seeing the most moderate returns while NVDA saw by far the greatest returns.  Finally, we will compare the returns of a portfolio consisting of these three assets, which are all elements of the Standard and Poor's (S&P) 1500 Composite, to the returns of an similar investment in the S&P 500 Composite during the same period.

  Netflix, Inc. is an internet based entertainment company headquartered in Los Gatos, CA and traded on NASDAQ.  While they began by offering video rentals through the mail, they have largely shifted away from this model and have began to focus on streaming video content.   This content is sourced from network and cable television programs as well as their own generated content.  They monetize this content by offering a montly subscription based service to users in exchange for unlimited streaming of their content.  Netflix has seen steadly increasing revenue in recent years due to increasing tendency towards "cord-cutting", or a reduced use traditional viewing methods and shifting towards more on-demand viewing.  With a 2017 revenue of \$11.7 billion Netflix ranks as the 10th largest internet company by revenue.  During the time period being analyzed they saw dramatic growth from an inital value of \$30.85, to a final value of \$291.94.

  Nvidia Corporation is also a technologically focused company that is also traded on NASDAQ.  Nvidia Corporation however produces computer hardware such as Advanced Micro Devices' (AMD's), Graphical Processing Unit's (GPU's) and System on a Chip units (SoCs).  Headquartered in Santa Clara, CA; Nvidia has seen rapid growth since late-2015 growing from a stock price of just \$22.00 per share in August 2015 to \$224.80 per share in April 2018.  This surge in price is likely due to increased demand for GPU's as a result of the rapid growth of Bitcoin which grew in value from \$283.73 to \$6,943.77 during this period peaking as high as \$13.850.49 during the same time frame.  The reason these two are likely tied is due to the use of GPU's in "mining" Bitcoin, a process in which people can earn this cryptocurrency by processing transactions completed utilizing the same currency.  However the volatility of Bitcoin may be cause for concern for long-term investment in Nvidia as if there is a dramaitic drop in the price of Bitcoin we will also likely see a similar dip in Nvidia stock.

  Acuity Brands, Inc. is an Atlanta, GA based company traded on the NYSE, and is in the Commercial and Industrial Lighting Industry.  They operate throughout North America, Europe and Asia andare the largest lighting manufacturer in North America in terms of market share.  During the analyzed period Acuity Brands saw steady growth until late 2016 peaking at nearly \$275.00 per share before steadily losing value through April 2018 to just $119.70.  The loss in value seems indicative of a potential long term issue as they follow repeated failures to meet projected quarterly earnings due to decreased margins.  However if they are able to correct this issue they are likely due for a long-term rebound.

  NVR, Inc. is a homebuilding and mortgage company based out of Reston Virginia, and is traded on the NYSE.  Their heavy focus in the housing industry means they will see consistently positive returns, while the housing market continues to climb which would make them a safe investment.  They would be highly succeptible to another market dip as they would see a reduction in revenue on the production front as well as the financial services front which could be compounded further if they were to see any increase in defaults on mortgages.

  The Standard and Poor's 500 is a market index based on 500 large companies which has common stock issued on either the NYSE or NASDAQ.  The S&P 500 weighs each company in it's index by using the number of shares each company has availible for public trading.  During the period the index saw overall positive returns as the value increased from \$1598 to \$2648 during the period of analysis.

## Descriptive Statistics

2.

```{r 2}
plot(proj.prices, main="Monthly Prices", col="blue", lwd=2)
plot(ret.simp, main="Monthly Simple Returns", panel=my.panel, col="blue", lwd=2)
plot(ret.cc, main="Monthly CC Returns", panel=my.panel, col="blue", lwd=2)

chart.CumReturns(ret.cc, wealth.index=TRUE, legend.loc="topleft",
                 lwd=2, main="Growth of $1")
```


  The series' appear to be time stationary for all four assets. Although they vary significantly their continuously compounding monthly returns all tended to be centered around 0.  The time independence of NVDA's variance however may be questionable as it appeared to be consistent before mid-2015 when the returns began varying greatly and the monthly prices began to increase rapidly.  Additionally, after early 2016, NVDA appears to also not have zero mean as the continuously compounding returns are above zero for the majority of the period, dipping below zero only a few times and only for brief periods.  These irregularities are likely due to the increased demand for graphics cards caused by the quick growth of bit coin during this time. There does appear to be a some positive correlation between NVDA and AYI as they seem to spike around the same periods as well as having decreasing rates of return together.


  During the time period April 2013-April 2018, a \$1 investment in NFLX, NVDA, AYI and NVR would yield a return of approximately \$6.50, \$13.00, \$1.50 and \$3.00 respectively.


3.


```{r Diagnostic Plots}
fourPanelPlot(ret.cc[, "NFLX", drop=FALSE])
fourPanelPlot(ret.cc[, "NVDA", drop=FALSE])
fourPanelPlot(ret.cc[, "AYI", drop=FALSE])
fourPanelPlot(ret.cc[, "NVR", drop=FALSE])

boxplot(ret.mat, col="cornflowerblue", main="Returns")
  abline(h=0)
```


   Of the assets, NVR appears to be the closest to being truly normally distributed as all of it's points closely follow the theoretical quantities on the Q-Q plot.  Similarly, their returns seem to closely follow a normal distribution while the box plot has a fairly compact interquartile range, with equally compact whiskers.  With NFLX and AYI however, there are some abnormalities in the densities; with NFLX having a higher concentration around the mean than would be expected in addition to having a mean slightly above 0, and AYI appearing to be slightly right skewed.  We also see slight abnormalities at the tail events with these two.  While lower end tail events for NFLX remain normal there is a higher concentration of high end tail events than would be expected with normally distributed returns.  As for AYI we see a higher concentration of tail events on both ends of the distribution than would be expected with an extreme outlier outside the lower second theoretical deviation.  These extreme events are captured in their box plots; while NFLX has an expected box plot we see an extreme outlier above 0.3, while AYI has a relatively compact interquartile range we it has an unexpectedly long upper whisker and an extreme lower outlier below -0.2.  


  Finally, we come to NVDA's returns, here we see a great deal of abnormality, it appears to be very dense around the zero mean however we see more events above, and fewer below, the mean than would be expected if returns were normal.  This is similarly apparent in the Q-Q plot where we see a very high occurrence of events above the theoretical quantile.  When looking at the box plot there is a relatively normal interquartile range with the mean nearly in the center, but with a very small lower whisker and a long upper whisker as well as an upper outlier above 0.3.


  All assets appear to be time independent although it appears that NVDA may have some time dependence during the 6th lag.


4.


```{r Descriptive Statistics}

xtable(stats.mat)
```


  NVDA has the highest expected returns at 4.76%, while AYI has the lowest expected returns of 0.86%.  The most volatile asset is NFLX at 11.9%, while the NVR is the most stable at 5.48%.  NFLX appear to be the most normally distributed with very low skewness and excess kurtosis values, and NVDA is the least normally distributed with extremely high skewness values as well as a large level of excess kurtosis.


5.
```{r Covariance}

xtable(cov.mat)
```


  There is very little covariance between the assets with the highest level of covariance NFLX and NVDA at 0.29% and the lowest covariance is between NVR and NFLX at 0.045%.  There are no surprising relationships, we may expect the covariance between NFLX and NVDA to be the largest as they are in similar industries and increased use of Netflix may relate to an increase in the demand for graphics cards.


6.

```{r Correlation}

xtable(cor.mat)

corrplot.mixed(cor.mat)
```

  The highest correlation is between NVDA and NFLX at 26.98%, while NFLX and NVR are the least correlated.  This seems to be a fairly diverse portfolio, although an substituting NFLX of NVDA for a less correlated asset would help to increase its diversity.

## Model Estimation

7.


```{r 95% CI, tidy=T}

xtable(muhat.mat)

xtable(sd.mat)
```

  NVDA has the most precisely estimated mean at nearly double the standard error, however the means of the remaining assets are not very precisely estimated as they have standard errors that are nearly as large as the estimates.  The standard deviations for the assets on the other hand are much more precisely estimated as they are each nearly ten times as large as their standard errors.

8.
```{r boot stats}
xtable(boot.mean, caption="Mean")

xtable(boot.sd, caption="Standard Deviation")
```


The bootstrap standard errors are smaller for both mean and standard deviation.  While the standard errors for estimates of the standard deviation remained the same as the number of samples increased to 10,000, the standard error estimates grew for mean as the number of samples increased.  There is not a significant difference resulting from the increased number of samples as the change in standard errors for the mean was only 5 hundred thousandths.

9.

```{r boot histograms}

plot(nvr.boot.1000.mean, Main="Mean, R=1000")
```


Although there appears to be slight skewness with the mean bootstrap, when looking at the Q-Q plot the mean appears to be largely nearly normally distributed. The central limit theorem does apply in this case, although it is not exactly normally distributed, it does seem to be approaching a normal distribution.

10.

```{r Correlation 95% CI}
xtable(rhohat.mat)
```

The most inaccurate estimates are those for the correlation between NFLX and AYI and NVR, while the most accurate are the estimates for cor(AYI,NVR) and cor(NFLX, NVDA).

11.

```{r normal VaR}

xtable(VaR.norm, caption = "Normal")
```

The Value at Risk estimates here relate estimated standard deviation from the mean.  Which corresponds to 2\*sd below the mean for 5% VaR and 3\*sd below the mean for 1% VaR.

12.

```{r bootstrap normal VaR}
xtable(VaR.boot.mat.norm.05)
xtable(VaR.boot.mat.norm.01)
```

The 5% VaR estimates are reasonably accurate although the standard error for the NVR is nearly a third of the estimated VaR.  The 1$ VaR estimates are much more accurate the all of the standard errors being about 10% of the estimated values.  NFLX has the highes VaR at both the 1% and 5% level, and NVR is the lowest.

13.

```{r empirical VaR}
xtable(VaR.empirical.mat)

xtable(VaR.boot.mat.emp.05)

xtable(VaR.boot.mat.emp.01)
```

The diffence in 5% VaR for NVDA and NFLX is rather significant, while it is not very significant for AYI and NVR.  For 1% VaR however the only significant difference between empirical and normal VaR is for NVDA.  This is rather expected as NVDA is the furthest from normal so the further towards tail events we get the more accurate the estimates will be for the assets with returns that are most normal.


## Portfolio Theory

14.

```{r Sharpe Ratio}
xtable(sharpe)
```

NVDA has the highest slope, and all of the assets other than NVR had precisely estimated slopes.

15.

```{r plot volatility er}
plot(sd.vals, muvals, xlim=c(0, 0.15), ylim=c(0, 0.050),
     ylab="Expected Return", xlab="Standard Deviation",
     cex=1.25, pch=16, col=c("forest green","cornflowerblue","red","black"))
  points(sd.sp500, er.sp500, lwd=2, col="gold", pch=16)
  points(0, rf, cex=2, pch=16, col="green")
  text(0, rf, labels="Rf", pos=3)
  text(sd.vals, muvals, labels=colnames(ret.cc),
       pos=c(1:4))
  text(sd.sp500, er.sp500, labels="S&P 500", pos=1)

```

NVDA has the highest risk-adjusted return, while AYI has the lowest.  Surprisingly AYI does not appear to perform any better than the S&P 500.

16.

```{r GMV}
gmv

plot(gmv, col="cornflowerblue")
```

All asset weights in the Global minimum variance portfolio are greater than zero.

17.

```{r GMV VaR}
xtable(gmv.VaR)
```

The Value at Risk of the Global Minimum Variance Portfolio is significantly lower than any individual asset in the portfolio.

18.

```{r no short GMV}
plot(gmin.port.ns, col="cornflowerblue")
gmin.port.ns
```

19.

```{r ns GMV VaR}
xtable(VaR.gmin.mat)
```

Since the GMV does not have any short sales we see no difference in the Value at Risk.

20.

```{r Efficient portfolio frontier}

xtable(as.matrix(e.port.max))

plot(ef, plot.assets=F, col="blue", lwd=2)
  abline(v=0)
  abline(h=0)
  points(gmv$sd, gmv$er, col="orange", lwd=2, pch=16) ## gmv
  points(sd.vals, muvals, pch=16, labels=colnames(ret.cc), col=c("forest green","cornflowerblue","red","black"))
  text(sd.vals, muvals, labels=colnames(ret.cc), col=c("forest green","cornflowerblue","red","black"), pos=4)
  text(gmv$sd, gmv$er, labels = "GMV", pos=2, col="orange")
```

21.

```{r tangency portfolio}

summary(tan.port, risk.free=rf)

plot(tan.port, col="cornflowerblue")

plot(ef, plot.assets = F, col="blue",lwd=2)
  points(0, rf, col="green", pch=16)
  points(gmv$sd, gmv$er, col="orange", lwd=2, pch=16) ## gmv
  points(tan.port$sd, tan.port$er, col="purple", pch=16)
  points(sd.vals, muvals, col=c("forest green","cornflowerblue","red","black"), pch=16)
  points(tan.ef.sd, tan.ef.er, lwd=2, col="purple", pch=20)
  points(0, rf, col="green", pch=16)
  text(sd.vals, muvals, labels=colnames(ret.cc), col=c("forest green","cornflowerblue","red","black"), pos=c(4,1,4,4))
  text(0, rf, "RF", pos=4)
  text(gmv$sd, gmv$er, labels = "GMV", pos=4, col="orange")
  text(tan.port$sd, tan.port$er+0.0008, "Tan Port", pos=2, col="purple")

```

The Sharpe Ratio of the tangency portfolio is greater than that of any of the individual assets.

22.

```{r r tangency portfolio no shorts}

plot(ef, plot.assets=F, col=c("blue"), lwd=2)
  points(sd.vals, muvals, col=c("forest green","cornflowerblue","red","black"), pch=16)
  points(ef.ns$sd, ef.ns$er, col="red", pch=16)
  points(gmv$sd, gmv$er, col="orange", lwd=2, pch=16) ## gmv
  text(sd.vals, muvals, labels=colnames(ret.cc), col=c("forest green","cornflowerblue","red","black"), pos=c(4,1,4,3))
  text(gmv$sd, gmv$er, labels = "GMV", pos=2, col="orange")

```

The efficient portfolio with no shorts is very similar for much of its frontier.

```{r portfolio exoected return of 0.02}
plot(ef, plot.assets=F, col=c("blue"), lwd=2)
  points(sd.vals, muvals, col=c("forest green","cornflowerblue","red","black"), pch=16)
  points(ef.ns$sd, ef.ns$er, col="red", pch=16)
  points(gmv$sd, gmv$er, col="orange", lwd=2, pch=16) ## gmv
  points(ns.ef.02$sd, ns.ef.02$er, col="green", pch=16)
  text(ns.ef.02$sd, ns.ef.02$er, col="green", labels = "E[R]=2%", pos=2)
  text(sd.vals, muvals, labels=colnames(ret.cc), col=c("forest green","cornflowerblue","red","black"), pos=c(4,1,4,3))
  text(gmv$sd, gmv$er, labels = "GMV", pos=2, col="orange")
```

Because 2% returns is below this portfolios GMV the two portfolios are identical.

23.

```{r No Short Tan Port}
xtable(stats.tan.mat)
xtable(tan.wts.mat)
```

The tangent portfolio without shorts has a sharpe ratio that is roughly .023 less than the tangent porfolio with shorts. This portfolio also has an expected rate of return that is roughly .75% a month less with 1% less volatility.  Surprisingly there is a significant share of AYI in the no shorts portfolio, an asset that was shorted in the tangent portfolio that allowed shorts, as well as an increased share of NFLX, while decreasing NVDA and NVR.

## Can you beat the S&P 500?

24.

```{r Equity Curves}
chart.CumReturns(sp500.tan, wealth.index=TRUE, legend.loc="topleft",
                 lwd=2, main="Growth of $1")
```

Between April 2013 and April 2018 a \$1.00 investment the S&P 500 would have yielded a return of aproximately \$1.75, while a similar investment in the tangent portfolio would have yielded a return of nearly $7.25 over the same period.  The tangent portfolio was able to outperform the S&P 500 due to the rapid growth of NVDA as well as the steady growth by NFLX and NVR over the same period.

25.

```{r portfolio SD equal to SD of SP500}
xtable(sp.var.port.wts)
xtable(sp.tan.mat)
chart.CumReturns(portsd.sp, wealth.index=TRUE, legend.loc="topleft",
                 lwd=2, main="Growth of $1")
```

This investor is very risk averse, as the portfolio is not only below the Global Minimum Variance portfolio, but also is composed more of the risk free rate than of the tangency portfolio.  The efficient portfolio with the same risk level as the S&P 500 was still able to outperform the S&P 500 over the period April 2013-April 2018, earning nearly \$3.00 on a \$1.00 investment, outperforming a similar investment in the S&P 500 by over \$1.00.
