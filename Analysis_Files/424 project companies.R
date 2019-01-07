install.packages("tseries")
install.packages("zoo")
install.packages("IntroCompFinR")
install.packages("lubridate")
install.packages("PerformanceAnalytics")
install.packages("corrplot")
install.packages("quantmod")
install.packages("boot")
install.packages("dygraphs")
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

firms <- read.csv("SP1500 Firms 4-2013-2018.csv", na.strings = "")

## remove assets that were not in SP1500 for the entire time frame

start.date = "2013-04-01"
end.date = "2018-04-30"

firms$from <- as.Date(as.character(firms$from), "%Y%m%d")

firms$thru <- as.Date(as.character(firms$thru), "%Y%m%d")

a.firms <- firms[firms$from <= as.Date(start.date),]

a.firms <- a.firms[is.na(a.firms$thru),]

to <- na.omit(firms[firms$thru >= as.Date(end.date),])

a.firms <- rbind(a.firms,to)

a.firms <- a.firms[order(as.numeric(row.names(a.firms))),]

## load asset names
assets <- as.character(a.firms$co_tic)

## remove incomplete data

# HPR

assets <- assets[-502]

# BKNG

assets <- assets[-148]

# BH-A

assets <- assets[-138]

## load empty frame for data

asset.m <- list()
incomplete <- list()
name.keep <- list()
c <- 0

## load all complete data

for(i in 1:length(assets)){
  a <- get.hist.quote(assets[i], start=start.date,end=end.date, quote="AdjClose",
                            provider="yahoo", origin = "1970-01-01",
                            compression="m", retclass="zoo")
  if (nrow(a) == 61) {
    asset.m[[i-c]] <-a
    name.keep[i-c] <- assets[i]
  } else {
    c <- c+1
    incomplete[c] <- assets[i]
  }
}

a.m <- as.data.frame(asset.m)

colnames(a.m) <- name.keep

adj.close <- zoo(as.data.frame(a.m))

rownames(adj.close) <- rownames(a.m)

index(adj.close) <- as.yearmon(index(a))


## cc returns

rets <- na.omit(Return.calculate(adj.close), "log")

rets.df <- coredata(rets)

rownames(rets.df) <- as.character(index(rets))

mu2 <- as.data.frame(apply(rets,2,mean))

## simple returns
View(simp.rets.df)

simp.rets <- exp(rets)-1

simp.rets.df <- coredata(simp.rets)

rownames(simp.rets.df) <- as.character(index(simp.rets))

cum <- cumprod(1+simp.rets)

cumret <- cum[60,]

cum.ret <- Return.cumulative(adj.close, geometric = F)

colnames(cum.ret) <- colnames(rets)

plot(cum.ret, range(cum.ret))

range(cum.ret)

cum.ret1 <- Return.cumulative(rets)

range(cum.ret1)

cum.ret2 <- Return.cumulative(simp.rets, geometric = F)

range(cum.ret2)

cum.ret3 <- as.data.frame(cumret)
colnames(cum.ret3) <- colnames(cum.ret)
## select top 20 cummulative returns
b <- rets
keep <- matrix(rep(0,20), ncol=1)
obs <- list()
m <- colMeans(rets)
for(i in 1:20){
  a <- which.max(m)
  obs[i] <- a
  keep[i] <- m[a]
  name[i] <-names(a)
  m <- m[-a]
}

rownames(keep) <- name
b <- as.data.frame(rets)[, name]


apply(er.k, 2, mean)
View(keep)
kee <- keep
names <-as.character(keep[,1])

name
obs
names
unlist(paste(unlist(obs), collapse=","))
er.k <- cum[, c(324,634,1113,957,1021,973,755,11,761,194,739,914,541,689,1073,959,63,765,53,321)]
View(er.k)
mu3 <- as.data.frame(apply(er.k, 2, mean))

rownames(sd) <- names(cum[, c(324,634,1113,957,1021,973,755,11,761,194,739,914,541,689,1073,959,63,765,53,321)])
View(mu3)
a <- sort(sd, decreasing=TF)

a <- intersect(mean,sd)

keep1 <- matrix(rep(0,20), ncol=1)
obs1 <- list()
name1 <- name

s <- apply(b, 2, sd)

View(sd)

keep <- kee

for(i in 1:20){
  a <- which.min(s)
  obs1[i] <- a
  keep1[i] <- s[a]
  name1[i] <-names(a)
  s <- s[-a]
}
rownames(keep1) <- name1
b <- cbind(keep,keep1)
View(keep1)
View(keep)
## create df of returns for firms

proj.prices <- adj.close[,keep[c(1:4),1]]

proj.prices

## cc returns

proj.ret.cc <- na.omit(Return.calculate(proj.prices), "log")

proj.ret.cc.df <- coredata(rets)

rownames(proj.ret.cc.df) <- as.character(index(proj.ret.cc))
mu.vec <- apply(proj.ret.cc,2,mean)
# NVR

nvr.ret.cc <- as.data.frame(proj.ret.cc[,1])
colnames(nvr.ret.cc) <- "NVR"

# GOOGL

googl.ret.cc <- as.data.frame(proj.ret.cc[,2])
colnames(googl.ret.cc) <- "GOOGL"

# AMZN

amzn.ret.cc <- as.data.frame(proj.ret.cc[,3])
colnames(amzn.ret.cc) <- "AMZN"

# AYI

ayi.ret.cc <- as.data.frame(proj.ret.cc[,4])
colnames(ayi.ret.cc) <- "AYI"

## simple returns

proj.ret.simp <- exp(proj.ret.cc)-1

proj.ret.cc.df <- coredata(proj.ret.cc)

names

# NVR

nvr.ret.s <- as.data.frame(proj.ret.simp[,1])
colnames(nvr.ret.s) <- "NVR"

# GOOGL

googl.ret.s <- as.data.frame(proj.ret.simp[,2])
colnames(googl.ret.s) <- "GOOGL"

# AMZN

amzn.ret.s <- as.data.frame(proj.ret.simp[,3])
colnames(amzn.ret.s) <- "AMZN"

# AYI

ayi.ret.s <- as.data.frame(proj.ret.simp[,4])
colnames(ayi.ret.s) <- "AYI"


# Equity Curves -----------------------------------------------------------

ec.port <- cumprod(1+proj.ret.simp)

ec.nvr <- cumprod(1+nvr.ret.s)

ec.googl <- cumprod(1+googl.ret.s)

ec.amzn <- cumprod(1+amzn.ret.s)

ec.ayi <- cumprod(1+ayi.ret.s)

#
# plot data
#
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}

plot(proj.prices, main="Monthly Prices", col="blue", lwd=2)
plot(proj.ret.simp, main="Monthly Simple Returns", panel=my.panel, col="blue", lwd=2)
plot(proj.ret.cc, main="Monthly CC Returns", panel=my.panel, col="blue", lwd=2)
plot.zoo(ec.port, main="Equity Curves", plot.type = "single", panel=my.panel, col=c("blue", "black", "red", "gold"), lwd=2)
legend(x="topleft", legend=names(ec.port),
       col=c("blue", "black", "red", "gold"), lwd=2, cex=0.75)
ec.port
# plot growth of $1 over the five years using PerformanceAnalytics function
# chart.CumReturns
chart.CumReturns(proj.ret.simp, wealth.index=TRUE, legend.loc="topleft", 
                 lwd=2, main="Growth of $1") 

#
# Create matrix of return data and compute pairwise scatterplots
#
ret.mat = coredata(proj.ret.cc)

# Note: expand graphics window in Rstudio to see legend

# show a 4 panel plot for vfinx returns - notice the use of drop=FALSE.
# This preserves the column name
for (i in 1:4){
  fourPanelPlot(proj.ret.cc[, i, drop=FALSE])
}

#
# boxplots of returns
#

boxplot(ret.mat, main="Returns", col="cornflowerblue")

#
# compute descriptive statistics
#
muhat.vals = colMeans(proj.ret.cc)
sd.vals = apply(proj.ret.cc, 2, sd)
skew.vals = apply(proj.ret.cc, 2, skewness)
ekurt.vals = apply(proj.ret.cc, 2, kurtosis)
cov.mat = var(proj.ret.cc)
cor.mat = cov2cor(cov.mat)

covhat.vals = cov.mat[lower.tri(cov.mat)]
rhohat.vals = cor.mat[lower.tri(cor.mat)]
names(covhat.vals) <- names(rhohat.vals) <- 
  c("NVR,GOOGL", "NVR,AMZN", "NVR,AYI", 
    "GOOGL,AMZN", "GOOGL,AYI", 
    "AMZN,AYI")
rhohat.vals

# empirical quantiles for VaR calculations
q.vals = apply(proj.ret.cc, 2, quantile, prob=c(0.01,0.05))

# display results in a table
stats.mat = rbind(muhat.vals, 
                  sd.vals,
                  skew.vals,
                  ekurt.vals,
                  q.vals)
rownames(stats.mat) = c("Mean", "Std Dev", "Skewness", 
                        "Excess Kurtosis", "1% Quantile", 
                        "5% Quantile")

# print statistics
stats.mat
cov.mat
cor.mat
#

#Bootstrap

bootstraped.mean = function(x, idx) {
  muhat <- mean(x[idx])
}

bootstraped.sd = function(x, idx) {
  sigmahat <- sd(x[idx])
}

## NVR
set.seed(1)

nvr.boot.mean = boot(ret.mat[, "NVR"], 
                      statistic=bootstraped.mean, R=1000)

nvr.boot.mean

nvr.boot.sd = boot(ret.mat[, "NVR"], 
                statistic=bootstraped.sd, R=1000)

nvr.boot.sd

boot.ci(nvr.boot, conf = 0.95, type = c("norm","perc"))
plot(nvr.boot)

## GOOGL
set.seed(1)
googl.boot = boot(ret.mat[, "GOOGL"], 
                       statistic=bootstraped.boot, R=10000)
googl.boot

boot.ci(googl.boot, conf = 0.95, type = c("norm","perc"))
plot(googl.boot)

## AMZN
set.seed(1)
amzn.boot = boot(ret.mat[, "AMZN"], 
                statistic=bootstraped.boot, R=1000)
amzn.boot

boot.ci(amzn.boot, conf = 0.95, type = c("norm","perc"))
plot(amzn.boot)

## AYI
set.seed(1)
ayi.boot = boot(ret.mat[, "AYI"], 
                  statistic=bootstraped.boot, R=10000)
ayi.boot

boot.ci(ayi.boot, conf = 0.95, type = c("norm","perc"))
plot(ayi.boot)

# SE and 95% confidence values for estimated correlations
n.obs = nrow(proj.ret.cc)
SE.rhohat = (1 - rhohat.vals^2)/sqrt(n.obs)
rho.lower = rhohat.vals - 2*SE.rhohat
rho.upper = rhohat.vals + 2*SE.rhohat

rhohat.mat = cbind(rhohat.vals, SE.rhohat, rho.lower, rho.upper)
colnames(rhohat.mat) = c("rho.hat", "SE", "LCL (0.95)", "UCL (0.95)")
rhohat.mat


#
# VaR analysis
#

# function to compute normal and empirical VaR for a matrix of cc returns

Value.at.Risk = function(x, p=0.05, w=100000, 
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
VaR.normal.05 = Value.at.Risk(ret.mat, p=0.05, 
                              method="normal",
                              return.type="cc")
VaR.normal.01 = Value.at.Risk(ret.mat, p=0.01)

# empirical VaR
VaR.empirical.05 = Value.at.Risk(ret.mat, p=0.05, 
                                 method="empirical",
                                 return.type="cc")
VaR.empirical.01 = Value.at.Risk(ret.mat, p=0.01, 
                                 method="empirical",
                                 return.type="cc")

VaR.normal.mat = cbind(VaR.normal.05, VaR.normal.01)
VaR.empirical.mat = cbind(VaR.empirical.05, VaR.empirical.01)

# function to bootstrap VaR for a single asset

ValueAtRisk.boot = function(x, idx, p=0.05, w=100000,
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
VaR.boot.amzn = computeSEconfintVaR(ret.mat[, "AMZN", drop=FALSE])
VaR.boot.nvr = computeSEconfintVaR(ret.mat[, "NVR", drop=FALSE])
VaR.boot.googl = computeSEconfintVaR(ret.mat[, "GOOGL", drop=FALSE])
VaR.boot.ayi = computeSEconfintVaR(ret.mat[, "AYI", drop=FALSE])

VaR.boot.mat = rbind(VaR.boot.amzn,
                     VaR.boot.nvr,
                     VaR.boot.googl,
                     VaR.boot.ayi)
rownames(VaR.boot.mat) = colnames(proj.ret.cc)
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

VaR.normal.05.annual = Value.at.Risk.annual(ret.mat, p=0.05, 
                                            method="normal",
                                            return.type="cc")
VaR.normal.01.annual = Value.at.Risk.annual(ret.mat, p=0.01, 
                                            method="normal",
                                            return.type="cc")

# empirical VaR
VaR.empirical.05.annual = Value.at.Risk.annual(ret.mat, p=0.05, 
                                               method="empirical",
                                               return.type="cc")
VaR.empirical.01.annual = Value.at.Risk.annual(ret.mat, p=0.01, 
                                               method="empirical",
                                               return.type="cc")

VaR.normal.annual.mat = cbind(VaR.normal.05.annual, VaR.normal.01.annual)
VaR.empirical.annual.mat = cbind(VaR.empirical.05.annual, VaR.empirical.01.annual)


#
# Portfolio theory allowing for short sales
#
rf = 0.005/12

SharpeRatios = (muhat.vals - rf)/sd.vals
SharpeRatios = as.matrix(SharpeRatios, ncol=1)
colnames(SharpeRatios) = "Sharpe"
SharpeRatios

# compute bootstrap standard error and 95% ci for sharpe ratio
# function to bootstrap VaR
sharpeRatio.boot = function(x, idx, risk.free) {
  muhat = mean(x[idx])
  sigmahat = sd(x[idx])
  sharpeRatio = (muhat - risk.free)/sigmahat
  sharpeRatio
}

## AMZN

sharpe.amzn.boot = boot(ret.mat[, "AMZN"], 
                        statistic=sharpeRatio.boot, R=1000, risk.free=rf)
sharpe.amzn.boot
boot.ci(sharpe.amzn.boot, conf = 0.95, type = c("norm","perc"))
plot(sharpe.amzn.boot)

## GOOGL

sharpe.googl.boot = boot(ret.mat[, "GOOGL"], 
                        statistic=sharpeRatio.boot, R=1000, risk.free=rf)
sharpe.googl.boot
boot.ci(sharpe.googl.boot, conf = 0.95, type = c("norm","perc"))
plot(sharpe.googl.boot)

## NVR

sharpe.nvr.boot = boot(ret.mat[, "NVR"], 
                        statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.nvr.boot
boot.ci(sharpe.nvr.boot, conf = 0.95, type = c("norm","perc"))
plot(sharpe.nvr.boot)

## AYI

sharpe.ayi.boot = boot(ret.mat[, "AYI"], 
                        statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.ayi.boot
boot.ci(sharpe.ayi.boot, conf = 0.95, type = c("norm","perc"))
plot(sharpe.ayi.boot)

# bootstrap SE and 95% CI for assets Sharpe Ratio

computeSEconfintSharpe = function(x, risk.free) {
  Sharpe.boot = boot(x, statistic=sharpeRatio.boot, R=1000, risk.free=risk.free)
  Sharpe.hat = Sharpe.boot$t0
  SE.Sharpe = sd(Sharpe.boot$t)
  CI.Sharpe = boot.ci(Sharpe.boot, conf = 0.95, type="norm")$normal
  CI.Sharpe = CI.Sharpe[-1]
  ans = c(Sharpe.hat, SE.Sharpe, CI.Sharpe)
  names(ans) = c("Sharpe", "SE", "LCL (0.95)", "UCL (0.95)")
  return(ans)
}

Sharpe.boot.amzn = computeSEconfintSharpe(ret.mat[, "AMZN", drop=FALSE], risk.free=rf)
Sharpe.boot.nvr = computeSEconfintSharpe(ret.mat[, "NVR", drop=FALSE], risk.free=rf)
Sharpe.boot.googl = computeSEconfintSharpe(ret.mat[, "GOOGL", drop=FALSE], risk.free=rf)
Sharpe.boot.ayi = computeSEconfintSharpe(ret.mat[, "AYI", drop=FALSE], risk.free=rf)

Sharpe.boot.mat = rbind(Sharpe.boot.amzn,
                        Sharpe.boot.nvr,
                        Sharpe.boot.googl,
                        Sharpe.boot.ayi)

rownames(Sharpe.boot.mat) = colnames(proj.ret.cc)
Sharpe.boot.mat   

# plot return-risk tradeoff and compute Sharpe ratios
#
## risk free rate

plot(sd.vals, muhat.vals, xlim=c(0, 0.15), ylim=c(0, 0.050),
     ylab="Expected Return", xlab="Standard Deviation",
     cex=2, pch=16, col="cornflowerblue")
points(0, rf, cex=2, pch=16, col="green")
text(0, rf, labels="Rf", pos=3)
text(sd.vals, muhat.vals, labels=colnames(proj.ret.cc),
     pos=3)

## compute global minimum variance portfolio

gmin.port <- globalMin.portfolio(muhat.vals, cov.mat)
attributes(gmin.port)
print(gmin.port)
summary(gmin.port, risk.free=rf)
plot(gmin.port, col="cornflowerblue")

# annualized mean and sd
12*gmin.port$er
sqrt(12)*gmin.port$sd

# 1% and 5% VaR
w0=100000
q.05 = (gmin.port$er + gmin.port$sd*qnorm(0.05))
VaR.gmin.05 = (exp(q.05) - 1)*w0
q.01 = (gmin.port$er + gmin.port$sd*qnorm(0.01))
VaR.gmin.01 = (exp(q.01) - 1)*w0
VaR.gmin.05
VaR.gmin.01

## compute global minimum variance portfolio with no short sales
gmin.port.ns <- globalMin.portfolio(muhat.vals, cov.mat, shorts=FALSE)
attributes(gmin.port.ns)
print(gmin.port.ns)
summary(gmin.port.ns, risk.free=rf)
plot(gmin.port.ns, col="cornflowerblue")

# compare weights
wts.mat = cbind(gmin.port$weights,
                gmin.port.ns$weights)
colnames(wts.mat) = c("gmin", "gmin.ns")

# annualized mean and sd
12*gmin.port.ns$er
sqrt(12)*gmin.port.ns$sd

# 1% and 5% VaR
w0=100000
q.05 = (gmin.port.ns$er + gmin.port.ns$sd*qnorm(0.05))
VaR.gmin.ns.05 = (exp(q.05) - 1)*w0
q.01 = (gmin.port.ns$er + gmin.port.ns$sd*qnorm(0.01))
VaR.gmin.ns.01 = (exp(q.01) - 1)*w0
VaR.gmin.ns.05
VaR.gmin.ns.01

VaR.gmin.mat = cbind(c(gmin.port$er*12, gmin.port$sd*sqrt(12), 
                       VaR.gmin.05, VaR.gmin.01),
                     c(gmin.port.ns$er*12, gmin.port.ns$sd*sqrt(12), 
                       VaR.gmin.ns.05, VaR.gmin.ns.01))
rownames(VaR.gmin.mat) = c("Annual Mean", "Annual SD", "VaR.05", "VaR.01")
colnames(VaR.gmin.mat) = c("Shorts", "No Shorts")

## efficient portfolio with target return equal to max returns
target.return <- max(muhat.vals)
e.port.max<- efficient.portfolio(muhat.vals, cov.mat, target.return)
e.port.max
summary(e.port.max, risk.free=rf)
plot(e.port.max, col="cornflowerblue")

## compute tangency portfolio
tan.port <- tangency.portfolio(muhat.vals, cov.mat, rf)
tan.port
summary(tan.port, risk.free=rf)
plot(tan.port, col="cornflowerblue")
muhat.vals
# annualized mean and sd
12*tan.port$er
sqrt(12)*tan.port$sd

## compute tangency portfolio with no short sales
tan.port.ns <- tangency.portfolio(muhat.vals, cov.mat, rf, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=rf)
plot(tan.port.ns, col="cornflowerblue")

# annualized mean and sd
12*tan.port.ns$er
sqrt(12)*tan.port.ns$sd

# compare weights
wts.mat = cbind(tan.port$weights,
                tan.port.ns$weights)
colnames(wts.mat) = c("tan", "tan.ns")

stats.tan.mat = cbind(c(tan.port$er*12, tan.port$sd*sqrt(12)), 
                      c(tan.port.ns$er*12, tan.port.ns$sd*sqrt(12)))
rownames(stats.tan.mat) = c("Annual Mean", "Annual SD")
colnames(stats.tan.mat) = c("Shorts", "No Shorts")


# plot portfolio weights
par(mfrow=c(2,1))
plot(gmin.port, col="cornflowerblue")
plot(tan.port, col="cornflowerblue")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(gmin.port.ns, col="cornflowerblue")
plot(tan.port.ns, col="cornflowerblue")
par(mfrow=c(1,1))

## compute efficient frontier
ef <- efficient.frontier(muhat.vals, cov.mat, alpha.min=-1, 
                         alpha.max=1.5, nport=20)

## plot efficient frontier allowing shortsales
plot(ef, plot.assets=TRUE, col="blue", lwd=2)
points(gmin.port$sd, gmin.port$er, col="orange", lwd=2)
points(tan.port$sd, tan.port$er, col="red", lwd=2)
text(tan.port$sd, tan.port$er, labels="tangency", pos=4)
sr.tan = (tan.port$er - rf)/tan.port$sd
abline(a=rf, b=sr.tan, col="green", lwd=2)
abline(v=0)
points(0, rf, col="green", lwd=2)
text(0, rf, labels="rf", pos=4)

## compute efficient frontier not allowing short sales
ef.ns <- efficient.frontier(muhat.vals, cov.mat, alpha.min=0, 
                            alpha.max=1, nport=20, shorts=FALSE)

## plot efficient frontier not allowing shortsales
plot(ef.ns, plot.assets=TRUE, col="blue", lwd=2)
points(gmin.port.ns$sd, gmin.port$er, col="orange", lwd=2)
points(tan.port.ns$sd, tan.port$er, col="red", lwd=2)
sr.tan.ns = (tan.port.ns$er - rf)/tan.port.ns$sd
abline(a=rf, b=sr.tan.ns, col="green", lwd=2)

# show short sale and no short sale frontiers together
plot(ef, plot.assets=TRUE, col="blue", lwd=2)
points(ef.ns$sd, ef.ns$er, type="b", col="red", lwd=2)

# cost in er for portfolio with sd = 0.02 (0.4%)
ef$er["port 4"] - ef.ns$er["port 18"]
ef$sd["port 4"]
ef.ns$sd["port 18"]

### Comparing to the S&P 500
# Do this part yourself.

sp500 <- get.hist.quote("^gspc", start=start.date,end=end.date, quote="AdjClose",
                    provider="yahoo", origin = "1970-01-01",
                    compression="m", retclass="zoo")

ret.sp500.s <- na.omit(Return.calculate(sp500, "simple"))

ret.sp500.cc <- na.omit(Return.calculate(sp500, "log"))

#equity curve

ec.sp500 <- cumprod(1+ret.sp500.s)

index(ec.sp500) <- as.yearmon(index(ec.sp500))

plot(ec.sp500, main="Equity Curves", panel=my.panel, col="blue", lwd=2)

# Tangency Portfolio equity curve

ec.tan <- cumprod(1 + port_return_xts)

ec <- merge(ec.tan, ec.sp500)

plot(ec, main="Equity Curves", panel=my.panel, col="blue", lwd=2)

# Weigh your tangency portfolio

mytan.port <- c(0.5891,-0.0939,0.5348,-0.0301)
tan.port.returns <- proj.ret.simp*mytan.port
port_return <- rowSums(tan.port.returns[,c("AMZN", "NVR", "GOOGL","AYI")])
port_return_xts <- xts(port_return,index(proj.ret.simp))

index(ret.sp500.s) <- as.yearmon(index(sp500[-1]))
index(ret.sp500.cc) <- as.yearmon(index(sp500[-1]))

#plot risk return tradeoffs of all assets

sd.sp500 <- sd(ret.sp500.cc)
er.sp500 <- mean(ret.sp500.cc)


plot(ef, plot.assets=TRUE, col="blue", lwd=2, pch=16) ## efficient frontier
  points(gmin.port$sd, gmin.port$er, col="orange", lwd=2, pch=16) ## gmv
  points(sd.sp500, er.sp500, lwd=2, col="gold", pch=16)  ## S&P 500 Return-variance
  sr.tan = (tan.port$er - rf)/tan.port$sd  ## tangency portfolio
  abline(a=rf, b=sr.tan, col="green", lwd=2)
  abline(v=0)
  points(tan.port$sd, tan.port$er, col="red", lwd=2, pch=16) ## tangency portfolio
  points(sd.sp500, sr.tan*sd.sp500+rf, lwd=2, col="green", pch=16) #var(ep) = var(SP500)
  points(0, rf, col="green", lwd=2)  ## risk free rate
  text(sd.sp500, er.sp500, labels="S&P 500", pos=4)
  text(tan.port$sd, tan.port$er, labels="tangency", pos=4)
  text(gmin.port$sd, gmin.port$er, labels="GMV", pos=2, offset =.5)
  text(sd.sp500, sr.tan*sd.sp500+rf+.002, labels=expression("Var[R"["ep"]*"] = Var[S&P 500]"), cex = 0.5,pos=1, offset = .5)
  text(0, rf, labels="rf", pos=4)



## Equity Curves
sd.sp500
ec.sp500
ec.ep <- efficient.portfolio(muhat.vals-rf, cov.mat, sr.tan*(sd.sp500-rf))

ecs <- ec.ep$weights*proj.ret.cc

ep.ret <- rowSums(ecs)

eqcrve <- cumprod(1+ep.ret)

ec <- merge(ec.sp500, eqcrve)

plot.zoo(ec, plot.type = "single", col=c("red", "blue"), lwd=2)  


# Efficient Portfolio with var = var[SP500]

sd.sp500

sd.sp500 <- apply(ret.sp500.cc, 1, sd)

x.tan <- sd.sp500/tan.port$sd

x.rf <- 1-x.tan

tan.port.returns.cc <- proj.ret.cc*mytan.port


a <- exp(eff.port.returns-1)
sd(returns)
eff_port_return <- rowSums(eff.port.returns)
eff_port_return_xts <- xts(port_return,index(proj.ret.cc))


returns <- rf+x.tan*(rowSums(tan.port.returns.cc)-rf)
apply(eff.port.returns,2,sd)
mu.pe <- rf+x.tan*(tan.port$er - rf)
mu.pe

sr.tan*sd.sp500

var.sp500 <- apply(ret.sp500.cc, 1, var)
er.sp500 <- mean(ret.sp500.cc)

target.var <- var.sp500
e.port.min<- efficient.portfolio(muhat.vals, cov.mat, target.var)
e.port.min
summary(e.port.min, risk.free=rf)
plot(e.port.min, col="cornflowerblue")
?portfolio.optim

er.eport.sp500.var <- e.port.min$er
sd.eport.sp500.var <- e.port.min$sd
