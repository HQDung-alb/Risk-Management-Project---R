library(fBasics)
library(tseries)
library(timeSeries)
library(rugarch)
library(fGarch)
vndirect=file.choose()
da=read.csv(vndirect, header=T)
head(da)
ts.plot(da$Price)
vnd=da[,2]
r_vnd=diff(log(vnd))
n_vnd=-r_vnd
basicStats(n_vnd)

normalTest(n_vnd, method=c("jb"))
adf.test(n_vnd)
kpss.test(n_vnd)

acf(n_vnd)
Box.test(n_vnd, lag=5)
Box.test(n_vnd, lag=5, type="Ljung-Box")

spec1=ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model="iGARCH", garchOrder=c(1,1)))
m1=ugarchfit(spec = spec1,data=n_vnd)
View(m1)
m1

ugarchforecast(m1, n.ahead = 5)
v1=0+qnorm(0.99)*0.04314
VaR1=v1*3000000000
VaR1

setwd("G:/Document/Tài liệu môn/QTRR/new data 2021")
source("Rmeasure.R")
RMeasure(0, 0.04314)

spec2=ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(model="fGARCH",submodel="GARCH", garchOrder=c(1,1)))
m2=garchFit(~arma(0,0)+garch(1,1),data=n_vnd,trace = F)
m2
predict(m2,5)
RMeasure(0.001343076, 0.03719489)
VaR2=0.08787133*3000000000
VaR2

VaR3=(-quantile(n_vnd,0.01))*3000000000
VaR3

set.seed(12345)
mu=mean(n_vnd)
sigma=stdev(n_vnd)
sim=rnorm(5000, mean=mu, sd=sigma)
VaR4=(-quantile(sim,0.01))*3000000000
VaR4



























