

rm(list=ls())
setwd("C:/Users/LAB4_36/Desktop")
data=read.csv("seaice.csv")
attach(data)
plot(Arctic,type='l',xaxt="n")
axis(1,at=seq(0,252,12))
M=ts(Arctic, start=c(1990,1),end=c(2010,12),frequency=12);M
s=decompose(M,type="additive")$figure;s
ds=Arctic-rep(s,21);ds
ds1=array(t(ds))
plot(ds,type='l')
M1=matrix(ds,ncol=12,byrow=T);M1
yavg=apply(M1,1,mean);yavg
plot(yavg,type='l')
n=1:21
summary(lm(yavg~n))

#lfit=lm(ds1~t)
#summary(lfit)
#qfit=lm(ds1~t+I(t^2))
#summary(qfit)

t=7:258
t1=t-0.5
Tt=10.037667-0.064268*t1/12
dsdt=ds1-Tt
plot(dsdt,type='l',xaxt='n')
axis(1,at=seq(1,255,1)
u=acf(dsdt)
u

arima(dsdt,order=c(1,0,0))
#ar()
?arima
?ar