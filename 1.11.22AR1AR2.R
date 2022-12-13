rm(list=ls())
setwd("C:/Users/YENISI/OneDrive/Documents/R/Time Series")
data=read.csv("sea_ice.csv")
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
#plot(yavg,type='l')
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

#finding r1 using formula
xbar=mean(dsdt)
length(dsdt)
c1=sum((dsdt[-252]-mean(dsdt))*(dsdt[-1]-mean(dsdt)))/252;c1
c0=sum((dsdt-mean(dsdt))^2)/252;c0
r1=c1/c0;r1


plot(dsdt,type='l',xaxt='n')
axis(1,at=seq(1,255,1)

#finding r1 using ACF rho(1)=r1
rho=acf(dsdt);rho
#r1=rho[1];r1 #alpha_hat
alpha_hat=0.695
r1=0.695
#xbar=mu_hat
alpha.Xtminus1=alpha_hat*(dsdt[-252]);alpha.Xtminus1
rss=mean((dsdt[-1]-alpha.Xtminus1)^2);rss


#AR2

#r2=rho[2];r2
r2=0.449
alpha1=r1*(1-r2)/(1-r1^2);alpha1
alpha2=(r2-r1^2)/(1-r1^2);alpha2
Xt=dsdt[-c(1,2)]
Xt_1=dsdt[-c(1,252)]
Xt_2=dsdt[-c(251,252)]
rss=mean((Xt-alpha1*Xt_1-alpha2*Xt_2)^2);rss