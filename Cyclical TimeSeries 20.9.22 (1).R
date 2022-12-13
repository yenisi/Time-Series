rm(list=ls())
setwd("C:/Users/YENISI/OneDrive/Documents/R/Time Series")
data=read.csv(file="Cyclical(1).csv");data
attach(data)
x=Plantation.Forest
x
plot(x,type='l')

M=matrix(x,ncol=4,byrow=T);M
colnames(M)=c("Q1","Q2","Q3","Q4")
rownames(M)=2001:2014
yearly.avg=apply(M,1,mean);yearly.avg
plot(yearly.avg, type='l')

t=1:14
fit=lm(yearly.avg~t)
summary(fit)
et=resid(lm(yearly.avg~t));et
MAPE=mean(abs(et/yearly.avg)*100);MAPE

fit1=lm(yearly.avg~t+I(t^2))
summary(fit1)
et1=resid(lm(yearly.avg~t+I(t^2)));et1
MAPE1=sum(abs(et1/yearly.avg))*100/14;MAPE1

#The quadratic fit is better: origin: 2000, unit: 1 year
t=2:57
yt=5709.005 - 347.958*(t+0.5)/4 + 33.52*(t+0.5)^2/16;yt
dt=x/yt;dt #detrended data since we assumed a multiplicative model

#ratio to trend method
M1=matrix(dt,ncol=4,byrow=T)
unadj_si=apply(M1,2,mean);unadj_si #for each quarter

adj_si=unadj_si*4/sum(unadj_si);adj_si
si=matrix(rep(adj_si, times=14),ncol=4,byrow=T);si
dts=M1/si #detrended/seasonal indices= has only irregular and cyclical
u=as.vector(t(dts))
u
plot(u,type='l')

#periodogram analysis
n=56
mu=30:50
A=array(0)
B=array(0)
for( i in 1:length(mu))
{
A[i]=(2/n)*sum(u*cos(2*pi*t/mu[i]))
B[i]=(2/n)*sum(u*sin(2*pi*t/mu[i]))
}
S.mu=A^2+B^2
plot(mu,S.mu,type='h')
lambda=mu[which(S.mu==max(S.mu))];lambda

#Harmonic Analysis
n1=lambda
t1=1:n1
th1=cos(2*pi*t1/n1)
th2=sin(2*pi*t1/n1)                                                                                                              
ut=u[1:n1]
A0=mean(ut)
A1=mean(2*ut*th1)
A2=mean(2*ut*th2)
uthat=A0+(A1*th1)+(A2*th2);uthat
plot(t1,ut,type="l")
lines(uthat)

