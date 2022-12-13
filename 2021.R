rm(list=ls())
data=read.csv("C:/Users/YENISI/OneDrive/Documents/R/Time Series/2021 Time Series 1.csv");data
attach(data)
d=(data[-1]);d
plot(c(X1,X2,X3,X4),type="l")

d1=matrix(c(X1,X2,X3,X4),nrow=12);d1 #Yearly average wipes out the seasonality present in monthly data
yearly.avg=apply(d1,2,mean);yearly.avg
t=seq(1,4,1)
lm(yearly.avg~t) #origin year before year 1
et=resid(lm(yearly.avg~t));et
MAPE=mean(abs(et/yearly.avg)*100);MAPE

a0=1197.92
a1=19.04

Tt=a0+a1*(t-0.5)/12; Tt

time=7:54
est.monthly.Tt=a0+a1*(time-0.5)/12;est.monthly.Tt
detrended=d1/est.monthly.Tt; detrended
dtrend=matrix(detrended,ncol=4);dtrend
unadj.seasonal.indices=apply(dtrend,1,mean);unadj.seasonal.indices
Adj.seasonal.indices=unadj.seasonal.indices*1200/sum(unadj.seasonal.indices)

deseasonalised2=detrended*100/rep(Adj.seasonal.indices,4);deseasonalised2

deseasonalised=d1/rep(Adj.seasonal.indices,4);deseasonalised
deseason=as.vector(deseasonalised);deseason
plot(deseason, type="l")
Tt=lm(deseason~time);Tt
trend=predict(lm(deseason~time));trend
desdet=deseason/trend;desdet
plot(desdet,type="l")

#2
rm(list=ls())
data=read.csv("C:/Users/YENISI/OneDrive/Documents/R/Time Series/2021 Time Series 2.csv");data
data=data[c(-17,-16,-15),];data
attach(data)
plot(data,type='l')
t=1:14
#linear fit
summary(lm(Production~t))
et=resid(lm(Production~t))
mape1=sum(abs(et/Production))*100/14;mape1
summary(lm(Production~t+I(t^2)))
et1=resid(lm(Production~t+I(t^2)));et1
MAPE1=sum(abs(et1/Production))*100/14;MAPE1


#they are almost same so we got for quadratic fit
a0=325.154
a1=14.82
Tt=a0+a1*t;Tt

tt=b0+b1*18+b2*18^2;tt
#3
rm(list=ls())
data=c(49,52,47,50,51,46,46,50,54,49)

