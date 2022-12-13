rm(list=ls())
setwd("C:/Users/LAB4_39/Downloads")
d=read.csv("data.csv",header=F);d
d=d[1:468,];d #OMITTING THE LAST 8 VALUES, SINCE THE DATA ON 1995 IS INCOMPLETE
plot(d$V2,type="l")


#RATIO TO TREND
d1=matrix(d$V2, nrow=12,ncol=39);d1 #Yearly average wipes out the seasonality present in monthly data
yearly.avg=apply(d1,2,mean);yearly.avg
t=seq(1,39,1)

#linear fit
lm(yearly.avg~t) #origin 1955
et=resid(lm(yearly.avg~t));et
MAPE=mean(abs(et/yearly.avg)*100);MAPE
a0=-120.3
a1=345.1
Tt=a0+a1*(t-0.5)/12;Tt #Origin: june 15th 1955

#quadratic fit
lm(yearly.avg~t+I(t^2))
et1=resid(lm(yearly.avg~t+I(t^2)));et1
MAPE1=sum(abs(et1/yearly.avg))*100/39;MAPE1
b0=993.958
b1=182.077
b2=4.077
Tt1=b0+b1*(t-0.5)/12+b2*(t-0.5)^2/144;Tt
#to get the monthly trend equation from the yearly trend equation we replace t by t/12
#t: 7 to 6+(39*12)

time=7:474
est.monthly.Tt=b0+b1*(time-0.5)/12+b2*(time-0.5)^2/144;est.monthly.Tt
detrended=d1/est.monthly.Tt;detrended
Dtrend=matrix(detrended*100,nrow=12,ncol=39);Dtrend
unadj.seasonal.indices=apply(Dtrend,1,mean);unadj.seasonal.indices
Adj.seasonal.indices=unadj.seasonal.indices*1200/sum(unadj.seasonal.indices)
deseasonalised=d1/rep(Adj.seasonal.indices,39);deseasonalised
deseason=as.vector(deseasonalised)
plot(deseason, type="l")
Tt=lm(deseason~time+I(time^2));Tt
trend=predict(lm(deseason~time+I(time^2)));trend
desdet=deseason/trend;desdet
plot(desdet,type="l")

