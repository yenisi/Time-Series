#2021Question1

rm(list=ls())
setwd("C:/Users/YENISI/OneDrive/Documents/R/Time Series")
data=read.csv("Data Q1 2020 Time Series.csv")
attach(data);data
#data=data.frame(data);data
data=ts(data, frequency=12);data
Y=decompose(data,type='multiplicative')$figure;Y
data=t(data);data
#data[-1,]
data=ts(data[-1,], frequency=1);data
data=c(data[1,],data[2,],data[3,],data[4,]);data
#data=ts(data);data

data1=data[7:42];data1

MA=0
for(i in 1:37)
{
MA[i]=((data[i]+data[i+1]+data[i+2]+data[i+3]+data[i+4]+data[i+5]+data[i+6]+data[i+7]+data[i+8]+data[i+9]+data[i+10]+data[i+11])/12)
}
MA
MAA=0
for(i in 1:36)
{
MAA[i]=(MA[i]+MA[i+1])/2 
}
MAA #twopt moving avg

mov.avg=data1*100/MAA #detrended
moving.avg=c(rep(0,6),mov.avg,rep(0,6));moving.avg
ma=matrix(moving.avg,nrow=4,byrow=T);ma
USI=apply(ma,2,sum)/3;USI #unadj

sum(USI)
ASI=USI*1200/sum(USI);ASI
sum(ASI)

#colnames(data) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#decompose(data,type="multiplicative")

#2

rm(list=ls())
Year=seq(1921,2001,10);Year
Population=c(252,251.2,278.9,318.5,361,439.1,547.9,602.2,646.3)
data1=data.frame(Year, Population);data1
S1=sum(Population[1:3]);S1
S2=sum(Population[4:6]);S2
S3=sum(Population[7:9]);S3
D2=S3-S2
D1=S2-S1
D=D2/D1
b=D1*(D^{1/3}-1)/((D^{1/3})*(D-1)^2);b
a=(S1-(D1/(D-1)))/3;a
c=D^{1/3};c
t=seq(1,9,1);t
a+b*c^t
#plot(t,a+b*c^t)
et=Population-a-b*c^t;et
mape=sum(abs(et/Population)*100)/36;mape
matplot(cbind(Population, a+b*c^t),type="l")

#Prediction for the year 2011
t0=10
Pop.2011=a+b*c^t0;Pop.2011

#3

rm(list=ls())
setwd("C:/Users/YENISI/OneDrive/Documents/R/Time Series")
data=read.csv("2020 Q3 Data.csv")
attach(data);data
data=ts(data);data
plot(data, type='l')
dsdt=Value;dsdt
xbar=mean(dsdt)
length(dsdt)
c1=sum((dsdt[-27]-mean(dsdt))*(dsdt[-1]-mean(dsdt)))/27;c1
c0=sum((dsdt-mean(dsdt))^2)/27;c0
r1=c1/c0;r1


plot(dsdt,type='l',xaxt='n')
axis(1,at=seq(1,30,1))

#finding r1 using ACF rho(1)=r1
rho=acf(dsdt);rho
r1=rho[1];r1 #alpha_hat
r1=0.873
#xbar=mu_hat
#AR2

r2=rho[2];r2
r2=0.785
alpha1=r1*(1-r2)/(1-r1^2);alpha1
alpha2=(r2-r1^2)/(1-r1^2);alpha2
Xt=dsdt[-c(1,2)];Xt
Xt_1=dsdt[-c(1,27)];Xt_1
Xt_2=dsdt[-c(26,27)];Xt_2

#part ii

alpha2=seq(0.1,0.3,0.025)
data=ts(Value,start=1,end=27,frequency=1);data
s=HoltWinters(data,alpha=alpha2,beta=F,gamma=F)
predict(s,1)














