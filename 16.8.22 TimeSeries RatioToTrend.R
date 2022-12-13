rm(list=ls())
seven=c(41,41,44,40,39,29,26,25,28,32,39,42)
eight=c(43,42,46,43,39,28,25,24,24,31,37,42)
nine=c(39,39,38,36,31,23,23,23,26,32,37,39)
ten=c(43,39,42,40,35,22,23,21,25,32,33,36)
df=data.frame(seven,eight,nine,ten);df
t=seq(1,4,1)
M=matrix(c(seven,eight,nine,ten),nrow=12);M
YearlyMean=apply(M,2,mean)
lm(YearlyMean~t)
t=0
T=0
for (t in 1:48)
{
T[t]=36.875 - 1.192*(6+t-0.5)/12
}
T

percentage=df*100/T;percentage
monthly.avg=apply(percentage,1,mean);monthly.avg
adjusted=1200*monthly.avg/sum(monthly.avg);adjusted
plot(adjusted)

percentage1=(df-T);percentage1
monthly.avg1=apply(percentage1,1,mean);monthly.avg1
adjusted1=(monthly.avg1-sum(monthly.avg1)/12);adjusted1
plot(adjusted1)
