rm(list=ls())
?co2
plot(co2)
data=array(co2)
A=matrix(data,ncol=12,byrow=T)
yearly.avg=apply(A,1,mean)
year=seq(1959,1997,1)
plot(year,yearly.avg)


aggregate(co2,bylist=year,mean)
