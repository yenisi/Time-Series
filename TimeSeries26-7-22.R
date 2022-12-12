rm(list=ls())
year=seq(1951,1988,1);year
Pop=c(304454,316700,326372,334342,343838,350333,356195,361444,366253,372665,384773,395891,407024,417023,427330,440913,458438,479938,500378,520174,547563,558030,568500,584552,594518,605932,618210,627238,636442,648922,667381,684771,697570,708066,724952,746560,767648,792296)
data=data.frame(year,Pop);data

#a. fitting a modified exponential equation
Year=seq(1953,1988,1)
Population=c(326372,334342,343838,350333,356195,361444,366253,372665,384773,395891,407024,417023,427330,440913,458438,479938,500378,520174,547563,558030,568500,584552,594518,605932,618210,627238,636442,648922,667381,684771,697570,708066,724952,746560,767648,792296)
data1=data.frame(Year, Population);data1
S1=sum(Population[1:12]);S1
S2=sum(Population[13:24]);S2
S3=sum(Population[25:36]);S3
D2=S3-S2
D1=S2-S1
D=D2/D1
b=D1*(D^{1/12}-1)/((D^{1/12})*(D-1)^2);b
a=(S1-(D1/(D-1)))/12;a
c=D^{1/12};c
t=seq(1,36,1);t
a+b*c^t
#plot(t,a+b*c^t)
et=Population-a-b*c^t;et
mape=sum(abs(et/Population)*100)/36;mape
matplot(cbind(Population, a+b*c^t),type="l")

#b gompertz curve
rm(list=ls())
Population=c(326372,334342,343838,350333,356195,361444,366253,372665,384773,395891,407024,417023,427330,440913,458438,479938,500378,520174,547563,558030,568500,584552,594518,605932,618210,627238,636442,648922,667381,684771,697570,708066,724952,746560,767648,792296)
Lpop=log(Population)
data2=data.frame(Year, Lpop)
S1=sum(Lpop[1:12]);S1
S2=sum(Lpop[13:24]);S2
S3=sum(Lpop[25:36]);S3
D2=S3-S2
D1=S2-S1
D=D2/D1
b=D1*(D^{1/12}-1)/((D^{1/12})*(D-1)^2);b
B=exp(b);B
a=(S1-(D1/(D-1)))/12;a
A=exp(a);A
C=D^{1/12};c
t=seq(1,36,1);t
fit=A*B^C^t;fit
et=Population-A*B^C^t;et
mape=sum(abs(et/Population)*100)/36;mape
#par(mfrow=c(1,2))
#{
#matplot(cbind(Population, A*B^C^t),type="l")
#matplot(cbind(Population, a+b*c^t),type="l")
#}


#c
rm(list=ls())
Population=c(326372,334342,343838,350333,356195,361444,366253,372665,384773,395891,407024,417023,427330,440913,458438,479938,500378,520174,547563,558030,568500,584552,594518,605932,618210,627238,636442,648922,667381,684771,697570,708066,724952,746560,767648,792296)

xt=1/Population
data2=data.frame(Year, xt)
S1=sum(xt[1:12]);S1
S2=sum(xt[13:24]);S2
S3=sum(xt[25:36]);S3
D2=S3-S2
D1=S2-S1
D=D2/D1
b=D1*(D^{1/12}-1)/((D^{1/12})*(D-1)^2);b
a=(S1-(D1/(D-1)))/12;a
A=1/a;A
c=D^{1/12};c
log(c)
B=log(b)+log(A);B
t=seq(1,36,1);t
mape=sum(abs(et/Population)*100)/36;mape
et=Population-A*B^C^t;et


