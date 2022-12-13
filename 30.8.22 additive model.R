rm(list=ls())
d=as.matrix(co2);d
plot(d,type="l")
#WE FIT AN ADDITIVE MODEL TO THE DATA
SI=decompose(co2)$figure;SI #SEASONAL INDICES
res=d-rep(SI,39);res #DE-SEASONALIZED DATA
plot(res,type="l") #THIS GRAPH HAS NO SEASONALITY, ONLY TREND IS PRESENT
t=1:468
lm(res~t) #FITTING A LINEAR EQUATION TO THE DATA
p=predict(lm(res~t));p #TREND VALUES
res2=res-p;res2 #DE-SEASONALZED AND DE-TRENDED DATA
plot(res2,type="l")

rm(list=ls())
setwd("C:/Users/YENISI/OneDrive/Documents")
d=read.csv("data.csv",header=F);d
d=d[1:468,];d #OMITTING THE LAST 8 VALUES, SINCE THE DATA ON 1995 IS INCOMPLETE
plot(d$V2,type="l")
#WE FIT A MULTIPLICATIVE MODEL TO THIS DATA
d1=as.matrix(d$V2);d1
ts(d1,frequency=12) #CONVERTING TO TIME SERIES
SI=decompose(ts(d1,frequency=12),type="multiplicative")$figure;SI #SEASONAL INDICES
res=d1/rep(SI,39);res #DE-SEASONALIZED DATA

plot(res,type="l") #THIS GRAPH HAS NO SEASONALITY, ONLY TREND IS PRESENT
t=1:468
#FITTING A LINEAR EQUATION :
p1=predict(lm(res~t));p1 #TREND VALUES
res1=res/p1;res1 #DE-TRENDED AND DE-SEASONALIZED DATA
plot(res1,type="l")
r1=resid(lm(res~t));r1 #RESIDUALS AFTER FITTING THE LINEAR EQUATION
MAPE1=mean(abs(r1/res)*100);MAPE1

#FITTING A QUADRATIC EQUATION
p2=predict(lm(res~t+I(t^2)));p2 #TREND VALUES
res2=res/p2;res2 #DE-TRENDED AND DE-SEASONALIZED DATA
plot(res2,type="l")
r2=resid(lm(res~t+I(t^2)));r2 #RESIDUALS AFTER FITTING THE QUADRATIC EQUATION
MAPE2=mean(abs(r2/res)*100);MAPE2

#SINCE MAPE2<MAPE1, A QUADRATIC EQUATION GIVES BETTER FIT