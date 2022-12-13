rm(list=ls())
seven=c(41,41,44,40,39,29,26,25,28,32,39,42)
eight=c(43,42,46,43,39,28,25,24,24,31,37,42)
nine=c(39,39,38,36,31,23,23,23,26,32,37,39)
ten=c(43,39,42,40,35,22,23,21,25,32,33,36)
mean(seven)
data=c(seven,eight,nine,ten)

seven1=c(26,25,28,32,39,42)
eight1=c(43,42,46,43,39,28,25,24,24,31,37,42)
nine1=c(39,39,38,36,31,23,23,23,26,32,37,39)
ten1=c(43,39,42,40,35,22)
data1=c(seven1,eight1,nine1,ten1)

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
MAA
mov.avg=data1*100/MAA #detrended
moving.avg=c(0,0,0,0,0,0,mov.avg,0,0,0,0,0,0)
ma=matrix(moving.avg, nrow=4,byrow=T) ;ma  

USI=apply(ma,2,sum)/3;USI
sum(USI)   
sum(USI)*1200/1200.198 
Detrend=USI*1200/1200.198; Detrend

#model=xt=st+Tt+It
detrended.val=data1-MAA;detrended.val
fortyeightval=c(0,0,0,0,0,0,detrended.val,0,0,0,0,0,0);matrix
additivematrix=matrix(fortyeightval,nrow=4,byrow=T)
USIadd=apply(additivematrix,2,sum)/3
sum(USIadd)
AdjustedIndices=USIadd-sum(USIadd)/12;AdjustedIndices
sum(AdjustedIndices)

