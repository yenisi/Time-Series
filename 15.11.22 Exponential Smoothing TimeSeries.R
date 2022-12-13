data=c(49,52,47,50,51,46,46,50,54,49)
?HoltWinters
data1=ts(data,frequency=1,start=1,end=10);data1
alpha=0.1
beta=F
gamma=F
#set beta and gamma as false always
a=HoltWinters(data1,alpha,beta=F,gamma=F)
a$fitted
a$SSE
predict(a,1)

#find out the value of alpha in 0.1 to 0.3 for which SSE is minimum
alpha1=seq(0.1,0.3,0.0005)
length(alpha1)
e=array(0,length(alpha1)) #to store the SSEs
for(i in 1:401)
{
a=HoltWinters(data1,alpha1[i],beta=F,gamma=F)
e[i]=a$SSE
}
plot(a)
optim=alpha1[which(e==min(e))]
d=data.frame(alpha1,e)
#optim_alpha=d[d$e==min(e),1]

estimated.series=HoltWinters(data,alpha=0.1,beta=F,gamma=F)
plot(estimated.series)

alpha2=seq(0.1,0.3,0.025)

s=HoltWinters(data,alpha=alpha2,beta=F,gamma=F)
plot(s)
#also gives the plot for alpha corresponding to the min SSE