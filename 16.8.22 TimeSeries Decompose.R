rm(list=ls())
co2
plot(co2)
data=array(co2)
data=matrix(data)
data
Seasonal.Indices=decompose(co2)$figure;Seasonal.Indices
?decompose

seven=c(41,41,44,40,39,29,26,25,28,32,39,42)
eight=c(43,42,46,43,39,28,25,24,24,31,37,42)
nine=c(39,39,38,36,31,23,23,23,26,32,37,39)
ten=c(43,39,42,40,35,22,23,21,25,32,33,36)
ore=data.frame(seven,eight,nine,ten);ore
data=array(ore)
data=ts(data,start=c(2007,1),end=c(2010,12),freq=12)
decompose(data)
?ts

