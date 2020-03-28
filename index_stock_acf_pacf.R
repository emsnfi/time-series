library(timeDate)
library(fBasics)
library(ggplot2)
library(tseries) 
setwd("C:/Users/emily/Desktop/dataFiles")
load()
stock <- read.csv("index_stock.csv", header = TRUE)

Brazil_r<-stock[c(1,6)] #brazil time and returns
names(Brazil_r)<-c("time","Breturn")



Brazil_r
result = ar(Brazil_r$Breturn,method = 'mle')

print(result$aic)
seq(from=0,to=12,by=1)
AIC<-data.frame(p=seq(from=0,to=12,by=1),AIC=result$aic)

AIC
marm<-matrix(data=result$aic,byrow = 0)
marm
