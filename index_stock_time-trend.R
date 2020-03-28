install.packages("forecast")
library(timeDate)
library(fBasics)
library(ggplot2)
library(tseries) 
library(forecast)
getwd()                                   # getwd: get work document                           
setwd("C:/Users/emily/Desktop/dataFiles")
load()
stock <- read.csv("index_stock.csv", header = TRUE)

stock
dim(stock)
names(stock)
stock_Br<-stock[,c(2,6)]

#Brazil price 

stock_Brprice<-stock[,c(1,2)] #??–brazil ??‚é?“å?Œå ±?…¬


names(stock_Brprice)<-c("time","Brazil_price") #add head (time price)

Brazil_price <- ts(stock_Brprice$Brazil_price, frequency = 365,start = c(2006,6,1),end=c(2010,3,31))

plot(Brazil_price)

#Brazil return 
stock_Breturn<-stock[,c(1,6)] #brazil time and returns

Date<-as.character.Date(stock_Breturn$time)
Date

names(stock_Breturn)<-c("time","Breturn")
format(Date,"%m/%Y/%d") #%Y%m%d
stock_Breturn$time<-as.Date(stock_Breturn$time,"%m/%d/%Y")
Brazil_return <- ts(stock_Breturn$Breturn, frequency = 365,start = c(2006,6,1),end=c(2010,3,31))
class(stock_Breturn$time)
plot(Brazil_return)

#
basicStats(stock_Breturn$Breturn)

# Brazil acf
Brazil_return_acf<-stock_Breturn$Breturn
acf(Brazil_return_acf, lag = 12)

# Brazil pacf
Brazil_return_pacf<-stock_Breturn$Breturn
pacf(Brazil_return_pacf, lag = 12)

#æª¢æ¸¬è³‡æ?™æ˜¯?¦å®šæ?? 
#å¢žå»£DFæª¢å?šï?Œç”¨ä¾†æ¸¬è©¦å?‚æ?œå?‡ARæ¨¡å?‹å?—å…¥??‚ï?Œæ˜¯?¦å­˜åœ¨?–®ä½è??(unit root)ï¼?
#??€ä¸‹é¢??? ?€alternative hypothesiså°ç?‹å?‡è¨­ï¼šå¹³ç©©â€? ä»?è¡¨è©²æª¢å?šç?„æ¸¬è©¦å?è±¡
#p-value?‚º0.01ï¼Œé? å?æ–¼?Ÿºæº–å€?0.05ï¼Œå¯ä»¥æŽ¨ç¿»è?›ç„¡??‡è¨­ï¼?
#?? æ­¤?¾?œ¨??„å?å?—æ˜¯å¹³ç©©???

adf.test(stock_Breturn$Breturn) #? ±?…¬

#p-value?‚º0.7909ï¼Œå¤§?–¼?Ÿºæº–å€?0.05ï¼Œä?èƒ½?Ž¨ç¿»è?›ç„¡??‡è¨­

adf.test(stock_Brprice$Bprice) #?ƒ¹? ¼

view(stock_Brprice)
write.csv(stock_Brprice,"Bprice.csv")
is.Date(stock_Brprice$time)
class(stock_Brprice)
stock_Brprice<- xts(stock_Brprice[,-1], order.by=stock_Brprice$time)
class(stock_Brprice$time)
as.POSIXct.Date(stock_Brprice$time)
stock_Brprice$time
plot(stock_Brprice)

ggplot(data = stock_Brprice, mapping = aes(x = factor(time), y =Bprice , group = 1)) + geom_line() + xlab('Year')
ggplot(data = stock_Breturn, mapping = aes(x = time, y = Breturn, group = 1)) + geom_line()


qxts <- xts(stock_Brprice[,-1], order.by=as.POSIXct(stock_Brprice$time))
f <- decompose(apts)


plot(f)

as.matrix(stock_Brprice)
as.xts(stock_Brprice)
barChart(stock_Brprice)
class(stock_Brprice$time)
#stock_Brprice$time=as.timeSeries((stock_Brprice$time))
#ggplot(data=stock_Brprice,aes(x=time,y=Brazil_price))
