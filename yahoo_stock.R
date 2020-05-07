install.packages("ggplot2")

library(tseries)
library(timeSeries)
library(barChart)
library(quantmod)#支援barchart
library(fUnitRoots)
library(forecast)

#http://blog.fens.me/r-zoo/

###### 2.1 ####

knitr::opts_chunk$set(echo = TRUE)
data <- c("2454.TW")
offer <- c("yahoo")       
from <- c("2019-01-01")
to<-c("2020-1-1")
item <- c("Open", "High", "Low", "Close", "Adjusted", "Volume")
aapl <- get.hist.quote(instrument = data, start = from, 
                      end = to,quote = item,provider = offer)

aapl<-as.timeSeries(aapl)
plot(aapl[,5],type = "l",main="聯發科調整後股價")


#對數日報酬率 exp(1) = e  (ln(Pt/Pt-1))
#ln(pt) - ln(pt-1) 
length(aapl[,1])
aapl_stock<-as.data.frame(aapl)
aapl_stock$Return<-c(1)#新增一個對數日報酬率的欄位

aapl_stock<-as.timeSeries(aapl_stock)
aapl_return<-numeric(length(aapl[,1]))
aapl_return<-dailyReturn(aapl[,5],type='log')
aapl_stock[,7]<-aapl_return
plot(aapl_stock[,7],type="l",main="對數日報酬率")
aapl_return<-na.omit(aapl_return)#去除na資料


##### 2.2 #####
#檢測資料是否具有單根

aapl_Adjusted<-aapl_stock$Adjusted
adfTest(aapl_Adjusted, lag = 12, type = "ct",title="日價格資料 adf test")
adfTest(aapl_return, lag = 12, type = "ct",title="對數日報酬率 adf test")

#aapl_return_new<-aapl_return[-238,]

#### 2.3 ####
m1 = ar(aapl_return, method = 'ols',order.max =12)
m1$resid
aic = m1$aic
AIC<-data.frame(p=seq(from=0,to=12,by=1),AIC=aic)

if(do.call(sum,m1[2])<1) #B(係數總和)
{
  print("stationary")
}


#根據上述的AR(p)模型所估計出的模型係數判斷，資料是否為定態
as.list(m1[2])
for (p in (1:12)){
  m1 = ar.ols(aapl_return,order=p)
  aic_model<-log(sum(m1$resid^2)/m1$n.used)+2*(p+1)/m1$n.used
  
  if(do.call(sum,m1[2])<1){
    print("stationary")
  }
  else{
    print(false)
  }
}

mod1<-auto.arima(aapl_return, ic=c("aic"),test="adf",trace = TRUE)
auto.arima(aapl_return, ic=c("bic"),test="adf",trace = TRUE)

mod<-auto.arima(aapl_return,test="adf")
if(do.call(sum,mod[2])<1) #B(係數總和)
{
  print("stationary")
}
plot(forecast(mod))
mod1<-arima(aapl_return,order = c(1,1,1))
plot(forecast(mod1))
arima()
mod1
summary(m1)
m4<-  ar(aapl_return, method = 'ols',order=4)


##### 2.4：模型殘差檢定：Jarque-Bera常態性檢定與獨立性檢定 #####
     
Box.test(mod$residuals, lag = 12, type = c("Ljung-Box"))
normalTest(mod1$residuals, method = c("jb"), na.rm = T)

shapiro.test(mod$residuals)
hist(mod1$resid,type="l")
summary(mod1$residuals)
qqnorm(mod1$residuals)
jarque.bera.test(rnorm(85))
plot(rnorm(85),type="l")

#### 2.5 ####

y.auto=auto.arima(aapl_return)
summary(y.auto)
