install.packages("ggplot2")

library(tseries)
library(timeSeries)
library(barChart)
library(quantmod)#支援barchart
library(ggplot2)
library(fUnitRoots)
library(forecast)
#http://blog.fens.me/r-zoo/

###### 2.1 ####
data <- c("2454.TW")  #search stock code on yahoo finance website: http://quote.yahoo.com/
offer <- c("yahoo")       
from <- c("2019-01-01")   
to<-c("2019-12-31")
item <- c("Open", "High", "Low", "Close", "Adjusted", "Volume")
MTK <- get.hist.quote(instrument = data, start = from, end = to,quote = item,
                        provider = offer)

MTK<-as.timeSeries(MTK)



plot.zoo(MTK[,5])
plot(MTK[,5],type = "l",main="聯發科調整後股價")


#對數日報酬率 exp(1) = e  (ln(Pt/Pt-1))
#ln(pt) - ln(pt-1) 
MTK_stock<-as.data.frame(MTK)


MTK_stock$Return<-c(1)#新增一個對數日報酬率的欄位
MTK_stock<-as.timeSeries(MTK_stock)
MTK_ln<-log(MTK_stock$Adjusted,base=exp(1))
MTK_ln #取完對數
MTK_return<-numeric(239)

i<-1
for(i in 1:240){
  MTK_return[i]<- MTK_ln[i+1]-MTK_ln[i]
}
MTK_return
MTK_stock[,7]<-MTK_return
MTK_stock[,7]
plot(MTK_stock[,7],type="l",main="對數日報酬率")
MTK_return<-na.omit(MTK_return)#去除na資料
MTK

MTK_return<-dailyReturn(MTK[,5],type='log')
MTK_return
Ad(MTK)
##### 2.2 #####
#檢測資料是否單根
MTK_Adjusted<-MTK_stock$Adjusted
adfTest(MTK_Adjusted, lag = 12, type = "ct",title="日價格資料 adf test")
adfTest(MTK_return, lag = 12, type = "ct",title="對數日報酬率 adf test")

#### 2.3 ####
#MTK_return_new<-MTK_return[-238,]
MTK_return<-data.frame(MTK_return)
dim(MTK_return)
MTK_return<-MTK_return[-239,]
MTK_return
mm1 = ar(MTK_return, method = 'ols',order.max =12)
aic = mm1$aic
aic

mm2<-auto.arima(MTK_return)
bic<-mm2$bic
bic

auto.arima(MTK_return, ic=c("bic"),test="adf" )
auto.arima(MTK_return, ic=c("aic"),test="adf" )
mod<-auto.arima(MTK_return, ic=c("aic"),test="adf")
plot(forecast(mod))
mod1<-arima(MTK_return,order = c(5,2,1))
plot(forecast(mod1))
arima()
mod1


##### 2.4：模型殘差檢定：Jarque-Bera常態性檢定與獨立性檢定 #####
y.auto=auto.arima(MTK_return)
y.auto

Box.test(MTK_return)            
Box.test(MTK_return, lag = 12, type = c("Ljung-Box"))
