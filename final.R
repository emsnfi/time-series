
library(tseries)
library(timeSeries)
library(barChart)
library(quantmod)#支援barchart
library(fUnitRoots)
library(forecast)
data <- c("AAPL") #MTK
offer <- c("yahoo")       
from <- c("2019-01-01")
to<-c("2020-1-1")
item <- c("Open", "High", "Low", "Close", "Adjusted", "Volume")
MTK <- get.hist.quote(instrument = data, start = from, 
                       end = to,quote = item,provider = offer)

MTK<-as.timeSeries(MTK)
plot(MTK[,5],type = "l",main="聯發科調整後股價")


#對數日報酬率 exp(1) = e  (ln(Pt/Pt-1))
#ln(pt) - ln(pt-1) 

MTK_stock<-as.data.frame(MTK)
MTK_stock$Return<-c(1)#新增一個對數日報酬率的欄位

MTK_stock<-as.timeSeries(MTK_stock)
MTK_return<-numeric(length(MTK[,1]))
MTK_return<-dailyReturn(MTK[,5],type='log')
MTK_stock[,7]<-MTK_return
plot(MTK_stock[,7],type="l",main="對數日報酬率")

MTK_return<-na.omit(MTK_return)#去除na資料

MTK_Adjusted<-MTK_stock$Adjusted
adfTest(MTK_Adjusted, lag = 12, type = "ct",title="日價格資料 adf test")
adfTest(MTK_return, lag = 12, type = "ct",title="對數日報酬率 adf test")

#acf pacf
class(MTK_return)

acf(MTK_stock[,7], lag = 12,main = "ACF of MTKe Returns") 
pacf(MTK_stock[,7], main = "PACF of MTKe Returns",lag=12)   # default = lag 30

m1 = ar(MTK_return, method = 'ols',order.max =12)
m1$resid
aic = m1$aic
AIC<-data.frame(p=seq(from=0,to=12,by=1),AIC=aic)
AIC
if(do.call(sum,m1[2])<1) #B(係數總和)
{
  print("stationary")
}

Box.test(MTK_return, lag = 12, type = c("Ljung-Box"))
Box.test(MTK_return, lag = 12, type = c("Box-Pierce"))

mod1<-auto.arima(MTK_return, ic=c("aic"),test="adf",trace = TRUE)
print(mod1)

if(do.call(sum,mod1[2])<1) #B(係數總和)
{
  print("stationary")
}

mod2<-auto.arima(MTK_return, ic=c("bic"),test="adf",trace = TRUE)
print(mod2)

Box.test(mod2$residuals, lag = 12, type = c("Ljung-Box"))

Box.test(mod1$residuals, lag = 12, type = c("Ljung-Box"))

normalTest(mod1$residuals, method = c("jb"), na.rm = T)

normalTest(mod2$residuals, method = c("jb"), na.rm = T)

summary(mod1)

summary(mod2)

