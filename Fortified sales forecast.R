
library(fpp2)
library(forecast)
library(readxl)
library(lubridate)
library(dplyr)
library(tseries)
data=read.csv(file.choose())
head(data)
data

#Converting data into time series data
data1<-ts(data$Fortified, start = c(1980),frequency = 12)
data1
ts.plot(data1)

#SPlitting into train and test data
train.data1<-slice(data, 1:168)
test.data1<-slice(data, 169:180)
test.data1

data1_train<-ts(train.data1$Fortified, start = c(1980),frequency = 12)
data1_test<- ts(test.data1$Fortified, start = c(1994), frequency = 12 )
length(data1_train)
length(data1_test)

#Plotting time series data
ts.plot(data1)

#Decomposition of time series data
decomp_data1=decompose(data1, type = c("multiplicative"))
plot(decomp_data1)

#Fitting regression model
train.lm=tslm(data1_train~trend+season)
train.lm
plot(data1_train, xlab="Time", ylab="Fortified sales", ylim=c(1000,6000), xlim=c(1980,1993), bty="l")
lines(train.lm$fitted.values,lwd=2)
train.lm.pred=forecast(train.lm,h=12,level=0)
plot(train.lm.pred)
plot(train.lm.pred, ylim=c(1000,6000),xaxt="n",xlim=c(1980,1993),flty=2)
axis(1, at=seq(1980,1993,1), labels=format(seq(1980,1993,1)))
lines(train.lm.pred$fitted,col="red")
lines(data1_test)
summary(train.lm)

Acf(data1_train,lag.max = 12)
Pacf(data1_train,lag.max = 12)

plot(train.lm.pred$mean,col="red",type="l", ylab="Fortified sales", main="Comparision of original & predicted values")
lines(data1_test,col="black")

accuracy(train.lm.pred$mean,data1_test)
ggAcf(train.lm.pred$residuals,lag=12,main="ACF")
ggPacf(train.lm.pred$residuals,lag=12,main="PACF")


Acf(data1_train,lag.max = 12)
Pacf(data1_train,lag.max = 12)
kpss.test(data1)
dif_ts=diff(data1)
ggAcf(dif_ts,lag.max = 12)
ggPacf(dif_ts)



#ARIMA MODEL
auto.arima(data1)


ar1=arima(data1,order=c(2,1,0),seasonal = c(2,1,1))
ar1
fore1=forecast(ar1,level=c(95),h=3)
fore1
accuracy(ar1)
autoplot(fore1)

ar2=arima(data1,order=c(0,0,0),seasonal = c(2,1,1))
ar2
fore2=forecast(ar2,level=c(95),h=3)
fore2
accuracy(ar2)
autoplot(fore2)
