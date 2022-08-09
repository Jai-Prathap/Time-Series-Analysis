# Time-Series-Analysis 
setwd("E:/R/RStudio")
data=read.csv("PCE.csv")
colSums(is.na(data))
library(imputeTS)
library(dplyr)
library(VIM)
library(ggplot2)
aggr(data,numbers=TRUE, prop=FALSE)
datasetM<- ts(data$PCE, start=c(1959,1), end=c(2021,12), frequency=12)
ts<-na_kalman(datasetM, model="auto.arima")
library(forecast)
par(mfrow=c(2,2))
plot(ts)
autoplot(ts,xlab = "Year",ylab = "PCE")
train<-window(ts, start=c(1959, 1), end=c(2003, 6), frequency=12)
test <- window(ts, start = c(2003,7), end = c(2021,12),frequency=12)
plot(ts,ylab="PCE")


#AVG
fcavg<-meanf(train, h = 221)
accuracy(fcavg,ts)
plot(fcavg,ylab="PCE",xlab="Year")
#Naive
fcnaive <- naive(train, h = 221)
accuracy(fcnaive,ts)
plot(fcnaive,ylab="PCE",xlab="Year")
#SeasonalNaive
fcsnaive <- snaive(train, h = 221)
accuracy(fcsnaive,ts)
plot(fcsnaive,ylab="PCE",xlab="Year")
#Drift
fcdrift<-rwf(train, h = 221, drift = T)
accuracy(fcdrift,ts)
plot(fcdrift,ylab="PCE",xlab="Year")
#EXPO
fcexpo <- ets(train)
plot(fcexpo)
#arima
fcarima<-auto.arima(train)
forecast(fcarima,xreg = test)
plot(fcarima)
############
accuracy(fcdrift, test)
accuracy(forecast(fcexpo, h = 221)$mean, test)
accuracy(forecast(fcarima, h=221)$mean, test)
#####
autoplot(ts) +
  autolayer(rwf(train, drift=TRUE, h=221),
            series="Drift", PI=FALSE) + 
  scale_color_manual(values=c("red","cyan","green"))+
  autolayer(forecast(fcexpo, h = 221)$mean,
            series="Exponential moving average") +
  autolayer(forecast(fcarima, h=221)$mean,
            series="ARIMA")+ylab("PCE")+xlab("Year")
##############################################
###Simple forecasting methods
# drift method ------consider seasonality and trend
fcdr <- rwf(train, drift=TRUE, h=221)	
accuracy(fcdr, test)
accuracy(forecast(fcexpo, h = 221)$mean, test)
accuracy(forecast(fcarima, h=221)$mean, test)

# October 2022 estimate

prediction<- predict(fcarima,232)
prediction$pred[length(prediction$pred)]

#One-step Ahead
fcari1 <- Arima(test, model=fcarima)
fc1 <- window(fitted(fcari1), start=c(2003,7))
plot(ts,ylab="PCE",xlab="Year")
lines(fc1,col="green")

fcex <- ets(ts, model=fcexpo,use.initial.values = TRUE)
fc3 <- window(fitted(fcex), start=c(2003,7))
plot(ts,ylab="PCE",xlab="Year")
lines(fc3,col="red")

fcdrift <- rwf(ts, h=221, drift = TRUE)
fc4 <- window(fitted(fcdrift), start = c(2003,7))
plot(ts,ylab="PCE",xlab="Year")
lines(fc4,col="cyan")

