###read data, sample file attached to the Repositry

data<-read.csv("visits.csv")


##Use ts() function to convert read file into timeseries

datats<-ts(data[,-1], start = c(2015,1), frequency = 12)
class(datats)

######plot ts 

plot(datats)

#### Decompose to check the components of timeseries 

plot(decompose(datats, type = "additive"))
datats.decom<-decompose(datats)


#We can also decompose using stl() function
#STL is an acronym for "Seasonal and Trend decomposition using Loess", 
#while Loess is a method for estimating nonlinear relationships.

#stl(datats, "seasonal")

#Disadvantage: it only provides facilities for additive decompositions.


#we can extract trend from decomposed ts

trend<-datats.decom$trend
plot(trend)
season<-datats.decom$seasonal
plot(season)
#plot season and trend together
ts.plot(cbind(trend,trend*season),lty=1:2)




##------------------------------- Smoothening techniques ---------------------------

#-------------Simple moving averages----------------

install.packages(TTR)
library(TTR)
#n=Number of periods to average over.
datats_smoothened<- SMA(datats,n=2)

#plot time series
plot.ts(datats)
plot.ts(datats_smoothened)


### SES - Simple or Single exponential smoothing 

sme<-ses(datats, h=12)
class(sme)
summary(sme)
smef<-as.data.frame(sme)

####holt

holt<-holt(datats)
summary(holt)
holtf<-as.data.frame(holt, h=14)


########### holt explo

holtE<-holt(datats, exponential=TRUE)
summary(holtE)
holtf<-as.data.frame(holt, h=14)

accuracy(holtE)

#------------- Exponential moving averages ----------------

#Apply exponential smoothening average - holt winters
datatsforecasts <- HoltWinters(datats, beta=FALSE, gamma=FALSE)
datatsforecasts
plot(datatsforecasts)
datatsforecasts$fitted
datatsforecasts$SSE
library(forecast)
datatsforecasts2 <- forecast.HoltWinters(datatsforecasts, h=12)
datatsforecasts2
plot.forecast(datatsforecasts2)
plot.ts(datatsforecasts2$residuals)

evm<-as.data.frame(datatsforecasts2)

############################### with default HoltWinters

datatsforecastsN <- HoltWinters(datats)
datatsforecastsN
plot(datatsforecastsN)
datatsforecastsN$fitted
datatsforecastsN$SSE
datatsforecastsN2 <- forecast.HoltWinters(datatsforecastsN, h=12)
datatsforecastsN2
class(datatsforecasts2)
plot.forecast(datatsforecastsN2)
plot.ts(datatsforecastsN2$residuals)

evm2<-as.data.frame(datatsforecastsN2)


####################### using ets  ETS - Error, Trend and Season 

dataets<-ets(datats)
forecast(dataets, h=12)
dataets2<-forecast.ets(dataets, h=12)
plot.forecast(dataets2)
plot.ts(dataets$residuals)
etsf<-as.data.frame(dataets2)



####################################################  Arima models

####################### ARIMA

#Plot the timeseries and check if the graph is stationary or not
plot(datats)

#By Looking at the plot, we can say timeseries is not stationary
#To make it stationary, use differencing

datatsdf<-diff(datats, 1)

plot(datatsdf)
plot(datats)

#test for stationary
#Check the significance through Unit root test or ADF test 

library(tseries)
adf<-adf.test(datatsdf)
adf

adfts<-adf.test(datats)
adfts

kps<-kpss.test(datatsdf)
kps




######## Estimating p,d q of Arima(p,d,q)
#from above differencing, it is found that d=1


acf(datatsdf, lag.max = 20)

acf(datatsdf, lag.max = 20, plot = F)


#q =9

pacf(datatsdf, lag.max = 20)

#p=2

#Calculate p,q ARMA(2,0) OR ARMA(0,9) ADD P+Q
#Using principle of parsimony - ARMA(2,0)


datatsdfa <- Arima(datats, order = c(2,1,0))

datatsdfa2<-forecast.Arima(datatsdfa, h=12)

plot.forecast(datatsdfa2)

plot(datatsdfa$x)
lines(fitted(datatsdfa),col="blue")


acf(datatsdfa2$residuals, lag.max = 20)

Box.test(datatsdfa2$residuals, lag = 20, type = "Ljung-Box")

# P value should be above 0.05, so we can conclude that residuals are not correlated

plot.ts(datatsdfa2$residuals)

#Plot Histogram to check for bell shaped 
hist(datatsdfa2$residuals)



#### IF we consider ARMA(0,9)

datatsdfa9 <- Arima(datats, order = c(0,1,9))

datatsdfa92<-forecast.Arima(datatsdfa9, h=12)

plot.forecast(datatsdfa92)

plot(datatsdfa92$x)
lines(fitted(datatsdfa92),col="blue")

acf(datatsdfa92$residuals, lag.max = 20)

#Box text for residuals, p value >0.05, is good which means residuals are not correlated : type=Ljung_box  test

Box.test(datatsdfa92$residuals, lag = 20, type = "Ljung-Box")

hist(datatsdfa92$residuals)



###### Auto Arima - In Auto Arima, order is calculated automatically 

dataarima<-auto.arima(datats)
dataarima2<-forecast.Arima(dataarima, h=12)
plot.forecast(dataarima2)
plot.ts(dataarima$residuals)
plot(dataarima$x)
lines(fitted(dataarima),col="blue")
arimaf<-as.data.frame(dataarima2)

acf(dataarima$residuals)
