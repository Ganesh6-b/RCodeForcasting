install.packages("forecast")
install.packages("fpp")
install.packages("smooth")
install.packages("tseries")
library(forecast)
library(fpp)
library(smooth)
library(tseries)
setwd("C://Users//Ganesh//Downloads")

data <- read.xlsx("CocaCola_Sales_Rawdata.xlsx", 1)

#EDA 

hist(data$Sales)
#highest sales in the second year(1987) of the time period(given data)
#minimum sale in the last two year
mean(data$Sales)
#average sales throughout the year is 2994.35

tsdata <- ts(data$Sales, frequency = 4, start = c(86))
plot(tsdata)    

#it has a seasonality

#splitting the data

train <- tsdata[0:38]
test <- tsdata[38:42]

#holtwinter method

hw_a <- HoltWinters(train, alpha= 0.2, beta = F, gamma = F)
hw_a
plot(hw_a)

predhwa <- forecast(hw_a)
predvalues <- predict(hw_a, n.ahead = 5)
plot(predhwa)

hwa_mape <- MAPE(predvalues, test)* 100
hwa_mape
#####
hw_b <- HoltWinters(train, alpha=0.2, beta = 0.2 , gamma = F)
hw_b
plot(hw_b)
predhwb <- forecast(hw_b)
predvalueshwb <- predict(hw_b, n.ahead = 5)
predvalueshwb
plot(predhwb)

hwb_mape <- MAPE(predvalueshwb, test)
hwb_mape
#####
hw_c <- HoltWinters(train, alpha=0.2, beta = 0.1 , gamma = 0.1)
hw_c
plot(hw_c)
predhwc <- forecast(hw_c)
predvalueshwc <- predict(hw_c, n.ahead = 5)
plot(predhwc)

hwc_mape <- MAPE(predvalueshwc, test) * 100
hwc_mape

####
#moving average
ma_a <- sma(train, alpha= 0.2)
ma_a
plot(ma_a)

predictma_a <- data.frame(predict(ma_a, h = 5))
predictma_a
mapa_ma_a <- MAPE(predictma_a$Point.Forecast, test)* 100
mapa_ma_a
###

ma_b <- sma(train)

ma_b
plot(ma_b)

predictma_b <- data.frame(predict(ma_b, h = 5))
predictma_b
mapa_ma_b <- MAPE(predictma_b$Point.Forecast, test)* 100
mapa_ma_b

#mapa_ma_b having a least MAPA value 2.3