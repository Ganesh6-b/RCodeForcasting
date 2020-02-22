library(forecast)
library(fpp)
library(smooth)
library(tseries)
library(openxlsx)
setwd("C://Users//Ganesh//Downloads")

data <- read.xlsx("Airlines+Data.xlsx",1)

#EDA

hist(data$Passengers)
plot(data$Passengers)
#meanly 100 to 200 number of passengers travelling consistenly in all durations.
tsdata <- ts(data$Passengers, frequency = 30, start= c(0))
plot(tsdata)
#it has a multiplicative seasonality and it may be a constant increase through every month

#splitting the data into train and test

train =  tsdata[0:76]
test = tsdata[76:96]

#holtwinter method

hw_a <- HoltWinters(train, alpha= 0.2, beta = F, gamma = F)
hw_a
plot(hw_a)

predhwa <- forecast(hw_a)
predvalues <- predict(hw_a, n.ahead = 21)
predvalues
plot(predhwa)

hwa_mape <- MAPE(predvalues, test)* 100
hwa_mape #26.9
#####
hw_b <- HoltWinters(train, alpha=0.4, beta = 0.4 , gamma = F)
hw_b
plot(hw_b)
predhwb <- forecast(hw_b)
predvalueshwb <- predict(hw_b, n.ahead = 21)
predvalueshwb
plot(predhwb)

hwb_mape <- MAPE(predvalueshwb, test)
hwb_mape #0.12
#####
hw_c <- HoltWinters(train, alpha=0.2, beta = 0.1, gamma = F)
hw_c
plot(hw_c)
predhwc <- forecast(hw_c)
predvalueshwc <- predict(hw_c, n.ahead = 21)
plot(predhwc)

hwc_mape <- MAPE(predvalueshwc, test) * 100
hwc_mape #17.64

####
#moving average
ma_a <- sma(train, alpha= 0.2)
ma_a
plot(ma_a)

predictma_a <- data.frame(predict(ma_a, h = 21))
predictma_a
mapa_ma_a <- MAPE(predictma_a$Point.Forecast, test)* 100
mapa_ma_a #18.05
###

ma_b <- sma(train)

ma_b
plot(ma_b)

predictma_b <- data.frame(predict(ma_b, h = 21))
predictma_b
mapa_ma_b <- MAPE(predictma_b$Point.Forecast, test)* 100
mapa_ma_b #18.05


#hwb_mape has a lowest value of MAPE. hence it is considered as the best model.