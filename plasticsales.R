library(forecast)
library(fpp)
library(smooth)
library(tseries)
setwd("C://Users//Ganesh//Downloads")
data <- read.csv("PlasticSales.csv")
#EDA
plot(data$Sales)
hist(data$Sales)
mean(data$Sales)
#average of sale is 1162.367
#consistently highest sale occuring inbetween 1200 and 1300

tsdata <- ts(data$Sales, frequency = 12, start = c(49))
plot(tsdata)
#It has a best seasonality

#Splitting into train and test
train <- tsdata[0:48]
test <- tsdata[49:60]

#model building process

hw_a <- HoltWinters(train, alpha = 0.2, beta = F, gamma = F)
hw_a

modelhwa <- forecast(hw_a)

predvalueshwa <- predict(modelhwa, n.ahead = 12)
predvalueshwa

mapahwa <- MAPE(predvalueshwa, test) * 100
mapahwa

#model2
hw_b <- HoltWinters(train, alpha = 0.2, beta = 0.1, gamma = F)
hw_b

modelhwb <- forecast(hw_b)

predvalueshwb <- predict(modelhwb, n.ahead = 12)
predvalueshwb

mapahwb <- MAPE(predvalueshwb, test) * 100
mapahwb

#model3
hw_c <- HoltWinters(train, alpha = 0.2, beta = 0.1, gamma = 0.1)
hw_c

modelhwc <- forecast(hw_c)

predvalueshwc <- predict(modelhwc, n.ahead = 12)
predvalueshwc

mapahwc <- MAPE(predvalueshwc, test) * 100
mapahwc

#moving average

sma_a <- sma(train, alpha = 0.2)
sma_a

predictsma_a <- data.frame(predict(sma_a, h = 12))
predictsma_a

mapa_sma_a <- MAPE(predictsma_a, test) 
mapa_sma_a

#model2 moving average

sma_b <- sma(train)
sma_b

predictsma_b <- data.frame(predict(sma_b, h = 12))
predictsma_b

mape_sma_b <- MAPE(predictsma_b, test)
mape_sma_b

#MAPE value is less in the moving average model2... 1.15 #It is the best model