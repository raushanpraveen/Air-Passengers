data("AirPassengers")
AP <- AirPassengers

plot(AP)

abline(reg = lm(AP~time(AP)))



boxplot(AP~cycle(AP))

plot(log(AP))

plot(diff(log(AP)))


library(tseries)

acf(diff(log(AP)))
pacf(diff(log(AP)))


fit <- arima(log(AP), c(0,1,1), seasonal = list(order= c(0,1,1), period = 12))

pred <- predict(fit, n.ahead = 10*12)

pred1 <-2.718^pred$pred

ts.plot(AP, 2.718^pred$pred, log= "y", lty= c(1,3))


datawide <- ts(AP, frequency = 12,start = c(1949,1), end = c(1959,12))

fit <- arima(log(datawide), c(0,1,1), seasonal = list(order= c(0,1,1), period = 12))

data1 <- head(pred1,12)

predicted_1960 <- round(data1,0)

original_1960 <- tail(AP,12)


ts.plot(AP, 2.718^pred$pred, log= "y", lty= c(1,3))






