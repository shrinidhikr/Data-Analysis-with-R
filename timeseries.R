#TIME SERIES

View(AirPassengers)
print(AirPassengers)
class(AirPassengers)
plot(AirPassengers)
abline(lm(AirPassengers~time(AirPassengers)))
plot(aggregate(AirPassengers,FUN = mean)) #trend of data
plot(decompose(AirPassengers))

start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
cycle(AirPassengers)
boxplot(AirPassengers~cycle(AirPassengers)) #gives the seasonality
plot(log(AirPassengers))
abline(lm(log(AirPassengers)~time(AirPassengers)))
plot(diff(log(AirPassengers)))
abline(lm(diff(log(AirPassengers)))~time(AirPassengers))

#ARIMA model

acf(AirPassengers)
acf(diff(log(AirPassengers)))
#value of Q is 1 as inversion is seen at 2
pacf(diff(log(AirPassengers)))
#value of P is 0 as inversion is seen at 1
#value of D is based on no. of times diff is applied to see inversion in data

#order of input for arima p,q,d

#create an arima model using the above values
fit=arima(log(AirPassengers),c(0,1,1),seasonal = list(order=c(0,1,1),period=12))
fit
#predict using the model
pred=predict(fit,n.ahead = 10*12)
pred
#convert all values of prediction to decimal
pred1=2.718^pred$pred
pred1

#plot ts 
ts.plot(AirPassengers,pred1,log="y",lty=c(1,3))

#test the model
datawide = ts(AirPassengers,frequency = 12,start=c(1949,1),end=c(1959,12))
datawide

fit=arima(log(datawide),c(0,1,1),seasonal = list(order=c(0,1,1),period=12))
fit

pred=predict(fit,n.ahead = 10*12)
pred

pred1=2.718^pred$pred
pred1
round(pred1,digits = 0)
