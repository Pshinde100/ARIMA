## Time series ARIMA( Auto Regressive Integrated Moving Average)

data("AirPassengers")
plot(AirPassengers)
summary(AirPassengers)
abline(reg = lm(AirPassengers ~ time(AirPassengers)))
## the mean for the above plot is not constant, the variance mapped using creast and troughs
## is also not same above and below the regression line

class(AirPassengers)
start(AirPassengers)  # start of the time series
end(AirPassengers)  # end of the time series

frequency(AirPassengers) # cycle of the  time series is 12 month a year
summary(AirPassengers) # the no of passengers are distributed across the spectrum

cycle(AirPassengers) ## this print the cycle across year

plot(aggregate.ts(AirPassengers, FUN = mean))
boxplot(AirPassengers ~ cycle(AirPassengers))

## To make sure that the variance is constant through time series, we take log of the data

par(mfrow = c(1,2))
plot(log(AirPassengers))
plot(AirPassengers)

## To make sure that the mean is constant through the time series , we take the derivative of the 
# log of data

plot(diff(log(AirPassengers)))  # constant mean around zero
plot(AirPassengers)


## AR I MA
## p  d  q
## p, d and q are three parametersr that are used to build a time series model using ARIMA

acf(AirPassengers)  ## acf function to be directly applied if the time series is stationary

acf(diff(log(AirPassengers)))  ## acf function to be applied after transformation, if the series is
                                ## not statinary , this determine the value of q.

## Interpretaion : The line that get inverted , the index of the line just before that is q :in our case 
## line is 1 hence q is 1

pacf(diff(log(AirPassengers)))

# Intepretation: p is one line before the line that gets imvented, here the
# function that is used to generate this graph iscalled auto correlation
#function'; in our case the value of p is 0
#value od d : d determines the number of times you do differentiation to
#stationarize the time series, in our case we did differentiiation just once 
#hence d will be 1.

#lets fit the ARIMA model and predict the next 10years#
fit <- arima(log(AirPassengers), c(0,1,1), seasonal = list(order = c(0,1,1),
                                                           period = 12))

pred <- predict(fit, n.ahead = 10*12)  ## for 10 year prediction
pred1 <- 2.718 ^ pred$pred
## prediction are in the log form , hence to  convert to them to decimal interpretable values 
## we use the shown formula, the value of e is 2.718
par(mfrow= c(1,1))

## plotting the model
ts.plot(AirPassengers, 2.718 ^ pred$pred, log = "y", lty= c(1,3))

## tesing the model

datawide <- ts(AirPassengers, frequency = 12, start = c(1949,1),
                           end = c(1959,12))
fit <- arima(log(datawide), c(0,1,1), seasonal = list(order = c(0,1,1), 
                                                      period = 12))
pred <- predict(fit, n.ahead = 10*12)
pred1 <- 2.718^pred$pred
data1 <- head(pred1, 12)
predict_1960 <- round(data1, digits = 0)
original_1960 <- tail(AirPassengers, 12)
predict_1960
original_1960

