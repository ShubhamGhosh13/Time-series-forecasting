library(MLmetrics) # to use MAPE function.
library(tseries)
#Model 1

data <- read.csv("C:\\Users\\Rahul\\Desktop\\redhat\\tou.csv")
class(data)
tst <- ts(data, frequency = 12, start = c(1992))
tst
plot(tst)    #The plot shows that the dataset is already stationary as the mean and variance is
            #constant across the the series.
plot(aggregate(tst, FUN = mean))   #we can see gradual increase in trend since 2009.
cycle(tst)
monthplot(tst)  #There are seasonality in the data set as we can see is its max in the months june, july and august.
boxplot(tst~cycle(tst)) 

#Another way of testing stationarity is by using Augmented Dickey-Fuller Test.
#In this we have also taken care of unequal variances,if any, by using log.
#and addressed the trend component by taking difference of the series.
adf.test(diff(log(tst)), alternative="stationary", k=0)

#next step is used to find the right parameters used in the arima model.
# AR I MA
# P  D  Q

#using auto correlation function we'll find the values of P, D and Q which is needed for building arima model.

acf(tst)
pacf(tst)

# lets fit an arima model and predict the future of 2 years.
fit <- arima(tst, c(0,1,2),seasonal = list(order = c(0,1,2),period = 12))
pred <- predict(fit, n.ahead = 2*12)
round(pred$pred, digits = 0)

ts.plot(tst,pred$pred, lty = c(1:3))  #The plot shows us that there has been decrease in the overall night stay for the coming years.
    

#Testing out model for predicting values of 2014 and 2015 and comparing it with original values of the same year.
td <- ts(data, frequency = 12, start = 1992, end = c(2013,12))
fit1 <- arima(td, c(0,1,2),seasonal = list(order = c(0,1,2),period = 12))
pred1 <- predict(fit1, n.ahead = 2*12)
round(pred1$pred, digits = 0) # the values we got here for 2015 is not an exact match as that of the original data.
# we can calculate the error by using MAPE(Mean Absolute Percentage Error) function.

as.numeric(pred1$pred)
MAPE(pred1$pred,tst) #Lower the value of MAPE means lower is the error rate and our projection is more closer to accurate projection.

# Model 2: Built for past 7 years of data from 2009 to 2015.
#Reason for selecting this range is that after 2009 there has been gradual increase in the
#overnight stay as per the original data and almost no ups and downs is seen.

data2 <- read.csv("C:\\Users\\Rahul\\Desktop\\redhat\\tou2.csv")
tst2 <- ts(data2, frequency = 12, start = c(2009),end = c(2015,12))
tst2
plot(tst2)
adf.test(diff(log(tst2)), alternative="stationary", k=0)
acf(tst)
pacf(tst2)
fit2 <- arima(tst2, c(0,1,2),seasonal = list(order = c(0,1,2),period = 12))
pred2 <- predict(fit2, n.ahead = 2*12)
round(pred2$pred, digits = 0)
ts.plot(tst,pred2$pred, lty = c(1:3))
#The plot shows us that even 7 years of data gives us almost the same projections as that of using past 24 years of data.
#Upon looking closely upon the numbers, we can see that model 2's projecting numbers are even more closer to that of 2014 and 2015.
#This means we can make more accurate projections using the dataset ranging from the time when the time series is more stationary.
