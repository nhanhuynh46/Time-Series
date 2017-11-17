# import the wine csv file into R:
setwd("~/Dropbox/UCSB Classes/274/lab4_pstat274_fall2017")

wine.csv = read.table("monthly-australian-wine-sales-th.csv",sep=",", header=FALSE, skip=1, nrows=187) # price of wine sales
wine.csv[1:15,]
# notice the time period --> each year has 12 values: each value represents the price of wine each month. 


# convert wine.csv into time series 
wine = ts(wine.csv[,2], start = c(1980,1), frequency = 12)
# using the second column of wind.csv 
# start argument: starting period is 1980 Jan
# frequency argument (the number of observations per unit of time): 12 months per year

## Fun function: we can use findfrequency() in package 'forecast' to find frequency:
findfrequency(wine)

########################## part a ##########################
# plot wine as time series:
plot(window(wine,start=c(1980,1)),ylab='wine',lwd=2,type="c")
Month=c('J','F','M','A','M','J','Ju','A','S','O','N','D')
points(window(wine,start=c(1980,1)),pch=Month,col=1:12)
# describe the pattern of this plot:
# There is an increasing trend in the data.
# The variability of the data changes according to time (as seen roughly by the changing range of values across different time intervals). When time increases, the price of wine also increases. The variance increases as the mean increases. 
# There is also a strong seasonal component (predictable change/similar pattern that repeats over a one-year period: increases for half the year and then decreases, then the pattern is repeated for the next years).


####################### part b ############################
# Use Box-Cox transformation to transform data --> purpose: stabilize the variance (we aim for a new set of transformed data with constant variance --> stationary!!!)

# To do this, we fit a linear regression model with time as independent variable and wine as dependent variable:
library(MASS)
t = 1:length(wine) # create indepedent variable vector
fit = lm(wine ~ t) # fit a linear model
bcTransform = boxcox(wine ~ t,plotit = TRUE)
# plotit=TRUE --> provide visulization of values of lambda and the log-likelihood. 
# we pick value of lambda such that it maximizes the log-likelihood function of the transformed data. Pay attention to the 95% CI of lambda. 
# To pick the value of lambda that maximized the log likelihood:
bcTransform$x[which(bcTransform$y == max(bcTransform$y))]
# x here is lambda value; y is log-likelihood
# suggests lambda is 0.424242

# Notice that 95% CI contains 1/2, let's pick this value and do transformation. Also, it has nicer interpretation than picking 0.424242
wine.bc =wine^(1/2) # square-root transformation
# plot both original data and transformed data on the same data set:
par(mfrow = c(1,2))
ts.plot(wine,main = "Original data",ylab = expression(X[t]),lwd=2)
ts.plot(wine.bc,main = "Box-Cox tranformed data", ylab = expression(Y[t]),lwd=2)

# do you see the difference after transforming data?
# pattern between original data and transformed data is the same
# however, variance got reduced after transforming data (which is obvious since new values are square root of old values)
var(wine)
var(wine.bc)

# Calculate ACF (auto-correlation) and PACF (partial auto-correlation) for the transformed data:
acf(wine.bc,lag.max = 60,main = "")
pacf(wine.bc,lag.max = 60,main = "")
title("Box-Cox Transformed Time Series", line = -1, outer=TRUE)
# ACF figure: we clearly see strong periodic pattern. At lag 0, the covariance is always 1. Starting from lag 1 to lag 6, the covariance are significant and decreasing; from lag 6 to 12: covariance values are increasing again. Specifically, the covariance at lag 12 has the greatest value (strong correlation). The pattern is repeated for next several years --> strong seasonality with seasonal component of 12 (d=12). 
# The dependence on the past tends to occur most strongly at multiples of some underlying seasonal lag d --> This is a perfect example of Multiplicative Seasonal ARIMA Models. 

### Experiment 1: Remove the trend by integration with d=1 (take the difference between X_t and X_t-1)
y1 = diff(wine.bc,lag=1)
par(mfrow=c(1,1))
ts.plot(y1,main = "De-trended Time Series",ylab = expression(nabla~Y[t]),lwd=2,col="red")
abline(h = 0,lty = 2)

# Re-calculate the sample variance and examine the ACF and PACF
par(mfrow = c(1,2))
acf(y1,lag.max = 60,main = "")
pacf(y1,lag.max = 60,main = "")
title("De-trended Time Series", line = -1, outer=TRUE)
# Result of this experiment: if we diffence at lag 1, ACF figure: we still observe strong periodic pattern. 


## Experiment 2: Remove the trend by integration with d=12. Diference at lag = 12 (cycle determined by the ACF) to remove seasonal component (To be more precise, )
y12 = diff(y1,lag=12)
par(mfrow=c(1,1))
ts.plot(y12,main = "De-trended/seasonalized Time Series",ylab = expression(nabla[12]~Y[t]),lwd=2,col="red")
abline(h = 0,lty = 2)

# Re-calculate the sample variance and examine the ACF and PACF
op = par(mfrow = c(1,2))
acf(y12,lag.max = 60,main = "")
pacf(y12,lag.max = 60,main = "")
title("De-trended/seasonalized Time Series",line = -1, outer=TRUE)
# Result of experiment 2: if we difference at lag 12, we no longer observe strong periodic pattern in ACF plot (compared to difference at lag 1)

###############################
######## Additional notes: 

# A model with no orders of differcing assumes that the original series is stationary. A model with one order of differencing assumes that the original series has a constant average trend. We difference in order to achieve stationary time series. Making forecast with stationary time series is more reliable. 

### Example to show if we take difference, stationary can be achieved even though the original time series doesn't seem to be stationary. 

# simulate MA(1) process (MA process is always stationary/linear function of past white noise)
# theta_1=0.7345

ma1 <-arima.sim(model =list(ma =c(0.7345),sd = 1),n = 200)
# create linear trend:
l<- .421*1:200
# create a new time series:
Xt=ts(l+ma1)
plot.ts(Xt) # obviously not stationary

## now, we remove the trend by take the difference between two time points (Xt and X(t-1))
Yt=diff(Xt,1)
plot.ts(Yt)

