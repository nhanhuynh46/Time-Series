# set working directory:
setwd("~/Dropbox/UCSB Classes/274/lab5_pstat274_fall2017")
# import text file in R:
data<- scan("lab5data.txt")
# Convert to time series object
data.ts <- ts(data)
ts.plot(data.ts,main = "Raw data",lwd=2)

# Transform data using boxcox()
require(MASS)
bcTransform <- boxcox(data.ts ~ as.numeric(1:length(data.ts)))
lambda = bcTransform$x[which(bcTransform$y == max(bcTransform$y))]
lambda
# Box-Cox() suggests square-root transformation 
# Note: You can select the value of lambda to maximize the log-likelihood. 
# In this problem, 95% CI contains lambda=1/2 --> Use this one to have nice interpretation. Also lambda=1/2 is close enough to lambda=0.50505
y.sqrt <- sqrt(data.ts) 

# Display the orignial time series and the transformed data
par(mfrow=c(1,2))
ts.plot(data.ts,lwd=2)
ts.plot(y.sqrt,lwd=2)

# There is a clear linear trend, which makes the time series not stationary. To remove the trend, we take the difference at lag 1. (Also: no clear seasonal trend)
par(mfrow=c(1,1))
y.sqrt1 <- diff(y.sqrt,1) # differencing at lag 1
ts.plot(y.sqrt1,main = "Differenced Data at lag = 1",ylab=expression(paste(nabla,y.sqrt)),lwd=2)
# The new time series seems to have stationary charac.

op <- par(mfrow = c(1,2))
acf(y.sqrt1)
pacf(y.sqrt1)
### Using ACF --> No MA terms. Only lag 0 is significant.
### Using PACF --> AR model at lag 5 is significant. Also, after lag 5, no other lags are significant --> AR model
### We use Yule-Walker equation to estimate the coeffcients for this AR process. Note: Yule Walker equation optimizes the coefficients for AR process (if looking at the set up of Y-W --> similar to least squares estimation methods in linear model). For MA and ARMA models, Y-W won't provide optimial results. 
?ar
# estimate the coefficients for AR process. Fit multiple AR processes and pick model with smallest value in AIC. 
(fit <- ar(y.sqrt1, method="yule-walker"))
