## analyze Lake Huron data ##
data("LakeHuron")
? LakeHuron
# Annual measurements of the level of Lake Huron in the period of 1875-1972 (unit: feet)
class(LakeHuron)
# it is stored as a time series class in R
View(LakeHuron)
# create time series plot for this data:
plot.ts(LakeHuron,lwd=2,axes=F)
axis(2)
time=1875:1972 # create time span
tlab=seq(from=1875,by=1,length.out=length(time)) # create ticks for x-axis
axis(1,at=tlab,labels=time,cex.axis=0.8)

## What kind of pattern do we see here?
# 1. Time series is not stationary: the average level is varied with time; variance doesn't remain constant over the time period. (Recall the stationary time series requires its mean, variance, and the auto-covariance are time invariant). 
# additional hypothesis test to see if time series is stationary or not: using augmented Dickey-Fuller test. The null hypothesis is time series is not stationary
library(tseries)
adf.test(LakeHuron) # we fail to reject the null as p-value is large --> therefore, time series is not stationary.

# 2. Is there any trend? It seems like overall, the water level is decreasing but the trend might be not siginficant. 
# 3. No indication of seasonal trend. 

### Plot sample ACF and PACF:
par(mfrow=c(2,1))
lines(acf(LakeHuron,lag.max=80,lwd=2,main="Sample ACF")$acf,col="red",lwd=2)
# observation? - it is slowly decreasing. The first 9 lags are highly significant. Also, no periodic pattern as we often see in times series with strong seasonal trend. This confirms our hypothesis about no seasonal trend from observing the original time series plot.

pacf(LakeHuron,lag.max=80,lwd=2,main="Sample PACF")
# observation? - the first two lags are highly significant --> AR(2) model might be a good starting model. Note: at later lags, some of them are a bit outside the scope of two blue lines --> marginally significant (no neeed to pay much attension).

######################################################
## ACF: determine the order of MA model           ####
## PACF: determine the order of AR model          ####
######################################################

# To fit a AR model --> Yule-Walker equation. Note: Yule Walker equation often yields the optimal coefficients for AR model because of its special functional form. Recall how we construct matrix form to solve the coefficients in AR --> it is the same setup as in linear model (in linear model/pstat126, using least squares method yields the optimal coefficients).
ar(LakeHuron,method="yule-walker")

# We can also use Yule-Walker equations to estimate coefficients for MA and ARMA process, but it won't yield optimal estimated coefficients b/c such processes are not linear in the parameters.
# From ACF plot, suggests we can possibly try MA with order 9. If Yule-walker equation is not a good option to fit MA model, what can we do to estimate the coefficients. In this case, estimate theta_1, theta_2,..., theta_9? --> Use 'Innovative Algorithm', which uses the functional form of autocovariance (Review this formula)
# The unknown variables are 9 coeffients plus variance of white noise. 
# Using functional form of autocovariance, we have 10 equations. For each equation we replace the the theoretical autocovarince by the sample autocovariance.
# 10 unknown variables with 10 equations --> solve these easily using computer. 
# make sure the current working directory contains innovation.R
setwd("~/Dropbox/UCSB Classes/274/lab6_pstat274_fall2017") # change this line accordingly to your current working folder

source("innovations.R")
?acf 
acvf = acf(LakeHuron, plot=FALSE, lag.max = length(LakeHuron))$acf[,1,1] * var(LakeHuron)
# first obtain sample acf using acf function in R. The result is stored as an array in R. # if we want to call out only acf value, use $acf. Also, we see [,1,1]--> dimension of the array: we want to get all indexes in the first column and slice 1. 
# recall: for MA model: correlation(k)= autocovariance(k)/autocovariance(0) --> ACVF(k)=ACF(k)*Var(0)
m = length(acvf)
lh.ia = innovations.algorithm(m, acvf)
# function has two arguments: first argument is length of acvf; second argument is the estimated values of acvf
# the coefficients for MA(9):
lh.ia$thetas[9,1:9] # goes to row 9 and obtain the first 9 elements 

# Fit the different models under consideration using maximum likelihood estimation and compare the model fits using AICC (Hint: use arima() for estimation and AICc() in R library qpcR for model comparison - you will need to install this package into R first). Which model is preferred?
? arima # fit ARIMA model for time series using 2 methods: maximum likelihood or minimize conditional sum of squares.
fit_ar2 = arima(LakeHuron, order = c(2,0,0), method = "ML")
fit_ma9 = arima(LakeHuron, order = c(0,0,9), method = "ML")
fit_ar2
# compare maximum likehood and yule-walker method for AR(2): the estimated coefficients are close. We are also given the intercept of AR(2) model (intercept is the mean of time series- similar to linear regression)
fit_ma9
# compare maximum likihood and innovation algorithm: the estimated coefficeints are not close after a first few coefficients. 

### like linear regression world, we can use model selection criterions to compare models. In this case, Akaike Information Criterion (AIC) can be deployed. The smaller AIC value, the better the model is at fitting the data. Similarly, AICc is the corrected version of AIC to incorporate data with small sample size. 
# install.packages("qpcR")
library(qpcR)
AICc(fit_ar2) # note: AIC(fit_ar2) should give you close result to this.
AICc(fit_ma9)
## Pick AR(2) model.

####### small experiment using the results we got from AR(2) #######
# if curious how theoretical acf and pacf plot of AR(2) process with same phi1 and phi2, we can simulate this model and examine the plot
ar2= arima.sim(list(ar=c(1.0436,-0.2495)),sd=sqrt(0.4788),n=100)
acf(ar2,lag.max=100,lwd=2)
pacf(ar2,lag.max=100,lwd=2)
####################################################################


### Plot the residuals of the chosen model. Do they look like white noise? Test using the Ljung Box test (Hint: function Box.text(residuals, type="Ljung")).
# if model fits the data well, it should explain systematic components in the data. The leftover is the residuals, which are supposed to be only random errors with no correlation between time lags. 
Box.test(residuals(fit_ar2), type = "Ljung")
# p-value =0.768 with null hypothesis: there are no correlations between the residuals
# we fail reject the null --> No correlations between residuals

# another function that helps perform diagnostics for time series model:
tsdiag(fit_ar2)
# 3 plots showing up: (a) standardized residuals (regular residuals got standardized --> usuage: spot outliers); (b) sample ACF for the residuals: only correlation at lag 0 is significant--> check independence purpose; (c) p-value for Ljung-Box statistic (test independence between each time lags)

### Forecast the next 10 observations using the model:
pred = predict(fit_ar2, n.ahead=10) # using genetic predict() in R to do prediction for time series model object; specifically makes 10 predictions ahead of time
# the first forecast is for 1873
# the last forecast is for 1982

# 95% lower CI of the prediction:
lower= pred$pred-1.96*pred$se
# 95% upper CI of the prediction:
upper=pred$pred+1.96*pred$se

par(mfrow=c(1,1))
ts.plot(LakeHuron,xlim=c(1875,1982),lwd=4)
polygon(c(1973:1982, rev(1973:1982)),c(lower,rev(upper)),
        col = "thistle", border = NA) # shaded the CI region first
points(1973:1982,pred$pred,pch=19) # add predicted points onto the plot
# the next 10 predicted steps show decreasing pattern in the future
lines(1973:1982,lower,col="red",lwd=4) # add 95% confidence interval for each predicted points
lines(1973:1982,upper,col="red",lwd=4)

# what is your observation after seeing this forecast?
# there is a wide 95% confidence interval --> not reliable forecast
# Also: not recommended to make prediction too far in the future. The further the prediction, there more uncertain the results are.



