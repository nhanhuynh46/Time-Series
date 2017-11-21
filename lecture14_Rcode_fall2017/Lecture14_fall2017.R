### Review codes in lecture 14 slides ######

## 1. Simulate 200 values of ARMA(2,1):
# where phi_1=1.3, phi_2=-0.7, and theta_1=0.7
set.seed(436)
arma21=arima.sim(list(order=c(2,0,1),ar=c(1.3,-0.7),ma=0.7),n=200,sd=1)
# order=c(2,0,1): combination of ar(2) and ma(1)
# list 2 coefficients of AR(2) and list a coefficient for ma(1)
# simulate 200 observations with variance of white noise of 1
plot.ts(arma21,lwd=2,ylab="ARMA(2,1)")

# study linear trend (if exists) by fitting a linear model (in this case, the simulated observations are our response variable, and time is the covariate)
abline(lm(arma21~as.numeric(1:200)),lwd=2) # no linear trend
abline(h=mean(arma21),col="red",lwd=2,lty=2) ## adding a horizontal line to the plot; horizonal line is the average of arma21 over time --> constant

# examine the behavior of ACF and PACF plot:
op=par(mfrow=c(1,2))
acf(arma21,lag.max = 100,main="ACF for ARMA(2,1)",lwd=2)
pacf(arma21,lag.max=50,main="PACF for AMRA(2,1)",lwd=2)
par(op)
## acf behavior: sample acf has sinusoidal bahavior (has sort of wave forms) --> typcial bahavior for AR or ARMA model. In this case, it is hard to use ACF plot to determine order of MA process.
## pacf behavior: the first 2 lags are highly sinigificant. Perhaps if we consider fitting an AR model, it will be AR(2).

# now, we're interested in knowing if this process is causal and invertible. Recall that: for ARMA(p,q) process, check if it is causal if we can construct AR polynomial equation and all the roots are outside the unit circle. Similary, the process is invertible if MA polynomial equation has all roots lying outside the unit circle. 
# in this section, we check causality and invertibility using written function "spec.arma":
setwd("~/Dropbox/UCSB Classes/274/lab7_fall2017") # set working directory to the folder that contains the R file
source("spec.arma.R")
spec.arma(ar=c(1.3,-0.7),ma=0.7)
# after running the code, no errors appear --> indicates model is both causal and invertible. 
# another example:
spec.arma(ar=c(-1.6),ma=c(1.2)) 
# we receive warinings that model is neither causal nor invertible. 

## To visualize the roots of AR and MA polynomial equations, we can use plot.roots.R:
source("plot.roots.R")
op=par(mfrow=c(1,2))
plot.roots(NULL,polyroot(c(1,-1.3,0.7)),main="roots of AR polynomial equation")
plot.roots(NULL,polyroot(c(1,.7)),main="roots of MA polynomial equation")
par(op)
# both equations have complex roots. The blue dots are imaginary part and red dots are real part. Pay attention to the real part. All of red dots are outside the unit circle --> Confirm the results.

############################## Fitting model####################
# keep in mind that we know the true form of this time series model (ARMA(2,1)) but pretend that we don't know, or we are only given the time series to analyze. Using information we've learned so far to fit this model, we want to see if our final chosen model will be close to the true form of this model. 
# in practice, we often don't know true form of time series model.

# 1a. Using Yule_Walker equations to estimate AR model:
fitar=ar(arma21,method="yule-walker")
# this method also gives sample mean, standard deviations of the estimated coefficients and errors variance estimate:
names(fitar) # list of all components in the result model
fitar$x.mean # sample mean
fitar$ar # estimated coefficients
fitar$var.pred # estimated variance for white noise

# 2. Using a MLE method:
mleAR= arima(arma21,order=c(2,0,0),method="ML")
mleAR

# 3. Using for loop to fit several ARMA models and pick one that has smallest value in AICc:
library("qpcR",quietly=TRUE) # load package qpcR, suppress other information while loading
AICc_list=c()
for (i in 1:4){
  for (j in 1:4){
    print(c(i,j))
    current=arima(arma21,order=c(i,0,j),method="ML")
    print(AICc(current))
    AICc_list=c(AICc_list,AICc(current))
  }
}
min(AICc_list) 
# ARMA(3,2) has the smallest AICc of 609.5643
# ARMA(3,3) has the second smallest AICc of 609.7684
# AMRA(4,3) has the third smallest AICc of 609.8773
# these 3 models has pretty close AICc values --> pick ARMA(3,2)
# Note: the true model ARMA(2,1) has AICc of 611.3684

# Using this suggestion, let's fit ARMA(3,1):
arima(arma21,order=c(3,0,2),method="ML")

# 4. Using automatic arima function to fit this generated data:
library(forecast)
autofit=auto.arima(arma21)
# it suggests to fit ARMA(2,1) --> this is exactly the true form of the underlying model.
# we know true values of the coefficients; compare with estimated coefficients, they are very close!
# ADVICE: DON'T PUT ALL OF YOUR HOPE ON AUTO.ARIMA FUNCTION. It doesn't gaurantee to give a final model which matches the underlying model. Don't trust me? Go back and simulate a new time series model and use auto.arima() again. 

## Using method (3) and method (4): method (3) suggests ARMA(3,2) and method (4) suggests ARMA(2,1) --> pick the simpler one between these two. 

####### Model forecasting ########
# 2 important assumptions to check: normality and independence. 
# we assume that the white noise is from normal distribution
# if model fits the data well, it should explain most of the systematic trend in the data. What is left is the random errors, and we expect them to be uncorrelated. 
res.autofit= residuals(autofit)

# a. check normality
op=par(mfrow=c(1,2))
# histogram of the residuals
hist(res.autofit,probability = TRUE,breaks=10,main="Histogram of the residuals")
lines(density(res.autofit),lwd=2,col="red")
# Q-Q plot of the residuals
qqnorm(res.autofit)
qqline(res.autofit)

# b. check uncorrelation:
Box.test(res.autofit,type="Ljung-Box")
# p-value > alpha --> fail to reject the null. Conclusion: residuals are uncorrelated.
# Check ACF and PACF:
acf(res.autofit,lag.max=50,main="ACF for the residuals",lwd=2)
pacf(res.autofit,lag.max=50,main="PACF for the residuals",lwd=2)
par(op)
# both plots suggest no sinigificant correlation between lags. 

########################################################
########################################################
######### Part 2: Model building for real data #########
## The Federal Reserve Board Production Index ####

# how to read a data set from an online source:
library("RCurl")
myfile<- getURL("http://www.stat.pitt.edu/stoffer/tsa2/data/prod.dat.txt")
mydat<- read.table(textConnection(myfile),header=FALSE) 
# textConnection(): input the string which is the url address
mydat<- ts(mydat)
is.ts(mydat) # confirm that the current data is stored as a time series class
# or:
class(mydat)
length(mydat) # contain 372 observations

# plot this time series:
plot.ts(mydat,lwd=2)
# at this point, we don't know the frequnecy of the data (if exists!). However, it shows strong sign of linear trend --> the time series is not stationary. The mean increases in time, not constant over time. 

# determine deterministic linear trend
abline(lm(mydat~as.numeric(1:length(mydat))),lwd=2,lty=2)
# significant positive linear trend. 

# let's look at the acf and pacf plots to examine their bahavior:
op=par(mfrow=c(1,2))
acf(mydat,lag.max=100,main="Sample ACF")
pacf(mydat,lag.max = 50,main="Sample PACF")
par(op)
# ACF plot: decay slowly --> correlation between lags are highly significant. If we move forward on time, correlation decreases in magnitute but still significant --> indicates that correlation depends on time. Therefore, confims the observation that data is not stationary. # PACF: the partial correlation at lag 1 is almost as strong as 1. 

## we can also use tsdisplay() in library 'forecast' to display time series plot, acf and pacf altogether:
tsdisplay(mydat)

# in this case, we can try to take the difference at lag 1 to remove strong linear trend. 
mydat1=diff(mydat, lag=1)
ts.plot(mydat1,lwd=2)
# the linear trend is moved; however, when moving forward on time, variance increases. 
# let's plot sample ACF and PACF again:
op=par(mfrow=c(1,2))
acf(mydat1,lag.max=50,main="Sample ACF",lwd=2)
pacf(mydat1,lag.max = 50,main="Sample PACF",lwd=2)
par(op)
# we see strong periodic pattern in ACF. There is a high peak at every multiple of 12 lag --> suggest seasonal frequency is 12. To remove seasonal trend, take difference at lag 12.

mydat112= diff(mydat1,lag=12)
ts.plot(mydat112,lwd=2)
# we see further improvement. Mean is constant over time, and excepts some certain time point, the variance seems to be constant. 
op=par(mfrow=c(1,2))
acf(mydat112,lag.max=50,main="Sample ACF",lwd=2)
pacf(mydat112,lag.max = 50,main="Sample PACF",lwd=2)
par(op)

############ how should we determine non-seasonal and seasonal terms while examining ACF and PACF plot ##############
# Non-seasonal terms: Examine the early lags (1,2,3,...) to judge non-seasonal terms. Spikes in the ACF (at low lags) indicate non-seasonal MA terms. Spikes in the PACF (at low lags) indicates possible non-seasonal AR terms.
# Seasonal terms: Examine the patterns across lags that are multiple of S. For example, for monthly data, look at lags 12,24,36 and so on. 
####################################################

# in this problem: (a) to determine non-seasonal terms: we look at ACF and observe geometric decay --> AR term. Using PACF plot, the early 2 lags are significant --> p=2, q=0. Also, remeber we first difference at lag 1 to remove the linear trend --> d=1.
# (b1) to determine seasonal trend: using ACF, peaks at lag 12 and 24: P=0 and Q=2 (lag 36 is marginal sinigificant; it is your choice to consider Q=3 but let fit this model as well to see how it turns out). Also, remember we difference at lag 12 to remove the seasonal trend--> D=1. 
# (b2): using PACF, significant peaks lag 12 and 24 --> P=2 and using ACF plot: highest peak at lag 12 --> Q=1. Again D=1.
# can try to fit SARIMA(2,1,0)x(0,1,2)S=12 (fit1)
# or SARIMA(2,1,0)x(0,1,3)S=12 (fit2)
# or SARIMA(2,1,0)x(2,1,1)S=12 (fit3)


# Note: of course you can make similar argument to try other SARIMA models. As long as your argument about the use of ACF and PACF makes sense, try them out!

# to fit sarima models, need to load the package 'astsa'
library("astsa")
? sarima
fit1=sarima(mydat,2,1,0,0,1,2,12)
fit1

fit2=sarima(mydat,2,1,0,0,1,3,12)
fit2

fit3=sarima(mydat,2,1,0,2,1,1,12)
fit3

# Fit 2 produces the smallest value in AICc. Let's do model checking:
# also: using both acf plot and Q-Q plot --> some potential outliers and some certain time lags have marginally significant correlation. 
# if we ignore these violation and forecast:
sarima.for(mydat,n.ahead = 12,2,1,0,0,1,3,12)


