# PSTAT 174/274, Spring '17,  
# Lab 5
# Examples obtained from Shumway - Time Series Analysis and its applications, 4th edition

# Page 146
#### Example 3.46 , Seasonal AR Series
# ARIMA(0,0,0)x(1,0,0)_S=12
set.seed(666)
phi= c(rep(0,11),.9) # 12 elements, the first 11 elements are 0 and the last element is .9
sAR= arima.sim(list(order=c(12,0,0), ar=phi), n=37)
# using arima.sim() to simulate 
# order(p,d,q): in this case, we want AR(12) where phi_12=.9

sAR= ts(sAR, freq=12)# convert sAR to time series format
# manually input frequency of 12
layout(matrix(c(1,1,2, 1,1,3), nc=2))
par(mar=c(3,3,2,1), mgp=c(1.6,.6,0))
plot(sAR, axes=FALSE,main='seasonal AR(1)',xlab="year", type='c')
Months= c("J","F","M","A","M","J","  J","A","  S","O","  N","D")
points(sAR, pch=Months, cex=1.25, font=4, col=1:4)
axis(1, 1:4); abline(v=1:4, lty=2, col=gray(.7))
axis(2); box()
# clear seasonal trend with d=12

# obtain theoretical ACF of SAR(12)
ACF= ARMAacf(ar=phi, ma=0, 100)
# ar=phi: AR(12) and the coeffcients of this AR(12) process is in 'phi' vector
# obtain theoretical PACF of SAR(12)
PACF= ARMAacf(ar=phi, ma=0, 100, pacf=TRUE)
plot(ACF,type="h", xlab="LAG", ylim=c(-.1,1));
abline(h=0)
# the theoretical acf decays geometrically
plot(PACF, type="h", xlab  ="LAG", ylim=c(-.1,1));
abline(h=0)

# Write out form of this SAR(12) model: X_t= phi_12*X_(t-12)+Z_t
# Since it is AR model --> in ACF plot, we see correlation between lags decays geometrically. However, ACF plot is not useful to determine number of lags should be included in the model.
# PACF plot is used to determine number of lags. Only lag 12 is significant --> model contains X_(t-12)


##### Example 3.47,  A Mixed Seasonal Model 
# ARIMA(  0  ,0,  1)×(  1  ,0,  0), s=12
# model contains non-seasonal MA(1)
# seasonal AR(1) with seasonal frequency of 12

phi= c(rep(0,11),.8) # the first 11 elements (the first 11 coeffcients in AR(12) are 0 and the last coefficient is .8) 
# calculate theorectical ACF:
ACF= ARMAacf(ar=phi, ma=-.5, 50) #theta is -0.5 for MA(1)
PACF= ARMAacf(ar=phi, ma=-.5, 50, pacf  =TRUE)
par(mfrow  =c(1,2))
plot(ACF,  type="h", xlab  ="LAG", ylim=c(-.4,1));
abline(h=0)
plot(PACF  , type="h", xlab  ="LAG", ylim=c(-.4,1));
abline(h=0)
# Strong periodic pattern in ACF --> seasonal trend. Also, ACF plot decays geometrically.  
ACF
# Spikes in ACF at low lags --> Indicate non seasonal MA component
# Spikes significantly in PACF at lag 12 (Ps=12): after this at ever lag Ps(s=2,3,4..) not significant anymore --> suggests AR seasonal component. 

# Example 3.49 Air Passenger
x= AirPassengers
lx= log(x) # take log of original ts as original ts has all positive values and we want to stabilize the variance (variance is proportional to the mean as time increases)
dlx= diff(lx) # take difference at lag 1 to remove trend
ddlx= diff(dlx, 12) # take difference at lag 12 to remove seasonal trend
plot.ts(cbind(x,lx,dlx,ddlx), main="")
# below of interest for showing seasonal RW (not shown here):
par(mfrow=c(2,1))
monthplot(dlx) # collect all values from the same month and plot them as a group --> after taking difference at lag 1, trend is still present
monthplot(ddlx) # a majority of months have similar mean 
acf(ddlx  ,50)
pacf(ddlx  ,50)
