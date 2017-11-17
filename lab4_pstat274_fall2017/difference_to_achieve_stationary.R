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


