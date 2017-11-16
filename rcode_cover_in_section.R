### PSTAT 174/274 
### Lab 2

### this script only contains R code (for more information, go to Rmd file)

############ part 1 (white noise)

# simulate white noise from normal distribution with mean 0 and variance 1
set.seed(123) 
z_t <- rnorm(200,0,1)
plot.ts(z_t,xlab = "t",ylab = expression(z[t]),type = "l",main = "White Noise",lwd=2)
# what do we observe from this plot?
# 1. No trend 
# 2. Mean to be 0
# 3. Variance to be constant

############ part 2 (moving average)

? filter
# what is it for?
# in our problem, the current time point is the average between the previous time, the current time, and a future time point --> if we find the sum, we can find the average
# filter() in R helps find the sum base on the prior time points
# for the first time point, there is no prior time --> NA
# for the second time point, we have: 1+2+3=6
# for the third time point, we have: 2+3+4=9
# for the fourth time point, we havev: 3+4+5=12

# create a new variable y_t:
y_t = filter(z_t, filter = rep(1/3,3), sides = 2, method = "convolution")
# filter argument: rep(1/3,3): first argument to tell R to take the average (all three points have equal weight); second argument to tell R we want do the sum of 3 data time points
# method= convolution: to tell R that we want to use MA model
# what about sides=2?

## if we place these two on the same figure:
par(mfrow=c(2,1))
plot.ts(z_t,xlab = "t",ylab = expression(z[t]),type = "l",main = "White Noise",lwd=2)
plot(y_t,lwd=2,ylab=expression(y[t]))
## what do we observe from these two figures?
# figure 1: just white noise
# figure 2: 3-point moving average of Z_t. Since we take the average from the points in Z_t, Y_t is smoother. 

### Exercise (at home): see what happens if we do 5-point moving average, and then 11-point moving average. Will the curves are much more smoother than the orignial white noise plot?

########## part 3 (Signal) -- play with the code on your own time
# Some forms of signal can be expressed as a time series, some of them can't.
# A time series is always indexed by time, but a signal can be more general. 

######### part 4 (auto regression)

# create x_t variable as AR:
x_t <- filter(z_t,filter = c(1,-0.9),method = "recursive")
# method="recursive" to create AR
# two coeffcients of AR(2) is 1 and -0.9
par(mfrow=c(1,1))
plot(x_t,xlab  = "t",ylab  = expression(x[t]),type = "l", main = "Autoregressive Model",lwd=2)

######### part 5 (random walk)

# from the equation: 
# 1. simulate 500 observations of a random walk with drift =0.6 and 0.4 respectively. 

# a. simulate the white noise from normal distribution:
set.seed(1)
z_t <- rnorm(499,0,1) # note: we only simulate 499 observations

# b. create the cumulative sum of the white noise:
x_t <- c(0,cumsum(z_t)) # the first element is 0
# since we manually add the first element in here, x_t now has 500 observations

# c. Create two random walks with difference drift parameters:
rw1 <- 1:500*0.6 + x_t
rw2 <- 1:500*0.4 + x_t

# d. Display them on the same figure:
plot(rw1,type = "l",xlab = "t",ylab = expression(x[t]),
     main = "Random Walk")
# note: the first random walk has a bigger drift parameter --> above the second random walk. 
lines(rw2,col = "blue",type = "l")

############ Part 5 (Measures of Dependence of a Time Series)

# Simulate a time series of length 100 from an MA(2) process when theta_1=0.45 and theta_2=0.55
theta_1=0.45
theta_2=0.55
# simulate MA using arima.sim()
x1<- arima.sim(n=100,model=list(ma=c(theta_1,theta_2)))
# plot the sample acf using acf():
acf(x1,main=expression(theta[1]==0.45~theta[2]==0.55))





