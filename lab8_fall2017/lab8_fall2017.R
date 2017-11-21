# change working directory to the folder that contains the file we want to import 
setwd("~/Dropbox/UCSB Classes/274/lab8_fall2017")

# for more information on this data set, please visit this link below:
# https://www.census.gov/retail/index.html
# this time series data contains monthly retail and food service sales in the US from 1992-2017. In 2017, we are missing 3 realizations from Oct-Dec
retail=read.csv("retail_trade_food_services.csv")
View(retail)
# conver this to time series, specify the frequnecy and starting year
retail=ts(as.numeric(as.character(retail[-c(1:6),-1])),frequency=12,start=1992)
is.ts(retail)

# visualize:
plot.ts(retail,lwd=1.5,type="b",pch=19,cex=0.3)
# we see strong seasonal pattern plus trend. In this case, we can perform decomposition analysis to describe the trend and seasonal factors in this time series. 

############################################################
################## Decomposition Model #####################
# 2 main structures for decomposition:
# (a) additive: time series = trend +seasonal + random
# (b) multiplicative: time series = trend * seasonal * random
# when to use what? (a): seasonal variation is constant over time; (b): seasonal variation flutuates onver time. 

# for example: in this 'retail' data set, additive decomposition is applied in several steps:
# step 1: estimate the trend (linear trend, quarterly trend, cubic trend...)
# step 2: remove the estimated trend (subtract the estimated trend from the original time series).
# step 3: estimate the seasonal trend based on time series at the end of step 2.
# step 4: remove the seasonal trend (subtract the estimated seasonal factor from 'de-trend' time series).
# step 5: estimate random component (time series at the end of step 4)

decompose.result=decompose(retail)
plot(decompose.result)
# observed: original data
# trend: significantly positive linear trend
# seasonal: frequency of 12 with cosntant variation
# random: white noise 

##########################################################
##########################################################



