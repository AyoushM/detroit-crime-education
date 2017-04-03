library(acs)
library(geo)
library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(reshape) 
library(data.table)
library(astsa)


crime <- read.csv("crimewithzips.csv")
summary(crime)
colnames(crime)

#Selecting just month and year columns
crimebytime <- crime[,c(17:18)]

#Adding a dummy column for crime
crimebytime$DUMMY <- 1
crimebytime
tseries <- dcast(crimebytime, YEAR~MONTH,fun.aggregate = sum,value.var = "DUMMY")
tsr <-as.vector(t(tseries[,2:13]))
tsr <- tsr[1:96]

ts(tseries)

#Creating a time series 
times <- ts(tsr,frequency=12,start=c(2009,1))
plot(times)

#Decomposition of the time series
comp <- decompose(times)
plot(comp)


#When are crimes most likely


#Where do crimes occur


#Machine Learning
