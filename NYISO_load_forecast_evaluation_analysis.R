#  File NYISO_load_forecast_evaluation_analysis.R
#  Performs analysis of NYISO electricity load forecasting system
#  By A.A. Small, 2013-03/2013-06
#  Written in conjunction with article, "Are load forecasters rational?"


# Load packages and datasets ----------------------------------------------

# Provides support for eXtensible Time Series (xts) objects
library(xts)

# Package "xtsExtra" provides additional plotting support for xts objects
# install.packages("xtsExtra", repos='http://r-forge.r-project.org')
library(xtsExtra)
library(graphics)



# Load datasets prepared previously ---------------------------------------

#  Load into memory a set of .xts objects: 
#  for details, see "dataPrep.R" and "README.txt"
load('NYISO_load_forecast_evaluation_dataset.rda')

#  Load list of zone IDs for the eleven NYISO zones
load('zoneData.rda')

timeZone <- 'America/New_York'
Sys.setenv(TZ=timeZone)

# CREATE LABELS FOR VARIOUS SUBSETS OF THE DATA SERIES  ----------

###  Legend:
### Subsetting by NYISO load zone:
#  *.xts$zone==61761 : data for New York City
NYC <- obs.xts$zone==61761

### Subsetting by time:
# > attributes(index(forecasts.xts))$names
# [1] "sec"   "min"   "hour"  "mday"  "mon"   "year"  "wday"  "yday"  "isdst"

###  Subsetting by time of day:
#  index(*.xts)$hour==17  : data for Hour 17, a.k.a. 5:00-5:59 p.m.
Hr17 <- index(obs.xts)$hour==17
Hr00 <- index(obs.xts)$hour==0

#  all peak-load hours

#  all off-peak hours

### Subsetting by day of the week, or workdays vs. weekends:
#    Weekdays, Monday - Friday
Weekdays <- index(obs.xts) %in% 1:5


### Subsetting by season:
#   cooling season, months May - September

#   heating season, months Nov - March

#   Daylight Savings Time versus Standard Time
DST <- index(obs.xts)$isdst


# GENERATE SUMMARY STATISTICS AND PLOTS -----------------------------------

# Realized loads for NYC, 5-6p.m. on weekdays
plot(obs.xts[NYC & Hr17 & Weekdays]$obs)

min(obs.xts[NYC & Hr17 & Weekdays]$obs)

#  - Forecast updates for New York City (zone ID 61761), hour 17 (5:00-6:00 p.m)
x <- updates.xts[NYC & Hr17][,2:7]

plot(x, screens=1:6)
colMeans(x,na.rm=TRUE)
tail(x)
sapply(as.data.frame(x[!is.na(x)]),mean)
x[!is.na(x)]
head(!is.na(x))

x[!(!is.na(as.matrix(x))[,1] & !is.na(as.matrix(x))[,2])]

head(is.na(as.matrix(x)[,]))

nrow(is.na(as.matrix(x)))

mean(!is.na(x))



sapply(x[!is.na(x)],mean)
is.na(x)[,1]==TRUE

hist(x[,1],20)

x[is.na(x)]

is.na(x)
mean(as.data.frame(x),na.rm=TRUE)

A <- c(T,T,F,T,F)
as.logical(1-A)
!A
!is.na(x)

mean(A)

# Check for biases in forecast updates -------------------------------------



# Check for autocorrelation in the forecast updating process --------------


# cov(u.df,na.rm=TRUE)
# cor(u.xts[,5],u.xts[,6],na.rm=TRUE)
# cov(u.xts,na.rm=TRUE)


# Hurricane Sandy ---------------------------------------------------------

# subset.xts <- forecasts.xts[.indexhour(forecasts.xts)==0 & forecasts.xts$zone==61761]
# subset.xts <- subset.xts['2012-11']
# l1 <- subset.xts[,'lag1']

# main = 'Effects of Hurricane Sandy on the accuracy of NYISO day-ahead load forecasts'
# sub = 'NYISO load forecasting errors for New York City, 5-6pm weekdays. Errors measure 1-day-ahead load forecasts less hourly average of realized load, in MW.'
# ylab = 'Error in day-ahead load forecast (MW)'
# plot(l1,type='o',main=main,sub=sub, ylab=ylab,cex=0.5)
# 
# hist(l1,201,xlim=c(-500,500))
# 
# # hist(x, 50)
# # plot(x,main="Electricity consumption by N.Y.C. 5-6p.m. weekdays",ylab="MW")
# # plot(forecasts.xts[forecasts.xts$zone==61761]['T17:00/T17:01']$obs)



# END OF CODE -------------------------------------------------------------


# x <- errors.xts[errors.xts$zone==61761 & index(errors.xts)$hour==17]['2012-10-06/2013-02-25']
# x <- x[,2:7]
# plot(x[,'lag6'])
# hist(x[,'lag1'],102,xlim=c(-2000,2000))
# 
# u.xts <- updates.xts[updates.xts$zone==61761 & index(updates.xts)$hour==17]
# tail(u.xts)
# plot(u.xts[,'lag6.5'])
# tail(updates.xts)
# tail(errors.xts)
# u <- u['2012-11']
# u
# e <- errors.xts[errors.xts$zone==61761 & index(errors.xts)$hour==17]
# hist(u.xts[,'lag2.1'])
# mean(u.xts[,'lag2.1'])
# summary(u.xts)
# mboxplot(u.xts)
# 
# sqrt(var(u.xts['2012-10-06/']))
# cv(u.xts)
# plot(u$'lag1-0', ylim=c(-500,500))
# plot(u$'lag2-1', ylim=c(-500,500))
# plot(e$'lag3-2', ylim=c(-500,500))
# plot(e$'lag4, ylim=c(-500,500))
# plot(e$lag5, ylim=c(-500,500))
# plot(e$lag6, ylim=c(-500,500))
# 
# is.na(e)
# e <- e[is.na(e)==FALSE]
# var(e)
# plot(hist(e[,'lag6']))
