#  File NYISO_load_forecast_evaluation_analysis.R
#  Performs analysis of NYISO electricity load forecasting system
#  By A.A. Small, 2013-03/2013-06
#  Written in conjunction with article, "Are load forecasters rational?"

# Load packages and datasets ----------------------------------------------
require(stats)
# Provide support for eXtensible Time Series (xts) objects:
library(xts)
library(zoo)
# Provide additional plotting support for xts objects:
# install.packages("xtsExtra", repos='http://r-forge.r-project.org')
library(xtsExtra)
# Provide support for NERC/NYISO holidays:
library(timeDate)    
# Provide support for handy date-time utilities:
library(lubridate)
# Provide support for graphics:
library(graphics)
# Provide support for R-to-LaTeX conversion of output
library(xtable)
# library(stargazer)
# library(Hmisc)     # Provides support for latex() function

# Load datasets prepared previously ---------------------------------------

#  Load into memory a set of .xts objects: 
#  for details, see "dataPrep.R" and "README.txt"
load('NYISO_load_forecast_evaluation_dataset.rda')

#  Load list of zone IDs for the eleven NYISO zones
load('zoneData.rda')

timeZone <- 'America/New_York'
Sys.setenv(TZ=timeZone)

# CREATE LABELS FOR VARIOUS SUBSETS OF THE DATA SERIES  ----------

# Create a set of logical (TRUE/FALSE) vectors that pick out specific data subsets

### Subsetting by NYISO load zone:
NYC <- obs.xts$zone==61761      # Picks out records with data for New York City

### Subsetting by time:
# 'Time' is a vector of POSIXlt date-time objects spanning the period of record
#  Each element in Times corresponds to the start of one hour in the series
Time <- index(obs.xts)

#  Subsetting by time of day:
Hr17 <- hour(Time)==17 # Hour 17, a.k.a. 5:00-6:00 p.m.
Hr00 <- hour(Time)==0  # 

# Subsetting by peak load vs. off-peak hours
#   (For definition, see README.txt)
PeakHours <- hour(Time) %in% 7:22   # 7:00 a.m. - 11:00 p.m.
Weekdays  <- wday(Time) %in% 1:5    # Monday - Friday
Holidays  <- yday(Time) %in% yday(holidayNERC())
onPeak    <- PeakHours & Weekdays & !Holidays

# Subsetting by season:
Spring <- month(Time) %in% 3:5
Summer <- month(Time) %in% 6:8
Fall   <- month(Time) %in% 9:11
Winter <- month(Time) %in% c(12,1,2)

#   Daylight Savings Time versus Standard Time
DST <- dst(Time)

# Various smaller subsets, for closer examination
NYCpeak17 <- NYC & onPeak & Hr17

# GENERATE SUMMARY STATISTICS AND PLOTS -------------------------------

# Realized loads for NYC, 5-6p.m. on weekdays
# plot(obs.xts[NYCpeak17 & Summer]['2012']$obs)
# 
# 
# NYC17peakLoads <- obs.xts[NYCpeak17]$obs
# 
# tail(NYCpeakLoads)
# str(NYCpeakLoads)
# dim(NYCpeakLoads)
# 
# obs.xts
# tail(NYC)
# tail(onPeak)
# tail(NYC & onPeak, 25)
# 
# tail(month(index(NYCpeakLoads)))
# NYC17peakLoads <- NYC17peakLoads[!is.na(NYC17peakLoads)]
# x <- NYCpeak17
# 
# tapply(NYCpeak,INDEX=month(index(NYCpeak)),mean)/sqrt(tapply(NYCpeak,INDEX=month(index(NYCpeak)),var))
# max(obs.xts[NYC & Hr17 & onPeak & Summer]$obs, na.rm=TRUE)
# Index <- index(x)
# Numx <- xts(!is.na(x),order.by=Index)
# head(NYCpeak)
# head(acf(x))
# class(acf(x))
# acfx$lag <- acfx$lag/3600
# acfx
# plot(acfx)
# index(x)
# acfx <- acf(x)
# class(acfx)
# str(acfx)
# frequency(NYCpeak)
# 
# x[!is.na(x[,1]) & !is.na(x[,2])]
# 
# head(x[,1][Numx[,1]])


# Forecast trends


# Q: Can the trend in successive forecasts help predict the direction of the last update?

first5Fcsts <- forecasts.xts[NYC & Hr00][,2:6]
lastUpdate  <- updates.xts[NYC & Hr00][,7]

goodRows <- complete.cases(first5Fcsts) & complete.cases(lastUpdate)

f5 <- first5Fcsts[goodRows]
lu <- lastUpdate[goodRows]

class(f5)
f5 <- t(as.matrix(first5Fcsts))

lag <- 1:5
model <- lm(fVecs ~ lag,na.action=na.exclude)
fs <- model$coefficients[2,]

us <- lastUpdate
ufs <- as.matrix(cbind(us,fs))
plot(ufs)
cor(ufs)

#  Forecast errors

# x <- errors.xts[NYC][,2:7]
# RowsWithNoNAs <- !is.na(as.matrix(x))[,1] & !is.na(as.matrix(x))[,2] & !is.na(as.matrix(x))[,3] & !is.na(as.matrix(x))[,4] & !is.na(as.matrix(x))[,5] & !is.na(as.matrix(x))[,6]
# x <- x[RowsWithNoNAs]
# cor(x)
# plot(x)
# 
# errorFrac.xts <- errors.xts/forecasts.xts
# errorFracNYC.xts <- (errors.xts[NYC][,2:7])/(forecasts.xts[NYC][,2:7])
# head(errorFracNYC.xts)
# tail(errorFracNYC.xts)
# plot(errorFracNYC.xts)
# abs(tail(errors.xts))
# x <- errorFracNYC.xts[abs(errorFracNYC.xts[,1])<0.5]
# plot(x)
# breaks <- seq(-0.45,0.45,by=0.005)
# yLim=c(0,4000)
# xLim=c(-0.25,0.25)
# hist(x[,1],breaks=breaks,ylim=yLim,xlim=xLim)
# sqrt(var(x))
# cov(x)==var(x)
# cor(x)

#  Forecast updates for N.Y.C. (zone ID 61761), hour 17 (5:00-6:00 p.m), on-peak:
x <- updates.xts[NYC][,2:7]
RowsWithNoNAs <- !is.na(as.matrix(x))[,1] & !is.na(as.matrix(x))[,2] & !is.na(as.matrix(x))[,3] & !is.na(as.matrix(x))[,4] & !is.na(as.matrix(x))[,5] & !is.na(as.matrix(x))[,6]
x <- x[RowsWithNoNAs]
cor(x)


# Generate a LaTeX table of correlation coefficients
latex(cor(x))


# Index <- yday(index(x))
# 
# x[wday(index(x))==1]
# 
# tapply(x,INDEX=Index,cor)
# 
# 
# plot(x, screens=1:6)
# 
# Vec <- tapply(x[,6],INDEX=Index,mean)/sqrt(tapply(x[,6],INDEX=Index,var,na.rm=TRUE))
# hist(Vec)
# qqnorm(Vec)
# qqplot(Vec)
# apply(x[,1:6], 2, FUN = mean, na.rm=TRUE)
# apply(x[,1:6], 2, FUN = sd, na.rm=TRUE)
# apply(x[,1:6], 2, FUN = mean, na.rm=TRUE)/apply(x[,1:6], 2, FUN = sd, na.rm=TRUE)
# sapply(x, FUN = cov, na.rm=TRUE)
# 
# sapply(x[,1], FUN=hist)
# 
# mean(x[,6])
# 
# x[!is.na(x)]
# head(!is.na(x))
# 
# mean(x[,1])
# 
# head(as.matrix(x[,1:6]))==head(as.matrix(x))
# 
# class(as.matrix(x))
# index(as.matrix(x))
# head(is.na(as.matrix(x)[,]))
# str(as.matrix(x))
# nrow(is.na(as.matrix(x)))
# 
# xNArows <- union(index(x[is.na(x[,1])]),index(x[is.na(x[,2])]))
# mean(x[,1][!xNAs])
# x[is.na(x),1]
# length(is.na(x))
# length(x)
# head(is.na(x))
# dim(is.na(x))
# 
# xNArows
# 
# hist(x[,6],20)
# 
# hist(x[,1])
# x[is.na(x)]
# 
# is.na(x)
# mean(as.data.frame(x),na.rm=TRUE)
# 
# A <- c(T,T,F,T,F)
# as.logical(1-A)
# !A
# !is.na(x)
# 
# mean(A)

# BIAS: Check for biases in forecast updates -----------------------------------


# AUTOCORRELATION: Check for autocorrelation in the forecast updating process --------------


# cov(u.df,na.rm=TRUE)
# cor(u.xts[,5],u.xts[,6],na.rm=TRUE)
# cov(u.xts,na.rm=TRUE)


# SANDY: Hurricane Sandy -------------------------------------------------------

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
