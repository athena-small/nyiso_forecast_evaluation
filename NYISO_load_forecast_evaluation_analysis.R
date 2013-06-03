#  File NYISO_load_forecast_evaluation_analysis.R
#  Performs analysis of NYISO electricity load forecasting system
#  By A.A. Small, 2013-03
#  Written in conjunction with article, "Are load forecasters rational?"

library(xts)
library(graphics)
load('NYISO_load_forecast_evaluation_dataset.rda')
load('zoneData.rda')
timeZone <- 'America/New_York'
Sys.setenv(TZ=timeZone)


## CURRENTLY THIS FILE IS JUST A SET OF EXAMPLE OPERATIONS ####
#  To see index of zone IDs, enter "zoneData" at the '>' prompt 
zoneData
# # Select out subset: loads for N.Y.C., 5:00 p.m. on weekdays
subset.xts <- forecasts.xts[.indexhour(forecasts.xts)==0 & forecasts.xts$zone==61761]


subset.xts <- subset.xts['2012-11']

subset.xts
subset.df <- as.data.frame(subset.xts)


coredata(subset.xts,rownames=.index(subset.xts))
.index(subset.xts)

length(coredata(subset.xts))


matrix(.index(subset.xts),coredata(subset.xts))
subset.df <- as.data.frame[subset.xts]

head(subset.xts,20)
l1 <- subset[,'lag1']

main = 'Effects of Hurricane Sandy on the accuracy of NYISO day-ahead load forecasts'
sub = 'NYISO load forecasting errors for New York City, 5-6pm weekdays. Errors measure 1-day-ahead load forecasts less hourly average of realized load, in MW.'
ylab = 'Error in day-ahead load forecast (MW)'
plot(l1,type='o',main=main,sub=sub, ylab=ylab,cex=0.5)

hist(l1,201,xlim=c(-500,500))

# hist(x, 50)
# plot(x,main="Electricity consumption by N.Y.C. 5-6p.m. weekdays",ylab="MW")
# plot(forecasts.xts[forecasts.xts$zone==61761]['T17:00/T17:01']$obs)

# Tests to make sure output is OK
# head(forecasts.xts)
# head(forecasts.xts[,nLags+2-lag])
# head(forecasts.xts[,nLags+2])
# head(forecasts.xts,2)
# tail(forecasts.xts,2)
# nrow(forecasts.xts['2012-11-04 01'])
# nrow(forecasts.xts)/(11*24)
# forecasts.xts['2012-11-04 01']
# forecasts.xts['2012-11-04 5']
# str(forecasts.xts)
# attributes(forecasts.xts)
attributes(index(forecasts.xts))$names[3]
unique(index(forecasts.xts))
length(unique(index(forecasts.xts)))
x <- unique(index(forecasts.xts))


forecasts.xts[index(forecasts.xts)$hour==17]
 
head(errors.xts)
str(errors.xts)
x <- errors.xts[errors.xts$zone==61761 & index(errors.xts)$hour==17]['2012-10-06/2013-02-25']
x <- x[,2:7]
plot(x[,'lag6'])
hist(x[,'lag1'],102,xlim=c(-2000,2000))

head(x)
tail(x)
summary(x)

f <- forecasts.xts[forecasts.xts$zone==61761 & index(forecasts.xts)$hour==17]['2012-10-6/2013-02-25']
f

e <- errors.xts[errors.xts$zone==61761 & index(errors.xts)$hour==17]
hist(e[,'lag6'],20)
sqrt(var(e['2012-10-06/']))
plot(e$lag1, ylim=c(-1500,2000))
plot(e$lag2, ylim=c(-1500,2000))
plot(e$lag3, ylim=c(-1500,2000))
plot(e$lag4, ylim=c(-1500,2000))
plot(e$lag5, ylim=c(-1500,2000))
plot(e$lag6, ylim=c(-1500,2000))

is.na(e)
e <- e[is.na(e)==FALSE]
var(e)
plot(hist(e[,'lag6']))
names(errors.xts)

head(errors.xts)
tail(errors.xts)