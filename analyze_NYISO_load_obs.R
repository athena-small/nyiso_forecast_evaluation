#  File analyze_NYISO_load_obs.R
#  Performs some basic analysis of NYISO electricity load data
#  By A.A. Small, 2013-03

library(xts)
load('NYISO_load_forecast_evaluation.rda')

# Select out subset: loads for N.Y.C., 5:00 p.m. on weekdays
x <- loads.xts[.indexhour(loads.xts)==17 & loads.xts$zone==61761 & .indexwday(loads.xts) %in% 1:5]$obs
hist(x, 50)
plot(x,main="Electricity consumption by N.Y.C. 5-6p.m. weekdays",ylab="MW")

plot(loads.xts[loads.xts$zone==61761]['T17:00/T17:01']$obs)

# Tests to make sure output is OK
# head(loads.xts)
# head(loads.xts[,nLags+2-lag])
# head(loads.xts[,nLags+2])
# head(loads.xts,2)
# tail(loads.xts,2)
# nrow(loads.xts['2012-11-04 01'])
# nrow(loads.xts)/(11*24)
# loads.xts['2012-11-04 01']
# loads.xts['2012-11-04 5']
# str(loads.xts)
# attributes(loads.xts)
# attributes(index(loads.xts))$names[3]
# head(coredata(loads.xts))
# loads.xts[index(loads.xts)$hour==17]
# 
# head(errors.xts)
# str(errors.xts)
# x <- errors.xts[errors.xts$zone==61761]
# plot(x[,2])
# hist(x[,2],21)

