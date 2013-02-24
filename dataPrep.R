#  File dataPrep.R
#  Fetches NYISO load forecasts and obs from local directory, creates data array
#  Array dimensions are: 
#    dateTime t: date+time for which forecast is valid. Class: POSIX. Range: period of record

#    hour h = 0, ..., 23: hour for which forecast is valid. Class: factor.
#    zone z = 1, ..., 11  : NYISO load control region. Class: factor. 
#    forecast lag time k=0,..., 6; number of days between forecast issued-on and valid-for dates. Class: factor.
#  Entries are forecasts (for k > 0) and obs (k=0) of load in MWh/hr
library(plyr)
library(xts)


#  Read forecast data in from external data file, and organize them in a data frame called "forecasts"
# fileName <- c("Data/forecasts_dec2007.csv")
# forecasts <- read.table(fileName, header = TRUE, sep = ",")
# dateTime <- paste(forecasts$year,forecasts$month,forecasts$day)

# Retrieve forecast data file from NYISO web site, reformat data into xts object

# Initialize parameters
URLdir <- "http://mis.nyiso.com/public/csv/isolf/"  # Location of NYISO files
startYr  <- 2013  # Year/month/day of first files
startMo  <-    2
startDay <-   17
seriesStartDate <- ISOdate(startYr,startMo,startDay,0,0,0, tz='America/New_York')

ndates <-  6  # Number of days of forecasts in each NYISIO file
nlags <- ndates+1  # Cumulative umber of forecasts issued per target hour, where lag=0 corresponds to the verifying observation.
nzones <- 11  # Number of NYISO load control zones
nhrs   <- 24  # Hrs/day, = number of forecasts issued per forecasted day

issueDate <- seriesStartDate

# For each date for which a
for(issueDate in seriesStartDate+1:2){
     Yr <- strftime(issueDate,format='%Y')
     Mo <- strftime(issueDate,format='%m')
     Day <- strftime(issueDate,format='%d')
     URL <- paste(URLdir,Yr,Mo,Day,"isolf.csv", sep="")
     fcsts <- read.table(URL, header = TRUE, sep = ",")
     
}



issueDate

URL


#  str(fcsts$Time.Stamp)

# str(fcsts)


# Create an empty array with the required dimensions
# Create a POSIXt object with the (six) forecast dates
dates <- issueDate + 3600*24*1:ndates
dates
# Create labels for the four dimensions of the array
dimnames = list(paste('Valid',strftime(dates,format='%Y-%m-%d')),paste(0:(nlags-1),'day lag',sep="-"),names(fcsts[2:12]),paste('hr',0:(nhrs-1),sep=""))

# Create the empty array, populated with NAs
temp.array <- array(dim=c(ndates,nlags,nzones,nhrs),dimnames=dimnames)
# dim(temp.array)
# [1]  6 24 11  7

# temp.array[1,1,1,'lag1']
# dimnames(temp.array)
# 
# temp.array[,'hr3:4','West','lag1']
# 
# temp.array[  , , ,'lag1']

# Create temp.array[day,lag,zone,hour]


for(date in 1:6){
     for (zone in 1:11){
          temp.array[date,date+1,zone,1:24]<- as.matrix(fcsts[(1:24)+(date-1)*24,zone+1])
     }
}


issueDate <- issueDate + 3600*24



temp.array[6,7 ,'N.Y.C.' , ]

dimnames(temp.array[ ,,,1])[[1]]

class(temp.array[[1]])

temp.array[,1,1,1]







######### DEPRECATED CODE ############

# Convert partially filled array into an xts object
# Alas, function as.xts() doesn't work on arrays, so need first to convert to a data frame 
temp.df <- as.data.frame(temp.array)
temp.xts <- as.xts(temp.df, order.by=dates)




head(temp.xts['2013-02-23'])
tail(temp.xts)$'hr6'

str(temp.xts)

dim(temp.df[1])
xtsible(temp.array)
temp.df <- as.data.frame(temp.array)
xtsible(temp.df)



validTime <- as.POSIXct(fcsts$Time.Stamp, format="%m/%d/%Y %H:%M", tz='America/New_York')
fcsts.xts <- as.xts(fcsts[2:12],order.by=validTime)
# str(fcsts.xts)

# head(fcsts.xts['/2013-02-20','North'])


tail(dateTime)
tail(dateTime) - issueDatetime

temp.df[1:3]
nrow(temp.df)
nrows(temp.array)
dimnames(temp.array)
df <- data.frame(11,7)
temp.xts <- as.xts(temp.df, order.by=dateTime[1:2])
temp.xts
nrow(temp.array)


# m = c(10,20,30) # means for each of the 3 point locations
Reps <- length(space)*length(dateTime)/length(m)
mydata = rnorm(length(space)*length(dateTime),mean=rep(m, Reps))
IDs = paste("ID",1:length(mydata))
#  mydata = data.frame(values = signif(mydata,3), ID=IDs)
# length(mydata$values)+length(mydata$ID)
length(fcsts[,2])

mydata <- as.vector(fcsts[2:12])
length(mydata)


nrow(fcsts[2:12])

# str(dateTime)
# dateTime
# zones <- names(fcsts)[2:12]
# xtsible(fcsts[2:12])
