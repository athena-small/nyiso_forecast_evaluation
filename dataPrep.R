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
timeZone <- 'America/New_York'
seriesStartDate <- ISOdate(2011,3,7,0,0,0,tz=timeZone)
seriesEndDate   <- ISOdate(2011,3,14,0,0,0,tz=timeZone)
nZones     <- 11   # Number of NYISO load control zones
nHrs       <- 24   # Hrs/day, = number of forecasts issued per forecasted day
# Actually, nHrs = 25, to allow for the one day per year when the clock transitions from EDT to EST
nForecasts <-  6   # Number of days of forecasts in each NYISIO file
nLags      <- nForecasts+1  # Cumulative number of forecasts issued per target hour, 
                            #   counting the verifying observation as lag 0
nSecsPerDay <- 3600*24

nFiles <- as.integer(seriesEndDate - seriesStartDate)+1
nValidDates <- nFiles+nForecasts-1
nValidDates
allValidDates <- seriesStartDate + nSecsPerDay*0:(nValidDates-1)
str(allValidDates)
allValidDates

# Fetch the names of the eleven NYISO zones (date is arbitrary)
URLdir <- "http://mis.nyiso.com/public/csv/isolf/"  # Location of NYISO files
URL <- paste(URLdir,'20130224isolf.csv', sep="")    # URL of a NYISO load forecast csv file
fileHeader <- read.table(URL, header = TRUE, sep = ",",nrows=1)
zoneNames <- names(fileHeader[1+1:nZones])


# Prepare a big array into which to load the forecasts and obs;
# lag0 corresponds to the verifying observation
dimnames = list(
     paste('Valid',strftime(allValidDates,format='%Y-%m-%d'))
     ,paste('lag',nForecasts:0,sep="")
     ,zoneNames
     ,paste('hr',0:(nHrs-1),sep=""))

big.array <- array(dim=c(nValidDates,nLags,nZones,nHrs),dimnames=dimnames)
dim(big.array)
dimnames(big.array)[[2]][7] <- "obs"
dimnames(big.array)

# For each date for which forecast file is issued: fetch data, load into big array
for(fileNumber in 1:nFiles){
     # Fetch the day's file from NYISO, read values into temporary table
     fileDate  <- seriesStartDate + (fileNumber-1)*nSecsPerDay
     Yr  <- strftime(fileDate,format='%Y')
     Mo  <- strftime(fileDate,format='%m')
     Day <- strftime(fileDate,format='%d')
     URL <- paste(URLdir,Yr,Mo,Day,"isolf.csv", sep="")  # URL of the NYISO load forecast csv file
     fcsts <- read.table(URL, header = TRUE, sep = ",")
     for (zone in 1:nZones){
          for(lag in 1:nForecasts){
               # Grab one day's data, put in big.array in the right place
               dateNumber <- (fileNumber-1)+lag
               big.array[dateNumber,nLags-lag,zone,1:24] <- as.matrix(fcsts[(1:24)+(lag-1)*24,zone+1])
          }
     }
}

length(big.array[is.na(big.array['Valid 2011-03-13',,'N.Y.C.',1])==TRUE])

big.array[is.na(big.array['Valid 2011-03-13',-7,'N.Y.C.',])==TRUE]

big.array['Valid 2011-03-13',-7,'N.Y.C.',]



######### DEPRECATED CODE ############

# Convert partially filled array into an xts object
# Alas, function as.xts() doesn't work on arrays, so need first to convert to a data frame 
temp.df <- as.data.frame(temp.array)
temp.xts <- as.xts(temp.df, order.by=dates)


head(temp.xts['2013-02-23'])
tail(temp.xts)$'hr6'

str(temp.xts)

dim(temp.df[1])
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
