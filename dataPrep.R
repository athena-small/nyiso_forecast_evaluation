#  File dataPrep.R
#  Fetches NYISO load forecasts and obs from local directory, creates data array
#  Array dimensions are: 
#    dateTime validDate: date for which forecast is valid. Class: POSIX. Range: period of record
#    zone zone = 1, ..., 11  : NYISO load control region. Class: factor. 
#    forecast lag time k=0,..., 6; number of days between forecast issued-on and valid-for dates. Class: factor.
#  Entries are forecasts (for k > 0) and obs (k=0) of load in MWh/hr
#library(plyr)
library(xts)
load('zoneData.rda')

# Initialize parameters ---------------------------------------------------

timeZone <- 'America/New_York'
Sys.setenv(TZ=timeZone)
nZones <- nrow(zoneData)  # Number of NYISO load control zones
SecsPerHr <- 3600 

nLags <-  6  # Number of days of forecasts in each NYISIO file

# Locations of NYISO forecast and load files
fcstURLdir <- "http://mis.nyiso.com/public/csv/isolf/"
obsURLdir  <- "./Data/"
# obsURLdir <- "http://mis.nyiso.com/public/csv/palIntegrated/"

seriesStartDate <- ISOdate(2012,11,4,0,0,0,tz=timeZone)
seriesEndDate   <- ISOdate(2012,11,4,0,0,0,tz=timeZone)
nFiles <- as.integer(seriesEndDate - seriesStartDate)+1

# Make object 'loads.xts' containting all forecasts and obs ----------------------------
#   Data are to be organized in an xts object in which each row corresponds
#   to one hour's obs and six lagged forecasts of that obs, for one zone.

fcst.array <- array(dim=c(nLags,nLags*24+1,1+nZones))
dimnames(fcst.array)[[3]] <- c("Valid for",zoneData$names)

fileDate  <- seriesStartDate
for(fileCount in 0:(nFiles-1)){
     Yr  <- strftime(fileDate,format='%Y')
     Mo  <- strftime(fileDate,format='%m')
     Day <- strftime(fileDate,format='%d')
     
     # Fetch today's obs from a local directory
     obsURL <- paste(obsURLdir,Yr,Mo,"01palIntegrated_csv/",Yr,Mo,Day,"palIntegrated.csv", sep="")
     obs <- read.table(obsURL,header=TRUE,sep=",",as.is=c(1))
     
     obsDateTime <- strptime(obs[,1],format="%m/%d/%Y %H:%M:%S",tz=timeZone)

     # Check for Daylight Savings Time transitions; handle with care     
     nHrs <- 24
     if(length(unique(obsDateTime$isdst))!=1){# If today is a DST transition day...
          if(nrow(obs)==nZones*(nHrs+1)){       # If today has 25 hours,
               nHrs <- 25                       
               obsDateTime$isdst[2*nZones+1:nZones] <- 0  # Set 3rd hr to EST  
          } else {                              # If not, 
               nHrs <- 23                       # then today must have 23 hours.
          }
     }

     # Create an xts object containing today's observations
     attributes(obsDateTime)$tzone <-  timeZone     
     todaysObs.xts <- as.xts(cbind(obs[,4],obs[,5]), order.by=obsDateTime)
     
     # Fetch the forecasts issued yesterday, located in a file named for today's date
     fcstURL <- paste(fcstURLdir,Yr,Mo,Day,"isolf.csv", sep="")
     fcstsIssuedYesterday.df <- read.table(fcstURL, header = TRUE, sep = ",")

     # Load the forecasts issued yesterday into the lag=1 slot of the forecast array
     x <- fcstsIssuedYesterday.df[,1:(nZones+1)]
     fcst.array[1,1:nrow(fcstsIssuedYesterday.df),1:(nZones+1)]  <- as.matrix(x)
     
     # Extract all the forecasts valid for today
     forecastsValidForToday.3D <- fcst.array[,1:nHrs,2:(nZones+1)]
     
     # Add lagged forecasts as columns in a merged xts object
     merged.xts <- todaysObs.xts
     for(lag in 1){
          x <- as.integer(as.vector(forecastsValidForToday.3D[lag,,]))
          hr <- 1     
          y <- x[hr + 25*(0:(nZones-1))]
          for(hr in 2:nHrs){
               y <- c(y,x[hr + 25*(0:(nZones-1))])
          }
          y.xts <- as.xts(y, order.by=obsDateTime)

          # Merge today's obs and forecasts into a combined xts object
          merged.xts <- merge(todaysObs.xts,y.xts,tzone=timeZone)
     }
     
     
     # Add today's forecasts and obs to those retrieved earlier for previous dates
     if(fileCount==0){ #  If this is the first day in the series...
          # Create an xts object to contain all obs and fcsts     
          loads.xts <- merged.xts  
     } else {           # If not... 
          # Append today's data to those from earlier dates
          loads.xts <- rbind(loads.xts,merged.xts)
     }
     
     # Prepare for next day's data:
     # Shift forecasts over by 1 lag
     # Clear the rest of the cells in fcst.array
     # [...]
     
     # Advance the clock by one day
     fileDate <- fileDate + nHrs*SecsPerHr
}

nLags <- 1          # Temporary: When finished coding, will reset to 6

# Order columns by decreasing lags, then obs
loads.xts <- loads.xts[,c(1,(nLags+2):2)]  
colnames(loads.xts) <- c('zone',paste('lag',nLags:1,sep=""),'obs')

head(loads.xts)
# Create an xts object of load forecasting errors
errors.xts <- zones.xts <- loads.xts[,1]
for(lag in nLags:1){
   x.xts <- loads.xts[,2+nLags-lag] - loads.xts[,2+nLags]
   errors.xts <- merge(errors.xts,x.xts)
}

list=c('loads.xts','errors.xts')
save(list=list,file='NYISO_load_forecast_evaluation.rda')


# ######### DEPRECATED CODE ############

head(loads.xts[,nLags+2-lag])
head(loads.xts[,nLags+2])
rm(errors.xts)

# Tests to make sure output is OK
head(loads.xts,2)
tail(loads.xts,2)
nrow(loads.xts['2012-11-04 01'])
nrow(loads.xts)/(11*24)
loads.xts['2012-11-04 01']
loads.xts['2012-11-04 5']
str(loads.xts)

attributes(index(loads.xts))$names[3]
head(coredata(loads.xts))
loads.xts[index(loads.xts)$hour==17]

errors.xts <- loads.xts[loads.xts$zone==61761][,2] - loads.xts[loads.xts$zone==61761][,3] 
errors.xts <- loads.xts[,2] - loads.xts[,3] 
zones.xts <- loads.xts[,1]
loads.xts[,1]

errors.xts <- merge(zones.xts,errors.xts)
head(errors.xts)
str(errors.xts)
errors.xts[errors.xts$zone==61761]


attributes(loads.xts)


# # m = c(10,20,30) # means for each of the 3 point locations
# Reps <- length(space)*length(dateTime)/length(m)
# mydata = rnorm(length(space)*length(dateTime),mean=rep(m, Reps))
# IDs = paste("ID",1:length(mydata))
# #  mydata = data.frame(values = signif(mydata,3), ID=IDs)
# # length(mydata$values)+length(mydata$ID)
