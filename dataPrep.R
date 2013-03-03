#  File dataPrep.R
#  Fetches NYISO load forecasts and obs from local directory, creates data array
#  Array dimensions are: 
#    dateTime validDate: date for which forecast is valid. Class: POSIX. Range: period of record
#    zone zone = 1, ..., 11  : NYISO load control region. Class: factor. 
#    forecast lag time k=1,..., 6; number of days between forecast issued-on and valid-for dates. Class: factor.
#  Entries are forecasts (for k > 0) and obs (k=0) of load in MWh/hr
#library(plyr)
library(xts)
load('zoneData.rda')
source('NYISO_ETL_utilities.R')

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

seriesStartDate <- ISOdate(2012,10,17,0,0,0,tz=timeZone)
seriesEndDate   <- ISOdate(2012,10,17,0,0,0,tz=timeZone)
nFiles <- as.integer(seriesEndDate - seriesStartDate)+1

# Make object 'fcsts.xts' containting all forecasts and obs ----------------------------
#   Data are to be organized in an xts object in which each row corresponds
#   to one hour's obs and six lagged forecasts of that obs, for one zone.

fcst.array <- array(dim=c(nLags,nLags*24+1,1+nZones))
dimnames(fcst.array)[[3]] <- c("Valid for",zoneData$names)

today  <- seriesStartDate
for(fileCount in 0:(nFiles-1)){
     Yr  <- strftime(today,format='%Y')
     Mo  <- strftime(today,format='%m')
     Day <- strftime(today,format='%d')
     
     # Fetch today's obs from a local directory
     obsURL <- paste(obsURLdir,Yr,Mo,"01palIntegrated_csv/",Yr,Mo,Day,"palIntegrated.csv", sep="")
     obs.df <- read.table(obsURL,header=TRUE,sep=",",as.is=c(1))
 
     todaysObs.xts <- obsdf2xts(obs.df)
     obsDateTime <- index(todaysObs.xts)
     nHrs <- length(unique(obsDateTime))
     
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
          fcsts.xts <- merged.xts  
     } else {           # If not... 
          # Append today's data to those from earlier dates
          fcsts.xts <- rbind(fcsts.xts,merged.xts)
     }
     
     # Prepare for next day's data:
     # Shift forecasts over by 1 lag
     # Clear the rest of the cells in fcst.array
     # [...]
     #  Temporary:
     fcst.array <- array(dim=c(nLags,nLags*24+1,1+nZones))
     dimnames(fcst.array)[[3]] <- c("Valid for",zoneData$names)
     
     # Advance the clock by one day
     today <- today + nHrs*SecsPerHr
}

nLags <- 1          # Temporary: When finished coding, will reset to 6

# Order columns by decreasing lags, then obs
fcsts.xts <- fcsts.xts[,c(1,(nLags+2):2)]  
colnames(fcsts.xts) <- c('zone',paste('lag',nLags:1,sep=""),'obs')
 
# Create an xts object of load forecasting errors
errors.xts <- forecastErrors(f.xts,o.xts)

head(errors.xts)

# list=c('fcsts.xts','obs.xts','errors.xts','updates.xts')
list=c('fcsts.xts','obs.xts','errors.xts')
save(list=list,file='NYISO_load_forecast_evaluation_dataset.rda')


# ######### DEPRECATED CODE ############

# # m = c(10,20,30) # means for each of the 3 point locations
# Reps <- length(space)*length(dateTime)/length(m)
# mydata = rnorm(length(space)*length(dateTime),mean=rep(m, Reps))
# IDs = paste("ID",1:length(mydata))
# #  mydata = data.frame(values = signif(mydata,3), ID=IDs)
# # length(mydata$values)+length(mydata$ID)
