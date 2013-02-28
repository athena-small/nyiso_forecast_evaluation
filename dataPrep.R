#  File dataPrep.R
#  Fetches NYISO load forecasts and obs from local directory, creates data array
#  Array dimensions are: 
#    dateTime validDate: date for which forecast is valid. Class: POSIX. Range: period of record
#    hour hr = 0, ..., 23: hour for which forecast is valid. Class: factor.
#    zone zone = 1, ..., 11  : NYISO load control region. Class: factor. 
#    forecast lag time k=0,..., 6; number of days between forecast issued-on and valid-for dates. Class: factor.
#  Entries are forecasts (for k > 0) and obs (k=0) of load in MWh/hr
#library(plyr)
library(xts)

# Initialize parameters ---------------------------------------------------

timeZone <- 'America/New_York'
Sys.setenv(TZ=timeZone)
nZones     <- 11   # Number of NYISO load control zones
nHrs       <- 24   # Hrs/day, = number of forecasts issued per (normal) forecasted day
# N.B. nHrs = 23 or 25 when clock transitions between Daylight Savings Time and EST
# nForecasts <-  6 # Number of days of forecasts in each NYISIO file
# nLags <- nForecasts+1  # Cumulative number of forecasts issued per target hour, counting the verifying observation as lag=0
nSecsPerDay <- 3600*24 # Useful in conversions between date classes

# Locations of NYISO forecast and load files
# fcstURLdir   <- "http://mis.nyiso.com/public/csv/isolf/"      
# loadURLdir   <- "http://mis.nyiso.com/public/csv/palIntegrated/"
loadLocalDir <-"./Data/"

# Fetch the names of the eleven NYISO zones from a forecast file (any will do, date is arbitrary)
# fcstURL <- paste(fcstURLdir,'20130224isolf.csv', sep="")    # URL of a sample load forecast csv file
# fcstFileHeader <- read.table(fcstURL, header = TRUE, sep = ",",nrows=1,as.is=c(1))
# zoneNames <- names(fcstFileHeader[1+1:nZones])
# rm(fcstFileHeader)


# MAKE LONG DATA FRAME ----------------------------------------------------

seriesStartDate <- ISOdate(2012,10,1,0,0,0,tz=timeZone)
seriesEndDate   <- ISOdate(2013,02,24,0,0,0,tz=timeZone)
nFiles <- as.integer(seriesEndDate - seriesStartDate)+1
# nValidDates <- nFiles+nForecasts-1
# allValidDates <- seriesStartDate + nSecsPerDay*0:(nValidDates-1)
# allValidDates

# For each date for which forecast file is issued: fetch data, load into big array
# fileNumber<-0
fileDate  <- seriesStartDate
for(fileNumber in 0:(nFiles-1)){
     # Fetch the obs from local location of downloaded & unzipped NYISO load obs csv file
     Yr  <- strftime(fileDate,format='%Y')
     Mo  <- strftime(fileDate,format='%m')
     Day <- strftime(fileDate,format='%d')
     URL <- paste(loadLocalDir,Yr,Mo,"01palIntegrated_csv/",Yr,Mo,Day,"palIntegrated.csv", sep="")
     loads <- read.table(URL,header=TRUE,sep=",",as.is=c(1))
     obsDateTime <- strptime(loads[,1],format="%m/%d/%Y %H:%M:%S",tz=timeZone)

     # Check for Daylight Savings Time transitions; handle with care     
     if(length(unique(obsDateTime$isdst))==1){ # "If today is not a DST transition day..." 
          fileDate <- fileDate + nSecsPerDay    # Advance ISOdate by 24 hours          
     } else { # "If today IS a DST transition day..." 
          if(nrow(loads)==nZones*(nHrs+1)){     # "If today has 25 hours..."
               obsDateTime$isdst[2*nZones+1:nZones] <- 0  # Reset 3rd hr to EST
               fileDate <- fileDate + nSecsPerDay+3600    # Advance ISOdate by 25 hours
          } else {
               fileDate <- fileDate + nSecsPerDay-3600    # Advance ISOdate by 23 hours               
          }          
     }
     
     # Create an xts object containing all load obs retrieved thus far
     temp.xts <- as.xts(cbind(loads[,4],loads[,5]), order.by=as.POSIXct(obsDateTime))
     if(fileNumber==0){
          allObs.xts <- temp.xts
     } else {
          allObs.xts <- rbind(allObs.xts,temp.xts)
     }     
}


# dfColnames <- c('ID','issuedDateTime','validDateTime','zone','loadMW')
colnames(allObs.xts) <- c('zone','obs')

#     allObs.xts<- rbind.xts(allObsInit.xts,temp.xts,tzone=timeZone)
     
#     str(temp.xts)     

head(allObs.xts,2)     
tail(allObs.xts,2)
nrow(allObs.xts['2012-11-04 01'])
nrow(allObs.xts)/(11*24)
allObs.xts['2012-11-04 01']
str(allObs.xts)
# data.frame(row.names='ID',check.rows=TRUE,check.names=TRUE)
# long.df <- data.frame()


# MAKE BIG ARRAY ----------------------------------------------------------

# Prepare a big array into which to load the forecasts and obs;
# lag0 corresponds to the verifying observation
dimnames = list(
     allValidDates
     #     strftime(allValidDates,format='%Y-%m-%d')
     ,paste('lag',nForecasts:0,sep="")
     ,zoneNames
     ,paste('hr',0:(nHrs-1),sep=""))
big.array <- array(dim=c(nValidDates,nLags,nZones,nHrs),dimnames=dimnames)
dimnames(big.array)[[2]][7] <- "obs"

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
     
     # Now fetch the obs:
#     URL <- paste(URLdirLoad,Yr,Mo,Day,"palIntegrated.csv", sep="")  # URL of the NYISO load obs csv file
     URL <- paste(localLoadDataDir,Yr,Mo,"01palIntegrated_csv/",Yr,Mo,Day,"palIntegrated.csv", sep="")  # local location of downloaded & unzipped NYISO load obs csv file
     loads <- read.table(URL, header = TRUE, sep = ",")
     for(hour in 0:23){
          big.array[fileNumber,'obs',1:nZones,hour] <- as.matrix(loads[hour*nZones+1:nZones,5])
     }
}


# ######### DEPRECATED CODE ############

#      N <- nrow(big.array) # = length(allValidDates)
#      rowsWithNAs <- c(1:5,(N-4):N)
#      rowsWithNAs 
#      big.array[-rowsWithNAs,,'N.Y.C.','hr16']
#      dim(big.array)
#      dim(big.array[-rowsWithNAs,,,])
#      big.array <- big.array[-rowsWithNAs,,,]
#      dimnames(big.array)
#      
#      big.array[,1,1,1]
#      attributes(big.array)
#      
#      big.array['Valid 2012-11-04',,'N.Y.C.','hr16']
#      big.array[,,'N.Y.C.','hr16']
#      small.array <- big.array[,,'N.Y.C.','hr16']
#      rowTest
#      rowTest <- c('Valid 2012-11-04','Valid 2012-11-05')
#      small.array[rowTest, ]
#      
#      small.array[, 'lag5']['Valid 2012-11-04']
#      dimnames(small.array)
#      
#      save(big.array,file='load_forecasts.dat')
#      
#      # length(big.array[is.na(big.array['Valid 2011-03-13',,'N.Y.C.',1])==TRUE])
#      # test <- c('Valid 2011-03-12','Valid 2011-03-13')
#      # big.array[is.na(big.array[test,-7,'N.Y.C.',1])==TRUE]
#      # attributes(big.array)$dim
#      
#      # 
     
# test.array[ ,'obs'][is.na(test.array[,'obs']==TRUE)]
# is.na(test.array[,'obs']==TRUE
#      
# # Convert partially filled array into an xts object
# # Alas, function as.xts() doesn't work on arrays, so need first to convert to a data frame 
# temp.df <- as.data.frame(temp.array)
# temp.xts <- as.xts(temp.df, order.by=dates)
# 
# 
# head(temp.xts['2013-02-23'])
# tail(temp.xts)$'hr6'
# 
# str(temp.xts)
# 
# dim(temp.df[1])
# temp.df <- as.data.frame(temp.array)
# xtsible(temp.df)
# 
# 
# 
# validTime <- as.POSIXct(fcsts$Time.Stamp, format="%m/%d/%Y %H:%M", tz='America/New_York')
# fcsts.xts <- as.xts(fcsts[2:12],order.by=validTime)
# # str(fcsts.xts)
# 
# # head(fcsts.xts['/2013-02-20','North'])
# 
# 
# tail(dateTime)
# tail(dateTime) - issueDatetime
# 
# temp.df[1:3]
# nrow(temp.df)
# nrows(temp.array)
# dimnames(temp.array)
# df <- data.frame(11,7)
# temp.xts <- as.xts(temp.df, order.by=dateTime[1:2])
# temp.xts
# nrow(temp.array)
# 
# # m = c(10,20,30) # means for each of the 3 point locations
# Reps <- length(space)*length(dateTime)/length(m)
# mydata = rnorm(length(space)*length(dateTime),mean=rep(m, Reps))
# IDs = paste("ID",1:length(mydata))
# #  mydata = data.frame(values = signif(mydata,3), ID=IDs)
# # length(mydata$values)+length(mydata$ID)
# length(fcsts[,2])
# 
# mydata <- as.vector(fcsts[2:12])
# length(mydata)
# 
# nrow(fcsts[2:12])
