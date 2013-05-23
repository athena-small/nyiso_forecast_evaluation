#  File ETL-Obs.R
#  Fetches NYISO load obs from local directory, organizes them into an xts object,
#    then saves the object as an R data file for future use.
#  By A.A. Small, 2013-03-01

library(xts)


# Initialize parameters ---------------------------------------------------
timeZone <- 'America/New_York'
Sys.setenv(TZ=timeZone)
nZones     <- 11   # Number of NYISO load control zones
nHrs       <- 24   # Hrs/day, = number of forecasts issued per (normal) forecasted day
nSecsPerDay <- 3600*24 # Useful in conversions between date classes

# Locations of NYISO load files
loadRemotedir   <- "http://mis.nyiso.com/public/csv/palIntegrated/"
loadLocalDir <-"./Data/"

seriesStartDate <- ISOdate(2012,10,1,0,0,0,tz=timeZone)
seriesEndDate   <- ISOdate(2013,02,24,0,0,0,tz=timeZone)

# Make a long xts object containting all load realizations ---------------------

nFiles <- as.integer(seriesEndDate - seriesStartDate)+1

# For each date: fetch data from daily file, transform, load into xts object
fileDate  <- seriesStartDate
for(fileNumber in 0:(nFiles-1)){
     Yr  <- strftime(fileDate,format='%Y')
     Mo  <- strftime(fileDate,format='%m')
     Day <- strftime(fileDate,format='%d')
     # Fetch obs from local location of downloaded & unzipped NYISO load obs csv file
     URL <- paste(loadLocalDir,Yr,Mo,"01palIntegrated_csv/",Yr,Mo,Day,"palIntegrated.csv", sep="")
     loads <- read.table(URL,header=TRUE,sep=",",as.is=c(1))
     #  Reclass observation time as POSIXlt class
     obsDateTime <- strptime(loads[,1],format="%m/%d/%Y %H:%M:%S",tz=timeZone)
     
     # Check for Daylight Savings Time transitions; handle with care     
     if(length(unique(obsDateTime$isdst))==1){ # "If today is not a DST transition day..." 
          fileDate <- fileDate + nSecsPerDay    # Advance ISOdate by 24 hours          
     } else { # "If today IS a DST transition day..." 
          if(nrow(loads)==nZones*(nHrs+1)){     # "If today has 25 hours..."
               obsDateTime$isdst[2*nZones+1:nZones] <- 0  # Reset 3rd hr to EST
               fileDate <- fileDate + nSecsPerDay+3600    # Advance ISOdate by 25 hours
          } else { # "If today has 23 hours... "
               fileDate <- fileDate + nSecsPerDay-3600    # Advance ISOdate by 23 hours               
          }
     }
     
     # Create an xts object containing all load obs retrieved thus far
     attributes(obsDateTime)$tzone <-  timeZone  # Needed to fix a glitch    
     todaysObs.xts <- as.xts(cbind(loads[,4],loads[,5]), order.by=obsDateTime)
     if(fileNumber==0){ # "If this is the first day in the series..."
          obs.xts <- todaysObs.xts  
     } else {           # "If not... "
          obs.xts <- rbind(obs.xts,todaysObs.xts) # Add today's obs to obs object
     }     
}

colnames(obs.xts) <- c('zone','obs')

# Make xts object containting all load forecasts ---------------------
# ...

# Save object for use in later analytic tasks
save(obs.xts,file='obs.rda')



