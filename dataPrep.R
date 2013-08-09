#  File dataPrep.R
#  Fetches load forecasts and observations for the New York Independent System Operator (NYISO) from local CSV files, organizes these into two eXtensible Time Series (xts) objects
#  Each value represents load (in MWh/hr) for one hour, for one NYISO zone. 

library(xts)
library(reshape2)
#library(plyr)
source('NYISO_ETL_utilities.R')

# Initialize parameters ---------------------------------------------------

load('zoneData.rda')  # Index of names and ID#'s for NYISO's eleven zones

timeZone        <- 'America/New_York'
Sys.setenv(TZ=timeZone)
seriesStartDate <- ISOdate(2005,07,01,0,0,0,tz=timeZone)
seriesEndDate   <- ISOdate(2013,05,31,0,0,0,tz=timeZone)
SecsPerHr       <- 3600 

nZones          <- nrow(zoneData)  # Number of NYISO load control zones
nLags           <- 6               # Number of days of forecasts in each NYISO forecast file
maxHrsAhead     <- nLags*24+1   # Extra hour needed for annual DST transitions
nFiles          <- as.integer(seriesEndDate - seriesStartDate)+1

# Locations of NYISO forecast and load files
# fcstDirURL      <- "http://mis.nyiso.com/public/csv/isolf/"
#obsDirURL      <- "http://mis.nyiso.com/public/P-58Clist.htm"
fcstDirURL       <- "../CK/nyiso_forecast_evaluation/Data/"
obsDirURL       <- "../CK/nyiso_forecast_evaluation/Data/"

# Begin main program ------------------------------------------------------

#  Prepare object 'recentFcsts' in which to store forecasts in between their respective "issued on" dates and "valid for" dates. 
#  'recentFcsts' is structured as a list of data frames, one for each lag in 1:nLags
recentFcsts <- emptyFcstList(nLags=nLags,nrow=maxHrsAhead,ncol=nZones,colnames=zoneData$names,periodName='hour',lagsAreCalled='days')

today  <- seriesStartDate

for(fileCount in 0:(nFiles-1)){

     # Fetch today's obs, re-format as an xts object
     todaysObs.xts <- getOneDaysObs(date=today,dirURL=obsDirURL)
     nHrs <- length(unique(index(todaysObs.xts)))

     # Create an xts object containing only the dateTime index and the zone IDs;
     # to be used as scaffold used to create xts objects of forecasts valid for today
     scaffold.xts <- todaysObs.xts[,-2]
     
     # Fetch all the forecasts issued the previous day...
     fcstsIssuedYesterday.df<- getOneDaysFcsts(today,dirURL=fcstDirURL)

     # ... and load them into the first (lag=1) slot of the 'recentFcsts' list
     recentFcsts[[1]][1:nrow(fcstsIssuedYesterday.df),]  <- fcstsIssuedYesterday.df[,1+1:nZones]
     
     # Subset 'recentFcsts' to extract only the forecasts valid sometime today
     fcstsValidToday.lst <- lapply(seq(recentFcsts), function(lag){
          topRows <- data.frame(recentFcsts[[lag]][1:nHrs,])
          return(topRows)
     })

     # Re-organize forecasts valid for today into an xts object
     fcstsValidToday.xts <- fcstList2xts(fcstsValidToday.lst,scaffold.xts)
          
     # Create or expand xts objects containing all forecasts and obs retrieved so far
     if(today==seriesStartDate){ #  If this is the first day in the series...
          # Create xts objects to contain obs and fcsts 
          obs.xts <- todaysObs.xts
          forecasts.xts <- fcstsValidToday.xts  
     } else {                    # If not... 
          # Append today's data in rows below those from earlier dates
          obs.xts <- rbind(obs.xts,todaysObs.xts)
          forecasts.xts <- rbind(forecasts.xts,fcstsValidToday.xts)
     }
     
     # Prepare for next day's data: In recentFcsts list
     #  - shift forecasts over by 1 lag
     #  - shift all rows up nHrs rows
     #  - set all other cells to NA
     oldRows <- (1+nHrs):maxHrsAhead
     newRows <- 1:(maxHrsAhead-nHrs)
     for(lag in (nLags-1):1){
          recentFcsts[[lag+1]][newRows,1:nZones] <- recentFcsts[[lag]][oldRows,1:nZones]
          recentFcsts[[lag+1]][-newRows,1:nZones] <- NA                         
     }
     recentFcsts[[1]][1:maxHrsAhead,1:nZones] <- NA

     # Advance the clock by one day
     today <- today + nHrs*SecsPerHr
}

# Create xts objects of load forecasting errors and updates
errors.xts <- forecastErrors(forecasts.xts,obs.xts,obsCol=2,fcstCols=(1+1:nLags),IDCols=1)
updates.xts <- forecastUpdates(errors.xts,nLags=6,IDCols=1)

# Save the new xts objects in an R data file
list=c('forecasts.xts','obs.xts','errors.xts','updates.xts')
save(list=list,file='NYISO_load_forecast_evaluation_dataset.rda')

# forecastsTail.df <- as.data.frame(tail(forecasts.xts,3000))
# write.csv(forecastsTail.df,file='forecastsTail.csv')
# obsTail.df <- as.data.frame(tail(obs.xts,3000))
# write.csv(obsTail.df,file='obsTail.csv')
# errorsTail.df <- as.data.frame(tail(errors.xts,3000))
# write.csv(errorsTail.df,file='errorsTail.csv')

# ######### END OF CODE ############
