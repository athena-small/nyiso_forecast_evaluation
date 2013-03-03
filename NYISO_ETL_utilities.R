# File NYISO_ETL_utilities.R
# A set of utility functionsthat perform basic tasks related extracting and 
# transforming NYISO load forecast and observation data. 
# Written by A.A. Small, 2013-03

# Function getObs()
# Retrieve's one day's observations from a remote (NYISO.com) or local URL,
# converts to a data frame
# getObs <- function(date){}

# Function obsdf2xts()
# Converts one day's NYISO load obs from a data frame object to an xts object
obsdf2xts <- function(obs.df
     ,format="%m/%d/%Y %H:%M:%S" # format of TimeStamp character string
     ,tzone='America/New_York'
     ,nColTimeStamp=1            # column containing dateTimes of observations
     ,nColID=4                   # column containing zone IDs
     ,nColObs=5                  # column containing observations
     ,nZones=11     # = length(unique(obs.df[,nColID])) = nrow(zoneData)
     ){
     require(xts)

     obsDateTime <- strptime(obs.df[,nColTimeStamp],format=format,tz=tzone)

     # Check for transitions off Daylight Savings Time; handle with care
     if(length(obsDateTime)==25*nZones){             # If day has 25 hours,
     #    obsDateTime$isdst[1*nZones+1:nZones] <- 1  # keep 2nd hr as DST,
          obsDateTime$isdst[2*nZones+1:nZones] <- 0  # switch 3rd hr to Standard Time            
     }

     # Create an xts object containing the observations
     attributes(obsDateTime)$tzone <-  tzone     
     obs.xts <- as.xts(cbind(obs.df[,nColID],obs.df[,nColObs]), order.by=obsDateTime)
     
     return(obs.xts)     
}

# Function forecastErrors()
# Given xts objects containing time series of forecasts and corresponding observations, 
# returns an xts object containing the forecast errors.
#
# Arguments:
#  fcsts.xts : An xts object containing forecasts.
#  obs.xts   : An xts object containing corresponding realizations
#  obsCol    : A positive integer, the number of the column in obs.xts containing observations
#  fcstCols  : A vector or positive integers, the numbers of the columns in fcsts.xts containing the forecasts to be evaluated.
#  IDCols    : A vector of positive integers, the number(s) of the column(s) in obs.xts containing ID tags that differentiate the several units of observation
#  
# Requirements:
#  * The obs and fcst xts objects must share the same dateTime index variable;
#  * Each row in obs.xts must contain exactly one observation value
#  * Each row of fcsts.xts must contain forecast values that share a common 'valid for' dateTime for observation with the corresponding row number and dateTime index value. 
# 
# Comments: 
#    Rolling forecast series or ensemble forecasts may be accommodated by choosing a 'fcstCols' vector  with more than one column value.  
#    
#    Column(s) obs.xts[,IDCols] will be copied as the left-most column(s) of errors.xts.
#    Good practice advises strongly that data inputs be formatted so that: identical(fcsts.xts[,IDCol],obs.xts[,IDcol])==TRUE. 
#
#    The function works on only one variable at a time. To compute errors for  
#    more than one variable, wrap this function inside a 'for' loop.
forecastErrors <- function(fcsts.xts,obs.xts,obsCol=2,fcstCols=c(2),IDCols=c(1)){
     x.xts <- fcsts.xts[,fcstCols]-obs.xts[,obsCol]
     errors.xts <- merge(obs.xts[,IDCols],x.xts)     
     return(errors.xts)
}

