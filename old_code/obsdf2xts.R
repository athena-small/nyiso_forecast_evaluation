# Function obsDf2Xts:
# Converts one day's NYISO load obs from data frame object to an xts object
# Written by A.A. Small, 2013-03

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


#      obsDateTime <- strptime(obs[,1],format="%m/%d/%Y %H:%M:%S",tz=timeZone)
# 
#      # Check for Daylight Savings Time transitions; handle with care     
#      nHrs <- 24
#      if(length(unique(obsDateTime$isdst))!=1){# If today is a DST transition day...
#           if(nrow(obs)==nZones*(nHrs+1)){       # If today has 25 hours,
#                nHrs <- 25                       
#                obsDateTime$isdst[2*nZones+1:nZones] <- 0  # Set 3rd hr to EST  
#           } else {                              # If not, 
#                nHrs <- 23                       # then today must have 23 hours.
#           }
#      }
# 
#      # Create an xts object containing today's observations
#      attributes(obsDateTime)$tzone <-  timeZone     
#      todaysObs.xts <- as.xts(cbind(obs[,4],obs[,5]), order.by=obsDateTime)
