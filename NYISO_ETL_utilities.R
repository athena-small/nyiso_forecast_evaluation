# File NYISO_ETL_utilities.R
# A set of utility functions that perform basic tasks related to extracting and 
# transforming NYISO load forecast and observation data. 
# Created by A.A. Small, 2013-03


#  Function emptyFctsList():
#  Prepares an empty object in which to store forecasts in between their respective "issued on" dates and "valid for" dates. 
#  The object is structured as a list of data frames, one for each lag in 1:nLags, with all values = NA.
emptyFcstList <- function(nLags=1,nrow=25,ncol=1,colnames=NULL,periodName='hour',lagsAreCalled='lags'){

     matrixOfNAs   <- matrix(nrow=nrow,ncol=ncol)
     row.names     <- c(paste(periodName,1:nrow))
     df            <- as.data.frame(matrixOfNAs,row.names=row.names)
     altColnames   <- c(paste('var',1:ncol,sep=""))
     if(length(colnames)!=ncol) colnames <- altColnames
     names(df)     <- colnames
     emptyList     <- replicate(nLags,df,simplify=FALSE)
     names(emptyList)   <- paste(1:nLags,lagsAreCalled,'_ago',sep="")

     return(emptyList)
}

# Function capwords()
# Makes zone names look nicer
# capwords <- function(s, strict = FALSE) {
#      cap <- function(s) paste(toupper(substring(s,1,1)),
# {s <- substring(s,2); if(strict) tolower(s) else s},
#           sep = "", collapse = " " )
#      sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
# }


# Function obsdf2xts()
# Converts one day's NYISO load obs from a data frame object to an xts object
obsdf2xts <- function(obs.df
     ,format="%m/%d/%Y %H:%M:%S" # format of TimeStamp character string
     ,tzone='America/New_York'
     ,TimeStampCol=1            # column containing dateTimes of observations
     ,IDCols=c(3)               # column(s) containing zone IDs
     ,obsCols=c(5)              # column(s) containing observations
     ,nZones=11  # = length(unique(obs.df[,IDCols])) = nrow(zoneData)
     ){     
     require(xts)
     
     obsDateTime <- strptime(obs.df[,TimeStampCol],format=format,tz=tzone)
     
     # Check for transitions off Daylight Savings Time; handle with care
     if(length(obsDateTime)==25*nZones){             # If day has 25 hours,
          #    obsDateTime$isdst[1*nZones+1:nZones] <- 1  # keep 2nd hr as DST,
          obsDateTime$isdst[2*nZones+1:nZones] <- 0  # switch 3rd hr to Standard Time            
     }
     
     # Create an xts object containing the observations
     attributes(obsDateTime)$tzone <-  tzone     
     obs.xts <- as.xts(data.frame(obs.df[,IDCols],obs.df[,obsCols]), order.by=obsDateTime)
     
     return(obs.xts)     
}

# Fetch today's obs from designated local directory, convert to an xts object
getOneDaysObs <- function(date,dirURL='./Data/'){
     require(xts)

     # To be added later: add tag 'downloadMissing'; if it ==TRUE:
     #   - Check for file first in local directory
     #   - If local file is missing, download it from NYISO
     
     Yr  <- strftime(date,format='%Y')
     Mo  <- strftime(date,format='%m')
     Day <- strftime(date,format='%d')
     file <- paste(dirURL,Yr,Mo,"01palIntegrated_csv/",Yr,Mo,Day,"palIntegrated.csv", sep="")
     asIsCols = c(1,3)   # Don't convert Time.Stamps or zone names to factors
     obs.df <- read.table(file,header=TRUE,sep=",",as.is=asIsCols)
     
     daysObs.xts <- obsdf2xts(obs.df,obsCols=5,IDCols=4,TimeStampCol=1)
     colnames(daysObs.xts) <- c('zone','obs')
     return(daysObs.xts)
}

# Fetch all the forecasts issued on a single date
getOneDaysFcsts <- function(date,dirURL='./Data/'){

     # To be added later: add tag 'downloadMissing'; if it ==TRUE:
     #   - Check for file first in local directory
     #   - If local file is missing, download it from NYISO

     # Fetch the forecasts issued yesterday, located in a file named for today's date
     dateString <-strftime(date,format='%Y%m%d')
     file.csv <- paste(dirURL,dateString,"isolf.csv", sep="")
     fcstsIssuedPreviousDay.df <- read.table(file.csv, header = TRUE, sep = ",")
     return(fcstsIssuedPreviousDay.df)
}

fcstList2xts <- function(fcsts.list,scaffold.xts){
     require(xts)
     
     index <- index(scaffold.xts)
     tzone <- attributes(scaffold.xts)$tzone
     Lags <-  length(fcsts.list)
     
     # Organize forecasts into long data frames with one value per line
     fcsts.long <- lapply(seq(fcsts.list), function(lag){
          long.df  <- data.frame(melt(t(fcsts.list[[lag]])))
          return(long.df)
     } )
     
     # Collect all the forecasts valid for today as columns in an xts object
     fcsts.xts <- scaffold.xts     
     for(lag in Lags:1){
          temp.df <- data.frame(fcsts.long[[lag]]$value)
          names(temp.df) <- paste('lag',lag,sep="")
          temp.xts <- as.xts(temp.df, order.by=index, tzone=tzone)
          fcsts.xts <- merge(fcsts.xts,temp.xts,tzone=tzone)
     }
     
     return(fcsts.xts)
}

# Function forecastErrors()
# Given xts objects containing time series of forecasts and corresponding observations, 
# returns an xts object containing the forecast errors.
#
# Arguments:
#  fcsts.xts : An xts object containing forecasts.
#  obs.xts   : An xts object containing corresponding realizations
#  obsCol    : A positive integer, the number of the column in obs.xts containing observations
#  fcstCols  : A vector of positive integers, the numbers of the columns in fcsts.xts containing the forecasts to be evaluated.
#  IDCols    : A vector of positive integers, the number(s) of the column(s) in obs.xts containing ID tags that differentiate the several units of observation
#  
# Requirements:
#  * The obs and fcst xts objects must share the same dateTime index variable;
#  * Each row in obs.xts must contain exactly one observation value
#  * Each row of fcsts.xts must contain forecast values that share a common 'valid for' dateTime for the observation with the corresponding row number and dateTime index value. 
# 
# Comments: 
#    Rolling forecast series or ensemble forecasts may be accommodated by choosing a 'fcstCols' vector with more than one column value.  
#    
#    Column(s) obs.xts[,IDCols] will be copied as the left-most column(s) of errors.xts.
#    Good practice advises strongly that data inputs be formatted so that: identical(fcsts.xts[,IDCols],obs.xts[,IDcols])==TRUE. 
#
#    The function works on only one variable at a time. To compute errors for  
#    more than one variable, wrap this function inside a 'for' loop.
forecastErrors <- function(fcsts.xts,obs.xts,obsCol=2,fcstCols=c(2),IDCols=c(1)){
     require(xts)
     
     errors.xts <- obs.xts[,IDCols]
     for(column in fcstCols){
          e.xts <- fcsts.xts[,column]-obs.xts[,obsCol]
          errors.xts <- merge(errors.xts,e.xts)          
     }
     return(errors.xts)
}

forecastUpdates <- function(errors.xts,nLags=6,IDCols=c(1)){
     require(xts)
     scaffold.xts <- errors.xts[,IDCols]
     u.xts <- errors.xts[,2:nLags+1]-errors.xts[,2:nLags]
     updates.xts <- merge(scaffold.xts,u.xts)
     updates.xts <- merge(updates.xts,errors.xts[,nLags+1])
     colNames <- paste('lag',nLags:1,'.',(nLags:1)-1,sep='')
     colnames(updates.xts) <- c('zone',colNames)
     return(updates.xts)
}


########## END OF CODE ############
