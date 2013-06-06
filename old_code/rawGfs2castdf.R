# Function rawGfs2castdf:
# A function that converts a raw GFS file containing temperature forecasts into
# a cast data frame organized by "valid for" datetime and forecast lead time.

rawGfs2castdf <- function(rawGfsFile){
     #  Perform transformations on GFS data:
     #   - Convert text dates + times into date-times in the POSIXt class
     #   - Replace initial_datetime with new column of forecast lead/lag times
     #   - Add new column labels
     #   - Reshape data frame into table by validTime, leadTime
     require(reshape2)
     colClasses <- c(rep("NULL",2), "factor", "character", "character")
     gfs <- raw.gfs <- read.csv(rawGfsFile, colClasses=colClasses)
     gfs[[2]] <- strptime(raw.gfs[[2]], format="%Y-%m-%d %H:%M", tz="GMT")
     gfs[[3]] <- strptime(raw.gfs[[3]], format="%Y-%m-%d %H:%M", tz="GMT")
     gfs[[2]] <- as.factor(gfs[[3]] - gfs[[2]])  
     gfs[[3]] <- as.factor(raw.gfs[[3]])          # Convert validFor datetimes into factors, for sorting using melt+cast
     names(gfs) <- c("Temperature", "hoursAhead", "validFor")
     gfs.molten <- melt(gfs, id=c("validFor","hoursAhead"))
     dcast(gfs.molten, ... ~ hoursAhead)
#     gfs.molten$validFor <- as.POSIX(gfs.molten$validFor)
#     gfs.molten[ , 3:23] <- as.numeric(gfs.molten[ , 3:23])
}

