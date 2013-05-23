#  FILE forecast_evaluation.R
#  An R program to perform statistical analysis of the accuracy of the GFS
#  forecasting system.
#    Forecast model: Mean of the ensemble MOS of NOAA's GFS
#    Variable: Hourly temperatures at NY's LGA airport, 2000-05-30 -- 2012-03-03
#    Observations: Records from NOAA/NCDC
#
library(reshape2)   # Utilities for reshaping data frames and arrays
library(xts)

# Read in functions to perform ETL on raw GFS file, Obs file
fl <- c("rawGfs2castdf.R")
source(fl)
fl <- c("rawObs2df.R")
source(fl)

#  Identify files containing the raw data
DataDir <- "/Users/arthursmalliii/Documents/Google\ Drive/Research_Projects/AIR/Data"
rawGfsFile <- paste(DataDir,"Wx/GFS/temperature_forecasts_raw.csv", sep="/")
rawObsFile <- paste(DataDir,"Wx/Obs/TempObsNycLga/2000-05-30--2012-03-03dat.txt", sep = "/")

#  Load GFS forecast data, transform it, create new data frame
#  gfs.castdf <- rawGfs2castdf(rawGfsFile)

# Utilities for testing performance of the rawGfs2castdf function:
# profFile <- tempfile()
# Rprof(profFile) # start gathering profile information
system.time(gfs.castdf <- rawGfs2castdf(rawGfsFile))
str(gfs.castdf)
# identical(gfs.castdf, gfs.cast)
# Rprof() # stop
# head(summaryRprof(profFile)$by.self)

system.time(obs.df <- rawObs2df(rawObsFile))

test <- gfs.castdf[1:100, ]
test$validFor <- as.POSIXct(test$validFor, tz="GMT")
test[,3:23] <- as.integer(test[,3:23], start=test[1,1], frequency=6)

test.ts <- as.ts(test[,3:23], start=2000-06-10, frequency=6)
str(test.ts)

test.window <- window(test.ts, start=2000-06-07)
summary(test.ts)
test.ts