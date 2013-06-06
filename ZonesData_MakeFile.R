#  File ZonesData_MakeFile.R
#  A utility program for making file zoneData.rda that contains all parameters
#  pertinent to the eleven NYISO zones.
#  Created by A.A. Small, 2013-03

nZones       <- 11  # Number of NYISO load control zones

# 'capwords()' function: See help(chartr)/Examples
capwords <- function(s, strict = FALSE) {
     cap <- function(s) paste(toupper(substring(s,1,1)),
{s <- substring(s,2); if(strict) tolower(s) else s},
          sep = "", collapse = " " )
     sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Fetch an arbitrary recenty load obs file, extract zone names and ID numbers 
obsURL <- 'http://mis.nyiso.com/public/csv/palIntegrated/20130301palIntegrated.csv'
zoneData <- read.table(obsURL,header=TRUE,sep=",",nrows=nZones,as.is=c(1))[1:nZones,3:4]
colnames(zoneData) <- c('names','IDs')
zoneData$names <- capwords(as.character(zoneData$names),strict=TRUE)
zoneData$names[9] <- 'N.Y.C.'

save(zoneData,file='zoneData.rda')
