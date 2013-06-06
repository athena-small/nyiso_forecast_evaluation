# Function to read in a NOAA-generated temp obs file and convert it to a data frame.

rawObs2df <- function(rawObsFile) {
     what <- c(rep(list(NULL), 2),
               list(
                    Date=character(),
                    HrMin=character()),
               rep(list(NULL), 2),
               list(
                    temp=numeric()),
               rep(list(NULL), 3))
     input <- scan(rawObsFile, what=what, sep=",", skip=2)
     dateTime <- paste(input$Date, input$HrMin)
     obsDateTime <- strptime(dateTime, format="%Y%m%d %H%M")
     temperature <- round(input$temp*1.8+32, digits=1) # Convert temp from Celsius to Farenheit, round to nearest tenth of a degree
     colswewant <- list(obsDateTime, temperature)
     names(colswewant) <- c("obsDateTime","Temperature")
     as.data.frame(colswewant)
}

testIn <- c("20111201", "20111201")
testOut <- strptime(testIn, format="%Y%m%d")
testOut
class(testOut)
dim(testIn)
dim(testOut)
length(testIn)
length(testOut)