#This was setting a temporary directory for the files to be saved to 
#tmpdir <- tempdir()

#Stores the files in a local directory for easier navigation
localDir <- '~/Cassandra/Dropbox/Forecast Evaluation/forecast_evaluation'
#Was designed to set the Data folder in Dropbox as the data directory
#dataDir <- paste(localDir,'Data/', sep="")
#Sets the working directory to the local drive, this step was found to 
#not be needed
#setwd(dir=localDir)

#Opens the url where the compressed gzip file can be found
#url <- 'http://archive.ics.uci.edu/ml/databases/tic/tic.tar.gz'

#The location and name of the data we are interested in
#The below code is not working due to the url not being able to be found
#I believe that if this url is fixed the rest of the code should run 
#without any problems
#may be paste the filename at the end of url
file <- '20130201palIntegrated_csv.zip'
#url <- 'http://mis.nyiso.com/public/paste(fileName, sep="")'
url <- 'http://mis.nyiso.com/public/'

#Stores the file name found at the end of the url
#file <- basename(url)

#Downloads the file to the local drive so that it can be unzipped
download.file(url, file)
#Was going to be used to write the file to a binary file but errors 
#were encountered
#writeBin(file, test.gz)

#This function unzips the compressed gzip files and saves all the 
#files to the local directory
untar(file, compressed = 'gzip')
#unzip(file)

#Shows the names of the files in the compressed gzip file as a list of 
#character strings
#list.files(tmpdir)

