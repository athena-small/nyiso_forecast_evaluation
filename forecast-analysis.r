# 	Set the working directory
setwd("/Users/arthursmalliii/Dropbox/Research_Projects/Electricity-Load-Forecasting/Data")
# 	Read forecast data in from external .dat file, and organize them in a data frame called "forecasts"
forecasts <- read.table("NYISO_integrated_load_forecasts_2007-december.dat", header = TRUE, sep = ",")
# 	Describe the structure of the "forecasts" data frame
str(forecasts)
# 	Trim down data frame to expunge forecasts, retain only realized load values and meta-data
loads <- forecasts[1:8]
names(loads)
str(loads)
#	Select out only records for New York City
Sel = loads$region.name == NYC
loadsNYC = loads[Sel,]