# Description of meta-data ------------------------------------------------

# The NYISO load forecast evaluation dataset includes four .xts (eXtensible Time Series) objects: obs.xts, forecasts.xts, errors.xts, and updates.xts
# All datasets cover the same period of record beginning 2005-07-01 00:00:00
# All values except zone IDs are in MWh/Hr

### obs.xts : Observed loads
# Example record:
#                     zone    obs
# 2013-05-31 23:00:00 61762 2799.9
#  Here:
#  obs.xts$zone : zone ID code
#  obs.xts$obs  : observed load, in MW
#  index(obs.xts) : date and time of the first second in the corresponding hour 
#                   (uses R's POSIXt class) 

# The eleven NYISO zones are coded as follows:
# > zoneData
# names   IDs
# 1  Capitl 61757
# 2  Centrl 61754
# 3  Dunwod 61760
# 4  Genese 61753
# 5  Hud Vl 61758
# 6  Longil 61762
# 7  Mhk Vl 61756
# 8  Millwd 61759
# 9  N.Y.C. 61761
# 10  North 61755
# 11   West 61752

### forecasts.xts: Load forecasts issued by NYISO
# Example record:
#                      zone lag6 lag5 lag4 lag3 lag2 lag1
# 2013-05-31 23:00:00 61762 2617 2637 2945 2919 2834 2858
#  index(forecasts.xts) and forecasts.xts$zone : forecast valid for the corresponding observation
#   forecasts.xts$lagN : forecasts valid for a given date-time, issued N periods before
#     N = 1, ... , 6
#     Example: $lag1 indicates the last forecast issued before load is observed,
#              issued (typically) on the same date as the corresponding observation
#              $lag2 indicates the penultimate forecast issued for that observation, etc.

### errors.xts : forecast errors
# The errors.xts object has exactly the same structure as the forecast.xts object
# Example record:
#                      zone    lag6    lag5   lag4   lag3   lag2   lag1
# 2013-05-31 23:00:00 61762  -182.9  -162.9  145.1  119.1   34.1   58.1
#  errors.xts$lagN : difference between corresponding forecast and observed values

### updates.xts : forecast updates, a.k.a. innovations
# Example record:
#                      zone lag6.5 lag5.4 lag4.3 lag3.2 lag2.1 lag1.0
# 2013-05-31 23:00:00 61762     20    308    -26    -85     24   58.1
#  updates.xts$lagN.N-1 : difference between corresponding values for 
#                         forecasts.xts$lagN and forecasts.xts$lagN-1
#  updates.xts$lag1.0   : same as errors.xts$lag1                     
