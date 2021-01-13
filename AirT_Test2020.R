####### Weekly SST Temperature ########
# ---- Install Packages ----
library(RNetCDF)
library(abind)
library(sp)
library(raster)
library(dplyr)

setwd("/Users/brandybiggar/Desktop/")
CRAB<-read.csv("TEST.csv")

# ---- EXTRACT TEMPERATURES FOR EACH SITE ----
# http://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_3.26/data/tmn
setwd("/Users/brandybiggar/Documents/Masters/Crabs_R/Temp.data/NEWAirT/")
weekly_sst<-open.nc("airtmp.dat.nc")
print.nc(weekly_sst)
sst<-read.nc(weekly_sst,unpack=T)
head(sst)
# I continue to use 'sst' for simplicity but it is AIR

# Create a vector of each variable to match up below for site and time
long<-data.frame(sst[1]) 
lat<-data.frame(sst[2])
time<-data.frame(sst[3])

##### MAKE GRID OF ALL POSSIBLE LAT AND LONG COMBINATIONS. 
all.lat.long1<-expand.grid(long[,1],lat[,1])
all.lat.long<-all.lat.long1    

## Make sst array  
sst.array<-abind(sst[4]) #Combine multidimensional arrays into a single array
##################################################################


#### Temporal aspects ####
# Convert Day, Month, Year into:
#   -Julian day with start time 1900-01-01 (coded as 'Time')
# https://www.metoffice.gov.uk/hadobs/hadisst/data/download.html

CRAB$DayInYear <- strptime(CRAB$date, "%d/%m/%Y")$yday + 1
# calculate day in year number 

# Day since 1-1-1900
## this is where it's important to know when your temp dataset (the CDF file) started
## so you can match your lat/long file appropriately 
CRAB$DaySince0 <- ceiling(
  julian(strptime(CRAB$date, format = "%d/%m/%Y"), origin = as.Date("1900-01-01")))
# this is to calculate # of days since 1900-01-01 (the start of my temp data) for each
# row of my lat/long data
CRAB$Time1 <- as.numeric(CRAB$DaySince0)
# this is simply to make sure it's in numeric format

############################################################################

#### CREATE BLANK MATRIX ####
##subset<-CRAB[201:207,]
temp.matrix<-matrix(0,dim(CRAB),3)
colnames(temp.matrix)<-c("MeanAir","MinAir","MaxAir")

# ---- MATCHING SITES W/ TEMP DATA BY TIME POINTS ----
####you will want to do so for your site and for winter or summer before
for (i in 1:dim(CRAB)[1]){
  diff.times<-abs(CRAB$Time[i]-time[,1]) #calculates difference in time difference between my days and sst days
  time.point<-which(diff.times==min(diff.times)[1]) #chooses time point where time diff is minimal (closest to survey date)
  time.point1yrprior<-time.point-11 #choose time point 1 year before survey. data sampled monthly 
  #### EUCLIDEAN DISTANCE 
  values<-sqrt(((CRAB[i,]$SiteLat)-(all.lat.long[,1]))^2+((CRAB[i,]$SiteLong)-(all.lat.long[,2]))^2) ## make sure 'latitude' and 'longitude' are named correctly some datasets they are lat or sitelat or..
  lat.long<-all.lat.long[which(values<=min(values))+15,]
  lat1<-which(lat==lat.long[,2][1]) # any row in column 1 == any row in column 1
  long1<-which(long==lat.long[,1][1]) 
 # temp.matrix[i,1]<-sst.array[long1,lat1,time.point][1]
  temp.matrix[i,1]<-mean(sst.array[long1,lat1,time.point1yrprior:time.point][1], na.rm=T)
  temp.matrix[i,2]<-min(sst.array[long1,lat1,time.point:time.point1yrprior[1]], na.rm=T) # Min temp in 1 year prior survey period
  temp.matrix[i,3]<-max(sst.array[long1,lat1,time.point:time.point1yrprior[1]], na.rm=T) # max temp in 1 year prior
  #temp.matrix[i,5]<-min(values) # distance between sample and matched temperature in euclidean distance degrees
}



#### Print ####
crab_airTmatch<-cbind(CRAB,temp.matrix)

setwd("/Users/brandybiggar/Desktop/")
write.csv(crab_airTmatch, "AirTest.csv")
