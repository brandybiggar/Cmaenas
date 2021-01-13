####### Weekly SST Temperature ########
# ---- Install Packages ----
library(RNetCDF)
library(abind)
library(sp)
library(raster)
library(dplyr)

# ---- EXTRACT TEMPERATURES FOR EACH SITE ----
#https://www.metoffice.gov.uk/hadobs/hadisst/data/download.html
## this link above is where I got my SST data from, I use CDF format in this code
setwd("/Users/brandybiggar/Documents/Masters/Crabs_R/Temp.data/")
weekly_sst<-open.nc("HadISST_sst.nc")
print.nc(weekly_sst)
# notes
# long 360 deg E, lat 180 deg N
# days since 1870-1-1
sst<-read.nc(weekly_sst,unpack=T)
head(sst)
# time = sst[1] below
# latitude = sst[3]
# long = sst[4]
# sst = sst[5]

# Create a vector of each variable to match up below for site and time
long<-data.frame(sst[4]) 
lat<-data.frame(sst[3])
time<-data.frame(sst[1])
# get the dimensions for these variables from above (head or print)

##### MAKE GRID OF ALL POSSIBLE LAT AND LONG COMBINATIONS. 
all.lat.long1<-expand.grid(long[,1],lat[,1])
all.lat.long<-all.lat.long1    

## Make sst array  
sst.array<-abind(sst[5]) #Combine multidimensional arrays into a single array
## note that you also have to change the dimension here to match the dimension
## of your temp variable in the CDF file

##################################################################


#### lat/long data frame ####

CRAB<-read.csv("egc_test_dat.csv")


##############################################################


#### Temporal aspects ####
# Convert Day, Month, Year into:
#   -Julian day with start time 1870-01-01 (coded as 'Time')
# https://www.metoffice.gov.uk/hadobs/hadisst/data/download.html


CRAB$DayInYear <- strptime(CRAB$date, "%d/%m/%Y")$yday + 1
# calculate day in year number 

# Day since 1-1-1870
## this is where it's important to know when your temp dataset (the CDF file) started
## so you can match your lat/long file appropriately 
CRAB$DaySince0 <- ceiling(
  julian(strptime(CRAB$date, format = "%d/%m/%Y"), origin = as.Date("1870-01-01")))
# this is to calculate # of days since 1870-01-01 (the start of my temp data) for each
# row of my lat/long data
CRAB$Time1 <- as.numeric(CRAB$DaySince0)
# this is simply to make sure it's in numeric format

###############################################################################



#### CREATE BLANK MATRIX ####
temp.matrix<-as.data.frame(matrix(0,dim(CRAB),3))
# for your results of the loop to be printed into
colnames(temp.matrix)<-c("HADiSST.Mean","HADiSST.Min","HADiSST.Max")
# the temp variables of interest


for (i in 1:dim(CRAB)[1]){
  diff.times<-abs(CRAB$Time1[i]-time[,1]) #calculates difference in time between my days and sst days
  time.point<-which(diff.times==min(diff.times)) #chooses time point where time diff is minimal (closest to survey date)
  time.point1yrprior<-time.point
  #### EUCLIDEAN DISTANCE ####
  values<-sqrt(((CRAB[i,]$SiteLong)-(all.lat.long[,1]))^2+((CRAB[i,]$SiteLat)-(all.lat.long[,2]))^2)
  lat.long<-all.lat.long[which(values<=min(values)),]
  lat1<-which(lat==lat.long[,2]) 
  long1<-which(long==lat.long[,1]) 
  temp.matrix[i,1]<-mean(sst.array[long1,lat1,time.point1yrprior:time.point], na.rm=T)
  temp.matrix[i,2]<-min(sst.array[long1,lat1,time.point:time.point1yrprior], na.rm=T) # Min temp in 1 year prior survey period
  temp.matrix[i,3]<-max(sst.array[long1,lat1,time.point:time.point1yrprior], na.rm=T) # max temp in 1 year prior
}


TEST<-cbind(CRAB,temp.matrix)
########################################################################

#### Print ####
setwd("/Users/")

write.csv(TEST,"TEST.csv")
