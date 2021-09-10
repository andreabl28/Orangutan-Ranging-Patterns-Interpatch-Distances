library(move)
library(dplyr)
library(lubridate)

#Set working directory
setwd("C:/Users/black/Documents/Documents/PAL/Homerange Overlap/Orangutan-Ranging-Patterns")

#GP file exported from Movebank 7-Sept-2021
GP<-read.csv("GPOCP.csv")
GP<-tbl_df(GP)

#' Make timestamp a date/time variable
GP$timestamp<-as.POSIXct(GP$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#Round timestamps down to the nearest minute. Make seconds into 0.
GP$timestamp2<-floor_date(GP$timestamp, unit="minute", week_start = getOption("lubridate.week.start", 7))
#filter by dates (after 2013)
GP1<-filter(GP, timestamp2 > "2013-06-01 00:00:00")
GP.temp<-filter(GP, timestamp2 > "2013-06-01 00:00:00")
#Add 1 minute to have every minute represented in the dataset
#objects are a measure of seconds
GP.temp$timestamp2<-GP.temp$timestamp2+60
#combine the two datasets
GP2<-rbind(GP1, GP.temp)

#filter by individual
BIB.dat<-filter(GP2, individual.local.identifier =="BIB")

#combine with orangutan behavioral data-------------------
#read in Bibi's behavioral data
BIB<-read.csv("BIB_behavioral_data.csv")
BIB$time<-paste(BIB$Date, BIB$Jam)
BIB$timestamp<-as.POSIXct(BIB$time, format="%m-%d-%Y %H:%M:%OS")
BIB$timestamp2<-floor_date(BIB$timestamp, unit="minute", week_start = getOption("lubridate.week.start", 7))
BIB<-filter(BIB, timestamp2 > "2013-06-01 00:00:00")

#combine the movebank data with the behavioral data--------------------
BIB.dat1<-merge(BIB.dat, BIB, by = "timestamp2")

#identify duplicate timestamps
BIB.dat1$dup<-duplicated(BIB.dat1$timestamp2)
#remove duplicate timestamps
BIB.dat2<-filter(BIB.dat1, dup == "FALSE")

#create a MOVE object--------------------------------------
BIB.move<-move(x=BIB.dat2$location.long, y=BIB.dat2$location.lat,
              time=as.POSIXct(BIB.dat2$timestamp2),
              proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
              data=BIB.dat2, animal=BIB.dat2$individual.local.identifier)
BIB.dist<-distance(BIB.move)
#add one value to even out the number of values to combine back into the dataframe
BIB.dist<-c(BIB.dist, 1)
#combine the distance into the BIB data
BIB.dat3<-cbind(BIB.dat2, BIB.dist)

#EXAMPLES------------------------------------------------
#create a MOVE object
GP.move<-move(x=GP1$location.long, y=GP1$location.lat,
              time=as.POSIXct(GP1$timestamp),
              proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
              data=GP1, animal=GP1$individual.local.identifier)

#distance between consecutive movement locations for the first orangutan alphabetically (AK)
dist<-distance(GP.move)[1]

