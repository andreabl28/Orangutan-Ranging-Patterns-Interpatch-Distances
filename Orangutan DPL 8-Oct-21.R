library(move)
library(dplyr)
library(lubridate)
library(forcats)
library(zoo)   


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

#--------------------------------------------------

#figure out which columns to use
#filter lokasi column for just M#
#measure cummulative distance between Bout end (Jam End) and M#
#also there is a column Makan Start Bout with S as the start time for an M#

#create a new column 
#need to create a label for between a bout (btwnM3M4)
#create a second column
#give each time that we want to include in our measurment, a specific name & number (Follow#_BtwnM3M4)
#group by these names to add distances between bouts

#use aggregate function

#---------------------------------------------------------
#rename column
BIB.dat3<-rename(BIB.dat3, follow = Summary.Reports.Master..Follow.Number)

#clean Makan.start.bout data for follow 6847
BIB.dat3$Makan.Start.bout<-as.factor(BIB.dat3$Makan.Start.bout)
BIB.dat3$Makan.Start.bout2<-fct_recode(BIB.dat3$Makan.Start.bout, "S" = "S2", "S" = "S")
#f6747<-filter(BIB.dat3, follow == "6847")
                                
#count total number of feeding bouts per follow
bouts_per_follow<-BIB.dat3%>%
  group_by(follow)%>%
  count(Makan.Start.bout2)
##rename column
bouts_per_follow<-rename(bouts_per_follow, ttl_makan = n)
#how many feeding bouts per day?
bouts_per_follow<-filter(bouts_per_follow, Makan.Start.bout2=="S")
#join ttl number of feeding bouts with dataset
BIB.dat4<-merge(BIB.dat3, bouts_per_follow, by ="follow")
#drop column
BIB.dat4= subset(BIB.dat4, select = -c(Makan.Start.bout2.y))

#row is or is not a feeding bout
BIB.dat4$is_feed<-ifelse(BIB.dat4$Buat.1 != "M","not feed","feed")

#The zoo R package contains the na.locf function, which is a generic function for replacing each NA
#with the most recent non-NA value prior to it.

#order dataset by timestamp
BIB.dat5<-BIB.dat4[order(as.POSIXct(BIB.dat4$timestamp.y)),]
#group by follow and add the feeding bout number that preceeds a non-feeding behavior
BIB.dat6<-BIB.dat5 %>% 
  group_by(follow, .drop=FALSE, add=TRUE) %>% 
  transmute(Makan.Bout.number2=na.locf(Makan.Bout.number, na.rm=FALSE))%>%
  ungroup()
#add new columns onto dataset
BIB.dat7<-cbind(BIB.dat5, BIB.dat6)

#Turn behaviors into categories to filter by, so we only add distances for the categories that we want

#combine 'feed or not feed behavior' & most recent feeding bout number
#probably needs to be cleaned up (remove NA's and only numbers)
BIB.dat7$makan_num<-paste(BIB.dat7$Makan.Bout.number2, BIB.dat7$is_feed)
BIB.dat7$S_makan_num<-paste(BIB.dat7$Makan.Start.bout2.x, BIB.dat7$makan_num)
#change into factors
#BIB.dat7$S_makan_num<-as.factor(BIB.dat7$S_makan_num)



#recode data values
#recode(char_vec, a = "Apple", b = "Banana")
BIB.dat7$S_makan_cat<-recode(BIB.dat7$S_makan_num, 
  " 1 not feed" = "cat 1_2", "S 2 feed" = "cat 1_2",
  " 2 not feed" = "cat 2_3", "S 3 feed" = "cat 2_3", 
  " 3 not feed" = "cat 3_4", "S 4 feed" = "cat 3_4",
  " 4 not feed" = "cat 4_5", "S 5 feed" = "cat 4_5",
  " 5 not feed" = "cat 5_6", "S 6 feed" = "cat 5_6",
  " 6 not feed" = "cat 5_6", "S 7 feed" = "cat 6_7",
  " 7 not feed" = "cat 7_8", "S 8 feed" = "cat 7_8",
  " 8 not feed" = "cat 8_9", "S 9 feed" = "cat 8_9")

#would need to filter before
BIB_distances<-aggregate(BIB.dat7$BIB.dist, by=list(Category=BIB.dat7$follow, BIB.dat7$S_makan_cat), FUN=sum)
BIB_distances$S_makan_cat<-BIB_distances$Group.2

#select just for the categories
BIB_distances<-filter(BIB_distances, S_makan_cat == "cat 1_2" | S_makan_cat == "cat 2_3" | S_makan_cat == "cat 3_4" |
        S_makan_cat == "cat 4_5" | S_makan_cat == "cat 5_6" | S_makan_cat == "cat 6_7" | S_makan_cat == "cat 7_8" |
          S_makan_cat == "cat 8_9")

mean(BIB_distances$x)
#144m
