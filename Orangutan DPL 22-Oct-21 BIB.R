#--------------------

library(move)
library(dplyr)
library(lubridate)
library(forcats)
library(zoo)
library(xts)

#GP file exported from Movebank 7-Sept-2021
GP<-read.csv("GPOCP.csv" ,row.names = 1)
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

#identify duplicate timestamps
BIB.dat$dup<-duplicated(BIB.dat$timestamp2)
#remove duplicate timestamps (only removes 1 duplicate)
BIB.dat<-filter(BIB.dat, dup == "FALSE")

#combine with orangutan behavioral data-------------------
#read in Bibi's behavioral data
BIB<-read.csv("BIB_behavioral_data.csv")
BIB$time<-paste(BIB$Date, BIB$Jam)
BIB$timestamp<-as.POSIXct(BIB$time, format="%m-%d-%Y %H:%M:%OS", tz="UTC")
BIB$timestamp2<-floor_date(BIB$timestamp, unit="minute", week_start = getOption("lubridate.week.start", 7))
BIB<-filter(BIB, timestamp2 > "2013-06-01 00:00:00")

#combine the movebank data with the behavioral data--------------------
BIB.dat1<-inner_join(BIB, BIB.dat,  by="timestamp2")

#identify duplicate timestamps
BIB.dat1$dup<-duplicated(BIB.dat1$timestamp2)
#remove duplicate timestamps (only one value)
BIB.dat2<-filter(BIB.dat1, dup == "FALSE")

#order dataset by timestamp
BIB.dat2<-BIB.dat2[order(as.POSIXct(BIB.dat2$timestamp2)),]

#create a MOVE object-----------------------
BIB.move<-move(x=BIB.dat2$location.long, y=BIB.dat2$location.lat,
              time=as.POSIXct(BIB.dat2$timestamp2),
              proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
              data=BIB.dat2, animal=BIB.dat2$individual.local.identifier)

#calculate distance moved between timestamps
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

#---------------------------------------------------------
#rename column
BIB.dat3<-rename(BIB.dat3, follow = Summary.Reports.Master..Follow.Number)

#clean Makan.start.bout data for follow 6847
BIB.dat3$Makan.Start.bout<-as.factor(BIB.dat3$Makan.Start.bout)
BIB.dat3$Makan.Start.bout2<-fct_recode(BIB.dat3$Makan.Start.bout, "S" = "S2", "S" = "S")
#f6747<-filter(BIB.dat3, follow == "6847")
                                
class(BIB.dat3$Makan.Bout.number)
#max(BIB.dat3$Makan.Bout.number, na.rm=T)

BIB.dat3$feeding_bout_num<-BIB.dat3$Makan.Bout.number
#maximum feeding bout number per follow
bouts_per_follow<-BIB.dat3 %>%
  group_by(follow) %>%
  summarise(max == max(Makan.Bout.number, na.rm=TRUE))

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
#rename follow column
BIB.dat6<-rename(BIB.dat6, follow2 = follow)
#add new columns onto dataset
BIB.dat7<-cbind(BIB.dat5, BIB.dat6)

#---------do not want to include after the last feeding bouts -- need to remove after last feeding bouts 
BIB.dat7$bout_error<-BIB.dat7$Makan.Bout.number2 > BIB.dat7$ttl_makan
BIB.bout.error<-filter(BIB.dat7, bout_error == "TRUE")
#ummm..... ~3700 observations
#are these errors in the behavioral data?
follows<-unique(BIB.bout.error$follow)
#122 follows

#-----------move below, haven't created column yet

#if feeding bout# == the total number of feeding bouts
#filter out all 'XX non feed' for last bout

#row is or is not a feeding bout
BIB.dat7$last_bout<-ifelse(BIB.dat7$Makan.Bout.number2 != BIB.dat7$ttl_makan, "not last","last")
BIB.dat7$S_makan_num_last<-paste(BIB.dat7$S_makan_num, BIB.dat7$last_bout)

BIB.dat8<-filter(BIB.dat7, S_makan_num_last != "1 feed last")
BIB.dat8<-filter(BIB.dat8, S_makan_num_last != "2 feed last")
BIB.dat8<-filter(BIB.dat8, S_makan_num_last != "3 feed last")
BIB.dat8<-filter(BIB.dat8, S_makan_num_last != "4 feed last")
BIB.dat8<-filter(BIB.dat8, S_makan_num_last != "5 feed last")
BIB.dat8<-filter(BIB.dat8, S_makan_num_last != "6 feed last")
BIB.dat8<-filter(BIB.dat8, S_makan_num_last != "7 feed last")
BIB.dat8<-filter(BIB.dat8, S_makan_num_last != "8 feed last")
BIB.dat8<-filter(BIB.dat8, S_makan_num_last != "9 feed last")

#-----------------------------------------------------------------------

#Turn behaviors into categories to filter by, so we only add distances for the non-feeding behaviors

#combine 'feed or not feed behavior' & most recent feeding bout number
#probably needs to be cleaned up (remove NA's and only numbers)
BIB.dat7$makan_num<-paste(BIB.dat7$Makan.Bout.number2, BIB.dat7$is_feed)
BIB.dat7$S_makan_num<-paste(BIB.dat7$Makan.Start.bout2.x, BIB.dat7$makan_num)
#change into factors
#BIB.dat7$S_makan_num<-as.factor(BIB.dat7$S_makan_num)


#------------------move code above here..?




#recode data values into categories (e.g. cat 1_2 is the travel distance between category 1 and 2)
#recode(char_vec, a = "Apple", b = "Banana")
BIB.dat7$S_makan_cat<-recode(BIB.dat7$S_makan_num, 
  " 1 not feed" = "cat 1_2", "S 2 feed" = "cat 1_2",
  " 2 not feed" = "cat 2_3", "S 3 feed" = "cat 2_3", 
  " 3 not feed" = "cat 3_4", "S 4 feed" = "cat 3_4",
  " 4 not feed" = "cat 4_5", "S 5 feed" = "cat 4_5",
  " 5 not feed" = "cat 5_6", "S 6 feed" = "cat 5_6",
  " 6 not feed" = "cat 6_7", "S 7 feed" = "cat 6_7",
  " 7 not feed" = "cat 7_8", "S 8 feed" = "cat 7_8",
  " 8 not feed" = "cat 8_9", "S 9 feed" = "cat 8_9", 
  " 9 not feed" = "cat 9_10", "S 10 feed" = "cat 9_10", 
  " 10 not feed" = "cat 10_11", "S 11 feed" = "cat 10_11",
  " 11 not feed" = "cat 11_12", "S 12 feed" = "cat 11_12",
  " 12 not feed" = "cat 12_13", "S 13 feed" = "cat 12_13",
  " 13 not feed" = "cat 13_14", "S 14 feed" = "cat 13_14", 
  " 14 not feed" = "cat 14_15", "S 15 feed" = "cat 14_15", 
  " 15 not feed" = "cat 15_16", "S 16 feed" = "cat 15_16", 
  " 16 not feed" = "cat 16_17", "S 17 feed" = "cat 16_17", 
  " 17 not feed" = "cat 17_18", "S 18 feed" = "cat 17_18", 
  " 18 not feed" = "cat 18_19", "S 19 feed" = "cat 18_19", 
  " 19 not feed" = "cat 19_20", "S 20 feed" = "cat 19_20", 
  " 20 not feed" = "cat 20_21", "S 21 feed" = "cat 20_21", 
  " 21 not feed" = "cat 21_22", "S 22 feed" = "cat 21_22", 
  " 22 not feed" = "cat 22_23", "S 22 feed" = "cat 22_23", 
  " 23 not feed" = "cat 23_24", "S 23 feed" = "cat 23_24", 
  " 24 not feed" = "cat 24_25", "S 24 feed" = "cat 24_25", 
  " 25 not feed" = "cat 25_26", "S 25 feed" = "cat 25_26", 
  " 26 not feed" = "cat 26_27", "S 26 feed" = "cat 26_27", 
  " 27 not feed" = "cat 27_28", "S 27 feed" = "cat 27_28", 
  " 28 not feed" = "cat 28_29", "S 28 feed" = "cat 28_29", 
  " 29 not feed" = "cat 29_30", "S 29 feed" = "cat 29_30", 
  " 30 not feed" = "cat 30_31", "S 30 feed" = "cat 30_31", 
  )

#would need to filter before
BIB_distances<-aggregate(BIB.dat7$BIB.dist, by=list(Category=BIB.dat7$follow, BIB.dat7$S_makan_cat), FUN=sum)
BIB_distances$S_makan_cat<-BIB_distances$Group.2

#select just for the categories
BIB_distances<-filter(BIB_distances, S_makan_cat == "cat 1_2" | S_makan_cat == "cat 2_3" | S_makan_cat == "cat 3_4" |
        S_makan_cat == "cat 4_5" | S_makan_cat == "cat 5_6" | S_makan_cat == "cat 6_7" | S_makan_cat == "cat 7_8" |
        S_makan_cat == "cat 8_9" | S_makan_cat == "cat 9_10" | S_makan_cat == "cat 10_11" | S_makan_cat == "cat 11_12" |
        S_makan_cat == "cat 12_13" | S_makan_cat == "cat 13_14" | S_makan_cat == "cat 14_15" |S_makan_cat == "cat 15_16" |
        S_makan_cat == "cat 16_17" | S_makan_cat == "cat 17_18" | S_makan_cat == "cat 18_19" | S_makan_cat == "cat 19_20" |
        S_makan_cat == "cat 20_21" | S_makan_cat == "cat 21_22" | S_makan_cat == "cat 22_23" | S_makan_cat == "cat 23_24" |
        S_makan_cat == "cat 24_25" | S_makan_cat == "cat 25_26" | S_makan_cat == "cat 26_27" | S_makan_cat == "cat 27_28" |
        S_makan_cat == "cat 28_29" | S_makan_cat == "cat 29_30" | S_makan_cat == "cat 30_31" 
        )

BIB_distances$OH<-c(rep("BIB", nrow(BIB_distances)))

mean(BIB_distances$x)
#157

#add in OH offspring age based on follow #
age_offspring<-read.csv("Age of youngest offspring export 14Oct2021.csv")
#rename columns
BIB_distances<-rename(BIB_distances, follow = Category)
BIB_distances<-rename(BIB_distances, dist_btw_bouts = x)
#merge datasets
BIB_distances2<-merge(BIB_distances, age_offspring, by = "follow")

#regression
reg1 <- lm(dist_btw_bouts~Age.of.Youngest.Offspring,data=BIB_distances2) 
summary(reg1)

plot(BIB_distances2$Age.of.Youngest.Offspring, BIB_distances2$dist_btw_bouts, 
     ylim=c(0,500))
abline(reg1, col=c("blue"))

#export dataset as a csv
#write.csv(BIB_distances2, "BIB_distances.csv")
