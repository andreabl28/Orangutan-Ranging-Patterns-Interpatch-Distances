#--------------------
library(stringr)
library(move)
library(dplyr)
library(lubridate)
library(forcats)
library(zoo)
library(xts)
library(tidyr)

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
TA.dat<-filter(GP2, individual.local.identifier =="TA")

#identify duplicate timestamps
TA.dat$dup<-duplicated(TA.dat$timestamp2)
#remove duplicated timestamps (only removes 1 duplicate) from the movebank GPS data
TA.dat<-filter(TA.dat, dup == "FALSE")

#combine with orangutan behavioral data-------------------
#read in TAi's behavioral data
all_behav<-read.csv("all_behavioral_data2.csv")
TA<-filter(all_behav, OH.Descriptions..Code. == "TA")
TA$time<-paste(TA$Date, TA$Jam)
TA$timestamp<-as.POSIXct(TA$time, format="%m-%d-%Y %H:%M:%OS", tz="UTC")
TA$timestamp2<-floor_date(TA$timestamp, unit="minute", week_start = getOption("lubridate.week.start", 7))
TA<-filter(TA, timestamp2 > "2013-06-01 00:00:00")

#combine the movebank data with the behavioral data--------------------
TA.dat1<-inner_join(TA, TA.dat,  by="timestamp2")

#remove duplicated timestamp values, but first copy of Makan Bout # and Makan start into both timestamps
#order dataset by timestamp
TA.dat2<-TA.dat1[order(as.POSIXct(TA.dat1$timestamp2)),]
#identify duplicate timestamps
TA.dat2$dup2<-duplicated(TA.dat2$timestamp2)
#mutate -- move values in duplicated timestamps to both cells
TA.dat2<-TA.dat2 %>%
  group_by(timestamp2) %>%
  mutate(start2= lag(Makan.Start.bout), start3= lead(Makan.Start.bout)) %>%
  mutate(makan2= lag(Makan.Bout.number), makan3 = lead(Makan.Bout.number))
#https://stackoverflow.com/questions/27126609/copy-values-of-a-column-into-another-column-based-on-a-condition-using-a-loop
TA.dat2$start2<-toupper(TA.dat2$start2)
TA.dat2$start3<-toupper(TA.dat2$start3)
table(TA.dat2$start2)
table(TA.dat2$makan2)
table(TA.dat2$start3)
table(TA.dat2$makan3)

#combine the start and makan bout number columns into one column

#TA.dat2$makan.bout.num<-unite(TA.dat2$Makan.Bout.number, TA.dat2$makan2, TA.dat2$makan3, na.rm=TRUE, remove=FALSE)

TA.dat2$makan.bout.num<-
  ifelse(TA.dat2$Makan.Bout.number>=1, paste(TA.dat2$Makan.Bout.number), 
  ifelse(TA.dat2$makan2>=1, paste(TA.dat2$makan2), 
  ifelse(TA.dat2$makan3>=1, paste(TA.dat2$makan3))))
table(TA.dat2$makan.bout.num)

#clean Makan.start.bout data for follow 6847
TA.dat2$Makan.Start.bout<-as.factor(TA.dat2$Makan.Start.bout)
table(TA.dat2$Makan.Start.bout)
#TA.dat2$Makan.Start.bout2<-fct_recode(TA.dat2$Makan.Start.bout, "S" = "S2", "S" = "S")
#f6747<-filter(TA.dat3, follow == "6847")

#art$artname <- with(art, ifelse(trivname == "", as.character(latname), as.character(trivname)))
TA.dat2$start.bout<- with(TA.dat2, ifelse(Makan.Start.bout == "", as.character(start2), as.character(Makan.Start.bout)))
#trying the within function - doesn't really work
#within(TA.dat2, start.bout<-ifelse(Makan.Start.bout == "", as.character(start2), as.character(Makan.Start.bout)))
TA.dat2$start.bout2<- with(TA.dat2, ifelse(start.bout == "", as.character(start2), as.character(start.bout)))

#remove duplicate timestamps (only removes 1 duplicate)
TA.dat2<-filter(TA.dat2, dup2 == "FALSE")
TA.dat2<-tbl_df(TA.dat2)

#create a MOVE object-----------------------
TA.move<-move(x=TA.dat2$location.long, y=TA.dat2$location.lat,
              time=as.POSIXct(TA.dat2$timestamp2, format="%Y-%m-%d %H:%M:%S"),
              proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
              data=TA.dat2, animal=TA.dat2$individual.local.identifier, 
              removeDuplicatedTimestamps=TRUE)

#calculate distance moved between timestamps
TA.dist<-distance(TA.move)
#add one value to even out the number of values to combine back into the dataframe
TA.dist<-c(TA.dist, 1)
#combine the distance into the TA data
TA.dat3<-cbind(TA.dat2, TA.dist)

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
TA.dat3<-rename(TA.dat3, follow = Summary.Reports.Master..Follow.Number)

class(TA.dat3$makan.bout.num)
TA.dat3$makan.bout.num<-as.numeric(TA.dat3$makan.bout.num)

#maximum feeding bout number per follow
Makan.Bout.max<-aggregate(makan.bout.num ~ follow, data = TA.dat3, max)
##rename column
Makan.Bout.max<-rename(Makan.Bout.max, Makan.Bout.max = makan.bout.num)
#merge data
TA.dat4<-merge(TA.dat3, Makan.Bout.max, by="follow")

table(TA.dat4$Buat.1)
#label if each row is or is not a feeding bout
TA.dat4$is_feed<-ifelse(TA.dat4$Buat.1 != "M","not feed","feed")

#The zoo R package contains the na.locf function, which is a generic function for replacing each NA
#with the most recent non-NA value prior to it.

#order dataset by timestamp
TA.dat5<-TA.dat4[order(as.POSIXct(TA.dat4$timestamp.y)),]
#group by follow and add the feeding bout number that preceeds a non-feeding behavior
TA.dat6<-TA.dat5 %>% 
  group_by(follow, .drop=FALSE, add=TRUE) %>% 
  transmute(makan.bout.num2=na.locf(makan.bout.num, na.rm=FALSE))%>%
  ungroup()
#rename follow column
TA.dat6<-rename(TA.dat6, follow2 = follow)
#add new columns onto dataset
TA.dat7<-cbind(TA.dat5, TA.dat6)


#Turn behaviors into categories to filter by, so we only add distances for the non-feeding behaviors

#combine 'feed or not feed behavior' & most recent feeding bout number
#probably needs to be cleaned up (remove NA's and only numbers)
TA.dat7$makan.bout.num3<-paste(TA.dat7$makan.bout.num2, TA.dat7$is_feed)

#add label to remove all last feeding bouts
TA.dat7$last_bout<-ifelse(TA.dat7$makan.bout.num2 == TA.dat7$Makan.Bout.max, "last", "not last")
TA.dat7$S_makan_num_last<-paste(TA.dat7$makan.bout.num3, TA.dat7$last_bout)
table(TA.dat7$S_makan_num_last)

#Creat a new lable to used to match to categories below 
TA.dat7$S_makan_num2<-paste(TA.dat7$start.bout2, TA.dat7$makan.bout.num3)
TA.dat7$S_makan_num2<-str_remove(TA.dat7$S_makan_num2, "NA")
TA.dat7$S_makan_num2<-str_remove(TA.dat7$S_makan_num2, "NA")
table(TA.dat7$S_makan_num2)

#-----------------------remove consecutive 'I' (rest) activity bouts--------------------
#count the number of 'I' rest bouts
#https://stackoverflow.com/questions/19998836/create-counter-within-consecutive-runs-of-values
#this works
TA.dat7$Buat.seq<-sequence(rle(as.character(TA.dat7$Buat.1))$lengths)
TA.dat7$Buat.seq2<-paste(TA.dat7$Buat.1, TA.dat7$Buat.seq)
table(TA.dat7$Buat.seq2)

#remove anything after 2 resting bouts in a row (I 2+)
TA.dat7$keep<- ifelse(TA.dat7$Buat.1 == "I" & TA.dat7$Buat.seq >= 3, "remove", "keep")
TA.dat8<-filter(TA.dat7, keep == "keep")

#----------------------------

#if feeding bout# == the total number of feeding bouts
#filter out all 'XX non feed' for last bout

#could rewrite code
#TA.dat8<-filter(TA.dat7, S_makan_num_last != "1 feed last" & S_makan_num_last != "2 feed last" & 
# S_makan_num_last != "3 feed last" & S_makan_num_last != "4 feed last" & S_makan_num_last != "5 feed last" & 
# S_makan_num_last != "6 feed last")

TA.dat8<-filter(TA.dat8, S_makan_num_last != "1 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "2 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "3 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "4 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "5 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "6 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "7 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "8 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "9 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "10 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "11 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "12 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "13 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "14 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "15 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "16 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "17 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "18 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "19 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "20 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "21 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "22 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "23 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "24 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "25 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "26 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "27 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "28 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "29 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "30 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "31 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "32 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "33 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "34 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "35 feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "1 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "2 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "3 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "4 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "5 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "6 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "7 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "8 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "9 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "10 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "11 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "12 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "13 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "14 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "15 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "16 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "17 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "18 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "19 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "20 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "21 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "22 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "23 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "24 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "25 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "26 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "27 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "28 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "29 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "30 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "31 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "32 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "33 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "34 not feed last")
TA.dat8<-filter(TA.dat8, S_makan_num_last != "35 not feed last")

#-----------------------------------------------------------------------

#NOT RUNNING BECAUSE NEED TO TAKE OUT 'NOT LAST'. Don't know where it gets added in.

#recode data values into categories (e.g. cat 1_2 is the travel distance between category 1 and 2)
#recode(char_vec, a = "Apple", b = "Banana")
TA.dat8$S_makan_cat<-recode(TA.dat8$S_makan_num2, 
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
  " 22 not feed" = "cat 22_23", "S 23 feed" = "cat 22_23", 
  " 23 not feed" = "cat 23_24", "S 24 feed" = "cat 23_24", 
  " 24 not feed" = "cat 24_25", "S 25 feed" = "cat 24_25", 
  " 25 not feed" = "cat 25_26", "S 26 feed" = "cat 25_26", 
  " 26 not feed" = "cat 26_27", "S 27 feed" = "cat 26_27", 
  " 27 not feed" = "cat 27_28", "S 28 feed" = "cat 27_28", 
  " 28 not feed" = "cat 28_29", "S 29 feed" = "cat 28_29", 
  " 29 not feed" = "cat 29_30", "S 30 feed" = "cat 29_30", 
  " 30 not feed" = "cat 30_31", "S 31 feed" = "cat 30_31",
  " 31 not feed" = "cat 31_32", "S 32 feed" = "cat 31_32",
  " 32 not feed" = "cat 32_33", "S 33 feed" = "cat 32_33",
  " 33 not feed" = "cat 33_34", "S 34 feed" = "cat 33_34",
  " 34 not feed" = "cat 34_35", "S 35 feed" = "cat 34_35",
  " 35 not feed" = "cat 35_36", "S 36 feed" = "cat 35_36" 
  )

#remove consecutive characters of 'IIIII' or Sarang Siang

#would need to filter before
TA_distances<-aggregate(TA.dat8$TA.dist, by=list(Category=TA.dat8$follow, TA.dat8$S_makan_cat), FUN=sum)
TA_distances$S_makan_cat<-TA_distances$Group.2

#select just for the categories
TA_distances<-filter(TA_distances, S_makan_cat == "cat 1_2" | S_makan_cat == "cat 2_3" | S_makan_cat == "cat 3_4" |
        S_makan_cat == "cat 4_5" | S_makan_cat == "cat 5_6" | S_makan_cat == "cat 6_7" | S_makan_cat == "cat 7_8" |
        S_makan_cat == "cat 8_9" | S_makan_cat == "cat 9_10" | S_makan_cat == "cat 10_11" | S_makan_cat == "cat 11_12" |
        S_makan_cat == "cat 12_13" | S_makan_cat == "cat 13_14" | S_makan_cat == "cat 14_15" |S_makan_cat == "cat 15_16" |
        S_makan_cat == "cat 16_17" | S_makan_cat == "cat 17_18" | S_makan_cat == "cat 18_19" | S_makan_cat == "cat 19_20" |
        S_makan_cat == "cat 20_21" | S_makan_cat == "cat 21_22" | S_makan_cat == "cat 22_23" | S_makan_cat == "cat 23_24" |
        S_makan_cat == "cat 24_25" | S_makan_cat == "cat 25_26" | S_makan_cat == "cat 26_27" | S_makan_cat == "cat 27_28" |
        S_makan_cat == "cat 28_29" | S_makan_cat == "cat 29_30" | S_makan_cat == "cat 30_31" | S_makan_cat == "cat 31_32" |
        S_makan_cat == "cat 32_33"| S_makan_cat == "cat 33_34" | S_makan_cat == "cat 34_35"| S_makan_cat == "cat 35_36"
        )

TA_distances$OH<-c(rep("TA", nrow(TA_distances)))


#add in OH offspring age based on follow #
age_offspring<-read.csv("Age of youngest offspring export 14Oct2021.csv")
#rename columns
TA_distances<-rename(TA_distances, follow = Category)
TA_distances<-rename(TA_distances, dist_btw_bouts = x)
#merge datasets
TA_distances2<-merge(TA_distances, age_offspring, by = "follow")

mean(TA_distances$dist_btw_bouts)
#97.256

#regression
reg1 <- lm(dist_btw_bouts~Age.of.Youngest.Offspring,data=TA_distances2) 
summary(reg1)

plot(TA_distances2$Age.of.Youngest.Offspring, TA_distances2$dist_btw_bouts, 
     ylim=c(0, 500))
abline(reg1, col=c("blue"))

#export dataset as a csv
write.csv(TA_distances2, "TA_distances.csv")


#-------------
#checking the data
TA.8615<-filter(TA.dat8, follow == "8165")
