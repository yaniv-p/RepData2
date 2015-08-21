# Download the files if its not there
if(!file.exists("data")) {
        library(R.utils)
        dir.create("data")
        urlData<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
        download.file(url=urlData, destfile="data\\repdata-data-StormData.csv.bz2")
        bunzip2("data\\repdata-data-StormData.csv.bz2",exdir = "data", remove=FALSE)
        
        urlDoc1<-'https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf'
        download.file(url=urlDoc1, destfile="data\\repdata-peer2_doc-pd01016005curr.pdf",mode='wb')
        
        urlDoc2<-'https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf'
        download.file(url=urlDoc2, destfile="data\\repdata-peer2_doc-NCDC Storm Events-FAQ Page.pdf",mode='wb')
}

# read the data
StromData<-read.csv('data\\repdata-data-StormData.csv')

# we can see the data set has 902,297 entries with 37 varibels
dim(StromData)

# taking a look at the varibles
names(StromData)

# Get info on varibales
str(StromData)


# look at EVTYPE
str(StromData$EVTYPE)

# many values(985) and not the expected 48 per the document.
# looking at some values
head(unique(StromData$EVTYPE),50)
tail(unique(StromData$EVTYPE),50)

# By looking at the document section 7, The most left 9 chanrcters after removing blanks are unique.
# So remove non ascii charcters, remove blanks and look at first 9 letters from the left side of EVTYPE
library(stringdist)
library(stringr)
length(unique(substr(toupper(str_replace_all(StromData$EVTYPE,"[[:punct:][:space:][:digit:]]","")),1,9)))

# Got 415 entries
# lets check this number per year

#convert the date string to a R date and to a year
library(lubridate)
StromData$BGN_DATE_D<-as.Date(StromData$BGN_DATE,"%m/%d/%Y")
StromData$year<-year(StromData$BGN_DATE_D)
m<-matrix(nrow = 2010-1950+1,ncol = 2,dimnames = list(1950:2010,c('year','EventNum')))
StromFunc <- function(x) 
{
        tmp<-StromData[StromData$year>x,"EVTYPE"]
        length(unique(substr(toupper(str_replace_all(tmp,"[[:punct:][:space:][:digit:]]","")),1,9)))
}
j<-1
for (i in 1950:2010) 
{
        m[j,1]<-i
        m[j,2]<-StromFunc(i)
        j<-j+1
} 

plot(m[,1],m[,2])
abline(h=48)
m

# number get close to the 48 from year 2002
# lets see later how to try and map them

# look at FATALITIES
str(StromData$FATALITIES)
summary(StromData$FATALITIES)
head(StromData$FATALITIES)

# look at INJURIES
str(StromData$INJURIES)
summary(StromData$INJURIES)
head(StromData$INJURIES)


# look at PROPDMG
str(StromData$PROPDMG)
summary(StromData$PROPDMG)
head(StromData$PROPDMG)


# look at PROPDMGEXP
str(StromData$PROPDMGEXP)
summary(StromData$PROPDMGEXP)
head(StromData$PROPDMGEXP)

# Based on the documnetation -
# "Estimates should be rounded to three significant digits, followed by an alphabetical character 
# signifying the magnitude of the # number, i.e., 1.55B for $1,550,000,000.
# Alphabetical characters used to signify magnitude include “K” for thousands, # “M” for millions, 
# and “B” for billions." 

# look at the PROPDMG of symboles I can't understand the values
StromData[StromData$PROPDMGEXP %in% c('+','-','?'),c("PROPDMG","year","PROPDMGEXP")]
# only 6 have non zero  values in 1994 and 1995 only. We will assume muliplier is one for them

# So I'll assume the follwing multiplier for the PROPDMGEXP:
# for null value - just take 1 as the mulitplier
# B - 10 ^ 9
# m/m - 10 ^ 6
# k - 10 ^ 3
# h/H - hundreds - 10 ^2
# numbers 0..9 - 10 ^ (that number 0..9). for example if we get a 1 then the muliplier is 10 ^ 1=10
# for the 3 charcters -, ? + I'll assume 10 ^ 0( =1)


# look at CROPDMG
str(StromData$CROPDMG)
summary(StromData$CROPDMG)
head(StromData$CROPDMG)

# look at CROPDMGEXP
str(StromData$CROPDMGEXP)
summary(StromData$CROPDMGEXP)
head(StromData$CROPDMGEXP)

# look at the PROPDMG of symboles I can't understand the values
StromData[StromData$CROPDMGEXP =='?',c("CROPDMG","year")]
# all are zero value so we can ignore them

# I'll assume the same assuptions as the above for PROPDMGEXP

# add to d.f unified multiplier and callacuate the actual PROPDMG and CROPDMG
Expdf<-data.frame(
EXP=c('','-','?','+',0:9,'B','m','M','k','K','h','H'),
Multplier=c(1,0,0,0,10 ^0,10 ^1,10 ^2,10 ^3,10 ^4,10 ^5,10 ^6,10 ^7,10 ^8,10 ^9,10 ^9,10 ^6,10 ^6,10 ^3,10 ^3,10 ^2,10^2))

StromData<-merge(StromData,Expdf,by.x ='PROPDMGEXP', by.y='EXP')
library(dplyr)
StromData<-mutate(StromData,DMGValue=PROPDMG*Multplier+CROPDMG*Multplier,Population=FATALITIES+INJURIES)

# find the dammges per year in order to check the assignemnt remark
# "The events in the database start in the year 1950 and end in November 2011. 
# In the earlier years of the database there are generally fewer events recorded, 
# most likely due to a lack of good records. 
# More recent years should be considered more complete."

Totals<-summarise(group_by(StromData,year),totalDMGValue=sum(DMGValue),totalDMGPopulation=sum(Population),count=n())

DMGValue<- select(filter(Totals,totalDMGValue>0),year,totalDMGValue,count)
DMGPopulation<- select(filter(Totals,totalDMGPopulation>0),year,totalDMGPopulation,count)
p<-merge(DMGPopulation,DMGValue)
library(tidyr)
library(ggplot2)
p<-gather(p, DMGtype, val, -year)
p$year<-as.Date(paste('01/01/',p$year),"%m/%d/%Y")

qplot(year, val, data = p, geom = "line", group = DMGtype) +
        facet_grid(DMGtype ~ ., scale = "free_y") 

# Based on the plot we can see that the count of events (with non zero DMG value) is starting to grow
# after 1990, so before that there are indded not many records.
# The Population health has data fron the beging
# The economic samage has mostely data from 1993 only.
# so based on that we can conclude that from around 1993 we have more complete data
# so we will continue on data from that period.

StromData1993<-filter(StromData,year>=1993)

Totals1993<-summarise(group_by(StromData1993,EVTYPE),totalDMGValue=sum(DMGValue),totalDMGPopulation=sum(Population))

DMGValue1993<- filter(Totals1993,totalDMGValue>0)
DMGPopulation1993<- filter(Totals1993,totalDMGPopulation>0)


# No try to match the EVTYPE to the ones defined in the document
lookup<-c("Astronomical Low Tide", "Avalanche", "Blizzard", 
          "Coastal Flood", "Cold/Wind Chill", "Debris Flow", 
          "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", 
          "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill", 
          "Flash Flood", "Flood", "Freezing Fog", "Frost/Freeze", 
          "Funnel Cloud", "Hail", "Heat", "Heavy Rain", "Heavy Snow", 
          "High Surf", "High Wind", "Hurricane (Typhoon)", "Ice Storm", 
          "Lake-Effect Snow", "Lakeshore Flood", "Lightning", 
          "Marine Hail", "Marine High Wind", "Marine Strong Wind", 
          "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet", 
          "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", 
          "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", 
          "Volcanic Ash", "Waterspout", "Wildfire", "Winter Storm", 
          "Winter Weather")

lookup1<-toupper(str_replace_all(lookup,"[[:punct:][:space:][:digit:]]",""))
lookup2<-unique(substr(lookup1,1,9))
d<-data.frame(l1=lookup,l2=lookup1,l3=lookup2)

EVType1933<-unique(c(as.character(unique(DMGPopulation1993$EVTYPE)),as.character(unique(DMGValue1993$EVTYPE))))

#a<-unique(substr(toupper(str_replace_all(EVType1933,"[[:punct:][:space:][:digit:]]","")),1,9))
a<-unique(toupper(str_replace_all(EVType1933,"[[:punct:][:space:][:digit:]]","")))
adf<-data.frame(
        a=a)

a2<-merge(adf,d,by.x='a',by.y='l2',all.x = T)
a3<-a2[is.na(a2$l1),'a']
#r<-amatch(a3,lookup1,maxDist =Inf)
r<-amatch(a3,lookup1,maxDist =1)
c<-data.frame(EvP=a3, trans=lookup1[r])
print(as.character(c[is.na(c$trans),"EvP"]),print.gap = 30)
