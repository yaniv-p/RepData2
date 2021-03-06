library(stringdist)
library(stringr)
library(R.utils)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
# Download the files if its not there
if(!file.exists("data")) {
        
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
length(unique(substr(toupper(str_replace_all(StromData$EVTYPE,"[[:punct:][:space:][:digit:]]","")),1,9)))

# Got 415 entries
# lets check this number per year

#convert the date string to a R date and add a varibile of a year
StromData$BGN_DATE_D<-as.Date(StromData$BGN_DATE,"%m/%d/%Y")
StromData$year<-year(StromData$BGN_DATE_D)

summary(StromData$year)

# we can see we have data from 1950 to 2011

# Matrix that will continue the number of disctinct EVTYPE per year
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
# number get close to the 48 as we are reaching 2011
m
# number get close to the 48 from year 2002
# lets see later how to try and map them to the 48 ones in the document


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

# So to summerize I'll assume the follwing multiplier for the PROPDMGEXP:
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

# merge with the multplier
StromData<-merge(StromData,Expdf,by.x ='PROPDMGEXP', by.y='EXP')

# calculate the $ value by summing Properity and crops 
# calculate the number of pepole hurt by summing FATALITIES and INJURIES 
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

# make it tidy
p<-gather(p, DMGtype, val, -year)
p$year<-as.Date(paste('01/01/',p$year),"%m/%d/%Y")

qplot(year, val, data = p, geom = "line", group = DMGtype) +
        facet_grid(DMGtype ~ ., scale = "free_y") 

# Based on the plot we can see that the count of events (with non zero DMG value) is starting to grow
# after 1990, espacly at 1995-2000, so before that there are indded not many records.
# The Population health has data fron the beging of the period.
# The economic data has mostely data from 1993 
# so based on that we can conclude that from around 1993 towards 1995-2000 we have more complete data
# Based on that we will answer the questions on data from 1997.

# filter the relavent data
StromDataPart<-filter(StromData,year>=1997)

# group by EVTYPE
TotalsPart<-summarise(group_by(StromDataPart,EVTYPE),totalDMGValue=sum(DMGValue),totalDMGPopulation=sum(Population))

# only non zero values for Damage value and in population affected is intersting 
DMGValuePart<- filter(TotalsPart,totalDMGValue>0)
DMGPopulationPart<- filter(TotalsPart,totalDMGPopulation>0)


# Now try to match the EVTYPE to the ones defined in the document

# the 48 EVTYPES from the document
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
# remove non Alpha charcters, remove blanks. make all uppercase
lookup1<-toupper(str_replace_all(lookup,"[[:punct:][:space:][:digit:]]",""))
#lookup2<-unique(substr(lookup1,1,9))
d<-data.frame(Trans=lookup,l2=lookup1) # build d.f to be used for merge later

# All the Events for non 0 DMG's and population effcted
EVTypePart<-unique(c(as.character(unique(DMGPopulationPart$EVTYPE)),as.character(unique(DMGValuePart$EVTYPE))))

# remove non Alpha charcters, remove blanks. make all uppercase
EVTypePart1<-toupper(str_replace_all(EVTypePart,"[[:punct:][:space:][:digit:]]",""))
#EVTypePart2<-substr(EVTypePart1,1,9)
EVTypePartdf<-data.frame(origEV=EVTypePart,shortE=EVTypePart1) # build d.f to be used for merge later

# Try to find a match
a2<-merge(EVTypePartdf,d,by.x='shortE',by.y='l2',all.x = T)
summary(a2)

# we still have lots of values not macthed
a3<-a2[is.na(a2$Trans),] # the value that are not matching
str(a3)
# 126 values needs to be macthed
# going to manulay do that by pasting the missing values to excel and manuly choose what of the 48 Events fits.
# Then will constarct R code to populate a mapping table.

# will copy this outpout to excel
print(a3$origEV)

# copied to excel and manuly mapped.
# below is the rsult of that mapping

# build the maaping table
Trans<-c('Frost/Freeze'
         ,'Coastal Flood'
         ,'Frost/Freeze'
         ,'Dust Storm'
         ,'Heavy Snow'
         ,'Wildfire'
         ,'Coastal Flood'
         ,'Coastal Flood'
         ,'Coastal Flood'
         ,'Marine Thunderstorm Wind'
         ,'Marine Thunderstorm Wind'
         ,'Extreme Cold/Wind Chill'
         ,'Extreme Cold/Wind Chill'
         ,'Extreme Cold/Wind Chill'
         ,'Extreme Cold/Wind Chill'
         ,'Flood'
         ,'Flood'
         ,'High Wind'
         ,'Heavy Snow'
         ,'Extreme Cold/Wind Chill'
         ,'Extreme Cold/Wind Chill'
         ,'Heavy Snow'
         ,'Flash Flood'
         ,'Flash Flood'
         ,'Dense Fog'
         ,'Frost/Freeze'
         ,'Frost/Freeze'
         ,'Frost/Freeze'
         ,'Frost/Freeze'
         ,'Frost/Freeze'
         ,'Frost/Freeze'
         ,'Frost/Freeze'
         ,'Frost/Freeze'
         ,'High Wind'
         ,'High Wind'
         ,'High Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Frost/Freeze'
         ,'Frost/Freeze'
         ,'High Surf'
         ,'High Surf'
         ,'High Surf'
         ,'High Surf'
         ,'High Surf'
         ,'High Surf'
         ,'High Surf'
         ,'High Surf'
         ,'High Wind'
         ,'High Wind'
         ,'Hurricane (Typhoon)'
         ,'Extreme Cold/Wind Chill'
         ,'Extreme Cold/Wind Chill'
         ,'Extreme Cold/Wind Chill'
         ,'Frost/Freeze'
         ,'Frost/Freeze'
         ,'Frost/Freeze'
         ,'Avalanche'
         ,'Avalanche'
         ,'Avalanche'
         ,'Heavy Snow'
         ,'Sleet'
         ,'Winter Weather'
         ,'Winter Weather'
         ,'Marine Thunderstorm Wind'
         ,'Sleet'
         ,'Sleet'
         ,'Sleet'
         ,'Avalanche'
         ,'Avalanche'
         ,'Avalanche'
         ,'Strong Wind'
         ,'Strong Wind'
         ,'Strong Wind'
         ,'Winter Weather'
         ,'Heavy Rain'
         ,'Sleet'
         ,'Excessive Heat'
         ,'Rip Current'
         ,'Lakeshore Flood'
         ,'Lakeshore Flood'
         ,'Avalanche'
         ,'Seiche'
         ,'High Surf'
         ,'Hail'
         ,'Heavy Snow'
         ,'Sleet'
         ,'Heavy Snow'
         ,'Heavy Snow'
         ,'Heavy Snow'
         ,'Storm Surge/Tide'
         ,'Strong Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Coastal Flood'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Thunderstorm Wind'
         ,'Hurricane (Typhoon)'
         ,'Winter Weather'
         ,'Heat'
         ,'Winter Weather'
         ,'Flood'
         ,'Heat'
         ,'Strong Wind'
         ,'Tornado'
         ,'Wildfire'
         ,'High Wind'
         ,'High Surf'
         ,'Sleet'
         ,'Sleet'
         ,'Sleet')

Orig<-c('AGRICULTURAL FREEZE'
        ,'ASTRONOMICAL HIGH TIDE'
        ,'BLACK ICE'
        ,'BLOWING DUST'
        ,'blowing snow'
        ,'BRUSH FIRE'
        ,'COASTAL EROSION'
        ,'COASTAL FLOODING'
        ,'COASTAL FLOODING/EROSION'
        ,'COASTAL STORM'
        ,'COASTALSTORM'
        ,'Cold'
        ,'COLD'
        ,'COLD AND SNOW'
        ,'COLD WEATHER'
        ,'DAM BREAK'
        ,'DROWNING'
        ,'DRY MICROBURST'
        ,'EXCESSIVE SNOW'
        ,'EXTREME COLD'
        ,'EXTREME WINDCHILL'
        ,'FALLING SNOW/ICE'
        ,'FLASH FLOOD/FLOOD'
        ,'FLOOD/FLASH/FLOOD'
        ,'FOG'
        ,'FREEZE'
        ,'FREEZING DRIZZLE'
        ,'Freezing Drizzle'
        ,'Freezing drizzle'
        ,'FREEZING RAIN'
        ,'Freezing Rain'
        ,'FROST'
        ,'GLAZE'
        ,'Gradient wind'
        ,'gradient wind'
        ,'GRADIENT WIND'
        ,'GUSTY WIND'
        ,'GUSTY WIND/HAIL'
        ,'Gusty winds'
        ,'Gusty Winds'
        ,'GUSTY WINDS'
        ,'HARD FREEZE'
        ,'HAZARDOUS SURF'
        ,'HEAVY SEAS'
        ,'HEAVY SURF'
        ,'Heavy surf and wind'
        ,'HEAVY SURF/HIGH SURF'
        ,'HIGH SEAS'
        ,'HIGH SURF ADVISORY'
        ,'HIGH SWELLS'
        ,'HIGH WATER'
        ,'HIGH WIND (G40)'
        ,'HIGH WINDS'
        ,'HURRICANE'
        ,'HYPERTHERMIA/EXPOSURE'
        ,'Hypothermia/Exposure'
        ,'HYPOTHERMIA/EXPOSURE'
        ,'ICE ON ROAD'
        ,'ICE ROADS'
        ,'ICY ROADS'
        ,'LANDSLIDE'
        ,'LANDSLIDES'
        ,'LANDSPOUT'
        ,'LATE SEASON SNOW'
        ,'LIGHT FREEZING RAIN'
        ,'LIGHT SNOW'
        ,'Light Snow'
        ,'MARINE TSTM WIND'
        ,'MIXED PRECIP'
        ,'Mixed Precipitation'
        ,'MIXED PRECIPITATION'
        ,'Mudslide'
        ,'MUD SLIDE'
        ,'MUDSLIDE'
        ,'NON-SEVERE WIND DAMAGE'
        ,'NON TSTM WIND'
        ,'NON-TSTM WIND'
        ,'OTHER'
        ,'RAIN'
        ,'RAIN/SNOW'
        ,'RECORD HEAT'
        ,'RIP CURRENTS'
        ,'RIVER FLOOD'
        ,'RIVER FLOODING'
        ,'ROCK SLIDE'
        ,'ROGUE WAVE'
        ,'ROUGH SEAS'
        ,'SMALL HAIL'
        ,'SNOW'
        ,'SNOW AND ICE'
        ,'SNOW SQUALL'
        ,'Snow Squalls'
        ,'SNOW SQUALLS'
        ,'STORM SURGE'
        ,'STRONG WINDS'
        ,'THUNDERSTORM'
        ,'THUNDERSTORM WIND (G40)'
        ,'TIDAL FLOODING'
        ,'TSTM WIND 45'
        ,'TSTM WIND'
        ,'TSTM WIND 40'
        ,'TSTM WIND (41)'
        ,'TSTM WIND AND LIGHTNING'
        ,'TSTM WIND (G45)'
        ,'TSTM WIND (G40)'
        ,'TSTM WIND (G35)'
        ,'TSTM WIND G45'
        ,'TSTM WIND/HAIL'
        ,'TYPHOON'
        ,'UNSEASONABLY COLD'
        ,'UNSEASONABLY WARM'
        ,'UNSEASONAL RAIN'
        ,'URBAN/SML STREAM FLD'
        ,'WARM WEATHER'
        ,'WET MICROBURST'
        ,'WHIRLWIND'
        ,'WILD/FOREST FIRE'
        ,'WIND'
        ,'WIND AND WAVE'
        ,'WINTER WEATHER MIX'
        ,'WINTER WEATHER/MIX'
        ,'WINTRY MIX')

# bild a d.f of the mapping
EVTypeMap<-data.frame(Orig,Trans )
DMGPopulationPart$EVTYPE_Short<-toupper(str_replace_all(DMGPopulationPart$EVTYPE,"[[:punct:][:space:][:digit:]]",""))

# merge with the orignal look up
tmpPop<-merge(DMGPopulationPart,d,by.x ='EVTYPE_Short', by.y ='l2',all.x = T)
# merge the rest with the new mapping
tmpPop<-merge(tmpPop,EVTypeMap,by.x ='EVTYPE', by.y ='Orig',all.x = T)


DMGValuePart$EVTYPE_Short<-toupper(str_replace_all(DMGValuePart$EVTYPE,"[[:punct:][:space:][:digit:]]",""))
# merge with the orignal look up
tmpValue<-merge(DMGValuePart,d,by.x ='EVTYPE_Short', by.y ='l2',all.x = T)
# merge the rest with the new mapping
tmpValue<-merge(tmpValue,EVTypeMap,by.x ='EVTYPE', by.y ='Orig',all.x = T)

#Manuly mapping 5 rows that where not mapped(becasue of extra blnks/punct values)
tmpValue[c(1,3,4,12,121),]$Trans.x= c('High Surf','Thunderstorm Wind','Thunderstorm Wind','Coastal Flood','Thunderstorm Wind')


#now make one varibile for the mapped EVTYPE for the two DMG and population d.f
for ( i in seq_along(tmpValue$EVTYPE))
{
        if(is.na(tmpValue[i,]$Trans.x))
                tmpValue[i,"FinalEV"]<-tmpValue[i,]$Trans.y
        else
                tmpValue[i,"FinalEV"]<-tmpValue[i,]$Trans.x
}

for ( i in seq_along(tmpPop$EVTYPE))
{
        if(is.na(tmpPop[i,]$Trans.x))
                tmpPop[i,"FinalEV"]<-tmpPop[i,]$Trans.y
        else
                tmpPop[i,"FinalEV"]<-tmpPop[i,]$Trans.x
}

# sumrize by the new EVTYPE and order by the value
FinalPop<-arrange(summarise(group_by(tmpPop,FinalEV),totalDMGPopulation=sum(totalDMGPopulation)),desc(totalDMGPopulation))
FinalValue<-arrange(summarise(group_by(tmpValue,FinalEV),totalDMGValue=sum(totalDMGValue)),desc(totalDMGValue))
# see some stats on the values
summary(FinalPop$totalDMGPopulation)
summary(FinalValue$totalDMGValue)

# the to 10 EVTYPE causing FATALITIES and INJURIES
head(FinalPop,10)

# the to 10 EVTYPE causing the higst damage in $ value
head(FinalValue,10)
