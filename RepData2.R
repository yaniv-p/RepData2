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

# taking a look at the list if varibles names
names(StromData)

# Get info on varibales
str(StromData)

#convert the date string to a R date
StromData$BGN_DATE_D<-as.Date(StromData$BGN_DATE,"%m/%d/%Y")

