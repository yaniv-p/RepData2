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
StromData<-read.csv('data\\repdata-data-StormData.csv')
