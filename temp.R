source('~/RepData2/RepData2.R')
install.packages(c("evaluate", "git2r", "knitr"))
install.packages('R.utils')
source('~/RepData2/RepData2.R')
source('~/RepData2/RepData2.R')
source('~/RepData2/RepData2.R')
str(StromData)
dim(StromData)
str(StromData)
as.date(head(StromData$BGN_DATE),"%m/%d/%Y")
as.Date(head(StromData$BGN_DATE),"%m/%d/%Y")
StromData$BGN_DATE_D<-as.Date(StromData$BGN_DATE,"%m/%d/%Y")
View(StromData)
object.size(StromData)
object.size(StromData)/1024
object.size(StromData)/1024/1024
object.size(StromData)/1024/1024/1024
str(StromData)
StromData$BGN_DATE_D > "1970-01-01"
sum(StromData$BGN_DATE_D > "1970-01-01")
sum(StromData$BGN_DATE_D > "1990-01-01")
summary(StromData$BGN_DATE_D)
hist(StromData$BGN_DATE_D)
hist(StromData$BGN_DATE_D,breaks = "year")
hist(StromData$BGN_DATE_D,breaks = "year",freq = TRUE)
hist(StromData$BGN_DATE_D,breaks = "year",freq = TRUE,plot = FALSE)
hist(StromData$BGN_DATE_D,breaks = "year",freq = TRUE,plot = TRUE)
hist(StromData$BGN_DATE_D,breaks = "year",freq = TRUE)
hist(StromData$BGN_DATE_D,freq = TRUE,breaks = "months")
hist(StromData$BGN_DATE_D,freq = TRUE,breaks = "quarters")
hist(StromData$BGN_DATE_D,freq = TRUE,breaks = "years")
StromData$BGN_DATE_D
ls
ls
StromData$BGN_DATE_D> "01/01/1970"
StromData$BGN_DATE_D> "01/01/1949"
StromData$BGN_DATE_D< "01/01/1949"
sum(StromData$BGN_DATE_D< "01/01/1949")
sum(StromData$BGN_DATE_D< "01/01/1950")
sum(StromData$BGN_DATE_D< "01/01/1960")
sum(StromData$BGN_DATE_D< "01/01/1970")
sum(StromData$BGN_DATE_D< "1/1/1970")
sum(StromData$BGN_DATE_D< to.Date ("1/1/1970","%m/%d/%Y")
)
sum(StromData$BGN_DATE_D< to.Date("1/1/1970","%m/%d/%Y"))
sum(StromData$BGN_DATE_D< as.Date("1/1/1970","%m/%d/%Y"))
sum(StromData$BGN_DATE_D< as.Date("1/1/1950","%m/%d/%Y"))
sum(StromData$BGN_DATE_D< as.Date("1/1/1960","%m/%d/%Y"))
sum(StromData$BGN_DATE_D< as.Date("1/1/1950","%m/%d/%Y"))
sum(StromData$BGN_DATE_D< as.Date("1/1/1955","%m/%d/%Y"))
sum(StromData$BGN_DATE_D< as.Date("1/1/1955","%m/%d/%Y"))
sum(StromData$BGN_DATE_D< as.Date("1/1/1925","%m/%d/%Y"))
summary(StromData$BGN_DATE_D)
sum(StromData$BGN_DATE_D< as.Date("1/1/1951","%m/%d/%Y"))
sum(StromData$BGN_DATE_D< as.Date("1/1/1950","%m/%d/%Y"))
sum(StromData$BGN_DATE_D< as.Date("1/1/1951","%m/%d/%Y"))
sum(StromData$BGN_DATE_D< as.Date("1/1/1952","%m/%d/%Y"))
table(StromData$BGN_DATE_D)
year(StromData$BGN_DATE_D)
format(StromData$BGN_DATE_D,"Y")
format(as.yearmon(StromData$BGN_DATE_D),"Y")
library(lubridate)
install.packages('lubridate')
library(lubridate)
year(StromData$BGN_DATE_D)
StromData$$year<-year(StromData$BGN_DATE_D)
StromData$year<-year(StromData$BGN_DATE_D)
hist(StromData$year,breaks = StromData$year,freq = TRUE)
hist(StromData$year,breaks = StromData$year)
hist(StromData$year)
plot(StromData$year,StromData$EVTYPE)
plot(StromData$year,StromData$EVTYPE)
