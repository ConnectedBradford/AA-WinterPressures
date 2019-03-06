
## TL - analyse SUS (v1) data to produce a model for admission frequency by time
## Target a virtual year
## it appears to me that the SUS data doesn't contain that many years...

# library(DBI)
# library(odbc)
library(dplyr)
library(rapportools)
library(purrrlyr)
library(digest)
#library(timeDate)
library(bizdays) ##this library is way quicker than timeDate (but we have to use timeDate's calendars for some reason)
max<-base::max
min<-base::min
#library(lubridate)
library(data.table)
# 
# ##Read in data from SQL - Must change username!##
# con <- dbConnect(odbc::odbc(), .connection_string="DRIVER=SQL Server;UID=lawtont;DATABASE=proj_TomLawton;WSID=5040-HW25RG2;APP=Microsoft Office 2010;Trusted_Connection=Yes;SERVER=bhts-conyprodwd;Description=bhts-conyprodwd")
# 
# print(dbListFields(con,"tbl_SUS_AdmittedPatientCare_Finished"))
# 
# result <- dbSendQuery(con,"SELECT * FROM [proj_TomLawton].[dbo].[tbl_SUS_AdmittedPatientCare_Finished] WHERE [Ward Code 1]!='' AND [Patient Classification]='1' AND ([Site Code (of Treatment) At Episode Start Date]='RAE01' OR [Site Code (of Treatment) at Episode End Date]='RAE01')")
# 
# data <- dbFetch(result)
data<-readRDS("../Data - Generated/SUSv1-basedata.rds")

print("* Loaded *")


##episodes<-mutate(data,bizday=isBizday(timeDate(`_Start_DateTime`),holidays=holidayLONDON(2000:2019))) - really slow
load_rmetrics_calendars(2000:2022) ##nb we only get these holidays so may need extending in futur
episodes<-filter(data,`_Elective`==FALSE)
#episodes<-data
episodes<-mutate(episodes,`_bizday`=is.bizday(`_Start_DateTime`,'Rmetrics/LONDON'),`_Start_Time`=(as.numeric(`_Start_DateTime`) %% 86400))
episodes<-mutate(episodes,`_2KMD_Date`=as.Date(format(`_Start_DateTime`,"2000-%m-%d")))




print("* Calculated Business Days (Patient table) *")

startDate <- as.POSIXct("2020-01-01 00:00:00 GMT")
endDate <- as.POSIXct("2020-12-31 23:59:59 GMT") ## one week for now
resolutionTime <- as.difftime(4,units="hours")
searchTimeWindow <- as.difftime(2,units="hours") ## two hours before and after
searchDateWindow <- as.difftime(1,units="weeks") ## 1 weeks before and after
dates <- seq.POSIXt(startDate,endDate,resolutionTime)
##bizDay <- isBizday(timeDate(dates),holidays=holidayLONDON(2000:2022)) ##nb year be in here
bizDay <- is.bizday(dates,'Rmetrics/LONDON')

#count <- filter(episodes,`_Start_DateTime`>(dates-searchDateWindow) & `_Start_DateTime`<(dates+searchDateWindow))

output <- data.table(`dateTime`=dates,`bizDay`=bizDay)
output <-mutate(output,`2KMD_Date`=as.Date(format(`dateTime`,"2000-%m-%d")))
#output <- mutate(output,`startDTSearch`=as.Date(format(`dateTime`-searchDateWindow,"2000-%m-%d")),`endDTSearch`=as.Date(format(`dateTime`+searchDateWindow,"2000-%m-%d")))
output <- mutate(output,`startDTSearch`=`2KMD_Date`-searchDateWindow,`endDTSearch`=`2KMD_Date`+searchDateWindow)
##remove modulo arithmetic, we'll do it manually later

output <- mutate(output,`startTime`=(as.numeric(dateTime) %% 86400))
output <- mutate(output,`startTSearch`=`startTime`-as.numeric(searchTimeWindow,units="secs"),`endTSearch`=`startTime`+as.numeric(searchTimeWindow,units="secs"))
#output <- mutate(output,`startTSearch`=`startTSearch`%%86400,`endTSearch`=`endTSearch`%%86400)
##remove modulo arithmetic, we'll do it manually later

output <- data.table(output)

print("* Started output table *")

episodes <- data.table(episodes)


#out1<-output[testepisodes,on=.(`startTSearch`<=`_Start_Time`,`endTSearch`>=`_Start_Time`),.N,by=.EACHI]
#,`_bizday`==`bizDay`



mainout<-episodes[output,on=.(`_bizday`=`bizDay`,`_Start_Time`>=`startTSearch`,`_Start_Time`<=`endTSearch`,`_2KMD_Date`>=`startDTSearch`,`_2KMD_Date`<=`endDTSearch`),j=.(.N,dateTime),by=.EACHI,allow.cartesian=TRUE]
##above doesn't wrap around the years

## add combinations of matching on _Start_Time+24hrs, -24hrs and date+1yr,-1yr  - ie modulo arithmetic
mainout$N<-0

for (toffset in c(-86400,0,86400)) {
  for (doffset in c(as.difftime(-365,units="days"),0,as.difftime(365,units="days"))) {
    tempout <- mutate(output,startTime=startTime+toffset,`2KMD_Date`=`2KMD_Date`+doffset) %>% 
      mutate(`startDTSearch`=`2KMD_Date`-searchDateWindow,`endDTSearch`=`2KMD_Date`+searchDateWindow) %>% 
      mutate(`startTSearch`=`startTime`-as.numeric(searchTimeWindow,units="secs"),`endTSearch`=`startTime`+as.numeric(searchTimeWindow,units="secs"))
    tmpcount<-episodes[tempout,on=.(`_bizday`=`bizDay`,`_Start_Time`>=`startTSearch`,`_Start_Time`<=`endTSearch`,`_2KMD_Date`>=`startDTSearch`,`_2KMD_Date`<=`endDTSearch`),j=.(.N,dateTime),by=.EACHI,allow.cartesian=TRUE]
    mainout$N<-mainout$N+tmpcount$N
  }
}


## counts need correcting by how often these things appear in the initial data - eg weekdays are more common, and time across years
## some dates are represented different numbers of times because the data isn't perfect years (see episodeEarliest,episodeLatest)
##then dividing by size of window


episodeEarliest<-min(episodes$`_Start_DateTime`)
episodeLatest<-max(episodes$`_Start_DateTime`)

##not sure this is a satisfactory way of working out denominators. Perhaps we should just use unique values to see if a day is represented?
##maybe only include it if there are more than N admissions so it's definitely part of the main dataset

#episodeBizdays<-bizdays(episodeEarliest,episodeLatest,'Rmetrics/LONDON')
#episodeTotaldays<-as.numeric(difftime(episodeLatest,episodeEarliest,units="days"))
#episodeEachDay<-seq(episodeEarliest,episodeLatest,by="days")

episodeAllDays<-data.frame(dat=as.Date(episodes$`_Start_DateTime`))

## only include days on which there were >4 admissions (might need changing)
episodeEachDay<-as.Date(names(which(table(episodeAllDays$dat)>20)))


episodeEachDay_2KMD<-as.Date(format(episodeEachDay,"2000-%m-%d"))
episodeDayCount<-data.frame(episodeEachDay,episodeEachDay_2KMD)
episodeDayCount<-mutate(episodeDayCount,`_bizday`=is.bizday(`episodeEachDay`,'Rmetrics/LONDON'))

episodeDayCount<-data.table(episodeDayCount)


denominatorout<-episodeDayCount[output,on=.(`_bizday`=`bizDay`,`episodeEachDay_2KMD`>=`startDTSearch`,`episodeEachDay_2KMD`<=`endDTSearch`),j=.(.N,dateTime),by=.EACHI,allow.cartesian=TRUE]
##above doesn't wrap around the years

## add combinations of matching on date+1yr,-1yr  - ie modulo arithmetic
denominatorout$N<-0

  for (doffset in c(as.difftime(-365,units="days"),0,as.difftime(365,units="days"))) {
    tempout <- mutate(output,`2KMD_Date`=`2KMD_Date`+doffset) %>% 
      mutate(`startDTSearch`=`2KMD_Date`-searchDateWindow,`endDTSearch`=`2KMD_Date`+searchDateWindow)
    tmpcount<-episodeDayCount[tempout,on=.(`_bizday`=`bizDay`,`episodeEachDay_2KMD`>=`startDTSearch`,`episodeEachDay_2KMD`<=`endDTSearch`),j=.(.N,dateTime),by=.EACHI,allow.cartesian=TRUE]
    
    denominatorout$N<-denominatorout$N+tmpcount$N
  }

mainout$`_denominatorDays`<-denominatorout$N
mainout<-as_tibble(mainout,.name_repair="unique")
##temp fix for the duplicate names caused by data.table's join
mainout<-mutate(mainout,`_correctedN`=N/`_denominatorDays`)


print(plot(mainout$dateTime,mainout$`_correctedN`,type="l"))

show_winter<-data.table(mainout)

show_winter<-show_winter[dateTime<as.POSIXct("2020-06-01"), dateTime := dateTime+365*24*3600]
                         

print(plot(show_winter$dateTime,show_winter$`_correctedN`,type="l"))

 print("* Finished *")