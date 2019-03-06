
## TL - horrendous version which manually loops over everything to transform SUS data (v1) into something we can use
## does eventually complete, but has issues with the mismatched critical care times (ie ward transfer and critical care episode times aren't exactly the same)

# library(DBI)
# library(odbc)
library(dplyr)
library(rapportools)
library(purrrlyr)
max<-base::max
min<-base::min
#library(lubridate)
# 
# ##Read in data from SQL - Must change username!##
# con <- dbConnect(odbc::odbc(), .connection_string="DRIVER=SQL Server;UID=lawtont;DATABASE=proj_TomLawton;WSID=5040-HW25RG2;APP=Microsoft Office 2010;Trusted_Connection=Yes;SERVER=bhts-conyprodwd;Description=bhts-conyprodwd")
# 
# print(dbListFields(con,"tbl_SUS_AdmittedPatientCare_Finished"))
# 
# result <- dbSendQuery(con,"SELECT * FROM [proj_TomLawton].[dbo].[tbl_SUS_AdmittedPatientCare_Finished] WHERE [Ward Code 1]!='' AND [Patient Classification]='1' AND ([Site Code (of Treatment) At Episode Start Date]='RAE01' OR [Site Code (of Treatment) at Episode End Date]='RAE01')")
# 
# data <- dbFetch(result)
#data<-readRDS("SUSdataRAW.rds")

data$`Episode Number`<-as.numeric(data$`Episode Number`)

firstepisodes<-filter(data,`Episode Number`=="1")
## give each spell a unique ID
firstepisodes$`_ID` <- seq.int(nrow(firstepisodes))

basedata<-select(firstepisodes,`_ID`,DigestPseudID,`Start Date (Hospital Provider Spell)`,`Discharge Date (From Hospital Provider Spell)`,`Start Time (Hospital Provider Spell)`,`Discharge Time (Hospital Provider Spell)`,`Admission Method (Hospital Provider Spell)`,`Year of Birth`,`Discharge Method (Hospital Provider Spell)`,`Discharge Destination (Hospital Provider Spell)`,`Patient Classification`)
unique(basedata$`Admission Method (Hospital Provider Spell)`)
## sense check above
## elective if first character is a 1
basedata$`_Elective`<-(substr(basedata$`Admission Method (Hospital Provider Spell)`,1,1)=="1")

##times - nb strptime seems to pass BST and GMT appropriately - ie assumes all the times in the SUS data are in local format (BST or GMT depending on date)
basedata$`_Start_DateTime`<-as.POSIXct(strptime(paste(basedata$`Start Date (Hospital Provider Spell)`,basedata$`Start Time (Hospital Provider Spell)`,sep=" "),format="%Y%m%d %H:%M:%S"))
basedata$`_Discharge_DateTime`<-as.POSIXct(strptime(paste(basedata$`Discharge Date (From Hospital Provider Spell)`,basedata$`Discharge Time (Hospital Provider Spell)`,sep=" "),format="%Y%m%d %H:%M:%S"))

##now we need to start looping

repeateddata <- data.frame()

generate_repeated_groups <- function(baserow) {
  #tempa <<-baserow
  spelldata <- filter(data,DigestPseudID==baserow$DigestPseudID & `Start Date (Hospital Provider Spell)`==baserow$`Start Date (Hospital Provider Spell)` & `Start Time (Hospital Provider Spell)`==baserow$`Start Time (Hospital Provider Spell)`)
  ## nb episodes may just be separate based on consultant/team changes without ward changes
  ## in that case, the ward dates might cover the whole ward stay (ie exceed the limits of the episode)
  
  index<-1
  ## index = output rows
  episode<-1
  ## episode = input rows
  start_datetime<-baserow$`_Start_DateTime`
  key<-baserow$`_ID`
  
  repeat{
    wardstay<-1
    critcare<-1
    episodedata<-head(filter(spelldata,`Episode Number`==episode),1)
    ##above because there appear to be some repeated rows
    currentLocation<-episodedata$`Ward Code at Episode Start Date`
    specialtyCode<-episodedata$`Main Specialty Code`
    treatmentCode<-episodedata$`Treatment Function Code`
    procedurePrimaryOPCS<-episodedata$`Primary Procedure (OPCS)`
    diagnosisPrimaryICD<-episodedata$`Diagnosis Primary (ICD)`
    procedurePrimaryDate<-episodedata$`Primary Procedure Date (OPCS)`
    procedurePrimaryDate<-strptime(procedurePrimaryDate,format="%Y%m%d")
    episodeStartDate<-episodedata$`Start Date (Consultant Episode)`
    episodeStartTime<-episodedata$`Start Time (Episode)`
    episodeEndDate<-episodedata$`End Date (Consultant Episode)`
    episodeEndTime<-episodedata$`End Time (Episode)`
    episodeStartDateTime<-strptime(paste0(episodeStartDate," ",episodeStartTime),format="%Y%m%d %H:%M:%S")
    episodeEndDateTime<-strptime(paste0(episodeEndDate," ",episodeEndTime),format="%Y%m%d %H:%M:%S")
    
    repeat{
      wardstayStartDate<-episodedata[[paste0("Start Date ",wardstay)]]
      wardstayEndDate<-episodedata[[paste0("End Date ",wardstay)]]
      wardstayStartTime<-episodedata[[paste0("Start Time (Ward Stay) ",wardstay)]]
      wardstayEndTime<-episodedata[[paste0("End Time (Ward Stay) ",wardstay)]]
      
      wardstayStartDatetime<-strptime(paste0(wardstayStartDate," ",wardstayStartTime),format="%Y%m%d %H:%M:%S")
      wardstayEndDatetime<-strptime(paste0(wardstayEndDate," ",wardstayEndTime),format="%Y%m%d %H:%M:%S")
      
      wardstayCode<-episodedata[[paste0("Ward Code ",wardstay)]]

      
      critcareStartDate<-episodedata[[paste0("Critical Care Start Date ",critcare)]]
      critcareDischargeDate<-episodedata[[paste0("Critical Care Discharge Date ",critcare)]]
      critcareDischargeReadyDate<-episodedata[[paste0("Critical Care Discharge Ready Date ",critcare)]]
      critcareStartTime<-episodedata[[paste0("Critical Care Start Time ",critcare)]]
      critcareDischargeTime<-episodedata[[paste0("Critical Care Discharge Time ",critcare)]]
      critcareDischargeReadyTime<-episodedata[[paste0("Critical Care Discharge Ready Time ",critcare)]]
      
      critcareStartDatetime<-strptime(paste0(critcareStartDate," ",critcareStartTime),format="%Y%m%d %H:%M:%S")
      critcareDischargeDatetime<-strptime(paste0(critcareDischargeDate," ",critcareDischargeTime),format="%Y%m%d %H:%M:%S")
      critcareDischargeReadyDatetime<-strptime(paste0(critcareDischargeReadyDate," ",critcareDischargeReadyTime),format="%Y%m%d %H:%M:%S")
      
      critcareL3Days<-as.numeric(episodedata[[paste0("Critical Care Level 3 Days ",critcare)]])
      critcareL2Days<-as.numeric(episodedata[[paste0("Critical Care Level 2 Days ",critcare)]])
      
      
      if (is.empty(critcareStartDate) & is.empty(wardstayStartDate)) {break}
      #quits this inner loop once we've run out of data. Need to increment wardstay or critcare on each loop (or both)
      
      
      
      tempframe <- data.frame(index=integer(),
                              critcare=logical(),
                              critcareL3Days=integer(),
                              critcareL2Days=integer(),
                              startDatetime=as.POSIXct(character()),
                              endDatetime=as.POSIXct(character()),
                              location=character(),
                              specialtyCode=character(),
                              treatmentCode=character(),
                              diagnosisPrimaryICD=character(),
                              procedurePrimaryDate=as.POSIXct(character()),
                              procedurePrimaryOPCS=character(),
                              critcareDischargeReadyDatetime=as.POSIXct(character()))
      ##creates a single row with nothing set
      tempframe[1,]<-NA
      tempframe$`_ID`<-key
      tempframe$index<-index
      tempframe$specialtyCode<-specialtyCode
      tempframe$treatmentCode<-treatmentCode
      tempframe$diagnosisPrimaryICD<-diagnosisPrimaryICD
      tempframe$procedurePrimaryDate<-as.POSIXct(procedurePrimaryDate)
      tempframe$procedurePrimaryOPCS<-procedurePrimaryOPCS
      
      ## if strptime can't do anything it returns NA
      
      if (is.na(wardstayStartDatetime)) {wardstayStartDatetime<-as.Date("2500-01-01")}
      if (is.na(critcareStartDatetime)) {critcareStartDatetime<-as.Date("2600-01-01")}
      #dummy dates to be far off in the future
      
      if (is.na(critcareStartDatetime) | (wardstayStartDatetime<critcareStartDatetime)) {
        if (!is.empty(wardstayCode)) {currentLocation<-wardstayCode}
        ##copy ward details
        tempframe$critcare<-FALSE
        tempframe$startDatetime<-as.POSIXct(max(wardstayStartDatetime,episodeStartDateTime))
        tempframe$endDatetime<-as.POSIXct(min(wardstayEndDatetime,episodeEndDateTime))
        tempframe$location<-currentLocation
        wardstay <- wardstay + 1
        index<-index+1
        
      } else if (is.na(wardstayStartDatetime) | (wardstayStartDatetime>critcareStartDatetime)) {
        ##copy crit care details
        tempframe$critcare<-TRUE
        tempframe$critcareL2Days<-critcareL2Days
        tempframe$critcareL3Days<-critcareL3Days
        tempframe$startDatetime<-as.POSIXct(max(episodeStartDateTime,critcareStartDatetime))
        tempframe$endDatetime<-as.POSIXct(min(episodeEndDateTime,critcareDischargeDatetime))
        tempframe$critcareDischargeReadyDatetime<-as.POSIXct(critcareDischargeReadyDatetime)
        tempframe$location<-currentLocation
        critcare <- critcare + 1
        index<-index+1
      } else {
        if (!is.empty(wardstayCode)) {currentLocation<-wardstayCode}
        ##assume they're equal
        ##might be worth rolling this into the above
        tempframe$critcare<-TRUE
        tempframe$critcareL2Days<-critcareL2Days
        tempframe$critcareL3Days<-critcareL3Days
        tempframe$startDatetime<-as.POSIXct(max(wardstayStartDatetime,episodeStartDateTime,critcareStartDatetime))
        tempframe$endDatetime<-as.POSIXct(min(wardstayEndDatetime,episodeEndDateTime,critcareDischargeDatetime))
        tempframe$critcareDischargeReadyDatetime<-as.POSIXct(critcareDischargeReadyDatetime)
        wardstay <- wardstay + 1
        critcare <- critcare + 1
        index<-index+1
      }
      
      ##add data frame
      
      repeateddata <<- rbind(repeateddata,tempframe)

      
      
      
    }
    
    ##finish if this is the last episode, or we've somehow run out
    episode<-episode+1
    
    if (nrow(episodedata)==0) {break}
    if (episodedata$`Last Episode In Spell Indicator`=="1") {break}
  }
  
}


#generate_repeated_groups(sample_n(basedata,1))
#adply(basedata,1,generate_repeated_groups)
by_row(basedata,generate_repeated_groups)

