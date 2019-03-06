
## TL - transform SUS (v2) data into something we can model with - NB uses version 2
## version 2 - trying to do it with reshaping (works much faster!)
## 20190131 - doesn't manage to fuzzy match dates and times so critical care stays end up being duplicated

# library(DBI)
# library(odbc)
library(dplyr)
library(rapportools)
library(purrrlyr)
library(digest)
library(tidyr)
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
#data<-readRDS("../Data Extracts/SUSv2_all.rds")

print("* Loaded *")

data$`Episode Number`<-as.numeric(data$`Episode Number`)

#data<-rowwise(data) %>% 
#  mutate(`_TLSpellDigest`=digest(c(`DigestPseudID`,`Start Date (Hospital Provider Spell)`,`Start Time (Hospital Provider Spell)`)))
##temporary digest to allow matching - will need replacing with official grouping variable later "HOSPITAL PROVIDER SPELL NUMBER" or "ACTIVITY IDENTIFIER"

data$`_TLSpellDigest`=data$`Hospital Provider Spell Number`
## hopefully we can just use the official number now!
print("* Digested *")

firstepisodes<-filter(data,`Episode Number`=="1")
## give each spell a unique ID - hopefully not required once we have "HOSPITAL PROVIDER SPELL NUMBER" or "ACTIVITY IDENTIFIER"
firstepisodes$`_SpellID` <- seq.int(nrow(firstepisodes))


basedata<-select(firstepisodes,`_SpellID`,`_TLSpellDigest`,Pkid.x,`Start Date (Hospital Provider Spell)`,`Discharge Date (From Hospital Provider Spell)`,`Start Time (Hospital Provider Spell)`,`Discharge Time (Hospital Provider Spell)`,`Admission Method (Hospital Provider Spell)`,`Year of Birth`,`Discharge Method (Hospital Provider Spell)`,`Discharge Destination (Hospital Provider Spell)`,`Patient Classification`)
unique(basedata$`Admission Method (Hospital Provider Spell)`)
## sense check above
## elective if first character is a 1
basedata$`_Elective`<-(substr(basedata$`Admission Method (Hospital Provider Spell)`,1,1)=="1")

##times - nb strptime seems to pass BST and GMT appropriately - ie assumes all the times in the SUS data are in local format (BST or GMT depending on date)
basedata$`_Start_DateTime`<-as.POSIXct(strptime(paste(basedata$`Start Date (Hospital Provider Spell)`,basedata$`Start Time (Hospital Provider Spell)`,sep=" "),format="%Y%m%d %H:%M:%S"))
basedata$`_Discharge_DateTime`<-as.POSIXct(strptime(paste(basedata$`Discharge Date (From Hospital Provider Spell)`,basedata$`Discharge Time (Hospital Provider Spell)`,sep=" "),format="%Y%m%d %H:%M:%S"))


print("* Base table created *")

# repeateddata<-select(data,"DigestPseudID","Episode Number","Last Episode In Spell Indicator","Main Specialty Code","Treatment Function Code","Primary Procedure (OPCS)","Diagnosis Primary (ICD)","Primary Procedure Date (OPCS)",starts_with("Ward Code"),starts_with("Start Time"),starts_with("Start Date"),starts_with("End Time"),starts_with("End Date"))
# 
# gathered <- gather(repeateddata,var,value,ends_with("1"),ends_with("2"),ends_with("3"),ends_with("4"))
# 
# separated <- separate(gathered,var,c("var","stay"),sep="[ ](?=[^ ]+$)")
# ##regex to match last space of a string
# 
# arranged <- arrange(separated,DigestPseudID,`Episode Number`,`Start Date (Hospital Provider Spell)`,`Start Time (Hospital Provider Spell)`)
# 
# spreaded <- spread(arranged,var,value)
# filtered <- filter(spreaded,`Start Date`!="")
# 

## in a piped format:

repeateddata <- select(data,"Pkid.x","_TLSpellDigest","Episode Number","Last Episode In Spell Indicator","Main Specialty Code","Treatment Function Code","Primary Procedure (OPCS)","Diagnosis Primary (ICD)","Primary Procedure Date (OPCS)",starts_with("Ward Code"),starts_with("Start Time"),starts_with("Start Date"),starts_with("End Time"),starts_with("End Date")) %>% 
  gather(var,value,matches("\\d$")) %>% 
  ## all the ones that end in a digit
  separate(var,c("var","stay"),sep="[ ](?=[^ ]+$)") %>% 
  ## regex to match last space of a string
  arrange(DigestPseudID,`Episode Number`,`Start Date (Hospital Provider Spell)`,`Start Time (Hospital Provider Spell)`) %>% 
  spread(var,value) %>% 
  filter(`Start Date`!="")


## needs matching to basedata on "HOSPITAL PROVIDER SPELL NUMBER" or "ACTIVITY IDENTIFIER" or some kind of date/time matching

repeateddata$`_EpisodeStart_DateTime` <- as.POSIXct(strptime(paste0(repeateddata$`Start Date (Consultant Episode)`," ",repeateddata$`Start Time (Episode)`),format="%Y%m%d %H:%M:%S"))
repeateddata$`_EpisodeEnd_DateTime` <- as.POSIXct(strptime(paste0(repeateddata$`End Date (Consultant Episode)`," ",repeateddata$`End Time (Episode)`),format="%Y%m%d %H:%M:%S"))
repeateddata$`_SegmentStart_DateTime` <- as.POSIXct(strptime(paste0(repeateddata$`Start Date`," ",repeateddata$`Start Time (Ward Stay)`),format="%Y%m%d %H:%M:%S"))
repeateddata$`_SegmentEnd_DateTime` <- as.POSIXct(strptime(paste0(repeateddata$`End Date`," ",repeateddata$`End Time (Ward Stay)`),format="%Y%m%d %H:%M:%S"))

repeateddata$`_SegmentStart_DateTime` <- pmax(repeateddata$`_SegmentStart_DateTime`,repeateddata$`_EpisodeStart_DateTime`,na.rm=TRUE)
repeateddata$`_SegmentEnd_DateTime` <- pmin(repeateddata$`_SegmentEnd_DateTime`,repeateddata$`_EpisodeEnd_DateTime`,na.rm=TRUE)
##account for the fact that ward stays can stretch over episodes so we need whatever's shortest of episode or ward stay
## need pmax,pmin or we get the min/max of the entire dataset
## na.rm helps us fill in gaps by using the episode end datetime if there isn't a discharge time (eg if episode changes during a ward stay)

print("* Repeated table created *")

critcaredata <- filter(data,`Critical Care Start Date 1`!="") %>% 
  select("DigestPseudID","_TLSpellDigest","Episode Number","Last Episode In Spell Indicator","Start Date (Hospital Provider Spell)","Start Time (Hospital Provider Spell)","Start Date (Consultant Episode)","Start Time (Episode)","End Date (Consultant Episode)","End Time (Episode)","Main Specialty Code","Treatment Function Code","Primary Procedure (OPCS)","Diagnosis Primary (ICD)","Primary Procedure Date (OPCS)",starts_with("Critical Care") ) %>% 
  gather(var,value,matches("\\d$")) %>%
  ## all the ones that end in a digit
  separate(var,c("var","ccstay"),sep="[ ](?=[^ ]+$)") %>% 
  ## regex to match last space of a string
  arrange(DigestPseudID,`Episode Number`,`Start Date (Hospital Provider Spell)`,`Start Time (Hospital Provider Spell)`) %>% 
  spread(var,value) %>% 
  filter(`Critical Care Start Date`!="")

critcaredata$`_SegmentStart_DateTime` <- as.POSIXct(strptime(paste0(critcaredata$`Critical Care Start Date`," ",critcaredata$`Critical Care Start Time`),format="%Y%m%d %H:%M:%S"))
critcaredata$`_SegmentEnd_DateTime` <- as.POSIXct(strptime(paste0(critcaredata$`Critical Care Discharge Date`," ",critcaredata$`Critical Care Discharge Time`),format="%Y%m%d %H:%M:%S"))
critcaredata$`_EpisodeStart_DateTime` <- as.POSIXct(strptime(paste0(critcaredata$`Start Date (Consultant Episode)`," ",critcaredata$`Start Time (Episode)`),format="%Y%m%d %H:%M:%S"))
critcaredata$`_EpisodeEnd_DateTime` <- as.POSIXct(strptime(paste0(critcaredata$`End Date (Consultant Episode)`," ",critcaredata$`End Time (Episode)`),format="%Y%m%d %H:%M:%S"))

critcaredata$`_SegmentStart_DateTime` <- pmax(critcaredata$`_SegmentStart_DateTime`,critcaredata$`_EpisodeStart_DateTime`,na.rm=TRUE)
critcaredata$`_SegmentEnd_DateTime` <- pmin(critcaredata$`_SegmentEnd_DateTime`,critcaredata$`_EpisodeEnd_DateTime`,na.rm=TRUE)
## need pmax,pmin or we get the min/max of the entire dataset
## na.rm helps us fill in gaps by using the episode end datetime if there isn't a discharge time (eg if episode changes during a critical care stay)

print("* Crit care table created *")

## needs matching to basedata on "HOSPITAL PROVIDER SPELL NUMBER" or "ACTIVITY IDENTIFIER" or some kind of date/time matching


## We need to get fuzzy to join the critical care episodes in; I can't think of a good way to do it other than iterating over the critical care episodes unfortunately
## This will be slow, but at least there's not many of them

##Iterate over spells:
ccspells <- unique(critcaredata$`_TLSpellDigest`)

for (ccspell in ccspells) {
  # is there a row with an overlap - pick the best one
  # if no overlap create a new row - actually is this necessary? May be that SUS data is just missing ward groups
  # can we assume that there will always be a ward stay if a critical care stay doesn't finish things?
  
  ## have a look at ccspell "5e7b8b72480f86e27c02acd5706eb2d1" who has lots of ICU stays
 ccstays<-filter(critcaredata,`_TLSpellDigest`==ccspell)
 wardstays<-filter(repeateddata,`_TLSpellDigest`==ccspell)
 for (i_cc in 1:nrow(ccstays)) {
   for (i_wd in 1:nrow(wardstays)) {
     diffStart<-as.numeric(difftime(ccstays[i_cc,]$`_SegmentStart_DateTime`,wardstays[i_wd,]$`_SegmentStart_DateTime`,units="hours"))
     diffEnd<-as.numeric(difftime(ccstays[i_cc,]$`_SegmentEnd_DateTime`,wardstays[i_wd,]$`_SegmentEnd_DateTime`,units="hours"))
     ccDuration<-as.numeric(difftime(ccstays[i_cc,]$`_SegmentEnd_DateTime`,ccstays[i_cc,]$`_SegmentStart_DateTime`,units="hours"))
     wardDuration<-as.numeric(difftime(wardstays[i_wd,]$`_SegmentEnd_DateTime`,wardstays[i_wd,]$`_SegmentStart_DateTime`,units="hours"))

     overlap<-max(0,max(ccDuration,wardDuration)-abs(diffStart)-abs(diffEnd))
     print(c(diffStart,diffEnd,ccDuration,wardDuration,overlap,overlap/min(ccDuration,wardDuration)))
     ##overlap quotient should be pretty good for the ICU, not so good for PCU as they always put 00:00 and 23:59
   }
   print("   ")
 }
  
}

print("* Crit care loop finished NOTIMPLEMENTEDYET *")

saveRDS(basedata,file="../Data - Generated/SUSv2-basedata.rds")
saveRDS(repeateddata,file="../Data - Generated/SUSv2-repeateddata.rds")
saveRDS(critcaredata,file="../Data - Generated/SUSv2-critcaredata.rds")
## save other bits?

print("* Finished *")

