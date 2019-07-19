
## TL - transform SUS (v2) data into something we can model with - NB uses version 2
## version 3 - trying to do it with reshaping (works much faster!)
## New plan - because ward data is only in SUS up to the 4th ward stay
##  (and we were going to have to reconstruct the "correct" wards anyway)
##  therefore - basedata will contain each spell
##  repeateddata will contain each *episode* (not ward stay)
##  critical care data as before.


#invisible(utils::memory.limit(32000))
## will force swap file usage (very slow!)
#commented out now I have 48Gb available!


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
data<-readRDS("../Data Extracts/SUSv2_all.rds")


## to remove blank columns - test2<-test[!sapply(test,function(x) all(format(x)==""))]

print("* Loaded *")

data$`Episode Number`<-as.numeric(data$`Episode Number`)


## There are duplicates in the SUSv2 data
## to bring out - dataTEST<-firstepisodes %>% group_by(`_TLSpellDigest`) %>% filter(dplyr::n()>1)
## mostly seems to be babies with weights being added later
## all are single episodes only


data<-dplyr::distinct(data,`Hospital Provider Spell Number`,`Episode Number`, .keep_all=TRUE)


#data<-rowwise(data) %>% 
#  mutate(`_TLSpellDigest`=digest(c(`DigestPseudID`,`Start Date (Hospital Provider Spell)`,`Start Time (Hospital Provider Spell)`)))
##temporary digest to allow matching - will need replacing with official grouping variable later "HOSPITAL PROVIDER SPELL NUMBER" or "ACTIVITY IDENTIFIER"

data$`_TLSpellDigest`=data$`Hospital Provider Spell Number`
## hopefully we can just use the official number now!
print("* De-duplicated and digested *")


##times - nb strptime seems to pass BST and GMT appropriately - ie assumes all the times in the SUS data are in local format (BST or GMT depending on date)
data$`_SpellStart_DateTime`<-as.POSIXct(strptime(paste(data$`Start Date (Hospital Provider Spell)`,data$`Start Time (Hospital Provider Spell)`,sep=" "),format="%Y%m%d %H:%M:%S"))
data$`_Discharge_DateTime`<-as.POSIXct(strptime(paste(data$`Discharge Date (From Hospital Provider Spell)`,data$`Discharge Time (Hospital Provider Spell)`,sep=" "),format="%Y%m%d %H:%M:%S"))


## generates some missing data on discharge - several which have impossible discharge times (ie during the "missing hour" on DST change), others have a time but no date
## presently not dealt with!


firstepisodes<-filter(data,`Episode Number`=="1")
## give each spell a unique ID - hopefully not required once we have "HOSPITAL PROVIDER SPELL NUMBER" or "ACTIVITY IDENTIFIER"
firstepisodes$`_SpellID` <- seq.int(nrow(firstepisodes))


basedata<-dplyr::select(firstepisodes,`_SpellID`,`_TLSpellDigest`,Pkid.x,Sex,`Start Date (Hospital Provider Spell)`,`Discharge Date (From Hospital Provider Spell)`,`Start Time (Hospital Provider Spell)`,`Discharge Time (Hospital Provider Spell)`,`Admission Method (Hospital Provider Spell)`,`Year of Birth`,`Discharge Method (Hospital Provider Spell)`,`Discharge Destination (Hospital Provider Spell)`,`Patient Classification`,`_SpellStart_DateTime`,`_Discharge_DateTime`,`Main Specialty Code`,`Treatment Function Code`)
print(unique(basedata$`Admission Method (Hospital Provider Spell)`))
## sense check above
## elective if first character is a 1
## may need to change this as codes 11,12 are being used in a funny way - possibly consider them non-elective (they're time-based anyway)
basedata$`_Elective`<-(substr(basedata$`Admission Method (Hospital Provider Spell)`,1,1)=="1")


print("* Base table created *")



## big change here from TL3 - repeated data is now EPISODE based rather than WARD STAY based. Segments


repeateddata <- dplyr::select(data,"Pkid.x","_TLSpellDigest","Episode Number","Last Episode In Spell Indicator","Main Specialty Code","Treatment Function Code","Primary Procedure (OPCS)","Diagnosis Primary (ICD)","Primary Procedure Date (OPCS)","_SpellStart_DateTime","_Discharge_DateTime","Start Date (Consultant Episode)","Start Time (Episode)","End Date (Consultant Episode)","End Time (Episode)")



## needs matching to basedata on "HOSPITAL PROVIDER SPELL NUMBER" or "ACTIVITY IDENTIFIER" or some kind of date/time matching

repeateddata$`_EpisodeStart_DateTime` <- as.POSIXct(strptime(paste0(repeateddata$`Start Date (Consultant Episode)`," ",repeateddata$`Start Time (Episode)`),format="%Y%m%d %H:%M:%S"))
repeateddata$`_EpisodeEnd_DateTime` <- as.POSIXct(strptime(paste0(repeateddata$`End Date (Consultant Episode)`," ",repeateddata$`End Time (Episode)`),format="%Y%m%d %H:%M:%S"))

#repeateddata$`_EpisodeStart_Offset` <- repeateddata$`_EpisodeStart_DateTime` - repeateddata$`_SpellStart_DateTime`
repeateddata$`_EpisodeStart_Offset` <- difftime(repeateddata$`_EpisodeStart_DateTime`,repeateddata$`_SpellStart_DateTime`,units="secs")

#repeateddata$`_EpisodeEnd_Offset` <- repeateddata$`_EpisodeEnd_DateTime` - repeateddata$`_SpellStart_DateTime`
repeateddata$`_EpisodeEnd_Offset` <- difftime(repeateddata$`_EpisodeEnd_DateTime`,repeateddata$`_SpellStart_DateTime`,units="secs")

##account for the fact that ward stays can stretch over episodes so we need whatever's shortest of episode or ward stay
## need pmax,pmin or we get the min/max of the entire dataset
## na.rm helps us fill in gaps by using the episode end datetime if there isn't a discharge time (eg if episode changes during a ward stay)

print("* Repeated table created *")

critcaredata <- filter(data,`Critical Care Start Date 1`!="") %>% 
  dplyr::select("Pkid.x","_TLSpellDigest","Episode Number","Last Episode In Spell Indicator","Start Date (Hospital Provider Spell)","Start Time (Hospital Provider Spell)","Start Date (Consultant Episode)","Start Time (Episode)","End Date (Consultant Episode)","End Time (Episode)","Main Specialty Code","Treatment Function Code","Primary Procedure (OPCS)","Diagnosis Primary (ICD)","Primary Procedure Date (OPCS)","_SpellStart_DateTime","_Discharge_DateTime",starts_with("Critical Care") ) %>% 
  gather(var,value,matches("\\d$")) %>%
  ## all the ones that end in a digit
  separate(var,c("var","ccstay"),sep="[ ](?=[^ ]+$)") %>% 
  ## regex to match last space of a string
  arrange(Pkid.x,`Episode Number`,`_SpellStart_DateTime`) %>% 
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
## helps fix some of the ward21 timing issues too by constraining the segment start and end times



## work out when they're fit for discharge - use discharge ready time, or 
critcaredata$`_SegmentDischReady_DateTime`<-as.POSIXct(strptime(paste0(critcaredata$`Critical Care Discharge Ready Date`," ",critcaredata$`Critical Care Discharge Ready Time`),format="%Y%m%d %H:%M:%S"))
critcaredata$`_SegmentDischReady_DateTime2`<-pmin(as.POSIXct(strptime(paste0(as.Date(as.character(critcaredata$`_SegmentEnd_DateTime`))," ","08:00"),format="%Y-%m-%d %H:%M")),critcaredata$`_SegmentEnd_DateTime`)
##nb as.character above because of the issue with dates shortly after midnight being put back to previous day
critcaredata$`_SegmentDischReady_DateTime2`<-pmax(critcaredata$`_SegmentEnd_DateTime`,critcaredata$`_SegmentDischReady_DateTime2`)
##estimate discharge times, but they can't be before arrival or after leaving!
critcaredata$`_SegmentDischReady_DateTime`<- as.POSIXct(ifelse(is.na(critcaredata$`_SegmentDischReady_DateTime`),critcaredata$`_SegmentDischReady_DateTime2`,critcaredata$`_SegmentDischReady_DateTime`),origin="1970-01-01 00:00:00")

critcaredata$`_SegmentDischReady_Offset`<-difftime(critcaredata$`_SegmentDischReady_DateTime`,critcaredata$`_SpellStart_DateTime`,units="secs")
critcaredata$`_SegmentEnd_Offset`<-difftime(critcaredata$`_SegmentEnd_DateTime`,critcaredata$`_SpellStart_DateTime`,units="secs")
critcaredata$`_SegmentStart_Offset`<-difftime(critcaredata$`_SegmentStart_DateTime`,critcaredata$`_SpellStart_DateTime`,units="secs")



##set ward 21 flag - times are 00:00-23:59, or local identifier not 8 digits starting with 4 digits of year (ie not 'proper' critical care unit)

critcaredata$`_RealCritCare` <- nchar(critcaredata$`Critical Care Local Identifier`)>7
##20190307 - 2748 real crit care admissions out of 4615

critcaredata$ccstay<-as.numeric(critcaredata$ccstay)

print("* Crit care table created *")

critcaredata$`_CCTransfer`<-0
critcaredata$`_ccseg`<-0
## transfer = we move directly between critical care beds rather than having a ward bed in the meantime (used in model to determine movements as they may not occur at the real times)

##iterate over critical care stay, if overlap between start and discharge times of adjacent segments, set both to be the one that's not 23:59 or 00:00 (ie ward 21)


##Iterate over spells:
ccspells <- unique(critcaredata$`_TLSpellDigest`)

for (ccspell in ccspells) {
  ccstays<-filter(critcaredata,`_TLSpellDigest`==ccspell) %>% arrange(`Episode Number`,ccstay)
  for (i_cc in 1:nrow(ccstays)) {
    ## nb multiple ccstays possible per episode
    curseg_ccstay<-ccstays[i_cc,]$ccstay
    curseg_episode<-ccstays[i_cc,]$`Episode Number`
    critcaredata$`_ccseg`[(critcaredata$`_TLSpellDigest`==ccspell)&(critcaredata$ccstay==curseg_ccstay)&(critcaredata$`Episode Number`==curseg_episode)]<-i_cc
    if (i_cc>1) {
      if (prevseg_end>=ccstays[i_cc,]$`_SegmentStart_DateTime`) {
        critcaredata$`_CCTransfer`[(critcaredata$`_TLSpellDigest`==ccspell)&(critcaredata$ccstay==prevseg_ccstay)&(critcaredata$`Episode Number`==prevseg_episode)]<-1
        ## one of them needs changing, pick the one that's not a real critical care episode
        if (prevseg_real) {
          ## correct this segment by setting start datetime to prevseg_end
          critcaredata$`_SegmentStart_DateTime`[(critcaredata$`_TLSpellDigest`==ccspell)&(critcaredata$ccstay==curseg_ccstay)&(critcaredata$`Episode Number`==curseg_episode)]<-prevseg_end
        } else {
          ## correct previous segment by setting end datetime to _segmentstart_datetime
          critcaredata$`_SegmentEnd_DateTime`[(critcaredata$`_TLSpellDigest`==ccspell)&(critcaredata$ccstay==prevseg_ccstay)&(critcaredata$`Episode Number`==prevseg_episode)]<-ccstays[i_cc,]$`_SegmentStart_DateTime`
        }
      }
    }
    
    prevseg_end<-ccstays[i_cc,]$`_SegmentEnd_DateTime`
    prevseg_real<-ccstays[i_cc,]$`_RealCritCare`
    prevseg_ccstay<-ccstays[i_cc,]$ccstay
    prevseg_episode<-ccstays[i_cc,]$`Episode Number`
    
    #      diffStart<-as.numeric(difftime(ccstays[i_cc,]$`_SegmentStart_DateTime`,wardstays[i_wd,]$`_SegmentStart_DateTime`,units="hours"))
    #      diffEnd<-as.numeric(difftime(ccstays[i_cc,]$`_SegmentEnd_DateTime`,wardstays[i_wd,]$`_SegmentEnd_DateTime`,units="hours"))
    
    
  }
}
 
 print("* Crit care loop finished *")
 
 saveRDS(basedata,file="../Data - Generated/SUSv2-byEpisode-basedata.rds")
 saveRDS(repeateddata,file="../Data - Generated/SUSv2-byEpisode-repeateddata.rds")
 saveRDS(critcaredata,file="../Data - Generated/SUSv2-byEpisode-critcaredata.rds")
## save other bits?

print("* Finished *")

