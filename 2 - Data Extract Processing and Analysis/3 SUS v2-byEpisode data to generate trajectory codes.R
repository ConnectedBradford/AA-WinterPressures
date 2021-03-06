
## TL - analyse processed SUS (v2) data to add trajectory data to each episode

##TODO - add critical care linking at bottom
  

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
data<-readRDS("../Data - Generated/SUSv2-byEpisode-basedata.rds")
spells<-data

episodes<-readRDS("../Data - Generated/SUSv2-byEpisode-repeateddata.rds")

print("* Loaded *")


source("../2 - Data Extract Processing and Analysis/SUS v2-byEpisode Filters.R")
#spells<-data
## bizarrely need to have as.character in there, or BST dates shortly after midnight get put back to the previous day
#spells<-mutate(spells,`_bizday`=is.bizday(as.character(`_SpellStart_DateTime`),'Rmetrics/LONDON'),`_Start_Time`=(as.numeric(`_SpellStart_DateTime`) %% 86400))
spells<-mutate(spells,`_2KMD_Date`=as.Date(format(`_SpellStart_DateTime`,"2000-%m-%d")))


combined<-left_join(spells,episodes,by="_TLSpellDigest")
combined<-dplyr::rename(combined,`Treatment Function Code`=`Treatment Function Code.y`)

print("* Combined tables *")


combined$age<-as.numeric(combined$`Start Date (Hospital Provider Spell)`)/10000-as.numeric(combined$`Year of Birth`)-0.5

combined$traj<-0


##single-sex wards will be dealt with in the simmer code itself rather than trying to do it here (and creating multiple trajectories)

## nb don't use double-character or/and here - only evaluates the first row (and applies to all!) - weird effects


## lots of code 180 = A&E. Could be anything! 190=anaesthesia probably represents ICU admissions

##Emergency 0 = generic
##Emergencies 1-99 = surgical
##Emergencies 100-199 = medical
##Emergencies 200-299 = paediatric

##Elective 1000 = generic
##Elective 


combined<-
  mutate(combined,traj = case_when(
    
    
    
    ## 1 - Emergency Gen Surgical
    `_Elective`==FALSE & (`Treatment Function Code`==100 | `Treatment Function Code`==104 | `Treatment Function Code`==102 | `Treatment Function Code`==106) ~ 1,
    
    ## 2 - Emergency Urology
    `_Elective`==FALSE & `Treatment Function Code`==101 ~ 2,
    
    ## 3 - Emergency Maxfax
    `_Elective`==FALSE & (`Treatment Function Code`==140 | `Treatment Function Code`==144 | `Treatment Function Code`==145 | `Treatment Function Code`==146) ~ 3,
    
    ## 4 - Emergency Plastics
    `_Elective`==FALSE & `Treatment Function Code`==160 ~ 4,
    
    ## 5 - Emergency ENT
    `_Elective`==FALSE & `Treatment Function Code`==120 ~ 5,
    
    ## 6 - Emergency Orthogeriatrics
    `_Elective`==FALSE & `Treatment Function Code`==110 & `age`>70 ~ 6,
    
    ## 7 - Emergency Ortho
    `_Elective`==FALSE & `Treatment Function Code`==110 ~ 7,
    
    ## 8 - Emergency Ophth
    `_Elective`==FALSE & `Treatment Function Code`==130 ~ 8,
    
    ## 9 - Emergency Vascular
    `_Elective`==FALSE & `Treatment Function Code`==107 ~ 9,
    
    ## 10 - Emergency Breast
    `_Elective`==FALSE & `Treatment Function Code`==103 ~ 10,
    
    ## 11 - Emergency Gynae
    ## nb a couple of obs patients sneak in as they're initially coded as gynae
    `_Elective`==FALSE & (`Treatment Function Code`==501 |`Treatment Function Code`==502|`Treatment Function Code`==503) ~ 11,
    
    
    
    ## 99 - Emergency, unknown surgical
    `_Elective`==FALSE & (`Treatment Function Code`==180 | `Treatment Function Code`==190) & `Primary Procedure (OPCS)`!="" ~ 99,
    
    
    
    
    ## 100 - Emergency General Medicine
    `_Elective`==FALSE & `Treatment Function Code`==300 ~ 100,
    
    ## 101 - Emergency Gastro/Hepatology
    `_Elective`==FALSE & (`Treatment Function Code`==301 | `Treatment Function Code`==306) ~ 101,
    
    ## 102 - Emergency Endocrine
    `_Elective`==FALSE & (`Treatment Function Code`==302 | `Treatment Function Code`==307)~ 102,
    
    ## 103 - Emergency Cardiology
    `_Elective`==FALSE & `Treatment Function Code`==320 ~ 103,
    
    ## 104 - Emergency Respiratory
    `_Elective`==FALSE & (`Treatment Function Code`==340 | `Treatment Function Code`==341)~ 104,
    
    ## 105 - Emergency ID
    `_Elective`==FALSE & `Treatment Function Code`==350 ~ 105,
    
    ## 106 - Emergency Haematology
    `_Elective`==FALSE & (`Treatment Function Code`==303 | `Treatment Function Code`==309) ~ 106,
    
    ## 107 - Emergency Stroke/Neuro
    `_Elective`==FALSE & (`Treatment Function Code`==328 | `Treatment Function Code`==329| `Treatment Function Code`==400) ~ 107,
    
    ## 108 - Emergency Renal
    `_Elective`==FALSE & (`Treatment Function Code`==361) ~ 108,
    
    ## 109 - Emergency Oncology
    `_Elective`==FALSE & (`Treatment Function Code`==370| `Treatment Function Code`==800) ~ 109,
    
    ## 110 - Emergency Geriatrics/Rehab
    `_Elective`==FALSE & (`Treatment Function Code`==430| `Treatment Function Code`==314) ~ 110,
    
    
    ## 199 - Emergency, unknown medical
    `_Elective`==FALSE & (`Treatment Function Code`==180 | `Treatment Function Code`==190) ~ 199,
    
    
    
    ## 200 - Emergency paediatric
    `_Elective`==FALSE & ((`Treatment Function Code`>=211 & `Treatment Function Code`<=291) | `Treatment Function Code`==171| `Treatment Function Code`==420| `Treatment Function Code`==321) ~ 200,
    
    
    ## 999 = Generic Emergency code
    `_Elective`==FALSE ~ 999,
    
    
    
    
    ## 1001 = Elective Gen Surgical
    `_Elective`==TRUE & (`Treatment Function Code`==100 | `Treatment Function Code`==104| `Treatment Function Code`==106) ~ 1001,
    
    ## 1002 = Elective Vascular
    `_Elective`==TRUE & (`Treatment Function Code`==107) ~ 1002,
    
    ## 1003 = Elective Ortho
    `_Elective`==TRUE & (`Treatment Function Code`==110) ~ 1003,
    
    ## 1004 = Elective ENT
    `_Elective`==TRUE & (`Treatment Function Code`==120) ~ 1004,

    ## 1005 = Elective MaxFax
    `_Elective`==TRUE & (`Treatment Function Code`==140|`Treatment Function Code`==144) ~ 1005,
    
    ## 1006 = Elective Plastics
    `_Elective`==TRUE & (`Treatment Function Code`==160) ~ 1006,
    
    ## 1007 = Elective Gynae
    `_Elective`==TRUE & (`Treatment Function Code`==502) ~ 1007,
    
    ## 1008 = Elective Urology
    `_Elective`==TRUE & (`Treatment Function Code`==101) ~ 1008,
    
    ## 1009 = Elective Breast
    `_Elective`==TRUE & (`Treatment Function Code`==103) ~ 1009,
    
    ## 1010 = Elective Ophth
    `_Elective`==TRUE & (`Treatment Function Code`==130) ~ 1010,
    
    ## 1011 = Elective Pain
    `_Elective`==TRUE & (`Treatment Function Code`==191) ~ 1011,
    
    
    
    
    ## 1100 = Elective Gen Medicine
    `_Elective`==TRUE & (`Treatment Function Code`==300) ~ 1100,
    
    ## 1101 = Elective Gastro/Hepatology
    `_Elective`==TRUE & (`Treatment Function Code`==301|`Treatment Function Code`==306) ~ 1101,
    
    ## 1102 = Elective Haem
    `_Elective`==TRUE & (`Treatment Function Code`==303|`Treatment Function Code`==309) ~ 1102,
    
    ## 1103 = Elective Cardiology
    `_Elective`==TRUE & (`Treatment Function Code`==320) ~ 1103,
    
    ## 1104 = Elective Respiratory
    `_Elective`==TRUE & (`Treatment Function Code`==340) ~ 1104,
    
    ## 1105 = Elective Oncology
    `_Elective`==TRUE & (`Treatment Function Code`==370 | `Treatment Function Code`==503 | `Treatment Function Code`==800) ~ 1105,
    
    ## 1106 = Elective Neurology
    `_Elective`==TRUE & (`Treatment Function Code`==400|`Treatment Function Code`==328|`Treatment Function Code`==329) ~ 1106,
    
    ## 1107 = Elective Rheumatology
    `_Elective`==TRUE & (`Treatment Function Code`==410) ~ 1107,
    
    ## 1108 = Elective Geriatrics
    `_Elective`==TRUE & (`Treatment Function Code`==430|`Treatment Function Code`==314) ~ 1108,
    
    ## 1109 = Elective Endocrine
    `_Elective`==TRUE & (`Treatment Function Code`==302) ~ 1109,
    
    ## 1110 = Elective ID
    `_Elective`==TRUE & (`Treatment Function Code`==350) ~ 1110,
    
    ## 1111 = Elective Renal
    `_Elective`==TRUE & (`Treatment Function Code`==361) ~ 1111,
    
    
    ## 1200 = Elective Paediatrics
    `_Elective`==TRUE & (`Treatment Function Code`==171|`Treatment Function Code`==321|`Treatment Function Code`==420|(`Treatment Function Code`>=211 & `Treatment Function Code`<=291)) ~ 1200,
    
    
    ## 1000 = Generic Elective code
    `_Elective`==TRUE ~ 1000,
    
  ))


##Second and subsequent episodes may need treating differently
## allow a "pre trajectory" which fits before

combined$pretraj<-NA

##10002=MAU
##10001=SAU
##10003=paeds
combined<-
  mutate(combined,pretraj = case_when(
    
    `Episode Number`==1 & (traj==1 | traj==2 | traj==9  | traj==11 | traj==99) ~ 10001,
    
    `Episode Number`==1 & (traj>=100 & traj <=199) ~ 10002,
    
    `Episode Number`==1 & traj==200 ~ 10003
    
  ))

print("* Added Trajectories *")

saveRDS(combined,"../Data - For Modelling/Both-Episodes.rds")

print("* Saved Trajectories *")

emergency_spells <- readRDS("../Data - For Modelling/Emergency-Spells.rds")
emergency_Spells_bak<-emergency_spells
elective_spells <- readRDS("../Data - For Modelling/Elective-Spells.rds")
icu_raw_segments<-readRDS("../Data - Generated/SUSv2-byEpisode-critcaredata.rds")



print("* Loaded Spells *")


icu_segments<-dplyr::select(icu_raw_segments,"_TLSpellDigest","Episode Number","Last Episode In Spell Indicator","ccstay","_ccseg","_SegmentStart_DateTime","_SegmentEnd_DateTime","_SegmentDischReady_DateTime","_SegmentStart_Offset","_SegmentEnd_Offset","_SegmentDischReady_Offset","_RealCritCare","_CCTransfer","Critical Care Level 2 Days","Critical Care Level 3 Days")

print("* ICU fixup *")


emergency_spells$ep1_row_id<-match(paste0(emergency_spells$`_SpellID`,"1"),paste0(combined$`_SpellID`,combined$`Episode Number`))
elective_spells$ep1_row_id<-match(paste0(elective_spells$`_SpellID`,"1"),paste0(combined$`_SpellID`,combined$`Episode Number`))

elective_spells$cc1_row_id<-match(paste0(elective_spells$`_TLSpellDigest`,"1"),paste0(icu_segments$`_TLSpellDigest`,icu_segments$`_ccseg`))
emergency_spells$cc1_row_id<-match(paste0(emergency_spells$`_TLSpellDigest`,"1"),paste0(icu_segments$`_TLSpellDigest`,icu_segments$`_ccseg`))


icu_segments$row_id<-1:nrow(icu_segments)

emerg_joined<-left_join(emergency_spells,icu_segments,by=c("cc1_row_id"="row_id"))

emerg_joined$cc_test<-abs(emerg_joined$`_SegmentStart_DateTime`-emerg_joined$`_SpellStart_DateTime`)

emergency_spells$cc_start<-ifelse(emerg_joined$cc_test<18*3600,1,0)
##if they come to ICU within 18hrs of arriving at the hospital, assume they need to be there at the start
##nb not true/false as we're using attributes in r-simmer which have to be numeric


elec_joined<-left_join(elective_spells,icu_segments,by=c("cc1_row_id"="row_id"))

elec_joined$cc_test<-abs(elec_joined$`_SegmentStart_DateTime`-elec_joined$`_SpellStart_DateTime`)

elective_spells$cc_start<-ifelse(elec_joined$cc_test<24*3600,1,0)

##if they come to ICU within 24hrs of arriving at the hospital, assume they need to be there at the start (nb - arrive 0700, could have a long case and not leave theatre until after midnight)
##or may even arrive previous day
##nb not true/false as we're using attributes in r-simmer which have to be numeric


elective_spells$cc_start[is.na(elective_spells$cc_start)]<-0
emergency_spells$cc_start[is.na(emergency_spells$cc_start)]<-0
elective_spells$cc1_row_id[is.na(elective_spells$cc1_row_id)]<-0
emergency_spells$cc1_row_id[is.na(emergency_spells$cc1_row_id)]<-0
# zero is our NA value

icu_segments$segN_row_id<-match(paste0(icu_segments$`_TLSpellDigest`,icu_segments$`_ccseg`+1),paste0(icu_segments$`_TLSpellDigest`,icu_segments$`_ccseg`))

combined$epN_row_id<-match(paste0(combined$`_SpellID`,combined$`Episode Number`+1),paste0(combined$`_SpellID`,combined$`Episode Number`))

print("* Linked-List Created *")

saveRDS(combined,"../Data - For Modelling/Both-Episodes-Linked.rds")
saveRDS(emergency_spells,"../Data - For Modelling/Emergency-Spells-Linked.rds")
saveRDS(elective_spells,"../Data - For Modelling/Elective-Spells-Linked.rds")
saveRDS(icu_segments,"../Data - For Modelling/CritCare-Segments-Linked.rds")

print("* Finished *")






