
## Generates modelling information from the EPR ward stay data (NOT SUS)
## obviously no critical care data etc

## For validation - ie remove patients who aren't in the model - eg day cases

# library(DBI)
# library(odbc)
library(dplyr)
library(rapportools)
library(purrrlyr)
library(digest)
library(tidyr)
max<-base::max
min<-base::min
select<-dplyr::select
library(lubridate)
# 
# ##Read in data from SQL - Must change username!##
# con <- dbConnect(odbc::odbc(), .connection_string="DRIVER=SQL Server;UID=lawtont;DATABASE=proj_TomLawton;WSID=5040-HW25RG2;APP=Microsoft Office 2010;Trusted_Connection=Yes;SERVER=bhts-conyprodwd;Description=bhts-conyprodwd")
# 
# print(dbListFields(con,"tbl_SUS_AdmittedPatientCare_Finished"))
# 
# result <- dbSendQuery(con,"SELECT * FROM [proj_TomLawton].[dbo].[tbl_SUS_AdmittedPatientCare_Finished] WHERE [Ward Code 1]!='' AND [Patient Classification]='1' AND ([Site Code (of Treatment) At Episode Start Date]='RAE01' OR [Site Code (of Treatment) at Episode End Date]='RAE01')")
# 
# data <- dbFetch(result)


datah<-readRDS("../Data Extracts/new_wardstay.rds")

datah<-dplyr::arrange(datah,desc(applicable_date))

data<-head(datah,480000)

print("* Loaded *")

data<-filter(data,nights_on_ward>0) %>% 
  filter(service_type_fk_provider_spell!="Maternity IP") %>% 
  filter(ward_name_fk_ward!="Mortuary BRI") %>% 
  filter(!grepl("BRIM",ward_name_fk_ward)) %>% 
  filter(grepl("BRI|SLH",ward_name_fk_ward))
  
  # filter(!grepl("WWP",ward_name_fk_ward)) %>% 
  # filter(!grepl("West",ward_name_fk_ward)) %>% 
  # filter(!grepl("System",ward_name_fk_ward)) %>% 
  # filter(!grepl("Yorks",ward_name_fk_ward))

data<-select(data,c(fk_patient,stay_end_date_dt,stay_start_date_dt,applicable_date,ward_name_fk_ward,service_type_fk_provider_spell))




data<-rowwise(data) %>% 
  mutate(`_TLSpellDigest`=digest(c(`fk_patient`,`applicable_date`)))
##temporary digest to allow matching - will need replacing with official grouping variable later "HOSPITAL PROVIDER SPELL NUMBER" or "ACTIVITY IDENTIFIER"

print("* Digested *")


## Force all times to midday as we only care about overnight 
#data$stay_start_date_dt<-update(data$stay_start_date_dt,hour=12,minute=0)
#data$stay_end_date_dt<-update(data$stay_end_date_dt,hour=12,minute=0)


data<-mutate(data,dur=difftime(stay_end_date_dt,stay_start_date_dt,units="secs"))

print("* Duration Calculated *")

data <- rename(data,c('ward_name_fk_ward'='req'))

data <- rename(data,c('applicable_date'='at'))

data <- dplyr::arrange(data,at,fk_patient,`_TLSpellDigest`,stay_start_date_dt)


## need to correct ward names
## eg 5 BRI = BRI Ward 05
## only a few added so far


recoder <-
  c(
    "BRI Ward 05" = "5 BRI",
    "BRI Ward 08" = "8 BRI",
    "BRI Ward 11" = "11 BRI",
    "BRI Ward 12" = "12 BRI",
    "BRI Ward 20" = "20 BRI",
    "BRI Ward 22" = "22 BRI",
    "BRI CCU" = "22 BRI",
    "BRI Ward 23" = "23 BRI",
    "BRI Ward 27" = "27 BRI",
    "BRI ICU" = "ICU BRI",
    "BRI DCU" = "ENT DCU BRI"
  )
data <- mutate(data,req=dplyr::recode(req,!!!recoder))



wards <- unique(data$req)

print("* Updated Wards *")

duplicates <- data[duplicated(data$`_TLSpellDigest`),]

data <- filter(data,!is.na(dur))

print("* Times Done *")

data <- nest(data,-c(at,fk_patient,`_TLSpellDigest`))

print("* Nested *")

saveRDS(data,file="../Data - Generated/validation_wardstays_nested.rds")
saveRDS(wards,file="../Data - Generated/validation_wardstays_wards.rds")
saveRDS(duplicates,file="../Data - Generated/validation_wardstays_duplicates.rds")


print("* Finished *")