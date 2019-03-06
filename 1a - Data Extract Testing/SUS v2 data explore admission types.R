
## TL - analyse SUS (v2) data to produce a model for admission frequency by time
## Target a virtual year
## more years in the SUS v2 data

# 
# Admission Method
# 
# 11 = very odd, happens every day ("waiting list")
# 12/13 look more sensible
# 
# Emergency
# 21 = A&E, looks good
# 22 = GP - very odd weekend spikes around end Sep/start Oct and then at Christmas
# 
# 28 = only appears relatively recently in data??
#   
  

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
data<-readRDS("../Data - Generated/SUSv2-basedata.rds")
load_rmetrics_calendars(2000:2022)
print("* Loaded *")

##Filters here for the R-markdown
data<-filter(data,`Patient Classification`==1) # only ordinary admissions (day cases ignored)
#data<-filter(data,!is.bizday(as.character(`_Start_DateTime`),'Rmetrics/LONDON'))

admissiontypes<-table (data$`Start Date (Hospital Provider Spell)`,data$`Admission Method (Hospital Provider Spell)`)

admDF<-data.frame(admissiontypes)
admDFw<-reshape(admDF,idvar="Var1",timevar="Var2",direction="wide")
admDFw$Emergency<-admDFw$Freq.21+admDFw$Freq.22+admDFw$Freq.23+admDFw$Freq.24+admDFw$Freq.28

admDFw$Elective<-admDFw$Freq.11+admDFw$Freq.12+admDFw$Freq.13

#plot(subset(admDF,Var2=="31",select=c(Var1,Freq)))
 print("* Finished *")