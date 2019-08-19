install.packages("odbc")
install.packages("DBI")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("smooth")
install.packages("Mcomp")
install.packages("timeDate")
install.packages("tseries")
install.packages("MASS")
install.packages("fUnitRoots")

library(DBI)
library(odbc)
library(dplyr)
library(ggplot2)
library(lubridate)
library(smooth)
library(Mcomp)
library(timeDate)
library(tseries)
library(MASS)
library(fUnitRoots)

setwd("U:/AHSN Academy Analysis/Abi/Patient flow")

##Read in data from SQL - Must change username!##
con<- dbConnect(odbc::odbc(), .connection_string="DRIVER=SQL Server;
                UID=duttona;DATABASE=proj_TomLawton;WSID=5040-HW25RG2;
                APP=Microsoft Office 2010;Trusted_Connection=Yes;
                SERVER=bhts-conyprodwd;Description=bhts-conyprodwd")

#print(dbListFields(con,"SUS_ALL_AdmittedPatientCare_Finished_PART1"))

result <- dbSendQuery(con,"SELECT [Ethnic Group],Sex,PartPCode,pseudoDOB,
                      [Start Date (Hospital Provider Spell)],[Discharge Date (From Hospital Provider Spell)],
                      [Start Time (Hospital Provider Spell)],[Discharge Time (Hospital Provider Spell)],
                      [Admission Method (Hospital Provider Spell)],[Ward Code 1],
                      [Main Specialty Code]
                      FROM [proj_TomLawton].[dbo].[SUS_ALL_AdmittedPatientCare_Finished_PART1]
                      WHERE [Patient Classification]='1'
                      AND [Episode Number]='1'
                      AND([Site Code (of Treatment) At Episode Start Date]='RAE'
                      OR[Site Code (of Treatment) At Episode Start Date]='RAE01'
                      OR[Site Code (of Treatment) At Episode Start Date]='RA')")

sus_data_all <-dbFetch(result)

## format dates
sus_data_all$`Start Date (Hospital Provider Spell)`<-as.Date(sus_data_all$`Start Date (Hospital Provider Spell)`,
                                                             format="%Y%m%d")
sus_data_all$`Discharge Date (From Hospital Provider Spell)`<-as.Date(sus_data_all$`Discharge Date (From Hospital Provider Spell)`,
                                                                      format="%Y%m%d")

## filter out incomplete date range
sus_data_all<-sus_data_all%>%
  subset(`Start Date (Hospital Provider Spell)`>"2005-03-31")

## table of admissions
Admit<-table(sus_data_all$`Start Date (Hospital Provider Spell)`)
Admit<-as.data.frame(Admit)
Admit$Var1<-as.Date(Admit$Var1,format="%Y-%m-%d")
names(Admit)<-c("Date","Count")

## fix incorrect classification
mean(Admit$Count[2833:4566]) #206
mean(Admit$Count[4567:5174]) #178

Admit$Count<-c(Admit$Count[1:4566]-32,Admit$Count[4567:5174])

## calculate ages
sus_data_all$pseudoDOB<-as.Date(sus_data_all$pseudoDOB,format="%Y%m%d")
sus_data_all$age<-year(sus_data_all$`Start Date (Hospital Provider Spell)`)-year(sus_data_all$pseudoDOB)

## calculate length of stay
sus_data_all$LoS<-sus_data_all$`Discharge Date (From Hospital Provider Spell)`-sus_data_all$`Start Date (Hospital Provider Spell)`

## read in weather data
weather<-read.csv("Weather.csv",header=TRUE)
weather$Date<-as.Date(weather$Date,format="%d/%m/%Y")

## merge admissions and weather
Admit<-merge(Admit,weather)
Admit$Day<-weekdays(Admit$Date)
Admit$Date<-as.timeDate(Admit$Date)
Admit$Bizday<-isBizday(Admit$Date,holidayLONDON(2004:2019))
Admit$Date<-as.Date(Admit$Date)

## save
saveRDS(sus_data_all,file="SUSv2_all.rds")
saveRDS(Admit,file="Admit.rds")
