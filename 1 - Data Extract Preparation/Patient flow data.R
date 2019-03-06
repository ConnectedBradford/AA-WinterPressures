## From Abi - aim to transform SUS data (v1) into a format we can use for modelling



install.packages("odbc")
install.packages("DBI")
install.packages("dplyr")

library(DBI)
library(odbc)
library(dplyr)

##Read in data from SQL - Must change username!##
con <- dbConnect(odbc::odbc(), .connection_string="DRIVER=SQL Server;UID=duttona;DATABASE=proj_TomLawton;WSID=5040-HW25RG2;APP=Microsoft Office 2010;Trusted_Connection=Yes;SERVER=bhts-conyprodwd;Description=bhts-conyprodwd")

print(dbListFields(con,"tbl_SUS_AdmittedPatientCare_Finished"))

result <- dbSendQuery(con,"SELECT * FROM [proj_TomLawton].[dbo].[tbl_SUS_AdmittedPatientCare_Finished] WHERE [Ward Code 1]!='' AND [Patient Classification]='1' AND ([Site Code (of Treatment) At Episode Start Date]='RAE01' OR [Site Code (of Treatment) at Episode End Date]='RAE01')")

data <- dbFetch(result)

##select the variables we need##
df<-select(data,DigestPseudID,`Episode Number`,`Start Date (Hospital Provider Spell)`,`Discharge Date (From Hospital Provider Spell)`,`Start Time (Hospital Provider Spell)`,`Discharge Time (Hospital Provider Spell)`,`Admission Method (Hospital Provider Spell)`,`Ward Code 1`:`End Time (Ward Stay) 4`,-contains("site code"),-contains("location class"),contains("critical care start date"),contains("critical care level 2"),contains("Critical care level 3"),`Main Specialty Code`,`Treatment Function Code`,`Last Episode In Spell Indicator`)
##add in indicator column for elective addmissions and columns for critical care level##
df1<-mutate(df,elective=(11<=`Admission Method (Hospital Provider Spell)`& `Admission Method (Hospital Provider Spell)`<=13),`Critical Care Level 1`=0,`Critical Care Level 4`=NA,`Critical Care Level 7`=NA,`Critical Care Level 10`=NA)
##add in extra ward columns for critical care stays##
varnames<-c("Ward Code","Start Date","Start Time (Ward Stay)","End Date","End Time (Ward Stay)")
names(df1)[13:17]<-paste0(varnames," ",rep(4,5))
names(df1)[18:22]<-paste0(varnames," ",rep(7,5))
names(df1)[23:27]<-paste0(varnames," ",rep(10,5))
varnames2<-c(varnames,"Critical Care Level")
df1[paste0(varnames2," ",c(rep(2,6),rep(3,6),rep(5,6),rep(6,6),rep(8,6),rep(9,6),rep(11,6),rep(12,6)))]<-NA
##reorder columns##
df2<-df1[,c(1:7,49:52,8:12,53,57:68,13:17,54,69:80,18:22,55,81:92,23:27,56,93:104,28:48)]


##match critical care stays with ward stays and attach critical care level to ward stay##
for (row in 1:nrow(df2)){
  for (i  in c(12,30,48,66)){
    for (j in 84:90){
      if(df2[row,i]=="ICU BRI"){
        if(df2[row,i+1]==df2[row,j]){
          #if there are level 3 cc days
          if(df2[row,j+14]!=0){
            df2[row,i+5]<-3
            #if there are also level 2 cc days
            if(df2[row,j+7]!=0){
              df2[row,i+11]<-2
              df2[row,i+6]<-df2[row,i]
              df2[row,i+7]<-as.numeric(df2[row,i+1])+as.numeric(df2[row,j+14])
              df2[row,i+8]<-"00:00:00"
              df2[row,i+9]<-as.numeric(df2[row,i+7])+as.numeric(df2[row,j+7])
              #if there are also level 1 cc days
              if(as.numeric(df2[row,i+1])+as.numeric(df2[row,j+7])+as.numeric(df2[row,j+14])!=as.numeric(df2[row,i+3])){
                df2[row,i+17]<-1
                df2[row,i+12]<-df2[row,i]
                df2[row,i+13]<-as.numeric(df2[row,i+9])+1
                df2[row,i+14]<-"00:00:00"
                df2[row,i+15]<-df2[row,i+3]
                df2[row,i+16]<-df2[row,i+4]
                df2[row,i+3]<-as.numeric(df2[row,i+1])+as.numeric(df2[row,j+14])
                df2[row,i+4]<-"23:59:00"
                df2[row,i+10]<-"23:59:00"
                #if there are level 2 and 3 days but no level 1  
              }else{
                df2[row,i+9]<-df2[row,i+3]
                df2[row,i+10]<-df2[row,i+4]
                df2[row,i+3]<-as.numeric(df2[row,i+1])+as.numeric(df2[row,j+14])
                df2[row,i+4]<-"23:59:00"
              }
              #if there are level 3 days but no level 2  
            }else{
              if(as.numeric(df2[row,i+1])+as.numeric(df2[row,j+14])!=as.numeric(df2[row,i+3])){
                df2[row,i+11]<-1
                df2[row,i+6]<-df2[row,i]
                df2[row,i+7]<-as.numeric(df2[row,i+1])+as.numeric(df2[row,j+14])+1
                df2[row,i+8]<-"00:00:00"
                df2[row,i+9]<-df2[row,i+3]
                df2[row,i+10]<-df2[row,i+4]
                df2[row,i+3]<-as.numeric(df2[row,i+7])-1
                df2[row,i+4]<-"23:59:00"
              }
            }#no level 3 cc days
          }else{
            #if there are level 2 cc days
            if(df2[row,j+7]!=0){
              df2[row,i+5]<-2
              #if there are also level 1 days
              if(as.numeric(df2[row,i+1])+as.numeric(df2[row,j+7])!=as.numeric(df2[row,i+3])){
                df2[row,i+11]<-1
                df2[row,i+6]<-df2[row,i]
                df2[row,i+7]<-as.numeric(df2[row,i+1])+as.numeric(df2[row,j+7])+1
                df2[row,i+8]<-"00:00:00"
                df2[row,i+9]<-df2[row,i+3]
                df2[row,i+10]<-df2[row,i+4]
                df2[row,i+3]<-as.numeric(df2[row,i+7])-1
                df2[row,i+4]<-"23:59:00"
              }
            }
          }
        }
      }
    }
  }
}


##create dataset for each episode number and reduce latter numbers to just ward and critical care data##
data1<-filter(df2,`Episode Number`==1)
data2<-filter(df2,`Episode Number`==2)
data2<-select(data2,DigestPseudID,`Start Date (Hospital Provider Spell)`,`Ward Code 1`:`Critical Care Discharge Date 6`,`Last Episode In Spell Indicator`)
data3<-filter(df2,`Episode Number`==3)
data3<-select(data3,DigestPseudID,`Start Date (Hospital Provider Spell)`,`Ward Code 1`:`Critical Care Discharge Date 6`,`Last Episode In Spell Indicator`)
data4<-filter(df2,`Episode Number`==4)
data4<-select(data4,DigestPseudID,`Start Date (Hospital Provider Spell)`,`Ward Code 1`:`Critical Care Discharge Date 6`,`Last Episode In Spell Indicator`)
data5<-filter(df2,`Episode Number`==5)
data5<-select(data5,DigestPseudID,`Start Date (Hospital Provider Spell)`,`Ward Code 1`:`Critical Care Discharge Date 6`,`Last Episode In Spell Indicator`)
data6<-filter(df2,`Episode Number`==6)
data6<-select(data6,DigestPseudID,`Start Date (Hospital Provider Spell)`,`Ward Code 1`:`Critical Care Discharge Date 6`,`Last Episode In Spell Indicator`)
data7<-filter(df2,`Episode Number`==7)
data7<-select(data7,DigestPseudID,`Start Date (Hospital Provider Spell)`,`Ward Code 1`:`Critical Care Discharge Date 6`,`Last Episode In Spell Indicator`)
data8<-filter(df2,`Episode Number`==8)
data8<-select(data8,DigestPseudID,`Start Date (Hospital Provider Spell)`,`Ward Code 1`:`Critical Care Discharge Date 6`,`Last Episode In Spell Indicator`)
data9<-filter(df2,`Episode Number`==9)
data9<-select(data9,DigestPseudID,`Start Date (Hospital Provider Spell)`,`Ward Code 1`:`Critical Care Discharge Date 6`,`Last Episode In Spell Indicator`)

##join datasets together, matching on ID and start date##
total<-left_join(data1,data2,by=c("DigestPseudID","Start Date (Hospital Provider Spell)"))
total<-left_join(total,data3,by=c("DigestPseudID","Start Date (Hospital Provider Spell)"))
total<-left_join(total,data4,by=c("DigestPseudID","Start Date (Hospital Provider Spell)"))
total<-left_join(total,data5,by=c("DigestPseudID","Start Date (Hospital Provider Spell)"))
total<-left_join(total,data6,by=c("DigestPseudID","Start Date (Hospital Provider Spell)"))
total<-left_join(total,data7,by=c("DigestPseudID","Start Date (Hospital Provider Spell)"))
total<-left_join(total,data8,by=c("DigestPseudID","Start Date (Hospital Provider Spell)"))
total<-left_join(total,data9,by=c("DigestPseudID","Start Date (Hospital Provider Spell)"))
total<-select(total,-contains("last episode in spell indicator"),-contains("episode number"))

total<-total %>% distinct()
