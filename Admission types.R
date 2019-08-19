install.packages("plyr")

library(DBI)
library(odbc)
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(smooth)
library(Mcomp)
library(timeDate)
library(tseries)
library(MASS)
library(fUnitRoots)
library(tscount)

setwd("U:/AHSN Academy Analysis/Abi/Patient flow")

sus_data_all<-readRDS("SUSv2_all.rds")

Dates<-seq.Date(as.Date("2005-04-01"),as.Date("2019-05-31"),by="day")
Dates<-data.frame(timestamp=Dates)
names(Dates)<-"Date"

### ELECTIVE ###

## create ts of elective admissions
elect<-c("11","12","13")
Elective<-subset(sus_data_all,sus_data_all$`Admission Method (Hospital Provider Spell)`%in%elect)
Admite<-table(Elective$`Start Date (Hospital Provider Spell)`)
Admite<-as.data.frame(Admite)
names(Admite)<-c("Date","Count")
Admite$Date<-as.Date(Admite$Date,format="%Y-%m-%d")
Admite<-full_join(Dates,Admite)
Admite<-Admite%>%
  mutate_each(list(ifelse(is.na(.),0,.)))
Admite$Date<-as.timeDate(Admite$Date)
Admite$Bizday<-isBizday(Admite$Date,holidayLONDON(2004:2019))
Admite$Date<-as.Date(Admite$Date,format="%Y-%m-%d")

## plot elective admissions
ggplot(data=Admite,aes(Date,Count))+geom_point(col="steelblue")+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of Elective Admissions per day")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=90))

## histogram of ages
ggplot(data=Elective,aes(age))+geom_histogram(binwidth=2,fill="steelblue",col="black")+
  xlab("Age")+ggtitle("Count of Admissions by Age")+
  scale_x_continuous(breaks=seq(0,120,by=10))

mean(Admite$Count[2833:4566]) #9.6
mean(Admite$Count[4567:5174]) #5.6

#Admite$Count<-c(Admite$Count[1:4566]-4,Admite$Count[4567:5174]) 
#can't do this because it results in -ve values

## plot coloured by bizday
pal<-c("TRUE"="steelblue","FALSE"="firebrick3")
ggplot(data=Admite,aes(Date,Count,col=Bizday))+geom_point()+
  scale_color_manual(values=pal,limits=names(pal))+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of elective admissions per day")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=90))
#since this has much less variation than the overall, model just as recent averages?
#around 9 for bizdays and 1 for weekends?
#much less variation in recent data too, model based on recent?

#"correct" admissions, since Oct 17
Admitecor<-Admite[4567:5174,]
ggplot(data=Admitecor,aes(Date,Count,col=Bizday))+geom_point()+
  scale_color_manual(values=pal,limits=names(pal))+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of elective admissions per day from October 2017")+
  scale_x_date(date_breaks = "1 month",date_labels = "%m-%y")+
  theme(axis.text.x = element_text(angle=90))

## poisson model
mode<-glm(Count~1+Bizday,data=Admitecor,family=poisson)
summary(mode)
Admitecor$pred<-predict(mode,type="response")
ggplot(data=Admitecor,aes(x=Date))+geom_point(aes(y=Count,colour="Observed"))+
  geom_point(aes(y=pred,colour="Predicted"))+ggtitle("Model prediction")+
  scale_colour_manual("",breaks = c("Observed","Predicted"),values = c("steelblue","seagreen4"))
AIC(mode) #3604.2

## tbats model
Admitets<-msts(Admitecor$Count,seasonal.periods=c(7,365.25),start=c(2017,274))
traine<-head(Admitets,round(length(Admitets))*0.6)
he<-length(Admitets)-length(traine)
teste<-tail(Admitets,he)

fite<-tbats(traine)
fce<-forecast(fite,h=he)
autoplot(fce)+autolayer(teste)+ylim(0,40) #TBATS(1,{0,0},-,{<7,3>,<365.25,8>})
plot(tbats.components(fite),main="Multi-Season Decomposition of Elective Admissions")
summary(tbats.components(fite))
fite$AIC #3329.1
#better than poisson!

## acf plots
ggAcf(resid(fite),lag.max=30)+ggtitle("ACF Plot for TBATS model")+ylim(-0.2,1)
ggPacf(resid(fite),lag.max=30)+ggtitle("PACF Plot for TBATS model")+ylim(-0.2,1)

### A&E ###

## create ts of a&e admissions
AE<-subset(sus_data_all,`Admission Method (Hospital Provider Spell)`=="21")
Admitae<-table(AE$`Start Date (Hospital Provider Spell)`)
Admitae<-as.data.frame(Admitae)
names(Admitae)<-c("Date","Count")
Admitae$Date<-as.timeDate(Admitae$Date)
Admitae$Bizday<-isBizday(Admitae$Date,holidayLONDON(2004:2019))
Admitae$Date<-as.Date(Admitae$Date,format="%Y-%m-%d")

## plot a&e admissions
ggplot(data=Admitae,aes(Date,Count))+geom_point(col="steelblue")+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of Overnight A&E Admissions per day")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=90))

## histogram of ages
ggplot(data=AE,aes(age))+geom_histogram(binwidth=2,fill="steelblue",col="black")+
  xlab("Age")+ggtitle("Count of Admissions by Age")+
  scale_x_continuous(breaks=seq(0,120,by=10))

mean(Admitae$Count[2000:2618]) #91.9
mean(Admitae$Count[2619:3200]) #87.4

Admitae$Count<-c(Admitae$Count[1:2618]-15,Admitae$Count[2619:5174])
Admitae$Count<-c(Admitae$Count[1:4566]-6,Admitae$Count[4567:5174])

## plot coloured by bizday
pal<-c("TRUE"="steelblue","FALSE"="firebrick3")
ggplot(data=Admitae,aes(Date,Count,col=Bizday))+geom_point()+
  scale_color_manual(values=pal,limits=names(pal))+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of overnight A&E admissions per day")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=90))
#so much closer than you'd think!

## poisson model
modae<-glm(Count~1+Date,data=Admitae,family=poisson)
summary(modae)
Admitae$pred<-predict(modae,type="response")
ggplot(data=Admitae,aes(x=Date))+geom_point(aes(y=Count,colour="Observed"))+
  geom_point(aes(y=pred,colour="Predicted"))+ggtitle("Model prediction")+
  scale_colour_manual("",breaks = c("Observed","Predicted"),values = c("steelblue","seagreen4"))
AIC(modae) #41,134.1

## tbats model
Admitaets<-msts(Admitae$Count,seasonal.periods=c(7,365.25),start=c(2005,91))
trainae<-head(Admitaets,round(length(Admitaets))*0.6)
hae<-length(Admitaets)-length(trainae)
testae<-tail(Admitaets,hae)

fitae<-tbats(trainae)
fcae<-forecast(fitae,h=hae)
autoplot(fcae)#+autolayer(testae) #TBATS(0.645,{1,2},-,{<7,3>,<365.25,6>})
plot(tbats.components(fitae),main="Multi-Season Decomposition of Overnight A&E Admissions")
summary(tbats.components(fitae))
fitae$AIC #38,738.33
#better than poisson

## acf plots
ggAcf(resid(fitae),lag.max=30)+ggtitle("ACF Plot for TBATS model")+ylim(-0.2,1)
ggPacf(resid(fitae),lag.max=30)+ggtitle("PACF Plot for TBATS model")+ylim(-0.2,1)

### OTHER EMERGENCY ###

## create ts of emergency admissions
emerg<-c("22","23","24","28")
Emergency<-subset(sus_data_all,`Admission Method (Hospital Provider Spell)`%in%emerg)
Admitem<-table(Emergency$`Start Date (Hospital Provider Spell)`)
Admitem<-as.data.frame(Admitem)
names(Admitem)<-c("Date","Count")
Admitem$Date<-as.timeDate(Admitem$Date)
Admitem$Bizday<-isBizday(Admitem$Date,holidayLONDON(2004:2019))
Admitem$Date<-as.Date(Admitem$Date,format="%Y-%m-%d")

## plot emergency admissions
ggplot(data=Admitem,aes(Date,Count))+geom_point(col="steelblue")+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of non-A&E Emergency Admissions per day")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=90))

## histogram of ages
ggplot(data=Emergency,aes(age))+geom_histogram(binwidth=2,fill="steelblue",col="black")+
  xlab("Age")+ggtitle("Count of Admissions by Age")+
  scale_x_continuous(breaks=seq(0,120,by=10))

## adjust to system change
Admitembiz<-Admitem[Admitem$Bizday>0,c("Date","Count","Bizday")]
mean(Admitembiz$Count[0:3156])    #45.5
mean(Admitembiz$Count[3157:3580]) #74.4
Admitembiz$Count<-c(Admitembiz$Count[0:3156]+25,Admitembiz$Count[3157:3580])

Admitemnbiz<-Admitem[Admitem$Bizday<1,c("Date","Count","Bizday")]
mean(Admitemnbiz$Count[0:1397])    #22.5
mean(Admitemnbiz$Count[1398:1594]) #34.9
Admitemnbiz$Count<-c(Admitemnbiz$Count[0:1397]+12,Admitemnbiz$Count[1398:1594])

Admitem<-join_all(list(Dates,Admitembiz,Admitemnbiz),by="Date",type="full")
Admitem<-join_all(list(Admitemnbiz,Admitem),by="Date",type="full")

Admitem<-Admitem[order(as.Date(Admitem$Date,format="%Y-%m-%d")),]

## plot coloured by bizday
pal<-c("TRUE"="steelblue","FALSE"="firebrick3")
ggplot(data=Admitem,aes(Date,Count,col=Bizday))+geom_point()+
  scale_color_manual(values=pal,limits=names(pal))+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of non-A&E Emergency admissions per day")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=90))

## poisson model
modem<-glm(Count~1+Bizday,data=Admitem,family=poisson)
summary(modem)
Admitem$pred<-predict(modem,type="response")
ggplot(data=Admitem,aes(x=Date))+geom_point(aes(y=Count,colour="Observed"))+
  geom_point(aes(y=pred,colour="Predicted"))+ggtitle("Model prediction")+
  scale_colour_manual("",breaks = c("Observed","Predicted"),values = c("steelblue","seagreen4"))
AIC(modem) #35,564.59

## tbats model
Admitemts<-msts(Admitem$Count,seasonal.periods=c(7,365.25),start=c(2005,91))
trainem<-head(Admitemts,round(length(Admitemts))*0.6)
hem<-length(Admitemts)-length(trainem)
testem<-tail(Admitemts,hem)

fitem<-tbats(trainem)
fcem<-forecast(fitem,h=hem)
autoplot(fcem)+autolayer(testem) #TBATS(1,{0,0},-,{<7,3>,<365.25,5>})
plot(tbats.components(fitem),main="Multi-Season Decomposition of non-A&E Emergency Admissions")
summary(tbats.components(fitem))
fitem$AIC #38,105.26
#relatively close but higher? tbats seems to model variation better?

## acf plots
ggAcf(resid(fitem),lag.max=30)+ggtitle("ACF Plot for TBATS model")+ylim(-0.2,1)
ggPacf(resid(fitem),lag.max=30)+ggtitle("PACF Plot for TBATS model")+ylim(-0.2,1)

### MATERNITY ###

## create ts of maternity admissions
mat<-c("31","32")
Maternity<-subset(sus_data_all,`Admission Method (Hospital Provider Spell)`%in%mat)
Admitm<-table(Maternity$`Start Date (Hospital Provider Spell)`)
Admitm<-as.data.frame(Admitm)
names(Admitm)<-c("Date","Count")
Admitm$Date<-as.Date(Admitm$Date,format="%Y-%m-%d")
Admitm<-full_join(Dates,Admitm)
Admitm<-Admitm%>%
  mutate_each(funs(ifelse(is.na(.),0,.)))
Admitm$Date<-as.Date(Admitm$Date,origin="1970-01-01")
Admitm$Date<-as.timeDate(Admitm$Date)
Admitm$Bizday<-isBizday(Admitm$Date,holidayLONDON(2004:2019))
Admitm$Date<-as.Date(Admitm$Date)

## plot maternity admissions
ggplot(data=Admitm,aes(Date,Count))+geom_point(col="steelblue")+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of Overnight Maternity Admissions per day")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=90))+ylim(0,60)

## histogram of ages
ggplot(data=Maternity,aes(age))+geom_histogram(binwidth=2,fill="steelblue",col="black")+
  xlab("Age")+ggtitle("Count of Admissions by Age")+
  scale_x_continuous(breaks=seq(0,120,by=10))
#range 0-92? definitely some coded wrong!

## use just recent data
Admitmcor<-Admitm[4567:5174,]
ggplot(data=Admitmcor,aes(Date,Count))+geom_point(col="steelblue")+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of Overnight Maternity Admissions per day")+
  scale_x_date(date_breaks = "1 month",date_labels = "%m-%Y")+
  theme(axis.text.x = element_text(angle=90))+ylim(0,6)

Admitmcor<-merge(Admitmcor,weather)

## poisson model
modmfull<-glm(Count~1+Month.no+Max+Min+AF+Sunshine+Rainfall,data=Admitmcor,family=poisson)
summary(modmfull)
modm<-stepAIC(modmfull,direction="both",trace=FALSE)
summary(modm)
Admitmcor$pred<-predict(modm,type="response")
ggplot(data=Admitmcor,aes(x=Date))+geom_point(aes(y=Count,colour="Observed"))+
  geom_point(aes(y=pred,colour="Predicted"))+ggtitle("Model prediction")+
  scale_colour_manual("",breaks = c("Observed","Predicted"),values = c("steelblue","seagreen4"))
AIC(modm) #1069.4

## tbats model
Admitmts<-msts(Admitmcor$Count,seasonal.periods=c(7,365.25),start=c(2017,274))
trainm<-head(Admitmts,round(length(Admitmts))*0.8)
hm<-length(Admitmts)-length(trainm)
testm<-tail(Admitmts,hm)

fitm<-tbats(trainm)
fcm<-forecast(fitm,h=hm)
autoplot(fcm)+autolayer(testm) #BATS(1,{0,0},0.934,-) no seasonal component?
plot(tbats.components(fitm),main="Multi-Season Decomposition of Overnight Maternity Admissions")
summary(tbats.components(fitm)) #components mean nothing
fitm$AIC #2816.9
#poisson better because there isn't a trend, may have to use tbats anyway?
#if compiling all

## acf plots
ggAcf(resid(fitm),lag.max=30)+ggtitle("ACF Plot for TBATS model")+ylim(-0.2,1)
ggPacf(resid(fitm),lag.max=30)+ggtitle("PACF Plot for TBATS model")+ylim(-0.2,1)

### BABIES ###

## create ts of baby admissions
bab<-c("82","83")
Babies<-subset(sus_data_all,`Admission Method (Hospital Provider Spell)`%in%bab)
Admitb<-table(Babies$`Start Date (Hospital Provider Spell)`)
Admitb<-as.data.frame(Admitb)
names(Admitb)<-c("Date","Count")
Admitb$Date<-as.Date(Admitb$Date,format="%Y-%m-%d")
Admitb<-full_join(Dates,Admitb)
Admitb<-Admitb%>%
  mutate_each(funs(ifelse(is.na(.),0,.)))
Admitb$Date<-as.Date(Admitb$Date,origin="1970-01-01")

#range of age 0-79? very old babies

## plot newborn admissions
ggplot(data=Admitb,aes(Date,Count))+geom_point(col="steelblue")+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of Overnight Newborn Admissions per day")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=90))+ylim(0,35)
## No admissions since Oct 2017 so no need for model

### NON-EMERGENCY TRANSFERS ###

## create df of non-emergency transfers
Transfer<-subset(sus_data_all,`Admission Method (Hospital Provider Spell)`==81)
Admitt<-table(Transfer$`Start Date (Hospital Provider Spell)`)
Admitt<-as.data.frame(Admitt)
names(Admitt)<-c("Date","Count")
Admitt$Date<-as.Date(Admitt$Date,format="%Y-%m-%d")
Admitt<-full_join(Dates,Admitt)
Admitt<-Admitt%>%
  mutate_each(funs(ifelse(is.na(.),0,.)))
Admitt$Date<-as.Date(Admitt$Date,origin="1970-01-01")
Admitt$Date<-as.timeDate(Admitt$Date)
Admitt$Bizday<-isBizday(Admitt$Date,holidayLONDON(2004:2019))
Admitt$Date<-as.Date(Admitt$Date,format="%Y-%m-%d")

## plot non-emergency transfers
ggplot(data=Admitt,aes(Date,Count))+geom_point(col="steelblue")+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of Non-Emergency Transfers per day")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=90))

## histogram of ages
ggplot(data=Transfer,aes(age))+geom_histogram(binwidth=2,fill="steelblue",col="black")+
  xlab("Age")+ggtitle("Count of Admissions by Age")+
  scale_x_continuous(breaks=seq(0,120,by=10))

## plot coloured by bizday
pal<-c("TRUE"="steelblue","FALSE"="firebrick3")
ggplot(data=Admitt,aes(Date,Count,col=Bizday))+geom_point()+
  scale_color_manual(values=pal,limits=names(pal))+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of non-Emergency Transfers per day")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=90))

Admitt<-merge(Admitt,weather)

## poisson model
modtfull<-glm(Count~1+Month.no+Max+Min+AF+Sunshine+Rainfall+Bizday,data=Admitt,family=poisson)
summary(modtfull)
modt<-stepAIC(modtfull,direction="both",trace=FALSE)
summary(modt)
Admitt$pred<-predict(modt,type="response")
ggplot(data=Admitt,aes(x=Date))+geom_point(aes(y=Count,colour="Observed"))+
  geom_point(aes(y=pred,colour="Predicted"))+ggtitle("Model prediction")+
  scale_colour_manual("",breaks = c("Observed","Predicted"),values = c("steelblue","seagreen4"))
AIC(modt) #16,642.8

## tbats model
Admitts<-msts(Admitt$Count,seasonal.periods=c(7,365.25),start=c(2005,91))
traint<-head(Admitts,round(length(Admitts))*0.6)
ht<-length(Admitts)-length(traint)
testt<-tail(Admitts,ht)

fitt<-tbats(traint)
fct<-forecast(fitt,h=ht)
autoplot(fct)+autolayer(testt) #BATS(1,{0,0},-,{<7,3>,<365.25,6>})
plot(tbats.components(fitt),main="Multi-Season Decomposition of Non-Emergency Transfers")
summary(tbats.components(fitm)) #components mean nothing
fitt$AIC #26,638.9
#poisson better

## acf plots
ggAcf(resid(fitt),lag.max=30)+ggtitle("ACF Plot for TBATS model")+ylim(-0.2,1)
ggPacf(resid(fitt),lag.max=30)+ggtitle("PACF Plot for TBATS model")+ylim(-0.2,1)
