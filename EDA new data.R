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

sus_data_all<-readRDS("SUSv2_all.rds")
Admit<-readRDS("Admit.rds")

## detailed plot of admissions
ggplot(data=Admit,aes(Date,Count))+geom_point(col="steelblue")+geom_smooth()+
        xlab("Date")+ggtitle("Frequency of admissions per day")+
        scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
        theme(axis.text.x = element_text(angle=90))

## fixing data
mean(Admit$Count[2833:4566]) #206
mean(Admit$Count[4567:5174]) #178
#Admit$Count<-c(Admit$Count[1:4566]-32,Admit$Count[4567:5174])

## histogram of ages - interesting!
ggplot(data=sus_data_all,aes(age))+geom_histogram(binwidth=2,fill="steelblue",col="black")+
  xlab("Age")+ggtitle("Count of Admissions by Age")+
  scale_x_continuous(breaks=seq(0,120,by=10))
                     
## bar chart of admission method
sus_data_all$`Admission Method (Hospital Provider Spell)`<-factor(sus_data_all$`Admission Method (Hospital Provider Spell)`)
levels(sus_data_all$`Admission Method (Hospital Provider Spell)`)
ggplot(data=sus_data_all,aes(`Admission Method (Hospital Provider Spell)`))+
  geom_bar(fill="steelblue")+xlab("Admission Method")+ylab("No. of Admissions")+
  ggtitle("Count of Admissions by Admission Method")

## bar chart of ethnic group
ggplot(data=sus_data_all,aes(`Ethnic Group`))+geom_bar()

## bar chart of specialities
ggplot(data=sus_data_all,aes(`Main Specialty Code`))+geom_bar(fill="steelblue")+
  theme(axis.text.x=element_text(angle=90))+ggtitle("Count of Admissions by Main Speciality")

## length of stay histogram - either very small or very weird - mostly low either way
ggplot(data=sus_data_all,aes(LoS))+geom_histogram(binwidth = 2)#+xlim(0,35)+ylim(0,5000)

## age against los
ggplot(data=sus_data_all,aes(age,LoS))+geom_point()+geom_smooth()
ggplot(data=sus_data_all,aes(age,LoS))+geom_smooth()+xlab("Age")+
  ggtitle("Length of Stay by Age")+ylab("Length of Stay")+
  scale_x_continuous(breaks=seq(0,110,by=10))

## admission method vs los - not useful
ggplot(data=sus_data_all,aes(`Admission Method (Hospital Provider Spell)`,LoS))+
          geom_boxplot()+ylim(0,30)

## boxplot by month
ggplot(Admit,aes(factor(Month.no),Count))+geom_boxplot()+xlab("Month")+
  ggtitle("Average Admissions per Day by Month")

## plot coloured by bizday
pal<-c("TRUE"="steelblue","FALSE"="firebrick3")
ggplot(data=Admit,aes(Date,Count,col=Bizday))+geom_point()+
  scale_color_manual(values=pal,limits=names(pal))+geom_smooth()+
  xlab("Date")+ggtitle("Frequency of admissions per day")+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.text.x = element_text(angle=90))

## histogram of admissions
ggplot(data=Admit,aes(Count))+geom_histogram(binwidth=5,col="black",fill="steelblue")+
  xlab("No. of Admissions per day")+ggtitle("Count of Numbers of Admissions per Day")

## poisson model
mod.full<-glm(Count~Month.no+Min+Max+AF+Rainfall+Sunshine+Bizday,data=Admit,family=poisson)
summary(mod.full)
mod.step<-stepAIC(mod.full,direction="both",trace=FALSE)
summary(mod.step)
AIC(mod.step) #47887.1
plot(mod.step)

cor(Admit$Min,Admit$AF)
cor(Admit$Min,Admit$Max)
cor(Admit$Min,Admit$Rainfall)
cor(Admit$Min,Admit$Sunshine)

Admit$pred<-predict(mod.full,type="response")
ggplot(data=Admit,aes(x=Date))+geom_point(aes(y=Count,colour="Observed"))+
  geom_point(aes(y=pred,colour="Predicted"))+ggtitle("Model prediction")+
  scale_colour_manual("",breaks = c("Observed","Predicted"),values = c("steelblue","seagreen4"))