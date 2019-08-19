install.packages("tscount")

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
library(tscount)

setwd("U:/AHSN Academy Analysis/Abi/Patient flow")

sus_data_all<-readRDS("SUSv2_all.rds")
Admit<-readRDS("Admit.rds")

## create time series
Admits<-ts(Admit$Count,frequency=365.25,start=c(2005,91))

test<-stl(Admits,s.window="period")
summary(test)
autoplot(test)+ggtitle("Decomposition of Admissions Time Series")
#overall and seasonal trend are small compared to the stochastic/random variation
#should mean stationary

summary(Admits) #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                #65.0   137.0   175.0   167.4   195.0   253.0

adf.test(Admits,alternative="stationary",k=0) #p<0.01 - reject null - stationary

## acf and pacf - not stationary?
ggAcf(Admits,lag.max=30)+ggtitle("ACF Plot of Admissions")+ylim(-0.2,1)
ggPacf(Admits,lag.max=30)+ggtitle("PACF Plot of Admissions")+ylim(-0.2,1)

## try and use differences to create stationary series
tsstationary<-diff(Admits,diff=2)
plot(tsstationary)
acf(tsstationary)
pacf(tsstationary)
#tried 1 and 2 differences - still not stationary - 2 is max widely used

## use seasonality
comp<-decompose(Admits)
seasonad<-Admits-comp$seasonal
tsstationary<-diff(seasonad,differences=2)
plot(seasonad,type="p")

ggAcf(tsstationary,lag.max=30)+ggtitle("ACF Plot of Stationary Series")+ylim(-0.4,1)
ggPacf(tsstationary,lag.max=30)+ggtitle("PACF Plot of Stationary Series")+ylim(-0.4,1)
#tried diff=1&2 again, still not stationary

## auto arima
arima1<-auto.arima(Admits)
summary(arima1)
#Series: Admits 
#ARIMA(5,1,1) 

#Coefficients:
#  ar1      ar2      ar3      ar4      ar5      ma1
#0.1049  -0.5590  -0.3679  -0.3313  -0.5971  -0.6394
#s.e.   0.0129   0.0105   0.0127   0.0104   0.0119   0.0125

#sigma^2 estimated as 619.2:  log likelihood=-23966.63
#AIC=47947.25   AICc=47947.27   BIC=47993.11

#Training set error measures:
#  ME     RMSE      MAE       MPE     MAPE      MASE
#Training set 0.002921176 24.86661 19.76964 -2.451969 12.73429 0.6260598
#ACF1
#Training set -0.1298297

## prediction - crap
pred<-predict(arima1,n.ahead=365)
ts.plot(Admits,pred$pred,log="y",lty=c(1,3),col=c("black","blue"),title="ARIMA(5,1,1) 1 Year Prediction")
pred$pred

## multi-season time series!
Admits<-msts(Admit$Count,seasonal.periods=c(7,365.25),start=c(2005,91))
train<-head(Admits,round(length(Admits))*0.6)
h<-length(Admits)-length(train)
test<-tail(Admits,h)

## tbats model
fit<-tbats(train)
fc<-forecast(fit,h=h)
# TBATS(1,{0,1},-,{<7,3>,<365.25,7>})
autoplot(fc)+autolayer(test)+xlab("Date")+ylab("Count of Admissions")
accuracy(fit)
summary(fit)
plot(tbats.components(fit),main="Multi-Season Decomposition of Admissions Time Series")
summary(tbats.components(fit))
fit$AIC #42902.54
#fit is better than poisson!!!! training set is good!

## acf plots
ggAcf(resid(fit),lag.max=30,calc.ci=TRUE)+ggtitle("ACF Plot for TBATS model")+ylim(-0.2,1)
ggPacf(resid(fit),lag.max=30)+ggtitle("PACF Plot for TBATS model")+ylim(-0.2,1)

## time series poisson?
coeff<-data.matrix(Admit[,c(5,11)])

mdl<-tsglm(Admits,model=list(past_obs=7,past_mean=7),distr="poisson",xreg=coeff,link="log")
summary(mdl)

pred<-predict(mdl,n.ahead=365)
y<-seq.Date(from=as.Date("2019-06-01"),to=as.Date("2020-05-31"),by="day")
ts.plot(Admits,pred$pred,log="y",lty=c(1,3),col=c("black","blue"))
acf(resid(mdl))
pacf(resid(mdl))
