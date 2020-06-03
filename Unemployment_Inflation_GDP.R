install.packages("tidyverse")
install.packages("corrgram")
install.packages("tseries")
install.packages("readxl")
install.packages("urca")
install.packages("forecast")
install.packages("trend")
install.packages("zoo")
install.packages("reshape")

library(tidyverse)
library(readxl)
library(corrgram)
library(tseries)
library(urca)
library(forecast)
library(trend)
library(zoo)
library(reshape)


setwd("~/TimeSeriesRegression")
Unemployment<-read_excel("~/TimeSeriesRegression/MacroData.xlsx",sheet="Sheet2")
Inflation<-read_excel("~/TimeSeriesRegression/MacroData.xlsx",sheet="Sheet3")
GDP<-read_excel("~/TimeSeriesRegression/MacroData.xlsx",sheet="Sheet7")


View(GDP)
#select variables from the larger dataset

Year<-Unemployment[1,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

GreeceUn<-Unemployment[464,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

GreeceInf<-Inflation[1082,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

GreeceGDP<-GDP[83,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47', 'Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

View(Year)
#transpose the columns to observations

t_GreeceGDP<-t(GreeceGDP)
t_GreeceGDP<-as.numeric(t_GreeceGDP)

View(t_GreeceGDP)

t_GreeceInf<-t(GreeceInf)
t_GreeceInf<-as.numeric(t_GreeceInf)
View(t_GreeceInf)


t_GreeceUnem<-t(GreeceUn)
t_GreeceUnem<-as.numeric(t_GreeceUnem)
View(t_GreeceUnem)

t_Year<-t(Year)
t_Year<-as.numeric(t_Year)
View(t_Year)


TimeSeriesGreece<-cbind(t_Year, t_GreeceGDP, t_GreeceInf, t_GreeceUnem)
View(TimeSeriesGreece)



TimeSeriesGreece<-as.data.frame(TimeSeriesGreece)

TimeSeriesGreece <- rename(TimeSeriesGreece, c(t_Year="Years"))
View(TimeSeriesGreece)


TimeSeriesGreece <- rename(TimeSeriesGreece, c(t_GreeceGDP="GrossDP"))

TimeSeriesGreece <- rename(TimeSeriesGreece, c(t_GreeceInf="Inflation"))

TimeSeriesGreece <- rename(TimeSeriesGreece, c(t_GreeceUnem="Unemploy"))

View(TimeSeriesGreece)


#tell R this is time series data#
tsUR<-zoo(TimeSeriesGreece$Unemploy, order.by = TimeSeriesGreece$Years)
tsIF<-zoo(TimeSeriesGreece$Inflation, order.by = TimeSeriesGreece$Years)
tsGDP<-zoo(TimeSeriesGreece$GrossDP, order.by = TimeSeriesGreece$Years)



#create plots of the timeseries#

ggplot(data = TimeSeriesGreece, aes(x = TimeSeriesGreece$Years, y = TimeSeriesGreece$GrossDP)) + geom_line()
ggplot(data = TimeSeriesGreece, aes(x = TimeSeriesGreece$Years, y = TimeSeriesGreece$Inflation))+ geom_line()
ggplot(data = TimeSeriesGreece, aes(x = TimeSeriesGreece$Years, y = TimeSeriesGreece$Unemploy))+ geom_line()


#test for stationarity#

adf.test(tsGDP)
adf.test(tsIF)
adf.test(tsUR)




kpss.test(TimeSeriesGreece$GrossDP, null = "Trend")
kpss.test(TimeSeriesGreece$Inflation, null = "Trend")
kpss.test(TimeSeriesGreece$Unemploy, null = "Trend")

#check the correlograms# 
acf(tsUR)
acf(tsIF)
acf(tsGDP)

pacf(tsUR)
pacf(tsIF)
pacf(tsGDP)

Box.test(tsUR)
Box.test(tsIF)
Box.test(tsGDP)

#transform the data#

#differencing#
InflDiff1=diff(tsIF)
InflDiff2=diff(tsIF, differences=2)
URdiff=diff(tsUR)
GDPdiff=diff(tsGDP)
YearDiff=diff(TimeSeriesGreece$Years)

kpss.test(InflDiff2, null = "Trend")

kpss.test(GDPdiff, null = "Trend")

acf(URdiff)
acf(InflDiff2)
acf(GDPdiff)

pacf(URdiff)
pacf(InflDiff2)
pacf(GDPdiff)

#detrending
m<-lm(coredata(GDPdiff)~index(GDPdiff))
GDPdetrend<-zoo(resid(m),index(GDPdiff))

plot(GDPdetrend)


#model data 



Arima(tsUR, order = c(1, 0, 0),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")


Arima(tsUR, order = c(1, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(tsGDP, order = c(2, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

#Unemployment Rate
Arima(tsUR, order = c(1, 0, 2),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(tsUR, order = c(1, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

#InflationRate
Arima(InflDiff2, order = c(0, 0, 9),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 10),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 8),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 7),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 2),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 2),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(GDPdiff, order = c(1, 0, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(GDPdiff, order = c(2, 0, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")


UmData<-Arima(tsUR, order = c(1, 0, 1),
              include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
              method = "ML")

InfData<-Arima(InflDiff2, order = c(0, 0, 2),
               include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
               method = "ML")

GDPData<-Arima(GDPdiff, order = c(2, 0, 1),
              include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
              method = "ML")

GDPData2<-Arima(tsGDP, order = c(2, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

plot(forecast(UmData,h=10))
plot(forecast(InfData,h=10))
plot(forecast(GDPData,h=10))
plot(forecast(GDPData2,h=100))

