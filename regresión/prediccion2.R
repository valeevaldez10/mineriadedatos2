rm(list=ls())
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(forecast)
library(plotly)
library(tseries)
#install.packages("WDI")
library(WDI)


#Variable: Tasa de natalidad bruta (1000 habitantes)
#Desde 1960 a 2024
datos <- WDI(country = "BO", indicator = "SP.DYN.CBRT.IN")
head(datos)
datosnew<-datos %>% rename(natalidad="SP.DYN.CBRT.IN") %>% arrange(year) %>% 
  na.omit()
View(datosnew)

#serie de tiempo
s1<-ts(datosnew$natalidad,start=c(1960))
plot(s1)
adf.test(s1) #no es estacionaria, no se toma en cuenta


#primera diferencia
ds1<-diff(s1)
plot(ds1)
adf.test(ds1) #no es estacionaria, no se toma en cuenta
acf(ds1)
pacf(ds1)


#diferencia2
ds2<-diff(ds1)
plot(ds2)
adf.test(ds2) #es estacionaria
acf(ds2)
pacf(ds2)


#suavizamiento exponencial
ls1<-log(s1)
plot(ls1)
adf.test(ls1) #no es estacionaria, no se toma en cuenta


#diferencia log
dls1<-diff(ls1)
plot(dls1)
adf.test(dls1) #no es estacionaria, no se toma en cuenta


#tentativo ARIMA(1,2,0), ARIMA(1,2,1), ARIMA(0,2,1)

#ARIMA(1,2,0)
m1<-arima(s1,order=c(1,2,0))
summary(m1) #AIC = -49.7
tsdiag(m1)
checkresiduals(m1) #H0: Residuos no autocorrelacionados
#se observa que se rechaza H0
autoplot(m1) #dentro del círculo unitario

#ARIMA(1,2,1)
m2<-arima(s1,order=c(1,2,1))
summary(m2) #AIC = -53.42
tsdiag(m2) 
checkresiduals(m2) #H0: Residuos no autocorrelacionados
#se rechaza H0
autoplot(m2)

#ARIMA(0,2,1)
m3<-arima(s1,order=c(0,2,1))
summary(m3) #AIC = -55.16
tsdiag(m3) 
checkresiduals(m3) #H0: Residuos no autocorrelacionados
#se rechaza H0
autoplot(m3)

#autoarima
m4<-auto.arima(s1)
summary(m4) #AIC = -55.16
checkresiduals(m4) #Se rechaza H0
autoplot(m4)

#autoarima log(s1)
m5<-auto.arima(ls1)
summary(m5) #AIC = -489.26
checkresiduals(m5) #No se rechaza H0
autoplot(m5)


#Por el principio de parsimonía se elige el modelo 3 ARIMA(0,2,1)
#que concuerda con el autoarima de la serie s1. Se elige sobre
#el modelo de log(s1) ya que no pasa la prueba de autocorrelación serial


############################################
#Variable: PIB per cápita (respecto a 2015)
datos1 <- WDI(country = "BO", indicator = "NY.GDP.PCAP.KD")
head(datos1)
datosnew1<-datos1 %>% rename(pibpc="NY.GDP.PCAP.KD") %>% arrange(year) %>% 
  na.omit()
View(datosnew1)

#serie de tiempo
st1<-ts(datosnew1$pibpc,start=c(1960))
plot(st1)
adf.test(st1) #no es estacionaria, no se toma en cuenta


#primera diferencia
dst1<-diff(st1)
plot(dst1)
adf.test(dst1) #no es estacionaria, no se toma en cuenta
acf(dst1)
pacf(dst1)


#diferencia2
dst2<-diff(dst1)
plot(dst2)
adf.test(dst2) #es estacionaria
acf(dst2)
pacf(dst2)


#suavizamiento exponencial
lst1<-log(st1)
plot(lst1)
adf.test(lst1) #no es estacionaria, no se toma en cuenta


#diferencia log
dlst1<-diff(lst1)
plot(dlst1)
adf.test(dlst1) #no es estacionaria, no se toma en cuenta

#diferencia2 log
dlst2<-diff(dlst1)
plot(dlst2)
adf.test(dlst2) #es estacionaria
acf(dlst2)
pacf(dlst2)


#Se elige una diferencia
#tentativo ARIMA(1,1,1), ARIMA(0,1,1), ARIMA(1,1,0)

#ARIMA(1,1,1)
mt1<-arima(st1,order=c(1,1,1))
summary(mt1) #AIC = 713.86
checkresiduals(mt1) #H0: Residuos no autocorrelacionados
#no se rechaza H0
tsdiag(mt1)
autoplot(mt1) #dentro del círculo unitario

#ARIMA(0,1,1)
mt2<-arima(st1,order=c(0,1,1))
summary(mt2) #AIC = 722.06
checkresiduals(mt2) #H0: Residuos no autocorrelacionados
#no se rechaza H0
tsdiag(mt2)
autoplot(mt2)

#ARIMA(1,1,0)
mt3<-arima(st1,order=c(1,1,0))
summary(mt3) #AIC = 718.15
checkresiduals(mt3) #H0: Residuos no autocorrelacionados
#no se rechaza H0
tsdiag(mt3)
autoplot(mt3)



#En este caso, al observar gráficamente por tsdiag se ve que el modelo 1
#y el modelo 3 no tienen autocorrelación serial, en este caso se elige
#el modelo 1 ARIMA(1,1,1) por tener un menor AIC. 

