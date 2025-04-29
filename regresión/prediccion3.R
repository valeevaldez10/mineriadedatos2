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

datos<-read_excel("excel/Bolivia - Exportaciones segun Actividad Economica y Producto por Año y Mes, 1992 - 2024.xlsx", range = "JG5:OG101")
variables<-read_excel("excel/Bolivia - Exportaciones segun Actividad Economica y Producto por Año y Mes, 1992 - 2024.xlsx", range = "B5:B101")
bd<-data.frame(variables,datos)
View(bd)

aux<-unique(bd$...1)

#se eligirá los productos Bananas aux[9] y Quinua aux[7]

#Trabajando primero con Bananas (seriem)

bdaux<-bd %>% rename(detalle=1) %>% filter(detalle==aux[9])
head(bdaux)

seriem<-bdaux %>% pivot_longer(cols = -detalle, names_to = "periodo", values_to = "valor")
mm<-rep(1:12,11)
head(seriem)
aa<-rep(2014:2024, each=12)
seriem$periodo<-my(paste0(mm,"-",aa))[-132]
head(seriem)


s1<-ts(seriem$valor,start = c(2014),freq=12)
plot(s1)
acf(s1,60)
pacf(s1,60)

d12s1=diff(s1,12)
plot(d12s1)
acf(d12s1,60)
pacf(d12s1,60)
auto.arima(s1)

#ARIMA(0,0,0)(0,1,1)
#ARIMA(1,0,0)(0,1,1)
#ARIMA(0,0,1)(0,1,1)
#ARIMA(0,0,0)(1,1,1)


#Trabajando con Quinua (seriem1)

bdaux1<-bd %>% rename(detalle=1) %>% filter(detalle==aux[7])
head(bdaux1)

seriem1<-bdaux1 %>% pivot_longer(cols = -detalle, names_to = "periodo", values_to = "valor")
mm<-rep(1:12,11)
head(seriem1)
aa<-rep(2014:2024, each=12)
seriem1$periodo<-my(paste0(mm,"-",aa))[-132]
head(seriem1)

st1<-ts(seriem1$valor,start = c(2014),freq=12)
plot(st1)
acf(st1,60)
pacf(st1,60)

dst1<-diff(st1)
plot(dst1)
acf(dst1,60)
pacf(dst1,60)

#ARIMA(1,1,1)

m1<-arima(st1,order=c(1,1,1))
summary(m1) #AIC = 473.54
tsdiag(m1) 
checkresiduals(m1) #H0: Residuos no autocorrelacionados
#se rechaza H0 al 10% de significancia
autoplot(m1)

#ARIMA(1,1,0)
m2<-arima(st1,order=c(1,1,0))
summary(m2) #AIC = 472.04
tsdiag(m2) 
checkresiduals(m2) #H0: Residuos no autocorrelacionados
#se rechaza H0 al 10% de significancia
autoplot(m2)


#ARIMA(0,1,1)
m3<-arima(st1,order=c(0,1,1))
summary(m3) #AIC = 473.54
tsdiag(m3) 
checkresiduals(m3) #H0: Residuos no autocorrelacionados
#se rechaza H0 al 10% de significancia
autoplot(m3)
