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

#IPC mensual
#datos
datos <- read_excel("excel/practica/c01050301.xls",range = "A8:M56")
datos<-na.omit(datos)

#se excluyen datos de 1980 a 1985 debido a la hiperinflación 
#que ocurrió durante este periodo

datos1<-datos %>% rename(periodo=1) %>% filter(!(periodo %in% 1980:1985))
datos_serie<-datos1 %>% pivot_longer(cols = -periodo,names_to = "tiempo",values_to = "ipc")
datos_serie$ipc<-as.numeric(datos_serie$ipc)

#serie de tiempo
s1 <- ts(datos_serie$ipc,start=c(1986),freq=12)
plot(s1)
acf(s1)
adf.test(s1) #no es estacionaria

#primera diferencia
ds1<-diff(s1)
plot(ds1)
adf.test(ds1) #es estacionaria
acf(ds1,60)
pacf(ds1,60)
#por los gráficos no se observa estacionalidad, sin embargo
#por el plot la variación no parece aleatoria


#diferencia2
ds2<-diff(ds1)
plot(ds2)
adf.test(ds2) #es estacionaria
acf(ds2,60)
pacf(ds2,60)
#por los gráficos no se observa estacionalidad
#por el plot la variación es más aleatoria


#suavizamiento exponencial
ls1<-log(s1)
plot(ls1)
adf.test(ls1) #no es estacionaria

#diferencia log
dls1<-diff(ls1)
plot(dls1)
adf.test(dls1) #es estacionaria
#sin embargo, la variación en el plot no parece ser aleatoria

#diferencia2 log
dls2<-diff(dls1)
plot(dls2)
adf.test(dls2) #es estacionaria
#sin embargo, se observan picos en el plot


#tentativo ARIMA(0,2,1) o ARIMA(1,2,1)

#ARIMA(0,2,1)
m1<-arima(s1,order=c(0,2,1))
summary(m1) #AIC = 246.7
tsdiag(m1)
checkresiduals(m1) #H0: Residuos no autocorrelacionados
#se observa que se rechaza H0
autoplot(m1) #dentro del círculo unitario

#ARIMA(1,2,1)
m2<-arima(s1,order=c(1,2,1))
summary(m2) #AIC = 228.1
tsdiag(m2) 
checkresiduals(m2) #H0: Residuos no autocorrelacionados
autoplot(m2)


#autoarima
m3<-auto.arima(s1)
summary(m3)
checkresiduals(m3) #Se rechaza H0
autoplot(m3)

#autoarima ls1
m4<-auto.arima(ls1)
summary(m4)
checkresiduals(m4) #Se rechaza H0
autoplot(m4) #salen del círculo unitario

#Por el principio de parsimonía, se toma el primer modelo
#ARIMA(0,2,1) y ya que pasa la prueba de Ljung-Box.

#forecast
pred<-predict(m1, n.ahead = 12) 
pred
plot(forecast(m1,h=12,level=95))  #gráfico
