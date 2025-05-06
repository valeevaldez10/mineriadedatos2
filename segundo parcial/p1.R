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

datos<-read_excel("excel/Bolivia - Exportaciones segun Actividad Economica y Producto por Año y Mes, 1992 - 2024.xlsx", sheet=2,range = "C5:OG101")
variables<-read_excel("excel/Bolivia - Exportaciones segun Actividad Economica y Producto por Año y Mes, 1992 - 2024.xlsx", sheet=2,range = "B5:B101")
bd<-data.frame(variables,datos)
View(bd)

aux<-unique(bd$...1)

#se eligirá los productos Oro metálico aux[44] y azúcar aux[56]

#Trabajando primero con Oro metálico (seriem)

bdaux<-bd %>% rename(detalle=1) %>% filter(detalle==aux[44])
head(bdaux)

seriem<-bdaux %>% pivot_longer(cols = -detalle, names_to = "periodo", values_to = "valor")
mm<-rep(1:12,11)
head(seriem)
aa<-rep(1992:2024, each=12)
seriem$periodo<-my(paste0(mm,"-",aa))[-396]
seriem$valor[is.na(seriem$valor)] <- 0
View(seriem)
str(seriem)


#a) Objeto ts
s1<-ts(seriem$valor,start = c(1992,1),freq=12)
plot(s1)
acf(s1,60)
pacf(s1,60)


#b) Evalue la estacionariedad de la serie
#Como se observa por el gráfico de s1 y el respecto acf y pacf
#la serie no es estacionaria. Ya que su acf no cae y la serie 
#tiene alta variación. Por eso se analizará las diferencias
#Tampoco se observa estacionalidad por lo que solo se va a diferenciar
#1 vez bajo un modelo ARIMA.

ds1=diff(s1)
plot(ds1)
acf(ds1,60)
pacf(ds1,60)
adf.test(ds1) #se RH0, es estacionaria

ds2=diff(ds1)
plot(ds2)
acf(ds2,60)
pacf(ds2,60)
adf.test(ds2) #se RH0, es estacionaria

#Analizando con logaritmo sumando una constante 1
ls1=log(s1+1)
plot(ls1)
acf(ls1,60)
pacf(ls1,60)
adf.test(ls1) #No se RH0, no es estacionaria

#primera diferencia
dls1=diff(ls1)
plot(dls1)
acf(dls1,60)
pacf(dls1,60)
adf.test(dls1) #No se RH0, no es estacionaria


#Al aplicar diferencia pareciera que el modelo es
#estacionario con una sola diferencia, además que pasa
#el test de Dickey-Fuller
#Sin embargo, cuando se aplica una diferencia al modelo con 
#logaritmo la variación es menor y se parece más a ruido blanco
#por ese motivo se probará tanto para la primera diferencia
#como para la primera diferencia con logaritmo
#Además, no se observa estacionalidad.



#c) Proponga modelo

#1. Modelo con una diferencia
#Por el ACF se observa que p puede ser 1 o 2
#Por el PACF se observa que q puede ser 1 o 2


#Se probarán:
#ARIMA(1,1,1)
#ARIMA(1,1,2)
#ARIMA(0,1,1)
#ARIMA(0,1,2)


#ARIMA(1,1,1)
m1<-arima(s1,order=c(1,1,1))
summary(m1) #AIC = 523.05
tsdiag(m1)
checkresiduals(m1) #H0: Residuos no autocorrelacionados
#se observa que se rechaza H0
autoplot(m1) #dentro del círculo unitario

#Por la prueba y el tsdiag se observa que los residuos presentan autocorrelación

#ARIMA(1,1,2)
m2<-arima(s1,order=c(1,1,2))
summary(m2) #AIC = 525
tsdiag(m2)
checkresiduals(m2) #H0: Residuos no autocorrelacionados
#se observa que se rechaza H0
autoplot(m2) #dentro del círculo unitario

#ARIMA(0,1,1)
m3<-arima(s1,order=c(0,1,1))
summary(m3) #AIC = 526.07
tsdiag(m3)
checkresiduals(m3) #H0: Residuos no autocorrelacionados
#se observa que se rechaza H0
autoplot(m3) #dentro del círculo unitario


#ARIMA(0,1,2)
m4<-arima(s1,order=c(0,1,2))
summary(m4) #AIC = 524.16
tsdiag(m4)
checkresiduals(m4) #H0: Residuos no autocorrelacionados
#se observa que se rechaza H0
autoplot(m4) #dentro del círculo unitario

#AUTOARIMA
m6<-auto.arima(s1)
summary(m6)
checkresiduals(m6)


#Ninguno de los modelos funciona ya que en todos los casos
#se prueba que los residuos están autocorrelacionados


#2. Modelo con una diferencia-logaritmo
#Se observa que el ACF se corta en 2 rezagos
#y el PACF cae exponencialmente por lo que
#se probará como un MA

#ARIMA(0,1,1)
#ARIMA(0,1,2)


#ARIMA(0,1,1)
ml1<-arima(ls1,order=c(0,1,1))
summary(ml1) #AIC = -384.99
tsdiag(ml1)
checkresiduals(ml1) #H0: Residuos no autocorrelacionados
#se observa que se rechaza H0
autoplot(ml1) #dentro del círculo unitario


#ARIMA(0,1,2)
ml2<-arima(ls1,order=c(0,1,2))
summary(ml2) #AIC = -385
tsdiag(ml2)
checkresiduals(ml2) #H0: Residuos no autocorrelacionados
#se observa que NO RH0
autoplot(ml2) #dentro del círculo unitario


#AUTOARIMA
ml3<-auto.arima(ls1)
summary(ml3)
checkresiduals(auto.arima(ls1))

#Como se observa la propuesta por el auto arima es lo mismo que el modelo
#logaritmo ml2 un ARIMA (0,1,2) y que está vez pasa la prueba de Ljung Box
#Por lo tanto el modelo es ARIMA(0,1,2) con log(s1+1)

#forecast
pred<-predict(ml2, n.ahead = 12) 
pred
plot(forecast(ml2,h=12,level=95))  #gráfico

#Se pronostica que el oro va a mantenerse en exportación en peso

#Trabajando con Azúcar (seriem1)

bdaux1<-bd %>% rename(detalle=1) %>% filter(detalle==aux[56])
head(bdaux1)

seriem1<-bdaux1 %>% pivot_longer(cols = -detalle, names_to = "periodo", values_to = "valor")
mm<-rep(1:12,11)
head(seriem1)
aa<-rep(1992:2024, each=12)
seriem1$periodo<-my(paste0(mm,"-",aa))[-396]
seriem1$valor[is.na(seriem1$valor)] <- 0
View(seriem1)
str(seriem1)

#A) Objeto ts
st1<-ts(seriem1$valor,start = c(1992),freq=12)
plot(st1)
acf(st1,80)
pacf(st1,80)
adf.test(st1) #Es estacionaria


#b) Analice estacionariedad
dst1<-diff(st1)
plot(dst1)
acf(dst1,60)
pacf(dst1,60)
adf.test(dst1) #Es estacionaria

lst1<-log(st1+1)
plot(lst1)
acf(lst1,60)
pacf(lst1,60)
adf.test(lst1) #Es estacionaria

#Se observa que la serie sin diferenciar ya es estacionaria
#pasando la prueba de Dickey Fuller, sin embargo la variación
#parece ser alta pese a que no existe tendencia.
#Al diferenciar una vez y al aplicar el logaritmo también se
#obtiene una serie estacionaria
#También se observa estacionalidad pero en poca medida

#Se va a probar con una diferencia


#c) Proponga un modelo ARIMA()SARIMA()

#ARIMA(0,1,1) ya que se corta en ACF y cae en PACF

mod<-arima(st1,order=c(0,1,1))
summary(mod) #AIC=7814.82
checkresiduals(mod) #no pasa la prueba

#ARIMA(0,1,2) ya que se corta en ACF y cae en PACF

modd<-arima(st1,order=c(0,1,2))
summary(modd) #AIC=7816
checkresiduals(modd) #no pasa la prueba

#AUTOARIMA
mod1<-auto.arima(st1)
summary(mod1)
checkresiduals(mod1) #pasa la prueba

mod2<-auto.arima(lst1)
summary(mod2)
checkresiduals(mod2) #pasa la prueba

#Haciendo el autoarima, tanto con la serie normal como con el logaritmo, se observa que este pasa la prueba
#de Ljung Box teniendo así un modelo sin autocorrelación en los 
#errores. Se escogerá el mod1, propio del auto arima de st1
#ya que no necesita la transformación logarítmica y además
#por el principio de parsimonía
#Así el modelo es: SARIMA(1,0,0)(1,0,0)[12]

#forecast
pred1<-predict(mod1, n.ahead = 12) 
pred1
plot(forecast(mod1,h=12,level=95))  #gráfico


