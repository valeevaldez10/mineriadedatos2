#clase 24-4-25
rm(list=ls())
library(wbstats)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggfortify)
library(plotly)
gdp<-wb_search("gdp")
tax<-wb_search("tax")
wb_search(pattern = "price index") 
bd<-wb_data("NY.GDP.MKTP.CD", country = "BO", lang = "es")
bd2<-wb_data("NY.GDP.MKTP.CD", country = c("BO","PE"), lang = "es")
class(bd$date)
bd<-bd %>% rename(y=5)
ggplot(bd, aes(date, y)) + geom_line()


#ts(bd$y, frequency = 1, start = 1960)
gdp<-ts(bd$y, frequency = 1, start = c(1960,1))
plot(gdp)
autoplot(gdp)
ggplotly(autoplot(gdp))
#diferencia
dgdp<-diff(gdp)
plot(dgdp)
#TASA DE CRECIMIENTO
plot(100*diff(log(gdp)))
Qt<-100*(diff(gdp)/bd$y[-1])
points(Qt, col="red", type = "l")

#base air passengers
AirPassengers
plot(AirPassengers)
autoplot(AirPassengers)


#aplicando prueba de estacionariedad
library(tseries)
gdp<-na.omit(gdp)
adf.test(gdp)
dgdp<-na.omit(dgdp)
adf.test(dgdp)
Qt<-na.omit(Qt)
adf.test(Qt)
adf.test(diff(gdp,differences = 2))
plot(AirPassengers)
adf.test(AirPassengers)


acf(gdp)
acf(dgdp)
acf(AirPassengers)
plot(decompose(AirPassengers))
s1<-diff(AirPassengers, lag = 12)
acf(s1)
plot(decompose(s1))
#Función de autocorrelación parcial
pacf(AirPassengers)



#suavizamiento exponencial
################CLASE 24-4-25

library(forecast)
m0<-ets(gdp)
summary(m0)
plot(m0)
m0p<-forecast(m0, h=5) #h indica la predicción a 5 años en el futuro
m0p

#nuestra predicción tiene bandas muy altas lo que indica alta
#variabilidad en la predicción, por lo cual no es muy certera
plot(m0p)
m02<-ets(diff(gdp,differences = 2))
#tampoco es buena
plot(forecast(m02,h=5))

m1<-ets(AirPassengers)
summary(m1)
plot(m1)

#predicción para 3 meses
#aquí si existe una buena predicción
plot(forecast(m1, h=36))

#predicción hacia doce meses
plot(forecast(m1, h=12), xlim=c(1960, 1965))


######## TRANSFORMACIÓN DE TABLA

rm(list = ls())
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(forecast)
library(plotly)
library(tseries)

#elige todas las celdas
bd<-read_excel("excel/c01030401.xls",range = "A9:AM44")
aux<-unique(bd$`D E T A L L E`)

#se elimina abrev y se elige la variable "Total Impuestos"
bdaux<-bd %>% rename(detalle=1) %>% filter(detalle==aux[2]) %>% select(-ABREV.)

#se traspone la tabla para tenerlo en formato de series de tiempo
#elige a detalle como pivote y pone los nombres como periodo y la variable como valor
serie<-bdaux %>% pivot_longer(!detalle, names_to = "periodo", values_to = "valor")

#limpieza de texto
serie<-serie %>% 
  mutate(periodo=str_replace(periodo, fixed("(1)"),""), 
         periodo=str_replace(periodo, fixed("(2)"),""),
         periodo=str_replace(periodo, fixed("(p)"),""),
         periodo=str_replace(periodo, fixed("*"),""),
         periodo=str_replace(periodo, fixed("p)"),""),
         periodo=as.numeric(periodo))

ggplot(serie, aes(x=periodo, y=valor))+geom_line()

g1<-ggplot(serie, aes(x=periodo, y=valor))+geom_line(color="darkgreen")+theme_minimal()

g1
ggplotly(g1)

#con 3 diferencias pasa el test de estacionariedad
adf.test(diff(s1, differences = 3))
acf(diff(s1))
m1<-ets(s1)

#el modelo de suavizamiento exponencial no es suficiente para este caso
#ya que las bandas son muy anchas
plot(forecast(m1,5))

########RESOLVER CON BASE DE DATOS EXPORTACIONES SEGÚN PRODUCTO


bd<-read_excel("excel/Bolivia - Exportaciones segun Actividad Economica y Producto por Año y Mes, 1992 - 2024.xlsx", range = "B5:OG101")

aux<-unique(bd$...1)

bdaux<-bd %>% rename(detalle=1) %>% filter(detalle==aux[2])

seriem<-bdaux %>% pivot_longer(!detalle, names_to = "periodo", values_to = "valor")
View(seriem)
mm<-rep(1:12,33)
aa<-rep(1992:2024, each=12)
seriem$periodo<-my(paste0(mm,"-",aa))[-396]

ggplot(seriem, aes(x=periodo, y=valor))+geom_line()

g1<-ggplot(seriem, aes(x=periodo, y=valor))+geom_line(color="darkgreen")+theme_minimal()

g1
ggplotly(g1)

s2<-ts(seriem$valor, frequency = 12, start = c(1992, 1))
plot(s2)
adf.test(s2)
adf.test(diff(s2))
acf(s2)
acf(diff(s2))
plot(decompose(s2))

plot(decompose(diff(s2)))

m2<-ets(s2)
summary(m2)
pp<-forecast(m2,h=36)


#no son tan buenas predicciones como se ve la banda de confianza
#es muy amplia

plot(forecast(m2,h=24), xlim=c(2015,2027))
plot(forecast(m2,h=12))
#aplicando diferencia 
s2_diff<-diff(s2)
m3<-ets(s2_diff)
fc_diff<-forecast(m3,h=36)
plot(fc_diff)



###Ejercicio exportaciones
# Obtener los valores originales antes de la diferenciación
fc_original<-cumsum(fc_diff$mean)+tail(s2, 1)[1]

s3<-ts(c(s2,fc_original), frequency = 12, start = c(1992, 1))

autoplot(s3)+geom_vline(xintercept = 2024, col="red")

impuestos<-s1
exportaciones<-s2
save(gdp, impuestos, exportaciones, file="datasets/seriesBO.RData")



############ARIMA
rm(list=ls())
library(dplyr)
library(forecast)#ARIMA
library(ggplot2)
library(plotly)
library(tseries)#TEST
library(lmtest)#TEST
library(writexl)#EXCEL
load("datasets/seriesBO.RData")
#Manual
plot(gdp)
gdp %>% autoplot() %>% ggplotly()
gdp %>% diff() %>% plot()

gdp %>% diff() %>% adf.test()

gdp %>% diff(differences = 2) %>% adf.test()

gdp %>% log() %>% diff(differences = 2) %>% adf.test()

gdp %>% log() %>% diff(differences = 2) %>% ggtsdisplay()

#nos muestra el nro. diferencias para que sea estacionaria que son 2
#luego muestra gráfico de serie y acf y pacf
#por acf se ve que el candidato para p es 2
#por pacf se puede ver que el candidato para q es 4

gdp %>% diff(differences = 2) %>% ggtsdisplay() #determina parámetros de arima


m1<-arima(gdp, order = c(2,2,4))
summary(m1) #se tiene un AIC igual a 2775.84

#como p-valor es 0.19 entonces pasa el test
#por lo tanto se puede decir que los residuos no están
#autocorrelacionados

checkresiduals(m1)# H0: Residuos no autocorrelacionados

#las variables relevantes en el modelo son ar2, ma1, ma2
#podríamos prescindir de ma3 y ma4 pero no de ar1 ya que debe existir
#si hay ar2
coeftest(m1)
autoplot(m1)

#forecast para 5 años
m1f<-forecast(m1, h=5) # LISTA

#este modelo ha reducido la variación en las bandas de confianza
#se puede controlar mejor la predicción de gdp vs suavizamiento exponencial

autoplot(m1f)
#automático
m1a<-auto.arima(gdp)
m1al<-auto.arima(log(gdp))
summary(m1a)
checkresiduals(m1a)# H0: Residuos no autocorrelacionados
autoplot(m1a)
m1af<-forecast(m1a, h=5) # LISTA
autoplot(m1af)
gdpf<-cbind(gdp, fitted(m1), fitted(m1a))
autoplot(gdpf)
write_xlsx(list("arima224"=data.frame(m1f),
                "arima222"=data.frame(m1af)), "arima.xlsx")
################################################################
m2a<-auto.arima(impuestos)
summary(m2a)
checkresiduals(m2a)
autoplot(m2a)
aux<-forecast(m2a, h=5)
autoplot(forecast(m2a, h=5))+xlim(c(2010,2026))

m2al<-auto.arima(log(impuestos))
summary(m2al)
checkresiduals(m2al)
autoplot(m2al)
aux<-forecast(m2al, h=5)
mean(aux$residuals)
aux$lower
aux$upper
aux<-data.frame(aux)
auxts<-ts(exp(aux$Point.Forecast), frequency = 1, start = 2024)
autoplot(cbind(impuestos, auxts))
################################################################
m3a<-auto.arima(log(exportaciones))
summary(m3a)
checkresiduals(m3a)
autoplot(m3a)
autoplot(forecast(m3a, h=24))
################################################################
m4a<-auto.arima(AirPassengers)
summary(m4a)
checkresiduals(m4a)
autoplot(m4a)
autoplot(forecast(m4a, h=24))
###########################
#ARIMAX
###########################
arima(impuestos, order=c(0,1,1), xreg = gdp)
impuestos
gdp
gdp87<-window(gdp, start=1987)
cor(gdp87, impuestos)

mx<-arima(impuestos, order=c(0,1,1), xreg = gdp87)
summary(mx)
AIC(mx)
AIC(m2a)
coeftest(mx)
checkresiduals(mx)
autoplot(mx)
mx

forecast(mx, h=5, xreg = m1af$mean)

aux<-predict(mx, newxreg = m1af$mean)
autoplot(cbind(impuestos,aux$pred))
autoplot(forecast(m2a, h=5))
autoplot(forecast(m1a, h=5))
