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

#Trabajando con Quinua (seriem1)

bdaux1<-bd %>% rename(detalle=1) %>% filter(detalle==aux[7])
head(bdaux1)

seriem1<-bdaux1 %>% pivot_longer(cols = -detalle, names_to = "periodo", values_to = "valor")
mm<-rep(1:12,11)
head(seriem1)
aa<-rep(2014:2024, each=12)
seriem1$periodo<-my(paste0(mm,"-",aa))[-132]
head(seriem1)
