###Ejercicio 1
library(arules)   #algoritmos
library(arulesViz)   #gráficos
library(readxl)  #excel
library(dplyr)

#ejercicio 1

#para la panadería
dfp<-read.csv("datasets/BreadBasket_DMS.csv")
dfp %>% filter(Item=="NONE")
dfp<-dfp %>% select(-c(1:2)) %>% filter(Item!="NONE")
dfp$Transaction<-paste0("T",dfp$Transaction)
##dfp1<-dfp[!duplicated(dfp), ]
bdp<-as(split(dfp$Item,dfp$Transaction),"transactions")
bdp
##bdp1<-as(split(dfp1$Item,dfp1$Transaction),"transactions")
##bdp1

#para el comercio electrónico




