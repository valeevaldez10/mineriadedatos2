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
#dfp1<-dfp[!duplicated(dfp), ]
bdp<-as(split(dfp$Item,dfp$Transaction),"transactions")
bdp
#bdp1<-as(split(dfp1$Item,dfp1$Transaction),"transactions")
#bdp1

#para el comercio electrónico
dfc <- read.csv("datasets/ecommerce.csv")
dfc <- dfc %>%
  filter(!is.na(Description), !is.na(InvoiceNo)) %>%
  distinct(InvoiceNo, Description)
dfa<-dfc
dfa$InvoiceNo <- as.character(dfa$InvoiceNo)
dfa<- dfa[grepl("^[0-9]+$", dfa$InvoiceNo), ]
dfa$InvoiceNo <- as.integer(dfa$InvoiceNo)
dfa$InvoiceNo <- order(dfa$InvoiceNo,decreasing = FALSE)
dfa$InvoiceNo<-paste0("T",dfa$InvoiceNo)
bda<-as(split(dfa$Description,dfa$InvoiceNo),"transactions")
bda


#531040 filas
#dfc$InvoiceNo <- order(dfc$InvoiceNo,decreasing = FALSE)
#dfc$InvoiceNo<-paste0("T",dfc$InvoiceNo)
#bdc <- as(split(dfc$Description,dfc$InvoiceNo),"transactions")
#bdc



