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
LIST(bda)[2]

#531040 filas
#dfc$InvoiceNo <- order(dfc$InvoiceNo,decreasing = FALSE)
#dfc$InvoiceNo<-paste0("T",dfc$InvoiceNo)
#bdc <- as(split(dfc$Description,dfc$InvoiceNo),"transactions")
#bdc



###Ejercicio 3
rm(list=ls())
library(arules)   #algoritmos
library(arulesViz)   #gráficos
library(readxl)  #excel
library(dplyr)
library(labelled)
library(tidyr)
library(haven)


load(url("https://github.com/AlvaroLimber/md1_2024/raw/refs/heads/main/_data/eh22.RData"))
#variable.names(eh22p)
#val_labels(eh22p)

bd <- eh22p %>% filter(s01a_03>=18)
bd<- eh22p %>% select(p0,ylab,niv_ed,s01a_09,cob_op) %>% 
  filter(!is.na(p0) & !is.na(ylab) & !is.na(niv_ed) & 
           !is.na(s01a_09) & !is.na(cob_op))

bd <- bd %>%mutate(dec_ylab = ntile(ylab, 10))

#base con factores
bd <- bd %>% select(-ylab)
bd <-bd %>% rename(pobreza=p0,
                   iden_indigena=s01a_09,
                   grupo_ocupacion=cob_op,
                   dec_ingreso=dec_ylab) %>% mutate(transaccion=row_number())
bd$pobreza<-paste0("pobreza=",bd$pobreza)
bd$niv_ed<-paste0("nivel_educativo=",bd$niv_ed)
bd$iden_indigena<-paste0("iden_indigena=",bd$iden_indigena)
bd$grupo_ocupacion<-paste0("grupo_ocupacion=",bd$grupo_ocupacion)
bd$dec_ingreso<-paste0("dec_ingreso=",bd$dec_ingreso)
bd$transaccion<-paste0("T",bd$transaccion)
bd

bd[,-ncol(bd)]<-lapply(bd[,-ncol(bd)],as.character)
lista_transacciones<-split(bd[,-ncol(bd)],bd$transaccion)
lista_transacciones<-lapply(lista_transacciones, unlist)
class(lista_transacciones)
bdt<-as(lista_transacciones,"transactions")
inspect(bdt)
bdt


#base con nombres de factores
#bd3 <- bd %>% mutate(pobreza=as_factor(p0),niv_ed=as_factor(niv_ed),
#                     ident_indig=as_factor(s01a_09),ocupacion=as_factor(cob_op))




