###EJERCICIO 2
rm(list=ls())
library(arules)   #algoritmos
library(arulesViz)   #gráficos
library(readxl)  #excel
library(dplyr)


#para el comercio electrónico
dfc <- read.csv("datasets/ecommerce.csv")
dfc <- dfc %>%
  filter(!is.na(Description), !is.na(InvoiceNo)) %>%
  distinct(InvoiceNo, Description)
dfa<-sample_n(dfc,size=10000)
#dfa$InvoiceNo <- as.character(dfa$InvoiceNo)
#dfa<- dfa[grepl("^[0-9]+$", dfa$InvoiceNo), ]
#dfa$InvoiceNo <- as.integer(dfa$InvoiceNo)
dfa <- subset(dfa, !grepl("^C", InvoiceNo))
dfa <- subset(dfa, !grepl("^A", InvoiceNo))
#dfa$InvoiceNo <- order(dfa$InvoiceNo,decreasing = FALSE)
dfa$InvoiceNo<-as.character(dfa$InvoiceNo)
dfa$InvoiceNo<-as.integer(dfa$InvoiceNo)
df_t <- dfa %>%group_by(InvoiceNo) %>%summarise(Items = list((unique(Description)))) %>%pull(Items)

dfa_t$InvoiceNo<-paste0("T",dfa_t$InvoiceNo)
trans<-split(dfa$Description,dfa$InvoiceNo)
trans[1:5]

bda<-as(trans, "transactions")
bda

##inspección bda
summary(bda)
inspect(bda[1:50])


###EJERCICIO 2
rm(list=ls())
library(arules)   #algoritmos
library(arulesViz)   #gráficos
library(readxl)  #excel
library(dplyr)


#para el comercio electrónico
aux <- read.csv("datasets/ecommerce.csv",fileEncoding="Latin1")
aux1<-dfc %>% group_by(Tran=InvoiceNo,Item=StockCode) %>% count()
bd1<-as(split(aux1$Item,aux1$Tran),"transactions")
LIST(bd)[1]

#algoritmo apriori
regla1<-apriori(bd1,parameter=
                  list(supp=0.01,conf=0.5))
summary(regla1)
inspect(sort(regla1,by="lift")[1:5])


regla2<-apriori(bd1,parameter=
                  list(supp=0.001,conf=0.5))
summary(regla2)
inspect(sort(regla2,by="lift")[1:5])

regla3<-apriori(bd1,parameter=
                  list(supp=0.02,conf=0.4))
summary(regla3)
inspect(sort(regla3,by="lift")[1:5])

#algoritmo eclat
me1<-eclat(bd1,parameter=list(supp=0.01,minlen=2))
summary(me1)
inspect(sort(me1,by="support")[1:5]) #devuelve los items más frecuentes

me2<-eclat(bd1,parameter=list(supp=0.001,minlen=2))
summary(me2)
inspect(sort(me2,by="support")[1:5]) #devuelve los items más frecuentes

me3<-eclat(bd1,parameter=list(supp=0.02,minlen=3))
summary(me3)
inspect(sort(me3,by="support")[1:5]) #devuelve los items más frecuentes


###Ejercicio 3
rm(list=ls())
library(arules)   #algoritmos
library(arulesViz)   #gráficos
library(readxl)  #excel
library(dplyr)
library(labelled)
library(tidyr)
library(haven)

load("datasets/eh23.RData")

bd <- eh23p %>% filter(s01a_03>=18)
bd<- bd %>% select(p0,ylab,niv_ed,s01a_09,s01b_11a,cob_op) %>% 
  filter(!is.na(p0) & !is.na(ylab) & !is.na(niv_ed) & 
           !is.na(s01a_09) & !is.na(cob_op))

bd <- bd %>%mutate(dec_ylab = ntile(ylab, 10))

#base con factores
bd <- bd %>% select(-ylab)
bd <-bd %>% rename(pobreza=p0,
                   iden_indigena=s01a_09,
                   grupo_ocupacion=cob_op,
                   dec_ingreso=dec_ylab,
                   migracion=s01b_11a)
bd1<-to_factor(bd)
eht<-transactions(bd1)

nived_r=apriori(eht,
               parameter=list(supp=0.01,conf=
                                0.2,minlen=2),appearance=
                 list(default="rhs",lhs="niv_ed=Primaria incompleta"))
summary(nived_r)
#Hay 9 reglas de asociación con 2 elementos
inspect(sort(nived_r,by="lift"))
#eligiendo los primeros 6 porque tienen un list>1
inspect(sort(nived_r,by="lift")[1:6])

nived_r1 <- sort(nived_r, by="lift", decreasing=TRUE)
nived_r1 <- nived_r1[1:6]
plot(nived_r1,method="graph") 
