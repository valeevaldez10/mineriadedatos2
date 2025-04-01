library(dplyr)
library(haven)
library(labelled)
library(ggplot2)
library(GGally)

load("datasets/eh23.RData")
#poblacion objetivo
bd<-eh23p %>% filter(s01a_03>=25 & is.na(ylab)==F)
#educación, área, sexo, edad, experiencia, horas trabajadas
bd<-bd %>% select(ylab,aestudio, area, s01a_02, s01a_03, s01a_10,
cmasi,tothrs,totper,ynolab, depto)
bd<-bd %>% na.omit()
bd<-bd %>% to_factor()
ggpairs(bd[,1:4])
################
#ylab
summary(bd$ylab)
ggplot(bd,aes(log(ylab)))+geom_histogram()
ggplot(bd,aes(log(ylab)))+geom_boxplot()
#X
ggplot(bd,aes(aestudio))+geom_bar()
ggplot(bd,aes(s01a_03))+geom_bar()
ggplot(bd,aes(area))+geom_bar()
ggplot(bd,aes(to_factor(s01a_02)))+geom_bar()
ggpairs(bd[,1:5]) #nos da una matriz de correlaciones
#modelo
m1<-lm(ylab~aestudio,data=bd)
summary(m1)
plot(m1)

#valores atípicos
m2<-lm(log(ylab)~aestudio,data=bd)
summary(m2)
plot(m2)  

bd[9926,c("ylab","aestudio")]
#se ve un dato atípico porque no tiene años de educacion
#y su salario es igual a 23243 Bs.
bd[581,c("ylab","aestudio")]
bd[9223,c("ylab","aestudio")]

#modelo con años de estudio como factor
m3<-lm(log(ylab)~to_factor(aestudio),data=bd)
summary(m3)
plot(m3)