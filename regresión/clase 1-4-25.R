library(dplyr)
library(haven)
#install.packages("labelled")
library(labelled)
library(ggplot2)
#install.packages("GGally")
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

#modelo con todas las variables independientes
m4<-lm(log(ylab)~ .,data=bd) 
summary(m4)

##########
#install.packages("car")
library(car)
#install.packages("mixlm")
library(mixlm)

residualPlots(m4)  #H0: NO se requiere X^2
#se observa que es necesario para aestudio,s01a_03,tothrs, ynolab
outlierTest(m4)


aux<-outlierTest(m4)
aux<-names(aux$rstudent)
aux
#explorando si en verdad son datos atípicos

#para ver los datos
bd %>% slice(as.numeric(aux)) %>% View()

bd1<-bd %>% slice(-as.numeric(aux))
bd1<-bd1 %>% mutate(aes2=aestudio^2,
                    aestudio=as.factor(aestudio),
                    edad2=s01a_03^2,
                    th2=tothrs^2,
                    logynl=log(ynolab+10)) %>% select(-ynolab)
influenceIndexPlot(m4,vars="Cook")

bd[c(6314,13395), ] %>% View()

m5<-lm(log(ylab)~.,data=bd1)
summary(m5)
m6<-step(m5)
summary(m6)
#no elimina las variables de tipo factor que no aportan al modelo, toma toda la variable
m7<-backward(m5)
summary(m7)
#sucede el mismo problema

#lo mejor es transformar a dummies por nuestra cuenta
install.packages("fastDummies")
library(fastDummies)
bd2<-dummy_cols(bd1,remove_first_dummy=T,remove_selected_columns = T)
bd2

md1<-lm(log(ylab)~.,data=bd2)
md2<-step(md1)
md3<-backward(md1,alpha=0.05)
summary(md2)
summary(md3)

########### paso 5
ee<-residuals(m5)
hist(ee)
install.packages("nortest")
library(nortest)  #H0: Existe normalidad
ad.test(ee)
lillie.test(ee)

#tendría que parecerse a una normal estándar, pero como se
#observa no parece mucha diferencia, quizás sobreestimara
#los datos
plot(density(scale(ee)))
curve(dnorm,add=T,col="red")
#Colinealidad
##Variance Inflation Factors
vif(m5)
sqrt(vif(m5))>2

# Verificar si la varianza es constante (homocedástico) o no (heterocedástico)
library(lmtest)
bptest(m5) # H0:  Homocedasticidad

#corrigiendo 
library(rms)
model_1 = lm(log(ylab)~s01a_02+s01a_03+aestudio,data=bd1)
bptest(model_1)
model_2 = ols(log(ylab)~s01a_02+s01a_03+aestudio ,data=bd1,x=T,y=T)
bptest(model_2)
summary(model_1)
robcov(model_2)
```

## Predicciones

```{r}
ypred<-exp(predict(m5))
bd1$ylab[1]
ypred[1]
#base de datos nuevo
bdp<-data.frame(
  aestudio="17",
  area="Urbana",  
  s01a_02="2. Mujer",
  s01a_03=20, 
  tothrs=50,  
  totper=1,   
  ynolab=0,   
  cmasi="Asiste",    
  s01a_10="1. SOLTERO/A",
  depto="La Paz" 
)
exp(predict(m5,bdp))


