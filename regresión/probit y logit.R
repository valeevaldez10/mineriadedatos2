## Probit y Logit


rm(list=ls())
library(haven)
library(dplyr)
library(margins)# efectos marginales (obtener relación X-Y)
library(labelled)
library(pscl)# Aproximación al ajuste del modelo
library(car)
library(mixlm)
#data
load("datasets/eh23.RData")
eh23p$p0
eh23p$pext0
unique(eh23p$zext)

bd<-eh23p %>% mutate(pobreza=(p0==1)) %>% 
  filter(s01a_03>=25)

bd<-bd %>% mutate(aestudio=to_factor(aestudio),
                  rural=(area==2),
                  depto=to_factor(depto),
                  mujer=(s01a_02==2),
                  edad=s01a_03,
                  ecivil=to_factor(s01a_10),
                  totper
) %>% select(pobreza, aestudio, rural, depto, mujer, edad, ecivil, totper)

table(bd$pobreza)
prop.table(table(bd$pobreza))*100

m1<-glm(pobreza~., 
        data=bd, family=binomial(link="logit"))
m2<-glm(pobreza~., 
        data=bd, family=binomial(link="probit"))

mm1<-margins(m1)
smm1<-summary(mm1)
plot(mm1)

mm2<-margins(m2)
smm2<-summary(mm2)
plot(mm2)

round(pR2(m1),3)
round(pR2(m2),3)

outlierTest(m1)
bd[18708,] %>% View()
influenceIndexPlot(m1, vars="Cook")
