
rm(list=ls())
library(dplyr)
##################
##código R básico
N<-nrow(iris)
set.seed(1256)#semilla aleatoria
ss<-sample(c(T,F),N,replace = T, prob = c(0.7,0.3))
table(ss)/N
#indexación
bdtrain<-iris[ss, ]
bdtest<-iris[!ss, ]
#dplyr
bdtrain<-iris %>% filter(ss==T)
bdtest<-iris %>% filter(ss==F)
##################
##código R básico (exacto)
N<-nrow(iris)
set.seed(759)
ss1<-order(runif(N))<=(N*0.7)
table(ss1)/N
#dplyr
bdtrain<-iris %>% filter(ss1==T)
bdtest<-iris %>% filter(ss1==F)
##################
##DPLYR + un ID/KEY/FOLIO
iris %>% mutate(aux=1,id=cumsum(aux))#o1
iris$id<-1:N#o2
set.seed(1348)
bdtrain<-iris %>% sample_frac(0.7)
bdtest<-iris %>% anti_join(bdtrain,by="id")
#EXPLORATORIO
bdaux<-bdtrain %>% mutate(tipo="E") %>% 
  bind_rows(bdtest %>% mutate(tipo="P"))
summary(bdtrain$Sepal.Length)
summary(bdtest$Sepal.Length)
library(ggplot2)
ggplot(bdaux,aes(Sepal.Length,col=tipo))+
  geom_density()
ggplot(bdaux,aes(Petal.Length,col=tipo))+
  geom_density()
ggplot(bdaux,aes(Sepal.Width,col=tipo))+
  geom_density()
ggplot(bdaux,aes(Petal.Width,col=tipo))+
  geom_density()
#Analítico
t.test(Sepal.Length~tipo ,data=bdaux)
t.test(Petal.Length~tipo ,data=bdaux)
t.test(Sepal.Width~tipo ,data=bdaux)
t.test(Petal.Width~tipo ,data=bdaux)

ks.test(Sepal.Length~tipo ,data=bdaux)
ks.test(Petal.Length~tipo ,data=bdaux)
ks.test(Sepal.Width~tipo ,data=bdaux)
ks.test(Petal.Width~tipo ,data=bdaux)
table(bdtrain$Species)/105
table(bdtest$Species)/45
chisq.test(table(bdaux$Species,bdaux$tipo))

  
#######2. Logit / Probit
rm(list=ls())
library(dplyr)
library(caret)
#0. Identificar la variable (1/0) que se requiere clasificar, definir covariables para construir el modelos
clase<-unique(iris$Species)
iris<-iris %>% mutate(y = Species==clase[2]) 
#crear la base de datos de entrenamiento y test (70/30). Omitir la variable Species (5 min)

iris$id<-1:nrow(iris)
set.seed(805)
bdtrain<-iris %>% sample_frac(0.7) 
bdtest<-iris %>% anti_join(bdtrain,by="id") 
bdtrain<- bdtrain %>% select(-id, -Species)
bdtest<- bdtest %>% select(-id, -Species)
#nota: se debe verificar el balance de las bases.

#1. Especificar el modelo (logit/probit)
m1<-glm(y ~ . , data=bdtrain, family = binomial(link="logit"))
m2<-glm(y ~ . , data=bdtrain, family = binomial(link="probit"))

#2. Identificar las variables significativas
step(m1)
step(m2)

#3. Construir el modelo con variables significativas
m3<-step(m1)
m4<-step(m2)

#4. Predecir la clase de pertenencia en la base de test ($prob>0.5$)

prob_l<-predict(m3, bdtest, type="response")
prob_p<-predict(m4, bdtest, type="response")

#5. Observar la clasificación dada con base a la probabilidad fijada

prob_l >0.5
prob_p >0.5

#6. Comparar lo observado y lo predicho (bdtest)

bdtest<-bdtest %>% mutate(yl=(prob_l >0.5),
                          yp=(prob_p >0.5))

tl<-table(bdtest$y, bdtest$yl)
tp<-table(bdtest$y, bdtest$yp)

#7. Generar la matriz de confusión (librería caret)

confusionMatrix(tl)
confusionMatrix(tp)

## Naive Bayes


library(dplyr)
data(iris)
iris$id<-1:nrow(iris)
set.seed(1107)
bdtrain<-iris %>% sample_frac(0.7) 
bdtest<-iris %>% anti_join(bdtrain,by="id") 
bdtrain<- bdtrain %>% select(-id)
bdtest<- bdtest %>% select(-id)

#1. Cargar la librería *e1071* y *naivebayes* emplear la función naiveBayes para construir el clasificador


library(e1071)
library(naivebayes)
#naiveBayes(Y~.,data=)
#naive_bayes(Y~.,data=)
m1<-naiveBayes(Species ~ . , data=bdtrain )
m2<-naive_bayes(Species ~ . , data=bdtrain)

#2. Explorar los resultados


m1
summary(m1)
m2
summary(m2)
plot(m2)


#3. Predecir los resultados en la base de testeo


c1<-predict(m1, bdtest, "class")
c2<-predict(m2, bdtest, "class")
table(c1,c2)
bdtest$c1<-c1
bdtest$c2<-c2
View(bdtest)


#4. Realizar la matriz de confusión


library(caret)
t1<-table(bdtest$Species, bdtest$c1)
confusionMatrix(t1)

## k-nearest neighbors - KNN (Vecino más cercano)


rm(list=ls())
library(class)
library(dplyr)
library(caret)
bd<-iris
#bases de entrenamiento y test
set.seed(1322)
aux<-sample(c(1,2),nrow(bd),replace = T,prob = c(0.7,0.3))
bdtrain<-bd %>% filter(aux==1)
bdtest<-bd %>% filter(aux==2)

#preparando las bases de datos
#cl
clase_train<-bdtrain$Species
#train
bdtrain<-bdtrain %>% select(-Species)
#cl
clase_test<-bdtest$Species
#test
bdtest<-bdtest %>% select(-Species)

# algoritmo
class(clase_train)
k<-round(sqrt(nrow(bdtrain)),0)
knn(bdtrain,bdtest,clase_train,k,prob = F)
pred_clase<-knn(bdtrain,bdtest,clase_train,k)
t1<-table(clase_test, pred_clase)
confusionMatrix(t1)
#######Normalizando
bdtrain_n<-scale(bdtrain)
bdtest_n<-scale(bdtest)

pred_clase<-knn(bdtrain_n,bdtest_n,clase_train,k)
t2<-table(clase_test,pred_clase)
confusionMatrix(t2)


## Árboles de decisión y CART (Classification and regression Tree - CART)

### Árboles de decisión


library(dplyr)
px<-prop.table(table(iris$Species))
entropia<-(-1)*sum(px*log2(px))

#regla
aux<-iris%>% filter(Sepal.Length>5.7)
aux1<-iris%>% filter(Sepal.Length<=5.7)
aux
aux1

px1<-prop.table(table(aux$Species))
(-1)*sum(px1*log2(px1))
px1

px2<-prop.table(table(aux1$Species))
(-1)*sum(px2*log2(px2))
px2



#CARET
rm(list=ls())
install.packages("rtools")
install.packages("future")
library(dplyr)
install.packages("C50")
library(C50)# árbol de decisión
install.packages("rpart")
library(rpart) # cart
install.packages("rpart.plot")
library(rpart.plot)# figura cart
install.packages("caret")
library(caret) # matrix conf
#######################################
bd<-iris
#bases de entrenamiento y test
set.seed(1246)
aux<-createDataPartition(bd$Species, p=0.7,list=F)
bdtrain<-bd[aux,]
bdtest<-bd[-aux,]
#preparando las bases de datos
m1<-C5.0(Species~. ,data=bdtrain)
m1
summary(m1)
plot(m1)

#no se quiere un resultado con un trials muy grande
m2<-C5.0(Species~.,data=bdtrain, trials=10)
m2
summary(m2)
plot(m2)

## calidad del modelo
clase_pre<-predict(m1, bdtest)

t1<-table(bdtest$Species,clase_pre)
confusionMatrix(t1)

### CART


set.seed(1307)
m3<-rpart(Species~ . , data=bdtrain)
rpart.plot(m3)

clase_cart<-predict(m3, bdtest, type = "class")

t2<-table(bdtest$Species,clase_cart)
confusionMatrix(t2)
summary(m3)
#prune.rpart# podado


## Introducción a redes neuronales


rm(list = ls())
install.packages("neuralnet")
library(neuralnet)
bd<-iris
##########
#grupos
unique(bd$Species)
bd$setosa<-bd$Species=="setosa"
bd$versicolor<-bd$Species=="versicolor"
bd$virginica<-bd$Species=="virginica"
##########
set.seed(1329)
aux<-sample(c(1,2),nrow(bd),replace = T, prob = c(0.7,0.3))
bdtrain<-bd[aux==1,]
bdtest<-bd[aux==2,]
#modelo

#Tenermos tres capas de salida, un nodo en la capa oculta
m1<-neuralnet(setosa+versicolor+virginica~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data = bdtrain, hidden = 1)

#Tenermos tres capas de salida, dos nodos en la capa oculta
m2<-neuralnet(setosa+versicolor+virginica~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data = bdtrain,hidden = 2)

#Tenermos tres capas de salida, tres nodos en la capa oculta
m3<-neuralnet(setosa+versicolor+virginica~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data = bdtrain,hidden = 3)

#Tenermos tres capas de salida, cuatro nodos en la capa oculta
m4<-neuralnet(setosa+versicolor+virginica~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data = bdtrain,hidden = 4)

summary(m1)
plot(m1)
plot(m2)
plot(m3)
plot(m4)

#nos muestra como está la respuesta del nivel 2 en relación
#a la tercera covariable (petal.length)

#muestra sensibilidad de la covariable en la red neuronal
gwplot(m4, selected.response = 2, 
       selected.covariate = 3)

rr<-compute(m4,bdtest)
rr #es como una predicción con la red neuronal
aux<-c("setosa","versicolor","virginica")[apply(rr$net.result,1,which.max)]

install.packages("caret")
library(caret)
t1<-table(bdtest$Species,aux)
t1
confusionMatrix(t1)
#la red neuronal tiene un porcentaje de precisión del 95%

#######################
rm(list = ls())
library(nnet)
bd<-iris
aux<-sample(c(1,2),nrow(bd),replace = T, prob = c(0.7,0.3))
bdtrain<-bd[aux==1,]
bdtest<-bd[aux==2,]

m5<-nnet(Species~.,data=bdtrain,size=3)

summary(m5)
m5p<-predict(m5,bdtest,type = "class")
m5p
t1<-table(m5p,bdtest$Species)
confusionMatrix(t1)


## Ejemplo EH
'''
- Dataset: EH 21
- Y (GRUPO): Área Urbano/rural
- Unidad de análisis: Personas de 18 años o más
- X (COVARIABLES): 
  + Sexo 
+ Edad
+ Años de educación
+ Condición de ocupación (condact)
+ Autoidentificación indígena
+ Ingreso laboral
+ Número de miembros
+ Cuenta con internet
'''

rm(list = ls())
library(dplyr)
library(haven)
library(labelled)
library(caret)
library(C50)
library(rpart)
library(rpart.plot)
library(naivebayes)
library(class)
#############################
load("_data/eh21_vf.RData")
bd<-eh21pe %>% select(folio,area,
                      sexo=s01a_02, 
                      edad=s01a_03,
                      aestudio,
                      condact,
                      indigena=s01a_09,
                      ylab,
                      totper
)
bd<-bd %>%left_join(
  eh21vi %>% select(folio, internet=s07a_28)
)
bd<-bd %>% filter(edad>=18)
bd<-na.omit(bd)# omitir los valores perdidos (Pob: 18 años o más con algún ingreso laboral)
##############################
# 0. Adecuación de formato 
##############################
bd<-bd %>% mutate(
  rural=(area==2)  
) %>% select(-area, -folio)
###############################
# 1. Base de entrenamiento y test
###############################
set.seed(753)
aux<-createDataPartition(bd$rural, p=0.7,list=F)
bdtrain<-bd[aux,]
bdtest<-bd[-aux,]
###############################
#Logit/Probit
###############################
m1<-glm(rural ~ . , data=bdtrain, family = binomial(link="logit"))
m2<-glm(rural ~ . , data=bdtrain, family = binomial(link="probit"))
#optimizar
m1<-step(m1)
m2<-step(m2)
#Probabilidades de pertenencia
prob_l<-predict(m1, bdtest, type="response")
prob_p<-predict(m2, bdtest, type="response")
#Clasificación
bdtest<-bdtest %>% mutate(yl=(prob_l >0.5), yp=(prob_p >0.5))
tl<-table(bdtest$rural, bdtest$yl)
tp<-table(bdtest$rural, bdtest$yp)
confusionMatrix(tl)
confusionMatrix(tp)
#REPORTE GLOBAL
reporte<-NULL

reporte<-c(reporte,
           confusionMatrix(tl)$overall[1])

reporte<-c(reporte,
           confusionMatrix(tp)$overall[1])
###########################
#naive bayes
###########################
bdtrain$rural<-factor(bdtrain$rural,c(T,F),c("Rural", "Urbano"))
bdtest$rural<-factor(bdtest$rural,c(T,F),c("Rural", "Urbano"))
#modelo
m3<-naive_bayes(rural ~ . , data=bdtrain)
#predicción
aux_nb<-predict(m3,bdtest)
tnb<-table(bdtest$rural,aux_nb)
confusionMatrix(tnb)
reporte<-c(reporte,
           confusionMatrix(tnb)$overall[1])
#######
#KNN
#######
clase_train<-bdtrain$rural
clase_test<-bdtest$rural
k<-floor(sqrt(nrow(bdtrain)))
m4<-knn(bdtrain[,-9], bdtest[,-c(9:11)], clase_train, k=k)
tknn<- table(m4, bdtest$rural)
confusionMatrix(tknn)
reporte<-c(reporte,
           confusionMatrix(tknn)$overall[1])
###############
#CART
###############
m6<-rpart(rural~ . , data=bdtrain,  control = rpart.control(cp = 0.01))
rpart.plot(m6)
clase_m6<-predict(m6, bdtest, type = "class")

tcart<-table(clase_m6, bdtest$rural)
confusionMatrix(tcart)
reporte<-c(reporte,
           confusionMatrix(tcart)$overall[1])
#######
#C50
#######
class(bd)
bd<-as.data.frame(bd)
bd<-remove_labels(bd)
bd$rural<-factor(bd$rural,c(T,F),c("Rural","Urbano"))
set.seed(753)
aux<-createDataPartition(bd$rural, p=0.7,list=F)
bdtrain<-bd[aux,]
bdtest<-bd[-aux,]

m5<-C5.0(rural~ . , data=bdtrain)
plot(m5)
aux<-predict(m5, bdtest)
tc50<-table(bdtest$rural, aux)
confusionMatrix(tc50)
reporte<-c(reporte,
           confusionMatrix(tc50)$overall[1])
names(reporte)<-c("logit","probit","Naive Bayes","knn","cart","c50")
