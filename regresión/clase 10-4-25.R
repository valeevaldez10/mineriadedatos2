ee<-residuals(m5)
hist(ee)
library(nortest)

##colinealidad
#VIF indica si una covariable está afectando a las demás covariables
#si VIF>raiz(2) hay una alta colinealidad

lm(log(ylab)~s01a03+aestudio+I(aestudio^2)+s01a_03:aestudio,data=bd)

#corrigiendo
library(rms)
model_1


#predicciones
predict(m5) #genera predicciones de todos los datos con el modelo
ypred<-exp(predict(m5)) #debido a que ponemos en logaritmo para predecir. 
bd1$ylab[2]
ypred[2]
#base de datos nuevo
bdp<-data.frame(
  aestudio="17",
  area="Urbana",
  s01a_02="2. Mujer",
  s01a_03=21,
  tothrs=40,
  totper=1,
  ynolab=0,
  cmasi="Asiste",
  s01a_10="1. SOLTERO/A",
  depto="La Paz"
)







