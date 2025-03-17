#MÉTODOS DE ASOCIACIÓN
#CLASE 27-2-25

choose(8,2:7)
sum(choose(8,2:7))+8+1+1
256*256 #número de reglas de asociación

#Ejercicio 
n<-20
choose(n,1:n)
card<-sum(choose(n,1:n))+1 #este 1 es el vacío
format(card*card,scientific = F)


#clase 6-3-25

'''En una bolsa de mercado con 4 items, ¿cuántas
posibles reglas de asociación existirían?'''

n<-4
choose(n,1:n)
card<-sum(choose(n,1:n))+1
format(card*card,scientific = F)

#Ejercicio: Transacciones de una farmacia

#regla: antigripal -> paracetamol
#Calculando supp(antigripal), supp(paracetamol), 
#supp(antigripal,paracetamol),P(antigripal -> paracetamol),
#lift(X->Y)

##PASOS

#procesamiento de datos
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("readxl")
library(arules)   #algoritmos
library(arulesViz)   #gráficos
library(readxl)  #excel

##leer transacciones a partir de
#LISTA
list_f<-list(
  T1=c("vitaminas","antigripal","paracetamol"),
  T2=c("ibuprofeno","complejoB","aspirina"),
  T3=c("complejoB","vitaminas"),
  T4=c("antigripal","paracetamol","aspirina"),
  T5=c("jarabe para la tos","vitaminas")
)
bdf<-as(list_f,"transactions") #se convierten en transacciones
class(bdf)
bdf

#MATRIZ
aux<-read_excel("datasets/ejemplo_asoc.xlsx",1,"B1:H6")
auxm<-as.matrix(aux)
row.names(auxm)<-paste0("T",1:5)
bdf2<-as(auxm,"transactions")
bdf2

#DATAFRAME
aux<-read_excel("datasets/ejemplo_asoc.xlsx",2) #lee la hoja 2
aux$TRANSACCION<-paste0("T",aux$TRANSACCION)
aux
bdf3<-as(split(aux$ITEM,aux$TRANSACCION),"transactions")
bdf3

data("Groceries") #base de ejemplo
dfg<-Groceries
class(dfg)
dfg

data("Adult") #base de ejemplo - equivalente a la encuesta de hogares
dfa<-Adult
class(dfa)
dfa


#clase 11/3/2025


#clase 13/03/2025
LIST(bdf)
LIST(bdf)[5]
LIST(dfg)[1500]
LIST(dfa)[90]


# size df --> nos ayuda a obtener la dimensión de items por cada 
#transacción en cada uno de estos conjuntos de transacciones
size(bdf)
size(dfg)
summary(size(dfg))
hist(size(dfg))
barplot(size(dfg))
size(dfa) #proviene de una encuesta donde se han creado 13 categorías
#por ejemplo [512] solo tiene 12
LIST(dfa)[512]


# summary df
# la densidad que muestra el comando summary que en bdf es de 0.3714
# representa el # de 1/# de frecuencias posibles en la matriz (5*7=35)
#en el caso : 13/35
# si es un valor bajo significa algo malo porque hay pocas compras por 
#transacción
# también puede darnos una idea de que tan probable es encontrar
#reglas de asociación


summary(bdf)
summary(dfg)
summary(dfa)



inspect(bdf)
inspect(dfg[1:4])
inspect(dfa[1:4])
###Gráfica

# image (df) nos permite ver cuantas transacciones en común tiene un item
# en cuales transacciones están presentes

image(bdf)
image(dfg) #no se entiende
image(dfg[101:200])
image(dfa[1:100])
image(dfg[sample(1:9835,100)])
image(dfg[sample(1:48842,100)])

itemFrequencyPlot(bdf)
itemFrequencyPlot(dfg)
itemFrequencyPlot(dfa)
itemFrequencyPlot(dfg,topN=15) #muestra los 15 más frecuentes
itemFrequencyPlot(dfa,topN=10) #muestra los 10 más frecuentes

#ALGORITMO APRIORI

regla0<-apriori(
  bdf,
  parameter=list(supp=0.3,conf=0.7)
)
summary(regla0)
## Ha encontrado dos reglas de asociación

inspect(regla0)
# paracetamol implica el antigripal
# antigripal implica el paracetamol
# el lift implica que estas relaciones son fuertes

regla1<-apriori(
  bdf,
  parameter=list(supp=0.1,conf=0.2)
)
summary(regla1)
inspect(regla1)

regla2<-apriori(
  bdf,
  parameter=list(supp=0.1,conf=0.9)
)
summary(regla2)
inspect(regla2)

#para supermercado

regla3<-apriori(dfg,parameter=
                  list(supp=0.02,conf=0.5))
summary(regla3)
inspect(regla3)

regla4<-apriori(dfg,parameter=
                  list(supp=0.002,conf=0.7))
summary(regla4)
inspect(regla4)
##rule length distribution (lhs + rhs):sizes
##3  4  5 
##22 59 13
#hay 22 reglas que involucran 3 items, 59 que involucran 4 items,...

inspect(sort(regla4,by="lift")[1:10]) #muestra las 10 reglas con los lifts más altos

# si queremos analizar las reglas respecto a un item en determinado
soda_r=apriori(dfg,
               parameter=list(supp=0.001,conf=
                                0.1,minlen=2),appearance=
                 list(default="rhs",lhs="soda"))
#min len nos dice que como mínimo deben haber 2 items en la regla y el default nos dice que
#soda debe estar a lado izquierdo de la regla, que productos se compran dado que se ha 
#comprado soda
summary(soda_r)
inspect(sort(soda_r,by="lift"))
#descartamos los lifts menores a 1 que no nos sirven


#BOTELLA DE AGUA
agua_r=apriori(dfg,
               parameter=list(supp=0.001,conf=
                                0.1,minlen=2),appearance=
                 list(default="rhs",lhs="bottled water"))
summary(agua_r)
inspect(sort(agua_r,by="lift"))
#figuras
plot(regla4)
plot(regla4,interactive=T)
plot(soda_r,method="graph") #este gráfico es más útil cuando le damos un item específico
plot(regla4,method="graph")

ruleExplorer(regla4)

#el ECLAT es lo mismo solo que más eficiente en tiempo
me1<-eclat(dfg,parameter=list(supp=0.01,minlen=2))
# reporte
summary(me1)
inspect(sort(me1,by="support")[1:2]) #devuelve los items más frecuentes

plot(me1)
me1r<-ruleInduction(me1,dfg,confidence=0.5)
plot(me1r)








