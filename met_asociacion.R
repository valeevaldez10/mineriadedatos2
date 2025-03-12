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
aux<-read_excel("",1,"B1:H6")
auxm<-as.matrix(aux)
row.names(auxm)<-paste0("T",1:5)
bdf2<-as(auxm,"transactions")
bdf2

#DATAFRAME
aux<-read_excel(,2)
aux$TRANSACCION<-paste0("T",aux$TRANSACCION)
aux
bdf3<-as(split(aux$ITEM,aux$TRANSACCION),"transactions")
bdf3

data("Groceries") #base de ejemplo
dfg<-Groceries
dfg

data("Adult") #base de ejemplo - equivalente a la encuesta de hogares
dfg<-Adult
dfg


#clase 11/3/2025


#clase 13/03/2025
#hola







