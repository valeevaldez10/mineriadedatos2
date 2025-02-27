#MÉTODOS DE ASOCIACIÓN
#CLASE 27-2-25

choose(8,2:7)
sum(choose(8,2:7))+8+1+1
256*256 #número de reglas de asociación

#Ejercicio 
n<-20
choose(n,1:n)
card<-sum(choose(n,1:n))+1
format(card*card,scientific = F)
