library(wbstats)
library(ggfortify)
library(plotly)
gdp<-wb_search("gdp")
tax<-wb_search("tax")
wb_search(pattern="price index")

bd<-wb_data("NY.GDP.MKTP.CD",
            country="BO",lang="es")

#Definiendo una serie de tiempo en R
gdp<-ts(bd$y,frequency=1,start=c(1960,1)) #aquí se guarda nuestra serie de tiempo
plot(gdp)
autoplot(gdp)
ggplotly(autoplot(gdp))

#diferencia

dgdp<-diff(gdp)
plot(dgdp)
#va a obtener la segunda diferencia
d2<-diff(gdp,2)


#tasa de crecimiento

plot(100*diff(log(gdp)))
Qt<-100*(diff(gdp)/bd$y[-1]) #devuelve todos los valores de bd$y menos el último
points(Qt,col="red",type="l")

##########Ejercicio 2


AirPassengers
#en este caso la base tiene un componente de periodo mensual
plot(AirPassengers)

'''existe una tendencia a que crezcan los pasajeros, también
se observa que hay un componente estacional donde en diciembre hay
más afluencia de pasajeros'''

autoplot(AirPassengers)

##########Verificando estacionariedad
library(tseries)
gdp<-na.omit(gdp)
adf.test(gdp) #no es estacionario
adf.test(dgdp) #no es estacionaria
adf.test(Qt) #es más estable, el p-valor cae pero sigue no siendo estacionaria
adf.test(diff(gdp,differences = 2)) #ahora si es estacionaria
adf.test(AirPassengers)

##########Función de autocorrelación
acf(gdp)      #el pib tiene influencia hasta 12 rezagos
acf(dgdp)     #en la primera diferencia aun existe autocorrelación a un rezago
acf(AirPassengers)    #como es estacional, el ACF tiene una tendencia

#como es estacional, se puede descomponer la serie de tiempo en el periodo
plot(decompose(AirPassengers))
#se observa que el ruido blanco tiene cierta tendencia, por esto se aplica una 
#diferencia en el periodo
s1<-diff(AirPassengers,lag=12)
acf(s1)
plot(decompose(s1))  #se ve que ahora el ruido blanco ya no tiene ese patrón



