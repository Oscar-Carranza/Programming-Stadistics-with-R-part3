#Proyecto de Estadísitica

#Entrada=poblacion de datos recabados
population=read.table(file="C:/Users/Propietario/Desktop/Poblacion.csv", 
                      sep=",", header=TRUE)

#Media del consumo de energía eléctrica 
media=mean(population$kWh)
#Desviación estándar del consumo de energía eléctrica 
desv=sd(population$kWh)
#Rango del consumo de energía eléctrica 
rango= max(population$kWh)-min(population$kWh)

#Se crea el histograma
hist(population$kWh, col="blue", 
     main="Histograma poblacional: consumo de energía eléctrica", xlab="kWh", 
     ylab="frecuencia", breaks="Scott", xlim=c(0, 1500), ylim=c(0, 80))

#Se crea un modelo de regresión lineal
#Variable independiente: kWh
x=population$kWh
#Variable dependiente: Costo
y=population$Costo

x2=x**2

y2=y**2

xy=x*y

SSxx=sum(x2)-(1/length(x))*(sum(x))**2
SSyy=sum(y2)-(1/length(y))*(sum(y))**2
SSxy=sum(xy)-(1/length(y))*(sum(x))*(sum(y))

#Modelo de regresión lineal por mínimos cuadrados de la forma:
#y= b0 +b1x
b1=SSxy/SSxx
b0=mean(y)-b1*mean(x)

#Covarianza
cov=SSxy/length(xy)

#Coeficiente de determinación lineal
r2=(SSxy**2)/(SSxx*SSyy)

#Coeficiente de correlación lineal
r=sqrt(r2)


#Diagrama de dispersión
plot(x, y,
     xlab = "Consumo de energía eléctrica [kWh]", 
     ylab = "Costo de energía eléctrica [MXN$]",
     main="Gráfica de Costo de energía eléctrica vs Consumo", 
     xlim=c(1, 1500), ylim=c(1, 1500))

#abline(lm(y~x), col="yellow")

#Grafica del modelo obtenido por regresión lineal
Vectorde1=vector("numeric", length = 1500)
for (i in 1:1500){
        Vectorde1[i]=1  
}
xgraph=vector("numeric", length = 1500)
for (i in 1:1500){
        xgraph[i]=i  
}
par(new=T) #Superponer gráficas
plot(xgraph, b1*xgraph +b0*Vectorde1, pch=".", col="blue", 
     xlab = "", 
     ylab = "",
     main="")
