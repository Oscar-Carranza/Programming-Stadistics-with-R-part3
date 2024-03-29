#Proyecto de Estad�sitica

#Entrada=poblacion de datos recabados
population=read.table(file="C:/Users/Propietario/Desktop/Poblacion.csv", 
                      sep=",", header=TRUE)

#Media del consumo de energ�a el�ctrica 
media=mean(population$kWh)
#Desviaci�n est�ndar del consumo de energ�a el�ctrica 
desv=sd(population$kWh)
#Rango del consumo de energ�a el�ctrica 
rango= max(population$kWh)-min(population$kWh)

#Se crea el histograma
hist(population$kWh, col="blue", 
     main="Histograma poblacional: consumo de energ�a el�ctrica", xlab="kWh", 
     ylab="frecuencia", breaks="Scott", xlim=c(0, 1500), ylim=c(0, 80))

#Se crea un modelo de regresi�n lineal
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

#Modelo de regresi�n lineal por m�nimos cuadrados de la forma:
#y= b0 +b1x
b1=SSxy/SSxx
b0=mean(y)-b1*mean(x)

#Covarianza
cov=SSxy/length(xy)

#Coeficiente de determinaci�n lineal
r2=(SSxy**2)/(SSxx*SSyy)

#Coeficiente de correlaci�n lineal
r=sqrt(r2)


#Diagrama de dispersi�n
plot(x, y,
     xlab = "Consumo de energ�a el�ctrica [kWh]", 
     ylab = "Costo de energ�a el�ctrica [MXN$]",
     main="Gr�fica de Costo de energ�a el�ctrica vs Consumo", 
     xlim=c(1, 1500), ylim=c(1, 1500))

#abline(lm(y~x), col="yellow")

#Grafica del modelo obtenido por regresi�n lineal
Vectorde1=vector("numeric", length = 1500)
for (i in 1:1500){
        Vectorde1[i]=1  
}
xgraph=vector("numeric", length = 1500)
for (i in 1:1500){
        xgraph[i]=i  
}
par(new=T) #Superponer gr�ficas
plot(xgraph, b1*xgraph +b0*Vectorde1, pch=".", col="blue", 
     xlab = "", 
     ylab = "",
     main="")
