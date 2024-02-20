#Importo las librerías necesarias para el análisis
library(scatterplot3d)
library(ggplot2)
library(plotly)
library(rgl)
library(broom)

#definiendo las variables de estudio
pasajeros <- c(15,17,13,23,16,21,14,20,24,17,16,18,23,25,16)
publicidad <- c(10,12,8,17,10,15,10,14,19,10,11,13,16,10,12)
ingreso_nacional <- c(2.4,2.72,2.08,3.68,2.56,3.36,2.24,3.2,3.84,2.72,2.07,2.33,2.98,1.94,2.17)



#Se crea un dataframe con los datos
datos <- data.frame(pasajeros,publicidad,ingreso_nacional)
colnames(datos) <- c("Pasajeros", "Publicidad", "Ingreso Nacional")

head(datos) #Muestro los primeros seis elementos
tail(datos) #Muestro los últimos seis elementos

#Se crea un gráfico en 3D para explorar la relación entre las variables
scatterplot3d(x=publicidad, y=ingreso_nacional, z=pasajeros, pch=16, cex.lab=1,
              highlight.3d=TRUE, type="h", xlab='Publicidad (en miles de dolares)',
              ylab='Ingreso nacional (en billones de dolares)', zlab='Pasajeros (en miles)')

#De la figura anterior se ve claramente que a medida que aumenta el gasto en publicidad 
#y aumenta el ingreso nacional se tienden a aumentar el numero de pasajeros en miles.

# Realizar la regresión lineal múltiple
modelo <- lm(pasajeros ~ publicidad + ingreso_nacional, data = datos)

# Mostrar un resumen del modelo
summary(modelo)

#El mismo diagrama de dispersión anterior se puede crear usando el paquete plotly. 
#El lector puede jugar con el diagrama, puede moverlo, girarlo, acercarse y muchas cosas más, la curiosidad le mostrará las diferentes posibilidades.

plot_ly(x=publicidad, y=ingreso_nacional, z=pasajeros, type="scatter3d", color=pasajeros) %>% 
  layout(scene = list(xaxis = list(title = 'Publicidad (en miles de dolares)'),
                      yaxis = list(title = 'Ingreso nacional (en billones de dolares)'),
                      zaxis = list(title = 'Pasajeros (en miles)')))


plot3d(x=publicidad, y=ingreso_nacional, z=pasajeros, type='s', col='green',
       xlab='Publicidad (en miles de dolares)',
       ylab='Ingreso nacional (en billones de dolares)',
       zlab='Pasajeros (en miles)')

# Se crea el gráfico 3d y se guarda en un objeto
mi_modelo_3d <- scatterplot3d(x=publicidad, y=ingreso_nacional, z=pasajeros, pch=16, cex.lab=1,
                       highlight.3d=TRUE, type="h", xlab='Publicidad (en miles de dolares)',
                       ylab='Ingreso nacional (en billones de dolares)', zlab='Pasajeros (en miles)')
# Para agregar el plano usamos $plane3d( ) con argumento modelo ajustado
mi_modelo_3d$plane3d(modelo, lty.box="solid", col='mediumblue')

#predicción dentro del rango muestra
n.x <- data.frame(x = seq(45,75))
predict(modelo,n.x)
anova(modelo)

#Correlación
cor(pasajeros,publicidad)
cor(pasajeros,ingreso_nacional)
cor(publicidad,ingreso_nacional)

#Los residuales son los errores
residuos <- rstandard(modelo)
residuos
valor_ajustado <- fitted(modelo)
valor_ajustado
plot(valor_ajustado,residuos)
qqnorm(residuos)






