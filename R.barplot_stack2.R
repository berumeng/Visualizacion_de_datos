##Establecer la carpeta de trabajo
setwd("Ruta/de/trabajo")

#Importar datos de excel (NO DEBE TENER ESPACIOS)
Datos <- read.table("COVID_NAY_4.txt", header=TRUE)
head(Datos)

#MODELO DE REGRESION LINEAL
regresion <- lm(Acumulados ~ Municipio, data= Datos)
summary(regresion)
#ECUACION RECTA ES:
# y = 193.33 + 57.67x

#GRAFICAR REGRESION LINEAL
plot(Datos$Municipio, Datos$Acumulados, xlab="Municipio", ylab = "Acumulados")
abline(regresion)

# library
library(dplyr)
library(ggplot2)
theme_set(theme_minimal())

#create graphic
ggplot(Datos, aes(x = Municipio, y = Acumulados))+
  geom_col(aes(fill=Casos), width = 0.7)


# Grouped
ggplot(Datos, aes(fill=Casos, y=Acumulados, x=Municipio)) + 
  geom_bar(position="stack", stat="identity")
