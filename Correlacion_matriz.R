#Cargar el archivo, seleccionando los datos: numerico, texto, numerico
setwd("/Users/berumen/Desktop/UAN/Clases/Posgrado/CBAP/2020/Análisis_visualización_datos_R/R_script/Graficas_correlacion")
library(readxl)
Cereales_2 <- read_excel("Cereales_2.xlsx")


#analisis de correlacion... de 2 variables
cor.test(Cereales_2$calorias, Cereales_2$proteina)

#MATRIZ DE CORRELACION

#VER QUE TIPO DE OBJETO ES.. NUMERICO, CARACTER
sapply(Cereales_2, class)
str(Cereales_2)

#CREAR LA MATRIZ DE CORRELACION DE LA COLUMNA 2 EN ADELANTE colocando "," 2:ncol ARCHIVO.
#PAIRWISE COMPLETO, QUE TOME TODAS LAS OBSERVACIONES, ES OPCIONAL PERO UTIL

res <- cor(Cereales_2[,2:ncol(Cereales_2), use="pairwise.complete.obs"])

res

#Establecer el número de decimales después del punto
round(res,2)

#Cargar librería
#install.packages("corrplot")
library(corrplot)

#Grafica de correlación completa (otra forma)
corrplot(res)

#Grafica de correlación
corrplot(res, method="circle", type = "upper", order = "hclust", 
         hclust.method="complete", tl.col = "black", tl.srt = 45)

##Librería util
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(res, histogram=TRUE, pch=19)

#Instalar psych.
library(psych)
pairs.panels(res, scale=TRUE)

#Instalar GGally
library(GGally)
ggcorr(res, nbreaks=8, palette='RdGy', label=TRUE, label_size=5, label_color='white')

