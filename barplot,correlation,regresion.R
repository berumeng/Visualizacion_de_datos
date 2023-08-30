#Cargar el archivo, seleccionando los datos: numerico, texto, numerico
setwd("/Users/berumen/Desktop/UAN/Clases/Posgrado/CBAP/2020/Análisis_visualización_datos_R/R_script/Graficas_correlacion")
library(readxl)
Cereales_2 <- read_excel("Cereales_2.xlsx")

#analisis de correlacion... de 2 variables
cor.test(Cereales_2$calorias, Cereales_2$proteina)

#MODELO DE REGRESION LINEAL
#El modelo de regresion lineal intenta encontrar la mejor línea para predecir el contenido proteinico basado en las calorias del producto
#La ecuacion es: proteina= b 
regresion <- lm(proteina ~ calorias, data= Cereales_2)
regresion
summary(regresion)

#ECUACION RECTA ES:
# y = -0.78265 + 0.02989*calorias

#GRAFICAR REGRESION LINEAL
plot(Cereales_2$calorias, Cereales_2$proteina, xlab="calorias", ylab = "proteina")
abline(regresion)

# library
library(ggplot2)

#Crear grafica de barras
ggplot(Cereales_2, aes(x = cereal, y = calorias))+
  coord_flip()+
  geom_col(aes(fill=calorias), width = 0.7)

# Grouped
ggplot(Cereales_2, aes(fill=proteina, y=proteina, x=cereal)) +
  theme_set(theme_minimal())+
  geom_bar(position="dodge", stat="identity")
