#CORRELACION Y REGRESION
setwd("/Users/berumen/Desktop/UAN/Clases/Posgrado/CBAP/2020/Análisis_visualización_datos_R/R_script/Graficas_correlacion")
#IMPORTAR DATOS MEDIANTE------ "IMPORT DATASET"----DERECHA.... ARCHIVO EXCEL DATOS
library(readxl)
LIBRO1 <- read_excel("/Users/berumen/Desktop/UAN/Clases/Posgrado/CBAP/2020/Análisis_visualización_datos_R/R_script/ANOVA_regresion_correlacion/LIBRO1.xlsx")
head(LIBRO1)

#analisis de correlacion. dentro de ahi es "nombre de objeto $ columna para correlacion, objeto$columna a correlacionar
cor.test(LIBRO1$Municipio, LIBRO1$Edad)

#correlacion de kendall, mas baja, se coloco exact porque no podia realizarse por el valor de p
cor.test(LIBRO1$Municipio, LIBRO1$Edad, method = "kendall", exact=FALSE)

#correlacion de spearman, mas baja, se coloco exact porque no podia realizarse por el valor de p
cor.test(LIBRO1$Municipio, LIBRO1$Edad, method = "spearman", exact=FALSE)

#Graficar esto
plot((LIBRO1))

#MODELO DE REGRESION LINEEAL
model1 <- lm(Edad~Municipio, data = LIBRO1)

#imprmir modelo
model1

#resumen de datos modelo de regresion
summary(model1)

#Grafica regresion lineal
scatter.smooth(x=LIBRO1$Edad, y=LIBRO1$Municipio, main="Edad ~Municipio")


#GRAFICOS DE CORRELACION, MATRIZ DE CORRELACION

#cargar libreria
library(corrplot)

#establecer objetos y tipo de objeto
sapply(LIBRO1, class)
str(LIBRO1)
sapply(LIBRO1, is.factor)

correlacion = cor(LIBRO1[sapply(LIBRO1, function(x)!is.factor(x))])

#MATRIZ DE CORRELACION
corrplot(correlacion)
corrplot(correlacion, method= "number")

