#Hacer gráficas de expresión
setwd("/Users/berumen/Desktop/")

library(readxl)
library(dplyr)
library(ggplot2)

#Cargar el marco de datos
Firmness <- read_excel("Physicochemicals.xlsx")

#Establecer las variables que son los factores
#Temperatura y Días
str(Firmness)
Firmness$Temperature = as.factor(Firmness$Temperature)
Firmness$Days = as.factor(Firmness$Days)
str(Firmness)

# Agrupar y añadir al marco de datos la media y desviación estándar
Firmness1 = Firmness %>%
  group_by(Temperature, Days) %>%
  summarise(Media_firmeza = mean(Firmness, na.rm=TRUE),
            Desvia_estandar = sd(Firmness, na.rm = TRUE))
str(Firmness1)

#Agregar al marco de datos (manualmente) las diferencias significativas
Firmness2 = Firmness1 %>%
  mutate(TUKEY=c("a","b","c"))
str(Firmness2)

#Gráfica de tendencia (líneas)
#ggplot2 trabaja con "capas"
imagen0 = ggplot(Firmness2, aes(x = Days, y = Media_firmeza, 
                      colour = Temperature, group=Temperature,
                      fill=Temperature))+
  geom_line() +
  geom_errorbar(aes(ymin=Media_firmeza-Desvia_estandar, ymax=Media_firmeza+Desvia_estandar), width=0.1,
                position=position_dodge(0.03)) + #añadir la desviación estándar 
  scale_fill_brewer(palette="Dark2") + #elige paleta de colores
  scale_y_continuous(expand = c(0,0)) + #quitar el espacio
  theme(axis.text.x = element_text(angle = 90, size = 12, colour = "black", vjust = 0.5, hjust = 1), #añadir face="bold para negritas 
        axis.title.y = element_text(size = 16, face = "bold"), legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12, face = "bold", colour = "black"), 
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(colour = "black", size = 12)) +
  labs(x= "Días", y = "Firmeza", fill="") +
  theme_classic() +
  theme(legend.text = element_text(face = "italic")) 
print(imagen0)
dev.off()

#Crear gráfica de barras 
Imagen1 = ggplot(Firmness2, aes(x = Days, y = Media_firmeza,
                      fill=Temperature))+
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Media_firmeza-Desvia_estandar, ymax=Media_firmeza+Desvia_estandar), width=0.3,
                position=position_dodge(0.9))+
scale_fill_brewer(palette="Dark2") + #Coloca el color
  scale_y_continuous(expand = c(0,0)) + #quitar el espacio (gap) bar
  theme(axis.text.x = element_text(angle = 90, size = 12, colour = "black", vjust = 0.5, hjust = 1), #añadir face="bold para negritas 
        axis.title.y = element_text(size = 16, face = "bold"), legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 12, face = "bold", colour = "black"), 
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(colour = "black", size = 12)) +
  labs(x= "Días", y = "Firmeza", fill="") +
  theme_classic() +
  theme(legend.text = element_text(face = "italic")) 
print(Imagen1)
dev.off()