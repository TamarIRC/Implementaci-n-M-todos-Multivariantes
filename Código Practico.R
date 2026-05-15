

View(Base_Multi)

#Inferencia Multivariada----
##Test de normalidad multivariada----
library(MVN)

mvn(data = Base_Multi[, 1:5], mvn_test = "mardia", univariate_test = "SW")
mvn(data = Base_Multi[, 1:5], mvn_test = "mardia", univariate_test = "Lillie") #Se rechaza de manera univariada EDAD
mvn(data = Base_Multi[, 1:5], mvn_test = "mardia", univariate_test = "AD") #Se rechaza de manera univariada EDAD
mvn(data = Base_Multi[, 1:5], mvn_test = "mardia", univariate_test = "CVM") #Se rechaza de manera univariada EDAD


##Dos muestras aleatorias homogéneas e independientes (Comparación de medias)----

#install.packages("DescTools")
library(DescTools)

# Separamos solo las variables numéricas (columnas 1 a 5) en "vec"
vec <- Base_Multi[, 1:5]
# Guardamos la variable Sexo para el filtro
Sexo <- Base_Multi$Sexo
# Ejecutamos el test comparando Hombres vs Mujeres con todos los datos
HotellingsT2Test(vec[Sexo == "HOMBRE", ], vec[Sexo == "MUJER", ])

##K (3) muestras aleatorias homogéneas e independientes(Comparación de medias MANOVA)----

#Creamos el modelo MANOVA
modelo_manova <- manova(cbind(Edad, Peso, IMC, Presión_Arterial_Sistólica) ~ Estado_Nutricional, data = Base_Multi)

summary(modelo_manova, test = "Wilks") #Este es el que normalmente en el curso de utilizaría 
summary(modelo_manova, test = "Pillai") #Dice que es uno más riguroso que Wilks
summary(modelo_manova, test = "Hotelling-Lawley")
summary(modelo_manova, test = "Roy")


##Homogeneidad de matrices de varianza-covarianza (M de Box)----

#install.packages("biotools")
library(biotools)

#Para grupos por sexo:
boxM(vec, Base_Multi$Sexo)
#Para grupos por estado nutricional:
boxM(vec, Base_Multi$Estado_Nutricional)


#REDUCCIÓN DE LA DIMENSIONALIDAD----

#install.packages("psych")
library(psych)

# Calculamos la matriz de correlación y aplicamos el test KMO
KMO(cor(vec))

##Componentes principales----

vecst <- Base_Multi[, c("Edad", "Peso", "IMC", "Presión_Arterial_Sistólica")] 

#Ejecutamos Componentes Principales y estandarizamos
pca <- prcomp(vecst, center = TRUE, scale. = TRUE) #Se hace una Estandarización dentro

pca                  # Muestra los coeficientes (cargas) de cada componente
summary(pca)         # Resumen de la varianza explicada - Con dos variables se explica el 81,44%
plot(pca, type = "lines") #Gráfico de sedimentación (Scree plot)

##Análisis Factorial----

# Ejecutamos el análisis factorial pidiendo 2 factores y sin rotación
fa1 <- principal(vecst, nfactors = 2, rotate = "none")
fa1

fa1$communality #valor de las comunalidades

###Rotación ortogonal----
fa2 <- principal(vecst, nfactors = 2, rotate = "varimax")
fa2

fa2$communality

###Rotación Oblicua----
fa3 <- principal(vecst, nfactors = 2, rotate = "promax")
fa3
fa3$communality

##ANÁLISIS DE CORRESPONDENCIA MULTIPLE----
#install.packages("cabootcrs")
library(cabootcrs)

#Seleccionamos solo las variables categóricas de la base
muestra_cat <- Base_Multi[, c("Sexo", "Estado_Nutricional", "fuma")]
#Calculamos la matriz de Burt
m_burt <- getBurt(muestra_cat)
m_burt

##Representación bidimensional----
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("ggplot2")
#install.packages("rlang")
library(ggplot2)
library(FactoMineR)
library(factoextra) # Requerida para el gráfico fviz_mca_var
library(rlang)
#Ejecutamos el modelo ACM 
acm <- MCA(muestra_cat, graph = FALSE)
#Mostramos los valores propios (varianza explicada)
acm$eig
#Generamos el gráfico bidimensional de las categorías
fviz_mca_var(acm, repel = TRUE, col.var = "contrib",
             gradient.cols = c("#d45394", "#d938e1", "#d93"))
