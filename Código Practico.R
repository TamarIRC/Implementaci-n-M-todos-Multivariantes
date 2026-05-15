

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