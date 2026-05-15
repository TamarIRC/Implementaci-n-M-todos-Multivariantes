

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
