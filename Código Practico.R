

View(Base_Multi)

#Inferencia Multivariada----
##Test de normalidad multivariada----
library(MVN)

mvn(data = Base_Multi[, 1:5], mvn_test = "mardia", univariate_test = "SW")
mvn(data = Base_Multi[, 1:5], mvn_test = "mardia", univariate_test = "Lillie") #Se rechaza de manera univariada EDAD
mvn(data = Base_Multi[, 1:5], mvn_test = "mardia", univariate_test = "AD") #Se rechaza de manera univariada EDAD
mvn(data = Base_Multi[, 1:5], mvn_test = "mardia", univariate_test = "CVM") #Se rechaza de manera univariada EDAD


