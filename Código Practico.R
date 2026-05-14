library(readxl)
Base_Multi <- read_excel("Base Multi.xlsx")
View(Base_Multi)

#Inferencia Multivariada----
##Test de normalidad multivariada----
library(MVN)
#de manera global, mardia si acepta normalidad
mvn(data = Base_Multi[, 1:5], mvnTest = "mardia", univariateTest = "SW") 
mvn(data = Base_Multi[, 1:5], mvnTest = "mardia", univariateTest = "Lillie") #Se rechaza de manera univariada EDAD
mvn(data = Base_Multi[, 1:5], mvnTest = "mardia", univariateTest = "AD")  #Se rechaza de manera univariada EDAD
mvn(data = Base_Multi[, 1:5], mvnTest = "mardia", univariateTest = "CVM") #Se rechaza de manera univariada EDAD
