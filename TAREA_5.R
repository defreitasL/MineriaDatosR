
# UNIDAD 5 - Tarea 5

wrkDir <- "C:/CURSO DM"
setwd(wrkDir)


# 1) Lea el archivo de datos “deudas”. Aplique el método KNN para identificar a
# los clientes que tendrán impagos a partir de las variables cuantitativas 
# conocidas (variables en las columnas 5 a 8 del conjunto de datos). Utilice una
# muestra aleatoria de 80 casos (o bien los 80 primeros) para el entrenamiento y
# los 20 restantes para validación.

library(class)

setwd("C:/CURSO DM")
load("deudas.RData") 

set.seed(12345)
muestra <- sample(1:nrow(deudas), 80) 
entrenamiento <- deudas[muestra, 5:8]
prueba <- deudas[-muestra, 5:8]

resultados <- knn(entrenamiento, entrenamiento, cl = deudas[muestra,"Impago"])
table(resultados, deudas[muestra,"Impago"])
t <- table(resultados, deudas[muestra,"Impago"])
t ; 100 * sum(diag(t)) / sum(t)

resultados <- knn(entrenamiento, prueba, cl = deudas[muestra,"Impago"])
table(resultados, deudas[-muestra,"Impago"])
t <- table(resultados, deudas[-muestra,"Impago"])
t ; 100 * sum(diag(t)) / sum(t)

predictores <- deudas[5:8] # toda la muestra, excluyendo la variable clase
resultados <- knn(predictores,predictores, cl = deudas$Impago)
table(resultados, deudas$Impago)
t <- table(resultados, deudas$Impago)
t ; 100 * sum(diag(t)) / sum(t)

# 2) Lea el conjunto de datos "vinos" (40 casos para entrenamiento, el resto 
# para validación). Aplique el método SVM para identificar la variedad de uva 
# utilizada en la elaboración del vino a partir de las concentraciones de ácidos
# orgánicos.

library(e1071)

load("vinos.RData")


set.seed(12345)
muestra <- sample(1:nrow(vinos), 40)
entrenamiento <- vinos[muestra, ]
prueba <- vinos[-muestra, ]

modelo <- svm(var ~ ., data = entrenamiento)
summary(modelo)

resultados.entrenamiento <- predict(modelo, newdata = entrenamiento, type = "class")
table(resultados.entrenamiento, entrenamiento$var)

resultados.prueba <- predict(modelo, newdata = prueba, type = "class")
t <- table(resultados.prueba, prueba$var)
t ; 100 * sum(diag(t)) / sum(t)

modelo <- svm(var ~ ., data = vinos)
summary(modelo)

resultados.final <- predict(modelo, newdata = vinos, type = "class")
t <- table(resultados.final, vinos$var)
t ; 100 * sum(diag(t)) / sum(t)
