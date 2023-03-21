
# UNIDAD 4 - Tarea 4

wrkDir <- "C:/CURSO DM"
setwd(wrkDir)


# 1) Lea el archivo de datos “vinos”. Aplique el método CART para identificar 
# la variedad de uva utilizada en la elaboración del vino a partir de las
# concentraciones de ácidos orgánicos determinadas analíticamente. Utilice
# una muestra de entrenamiento de 40 casos aleatorios (o bien los 40 primeros)
# y los 14 restantes como muestra de validación.

library(rpart)
library(rpart.plot)

load("vinos.RData")

set.seed(12345)
muestra <- sample(1:nrow(vinos), 40) # 40 números de caso elegidos al azar
entrenamiento <- vinos[muestra, ]
prueba <- vinos[-muestra, ]

modelo <- rpart(var ~ . , data = entrenamiento)
rpart.plot(modelo)
resultados <- predict(object = modelo, newdata = prueba, type = "class")
t <- table(resultados, prueba$var)
t ; 100 * sum(diag(t)) / sum(t)

resultados <- predict(object = modelo, newdata = entrenamiento, type = "class")
t <- table(resultados, entrenamiento$var)
t ; 100 * sum(diag(t)) / sum(t)


modelo <- rpart(var ~ . , data = vinos) # con toda la muestra
resultados <- predict(object = modelo, newdata = vinos, type = "class")
t <- table(resultados, vinos$var)
t ; 100 * sum(diag(t)) / sum(t)

rpart.plot(modelo)


# 2) Lea el conjunto de datos "deudas". Aplique el método C5.0 para identificar
# a los clientes que tendrán impagos a partir de sus características conocidas.
# Utilice una muestra de entrenamiento de 80 casos aleatorios (o bien los 80
# primeros) y los 20 restantes como muestra de validación.  Represente 
# gráficamente el árbol con la regla de clasificación. Construya la matriz de
# confusión para la muestra de entrenamiento y para la de prueba. 


library(C50)


load("deudas2.RData")
summary(deudas)

set.seed(12345)
muestra <- sample(1:nrow(deudas), 80) 
entrenamiento <- deudas[muestra, ]
prueba <- deudas[-muestra, ]


modelo <- C5.0(Impago ~ . , data = entrenamiento)
summary(modelo) # Información sobre el modelo

plot(modelo) # Gráfico


resultados.entrenamiento <- predict(modelo, newdata = entrenamiento, type = "class")
table(resultados.entrenamiento, entrenamiento$Impago)


resultados.prueba <- predict(modelo, newdata = prueba, type = "class")
table(resultados.prueba, prueba$Impago)


modelo <- C5.0(Impago ~ ., data = deudas)
resultados <- predict(modelo, deudas, type = "class")
table(resultados, deudas$Impago)


