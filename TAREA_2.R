

# UNIDAD 2 - Tarea 2

wrkDir <- "C:/CURSO DM"
setwd(wrkDir)


# 1) Lea el conjunto de datos “empresas” del archivo “empresas.RData”, con una 
# muestra de 1194 empresas, de las cuales 165 son empresas culturales, con 25 
# variables: la que identifica su carácter de empresa cultural y 24 adicionales 
# obtenidas de su balance y cuenta de resultados. Aplique el método Adaboost, 
# utilizando una muestra de entrenamiento de 1000 casos (los 1000 primeros o 1000
# casos aleatorios a elección del alumno) y los restantes 194 como muestra de 
# validación, para conocer si es posible predecir o identificar el carácter de
# empresa cultural (variable “CULTURAL”) a partir de las variables contables 
# (las 24 restantes). Valide los resultados con la muestra de validación. 
# Identifique las tres variables explicativas más relevantes

library(adabag) 

set.seed(4321)

load("empresas.RData")

names(empresas) 
summary(empresas) 

muestra <- sample(1:nrow(empresas), 1000) # indices aleatorios
entrenamiento <- empresas[muestra, ] # muestra de entrenamiento
prueba <- empresas[-muestra, ] # muestra de validación


modelo <- boosting(CULTURAL ~ ., data = entrenamiento)

sort(modelo$importance)

resultados.entrenamiento <- predict(modelo, newdata = entrenamiento, type = "class")

resultados.entrenamiento$confusion

resultados.prueba <- predict(modelo, newdata = prueba, type = "class")
t <- resultados.prueba$confusion
t ; 100 * sum(diag(t)) / sum(t)

resultados.prueba$prob



modelo <- boosting(CULTURAL ~ ., data = empresas)
resultados <- predict(modelo, newdata = empresas, type = "class")
t <- resultados$confusion
t ; 100 * sum(diag(t)) / sum(t)

# 2) Lea el conjunto de datos algas, del archivo "algas.RData, con 31 casos de
# 6 tipos diferentes de algas, y 19 variables con la concentración relativa de 
# diferentes pigmentos. Aplique el método Random Forest (sustituya “vinos” por 
# “algas” en las órdenes de la unidad) para identificar el tipo de alga (variable
# “clase”) a partir de las concentraciones de pigmentos. Utilice una muestra 
# aleatoria de 20 casos y valide los resultados con los 11 restantes.

library(randomForest) 

load("algas.RData") 

set.seed(1506)
muestra <- sample(1:nrow(algas), 20) # 40 números de fila o caso elegidos al azar
entrenamiento <- algas[muestra, ]
prueba <- algas[-muestra, ]

nombres <- names(algas)
modelo <- randomForest(clase ~ ., data=entrenamiento)
modelo 

modelo$predicted

varImpPlot(modelo) 

predicciones <- predict(modelo, prueba)
t <- with(prueba, table(predicciones, clase)) # Matriz de confusión
t ; 100 * sum(diag(t)) / sum(t) 

modelo <- randomForest(clase~ ., data=algas)
predicciones <- predict(modelo, algas) # con toda la muestra
t <- with(algas, table(predicciones, clase)) 
t ; 100 * sum(diag(t)) / sum(t)

