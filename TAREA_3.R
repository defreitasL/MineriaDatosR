

# UNIDAD 3 - Tarea 3

wrkDir <- "C:/CURSO DM"
setwd(wrkDir)


# 1) Lea el archivo de datos “deudas”. Aplique el método CHAID para analizar el
# perfil de los clientes con impagos utilizando todas las variables como 
# explicativas excepto la variable dependiente Impago. Es necesario transformar
# algunas variables previamente, ya que todas deben ser cualitativas para aplicar
# este método; utilice summary(deudas) para ver cuales son las 4 variables 
# cualitativas y las 4 cuantitativas:

library(CHAID)
library(dplyr)
library(RcmdrMisc)

load("deudas.RData")
summary(deudas)
  


## a) Las 4 variables cualitativas ordinales deben ser declaradas como tales 
#    (se muestra un ejemplo para la primera):

deudas$Edad <- ordered(deudas$Edad, levels=c('menos de 30' ,'entre 30 y 40','mas de 40'))
deudas$Formacion <- ordered(deudas$Formacion, levels=c('elemental' ,'bachillerato','estudios universitarios'))
deudas$Empleo <- ordered(deudas$Empleo, levels=c('menos de 5 a\xf1os' ,'entre 5 y 10 a\xf1os','m\xe1s de 10 a\xf1os'))
deudas$Residencia <- ordered(deudas$Residencia, levels=c('menos de 5 a\xf1os' ,'entre 5 y 10 a\xf1os','m\xe1s de 10 a\xf1os'))

summary(deudas)

## b) Las 4 variables cuantitativas deben ser recodificadas y a continuación
# (si se forman más de dos niveles) declaradas como ordinales. Por ejemplo podemos
# utilizar los centiles 33 y 67 para definir sistematicamente los niveles 
# bajo/medio/alto en todas las variables cuantitativas, como en el ejemplo que
# sigue (con la variable Ingreso; solo es necesario cambiar el nombre de la 
# variable las 6 veces que aparece para aplicarlo a las restantes):

nombres <- c("Ingreso", "Deud_ing", "Deud_tarj", "Deud_otr")

for (variable in nombres)
{
  q1 <- quantile(deudas[[variable]],0.33);   q3 <- quantile(deudas[[variable]],0.67)
  
  deudas[[variable]] <- recode(deudas[[variable]], 'lo:q1="bajo"; q1:q3="medio"; q3:hi="alto" ', as.factor=TRUE)
  
  deudas[[variable]] <- ordered(deudas[[variable]], levels=c('bajo' ,  'medio', 'alto'))
}


summary(deudas)

ch <- chaid( Impago ~ ., data = deudas)

print(ch) # imprime los resultados de chaid
plot(ch) # representa gráficamente el árbol de segmentación 

#%% 

# 2) Lea el conjunto de datos "algas", que tiene 31 casos y 20 variables. 
# Aplique el método Naive Bayes para identificar el tipo de alga (variable 
# “clase”) utilizando las restantes variables (concentraciones de pigmentos)
# como variables explicativas. Utilice una muestra aleatoria de 20 casos  (o bien
# los 20 primeros) como conjunto de entrenamiento y el resto para validación. 
# Construya la matriz de confusión (tabla de predicción y clase original) para 
# el conjunto de prueba y para todo el conjunto "algas".  


library(e1071)
set.seed(12345)

load("algas.RData")
summary(deudas)


muestra <- sample(1:nrow(algas), 20) # indices aleatorios
entrenamiento <- subset(algas[muestra, ],select = -clase) # muestra de entrenamiento
prueba <- subset(algas[-muestra, ],select = -clase) # muestra de validación


modelo <- naiveBayes(x = entrenamiento, y = algas$clase[muestra])


resultados <- predict(object = modelo, newdata = prueba, type = "class")
t <- table(resultados, algas$clase[-muestra])
t ; 100 * sum(diag(t)) / sum(t)


resultados <- predict(object = modelo, newdata = subset(algas,select = -clase), type = "class")
t <- table(resultados, algas$clase)
t ; 100 * sum(diag(t)) / sum(t)







