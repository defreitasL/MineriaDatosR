

# UNIDAD 1 - Tarea 1

library(latex2exp)
wrkDir <- "C:/CURSO DM"
setwd(wrkDir)


# 1) Lea el archivo de datos “vinos.RData”. Aplique el método k-medias para 
# obtener dos clases utilizando como variables explicativas las concentraciones
# de los distintos ácidos orgánicos. Relacione la clasificación obtenida con la
# variedad de uva, con el fin de averiguar hasta que punto esa variedad es un 
# criterio principal de la clasificación “natural” de los vinos o es necesario
# buscar criterios adicionales (como la zona, el año de la vendimia, u otros).


set.seed(1506)

load("vinos.RData")

modeloVinos <- kmeans(subset(vinos,select = -var), centers = 2, nstart = 10)

modeloVinos

table(modeloVinos$cluster, vinos$var)

SSE <- c ()


for (k in 1 : 8) {
  
  kmVinos <- kmeans (subset(vinos,select = -var), centers = k, nstart = 10)
  
  SSE [k] <- kmVinos$betweenss / kmVinos$totss
  
}

plot(SSE, xlab = "Numero de Clases", ylab = TeX('$ SSE_i/SSE_{tot} $'),ylim = c(0, 1))
lines(SSE)

# 2) Lea el conjunto de datos "deudas.RData". Se trata de una muestra de 100
# clientes de un banco, algunos de los cuales han presentado impagos, de los que
# se dispone de información relativa a su nivel de ingresos, relación entre deudas
# e ingresos, importe de las deudas por tarjeta de crédito, e importe de otras
# deudas, entre otras variables. Utilice esas 4 variables (columnas 5 a 8) para
# obtener con el método EM una clasificación con dos grupos o clases.  Averigüe
# si la clasificación obtenida está relacionada con la variable “Impago”.  

library(mclust)

load("deudas.RData")

modeloDeudas <- Mclust(deudas[,5:8], G = 2)

table(modeloDeudas$classification, deudas$Impago)




