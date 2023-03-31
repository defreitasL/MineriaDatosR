
# UNIDAD 5 - Tarea 5

wrkDir <- "C:/CURSO DM"
setwd(wrkDir)


# 1) Lea el archivo de datos “titanic”, del archivo “titanic.RData”, que 
# contiene 2201 casos relativos a las personas que viajaban en el Titanic con 
# cuatro variables indicando sexo, edad, clase en la que viajaban y si 
# sobrevivieron o no. Aplique el método Apriori (sustituya en las órdenes del
# ejemplo de la unidad “compra” por “titanic”) para intentar relacionar a los
# supervivientes con alguna de las características indicadas. Ordene las reglas
# obtenidas por el estadístico de interés “lift” en lugar de “confidence” e 
# interprete únicamente las reglas con lift > 1,20 que considere que parecen
# relevantes.


library(arules)


load("titanic.RData")
head(titanic) 

titanic2 <- as(titanic, "transactions")

reglas <- apriori(titanic2, parameter = list(support = 0.1, confidence = 0.90))
reglas.ordenadas <- sort(reglas, by = "lift")
as(reglas.ordenadas, "data.frame")



# 2) Lea el conjunto de datos "futbol.csv", que contiene un conjunto de 54
# partidos de futbol (datos reales tomados de una liga pasada) jugados por 20
# equipos, con 2 columnas o variables (PIERDE, GANA) indicando los equipos
# perdedor/ganador en cada partido (se han omitido los empates). Tiene más
# mérito ganar a un equipo que suele ganar que a otro que suele perder. 
# Admitiendo que el equipo que pierde emite un voto a favor del que gana y que
# ese voto tiene más valor cuando viene de un equipo más meritorio, construya un
# índice pagerank que permita ordenar a los equipos por “merito deportivo”.  


library(igraph) 


Dataset <- read.csv2("futbol.csv", header=TRUE,encoding="latin1")


grafo <- graph.data.frame(Dataset) # creamos el objeto ‘grafo’, transformación de Dataset
set.seed(123)
plot(grafo) # representamos el grafo


I <- page.rank(grafo) ; I # calcula el índice PageRank y lo escribe

rev(sort(I$vector)) # escribe los mismos valores en orden decreciente

set.seed(123)
plot(grafo, vertex.size=I$vector*200)


