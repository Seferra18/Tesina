# Heridos de arma de fuego
list.of.packages <- c("readxl","ggplot2","sp","rgdal","RColorBrewer","Matrix","lattice","classInt","gtools","sf","spdep","dplyr", "xlsx", "lubridate", "tidyverse","car","raster","rgeos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
for (paquete in list.of.packages) {suppressMessages(library(paquete,character.only = TRUE) ) }

# Poligonos
rosmap <- readOGR("C:/Users/FerraroS/Desktop/Todo_seba/Cosas/Quinto anio - Tesis/Bases", "Rosario2010")

# Conjunto de datos con info de heridos
df <- read_excel("C:/Users/FerraroS/Desktop/Todo_seba/Cosas/Quinto anio - Tesis/Bases/prohibido_1.xlsx")
prohibido <- df

# Se detectan registros duplicados
coordinates(df) <- ~X+Y
duplicados <- zerodist(df)
for (i in 1:nrow(duplicados)){
  rosmap <- rosmap[rosmap$POLY_ID != duplicados[i, 1],]
}
for (i in 1:nrow(duplicados)){
  prohibido <- prohibido[prohibido$POLY_ID != duplicados[i, 1],]
}

# Es necesario eliminar la observacion 842, ya que no contiene hogares
rosmap <- rosmap[rosmap$POLY_ID != 842, ]

# Dataset heridos
prohibido <- filter(prohibido, !POLY_ID == 842)

# Join entre ambos dataframes
heridos <- sp::merge(rosmap, prohibido, by = c("POLY_ID" = "POLY_ID"))
rosmap <- heridos

# heridos@data = heridos@data[!heridos@data$Heridos== 0, ]
# Por cuestiones de confidencialidad se agrega un ruido aleatorio a las proporciones 
set.seed(7)
#for (i in 1:nrow(rosmap)){
#  seba[i] <- sample(1:round(rosmap$THOGARES.x[i] * 0.01, 0), 1)
#}

for (i in 1:nrow(rosmap)){
  rosmap$Heridos[i] <- rosmap$Heridos[i] + sample(1:round(rosmap$THOGARES.x[i] * 0.02, 0), 1)
}
rosmap$Proporcion <- rosmap$Heridos / rosmap$THOGARES.x
x <- rosmap$Proporcion  #Variable de interes principal

arrow <- list("SpatialPolygonsRescale", layout.north.arrow(), offset=c(-60.75, -33), scale(10))
spplot(rosmap, "Proporcion", key.space = list(x = 0.62, y = 1, corner = c(0, 1)), sp.layout = list(arrow),
       colorkey = list(space = "right"), scales = list(draw = T),
       main = "Porcentaje de heridos de arma de fuego en Rosario", as.table = TRUE)

# Definicion del vecindario tipo reina
reina <- poly2nb(rosmap, queen = TRUE)
lista <- nb2listw(reina, style = "S")
moran.test(x, lista) # Supuesto de normalidad
moran.mc(x, lista, nsim = 999)# Test permutacional

# Distribucion del numero de vecinos
vecinos <- table(card(reina))
vecinos <- as.data.frame(vecinos)
names(vecinos)[1] = "Vecinos"
names(vecinos)[2] = "Frecuencia"
ggplot(vecinos, aes(x = Vecinos, y = Frecuencia)) +
  geom_bar(color = "blue", stat = "identity") +
  scale_x_discrete("Número de Vecinos") +
  scale_y_continuous("Frecuencia") +
  ggtitle("Distribución del Numero de Vecinos") +
  theme(
    plot.title = element_blank()
  )

#grafico de dispersion de moran
rezagos <- cbind(x, lag.listw(lista, x))
rezagos <- as.data.frame(rezagos)
names(rezagos)[1] = "Heridos"
names(rezagos)[2] = "Retardos"
ggplot(rezagos, aes(x = Heridos, y = Retardos)) +
  geom_point(color = "blue", size = 2, shape = 20) +
  stat_smooth(method = "lm", se = F, col = "red") +
  scale_x_continuous("Proporción de Heridos con arma de fuego") +
  scale_y_continuous("Retardo Espacial") +
  coord_cartesian(xlim = c(min(rezagos$Heridos), max(rezagos$Heridos)), ylim = c(min(rezagos$Retardos), max(rezagos$Retardos))) +
  ggtitle("Gráfico de dispersión de Moran") +
  theme(
    plot.title = element_blank()
  )
#Obtencion de los centroides de cada radio censal
#centroides <- gCentroid(rosmap, byid = TRUE)
#plot(centroides)
#ggsave() para guardar el grafico ggsave("miDibujito.jpg", g,width = 8, height = 5, units = "cm") puede ser pdf, etc
#hay opciones para llevarlo a powerpoint y editarlo alli
#ver ggfortify
# Obtencion de la matriz de pesos W
W <- nb2mat(reina, glist = NULL, style = "S", zero.policy = NULL)
# Calculo del indice de Moran utilizando las matrices 
# Se comprueba la igualdad con el obtenido directamente con la sentencia correspondiente
ni <-rosmap$Heridos #vector que contiene el numero de hogares con nbi en cada radio censal
m <- length(ni) #numero de areas o radios censales
xi <- rosmap$THOGARES.x# vector que contiene el numero de hogares en cada radio censal
pi <- ni / xi
pmedia <- sum(pi) / m
pdif <- pi - pmedia
denominador <- sum(W) * (t(pdif) %*% pdif)
numerador <- m * (t(pdif) %*% W %*% pdif)
Imoran <- numerador / denominador
print(Imoran)#Coincide con el calculado anteriormente

# EBI: Empirical Bayes Index
Q <- EBest(ni, xi, family = "poisson")
View(Q)#La primer columna contiene los pi y la segunda las estimaciones emp?ricas de bayes
# Calculo de EBI
ebi <- EBImoran.mc(ni, xi, lista, nsim = 999, zero.policy = TRUE)#observamos que se incrementa el valor del ?ndice con respecto al de referencia
print(ebi)
summary(ebi)
View(ebi)
plot(ebi)#----Pensar los graficos que se pueden hacer
# Grafico mejorado para el EBI
rezagos <- Q
rezagos <- as.data.frame(rezagos)
names(rezagos)[1] = "Heridos"
names(rezagos)[2] = "Retardos"
ggplot(rezagos, aes(x = Heridos, y = Retardos)) +
  geom_point(color = "blue", size = 2, shape = 20) +
  stat_smooth(method = "lm", se = F, col = "red") +
  scale_x_continuous("Proporción de Heridos de arma de fuego") +
  scale_y_continuous("Retardo Espacial") +
  coord_cartesian(xlim = c(min(rezagos$Heridos), max(rezagos$Heridos)), ylim = c(min(rezagos$Retardos), max(rezagos$Retardos))) +
  ggtitle("Gráfico de dispersión de Moran") +
  theme(
    plot.title = element_blank()
  )

# Obtencion de la matriz de pesos M
M <- W
diag(M) <- 2 #Para todo i=j--->mij=2, tal como se detalla en el paper
n <- sum(ni) #numero totales de hogares con nbi en todos los radios censales
x <- sum(xi) #numero total de hogares en todos los radios censales
b <- n/x #Proporcion de hogares con nbi. Distinto al propuesto por assuncao, pero esta bien programado (ver Oden)
ei <- ni/n #Vector que contiene la proporcion que representa cada radio censal con respecto al total de hogares con nbi
di <- xi/x #Vector que contiene la proporcion que representa cada radio censal con respecto al total de hogares
ed <- ei - di #Vector que contiene la diferencias entre las proporciones definidas anteriormente
# Se define Mij*
peso <- matrix(rep(sqrt(1 / di), m), ncol = m)*t(matrix(rep(sqrt(1 / di), m), ncol=m)) * M
# Matrices que contienen calculos parciales con el fin de facilitar el calculo
A <- sum(matrix(rep(di, m), ncol = m)*t(matrix(rep(di, m), ncol = m)) * peso)
B <- sum(di * diag(peso))
C <- sum(matrix(rep(di, m), ncol = m) * t(matrix(rep(di, m), ncol = m)) * (4 * peso ^ 2))
D <- sum(di * (diag(peso) ^ 2))
E <- sum(di * ((2 * di %*% peso) ^ 2))
FI <- sum(di * diag(peso) * (2 * di %*% peso))
s0 <- x ^ 2 * A - x * B
s1 <- (x ^ 2 * C) / 2 - 2 * x * D
s2 <- x ^ 3 * E - 4 * x ^ 2 * FI + 4 * x * D
# Numerador y denominador del indice de Oden
num <- n ^ 2 * (ed %*% (peso %*% ed)) - n * (1 - 2 * b) * sum(ei * diag(peso)) - n * b * sum(di * diag(peso))
den <- (b * (1 - b) * s0)
# Indice de Oden
Ipop <- num / den
# Esperanza y variancia
b2 <- 1 / (b * (1 - b)) - 3
aux <- x * ((x ^ 2 - 3 * x + 3) * s1 - x * s2 + 3 * s0 ^ 2) - b2 * (x * (x - 1) * s1 - 2 * x *s2 + 6 * s0 ^ 2)
espcuad <- aux / ((x - 1) * (x - 2) * (x - 3) * s0 ^ 2)
stdev <- sqrt(espcuad - 1 / (x - 1) ^ 2)
z <- (Ipop + 1 / (x - 1)) / stdev
var_aprox <- (2 * A ^ 2 + C /2 - E) / (A ^ 2 * x ^ 2) #A proximacion valida para x grande
resultados <-c (Ipop, -1 / (x - 1), stdev ^ 2, var_aprox, stdev, z, 1 - pnorm(z)) #pnorm indica la funcionn de distribucion normal, por lo tanto 1-pnorm(Zdesvio) es el p-valor
etiquetas <- c("I*pop", "Media", "Variancia", "Variancia Aproximada", "Desvio Estandar", "Z", "P-valor")
resultados_finales <- cbind(etiquetas, resultados)#concatena los vectores por columna
View(resultados_finales)
