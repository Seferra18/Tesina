#Probando nuevas cosas - Tesina
#después borrar todas las librerías q siguen después
list.of.packages <- c("readxl","ggplot2","sp","rgdal","RColorBrewer","Matrix","lattice","classInt","gtools","sf","spdep","dplyr", "xlsx", "lubridate", "tidyverse","car","raster","rgeos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
for (paquete in list.of.packages) {suppressMessages(library(paquete,character.only = TRUE) ) }
#Poligonos
rosmap <- readOGR("C:/Users/FerraroS/Desktop/Quinto año - Tesis/Bases", "Rosario2010")
#Conjunto de datos con info equivalente
df <- read_excel("C:/Users/FerraroS/Desktop/Quinto año - Tesis/Bases/prohibido_1.xlsx")
prohibido <- df
#Se detectan registros duplicados
coordinates(df) <- ~X+Y
duplicados <- zerodist(df)
for (i in 1:nrow(duplicados)){
  rosmap <- rosmap[rosmap$POLY_ID != duplicados[i, 1],]
}
for (i in 1:nrow(duplicados)){
  prohibido <- prohibido[prohibido$POLY_ID != duplicados[i, 1],]
}
#Es necesario eliminar la observacion 842, ya que no contiene hogares
rosmap <- rosmap[rosmap$POLY_ID != 842, ]
#Dataset heridos
prohibido <- filter(prohibido, !POLY_ID == 842)
#join entre ambos dataframes
heridos <- sp::merge(rosmap, prohibido, by = c("POLY_ID" = "POLY_ID"))
rosmap <- heridos
#heridos@data = heridos@data[!heridos@data$Heridos== 0, ]
x <- rosmap$Proporcion #Variable de interes principal
arrow <- list("SpatialPolygonsRescale", layout.north.arrow(), offset=c(-60.75, -33), scale(10))
spplot(rosmap, "porc", key.space = list(x = 0.62, y = 1, corner = c(0, 1)), sp.layout = list(arrow),
       colorkey = list(space = "right"), scales = list(draw = T),
       main = "Porcentaje de Hogares con NBI en Rosario", as.table = TRUE)
#Definición del vecindario tipo reina
reina <- poly2nb(rosmap, queen = TRUE)
lista <- nb2listw(reina, style = "S")
moran.test(x, lista)#0.38
moran.mc(x, lista, nsim = 999)#bajo los supuestos del test permutacional
#distribucion del numero de vecinos
vecinos <- table(card(reina))
vecinos <- as.data.frame(vecinos)
names(vecinos)[1] = "Vecinos"
names(vecinos)[2] = "Frecuencia"
ggplot(vecinos, aes(x = Vecinos, y = Frecuencia)) +
  geom_bar(color = "blue", stat = "identity") +
  scale_x_discrete("Número de Vecinos") +
  scale_y_continuous("Frecuencia") +
  ggtitle("Distribución del NUmero de Vecinos") +
  theme(
    plot.title = element_blank()
  )
#grafico de dispersion de moran
rezagos <- cbind(x, lag.listw(lista, x))
rezagos <- as.data.frame(rezagos)
names(rezagos)[1] = "Hogares"
names(rezagos)[2] = "Retardos"
ggplot(rezagos, aes(x = Hogares, y = Retardos)) +
  geom_point(color = "blue", size = 2, shape = 20) +
  stat_smooth(method = "lm", se = F, col = "red") +
  scale_x_continuous("Proporción de Hogares con NBI") +
  scale_y_continuous("Retardo Espacial") +
  ggtitle("Gráfico de dispersión de Moran") +
  theme(
    plot.title = element_blank()
  )
#Obtencion de los centroides de cada radio censal
centroides <- gCentroid(rosmap, byid = TRUE)
plot(centroides)
#ggsave() para guardar el grafico ggsave("miDibujito.jpg", g,width = 8, height = 5, units = "cm") puede ser pdf, etc
#hay opciones para llevarlo a powerpoint y editarlo alli
#ver ggfortify
#Obtencion de la matriz de pesos W
W <- nb2mat(reina, glist = NULL, style = "S", zero.policy = NULL)
#Cálculo del índice usual utilizando las matrices 
#Se comprueba la igualdad con el obtenido directamente con la sentencia correspondiente
ni <-rosmap$Heridos#vector que contiene el número de hogares con nbi en cada radio censal
#View(ni)
m <- length(ni)#número de áreas o radios censales
#View(m)
xi <- rosmap$THOGARES.x# vector que contiene el número de hogares en cada radio censal
#View(xi)
pi <- ni / xi
pmedia <- sum(pi) / m
pdif <- pi - pmedia
denominador <- sum(W) * (t(pdif) %*% pdif)
numerador <- m * (t(pdif) %*% W %*% pdif)
Imoran <- numerador / denominador
print(Imoran)#Coincide con el calculado anteriormente
#EBI: Empirical Bayes Index
Q <- EBest(ni, xi, family = "poisson")
View(Q)#La primer columna contiene los pi y la segunda las estimaciones empíricas de bayes
#Cálculo de EBI
ebi <- EBImoran.mc(ni, xi, lista, nsim = 999, zero.policy = TRUE)#observamos que se incrementa el valor del índice con respecto al de referencia
print(ebi)
summary(ebi)
View(ebi)
plot(ebi)#----Pensar los graficos que se pueden hacer
#obtención de la matriz de pesos M
M <- W
diag(M) <- 2#Para todo i=j--->mij=2, tal como se realiza en el estudio
#View(M)
n <- sum(ni)#número totales de hogares con nbi en los 1072 radios censales
#View(n)
x <- sum(xi)#número total de hogares en todos los radios censales
#View(x)
b <- n/x #Proporción de hogares con nbi. Distinto al propuesto por assuncao, pero esta bien programado (ver Oden)
#View(b)
ei <- ni/n#Vector que contiene la proporción que representa cada radio censal con respecto al total de hogares con nbi
#View(ei)
di <- xi/x#Vector que contiene la proporción que representa cada radio censal con respecto al total de hogares
#View(di)
ed <- ei - di#Vector que contiene la diferencias de proporciones definidas anteriormente
#View(ed)
#se define Mij*
peso <- matrix(rep(sqrt(1 / di), m), ncol = m)*t(matrix(rep(sqrt(1 / di), m), ncol=m)) * M
A <- sum(matrix(rep(di, m), ncol = m)*t(matrix(rep(di, m), ncol = m)) * peso)
B <- sum(di * diag(peso))
C <- sum(matrix(rep(di, m), ncol = m) * t(matrix(rep(di, m), ncol = m)) * (4 * peso ^ 2))
D <- sum(di * (diag(peso) ^ 2))
E <- sum(di * ((2 * di %*% peso) ^ 2))
FI <- sum(di * diag(peso) * (2 * di %*% peso))
s0 <- x ^ 2 * A - x * B
s1 <- (x ^ 2 * C) / 2 - 2 * x * D
s2 <- x ^ 3 * E - 4 * x ^ 2 * FI + 4 * x * D
num <- n ^ 2 * (ed %*% (peso %*% ed)) - n * (1 - 2 * b) * sum(ei * diag(peso)) - n * b * sum(di * diag(peso))
den <- (b * (1 - b) * s0)
Ipop <- num / den
print(Ipop)
b2 <- 1 / (b * (1 - b)) - 3
aux <- x * ((x ^ 2 - 3 * x + 3) * s1 - x * s2 + 3 * s0 ^ 2) - b2 * (x * (x - 1) * s1 - 2 * x *s2 + 6 * s0 ^ 2)
espcuad <- aux / ((x - 1) * (x - 2) * (x - 3) * s0 ^ 2)
stdev <- sqrt(espcuad - 1 / (x - 1) ^ 2)
z <- (Ipop + 1 / (x - 1)) / stdev
var_aprox <- (2 * A ^ 2 + C /2 - E) / (A ^ 2 * x ^ 2)#Válido para x grande
resultados <-c (Ipop, -1 / (x - 1), stdev ^ 2, var_aprox, stdev, z, 1 - pnorm(z))#pnorm indica la funciónn de distribución normal, por lo tanto 1-pnorm(Zdesvio) es el p-valor
etiquetas <- c("I*pop", "Media", "Variancia", "Variancia Aproximada", "Desvío Estándar", "Z", "P-valor")
resultados_finales <- cbind(etiquetas, resultados)#concatena los vectores por columna
View(resultados_finales)


