#Probando nuevas cosas - Tesina
list.of.packages <- c("readxl","ggplot2","sp","rgdal","RColorBrewer","Matrix","lattice","classInt","gtools","sf","spdep","dplyr", "xlsx", "lubridate", "tidyverse","car","raster","rgeos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
for (paquete in list.of.packages) {suppressMessages(library(paquete,character.only = TRUE) ) }
#Poligonos
rosmap <- readOGR("C:/Users/FerraroS/Desktop/Todo_seba/Cosas/Quinto año - Tesis/Bases", "Rosario2010")
#Conjunto de datos con info equivalente
df <- read_excel("C:/Users/FerraroS/Desktop/Todo_seba/Cosas/Quinto año - Tesis/Bases/Datos_2010.xlsx")
#Grafico antes de pasarlo a la clase spatyal points dataframe
ggplot(df, aes(X, Y)) +
  geom_point()
#Se detectan registros duplicados
coordinates(df) <- ~X+Y
duplicados <- zerodist(df)
for (i in 1:nrow(duplicados)){
  rosmap <- rosmap[rosmap$POLY_ID != duplicados[i, 1],]
  }
#Es necesario eliminar la observacion 842, ya que no contiene hogares
rosmap <- rosmap[rosmap$POLY_ID != 842, ]
x <- rosmap$prop #Variable de interes principal
#EDA
plot(rosmap, main="Radios censales de la ciudad de Rosario")

#ggplot(rosmap) + 
 # geom_map(aes(map_id = POLY_ID), map = positions)
arrow <- list("SpatialPolygonsRescale", layout.north.arrow(), offset=c(-60.75, -33), scale(10))
spplot(rosmap, "porc", key.space = list(x = 0.62, y = 1, corner = c(0, 1)), sp.layout = list(arrow),
       colorkey = list(space = "right"), scales = list(draw = T),
       main = "Porcentaje de Hogares con NBI en Rosario", as.table = TRUE)
#Definici?n del vecindario tipo reina
reina <- poly2nb(rosmap, queen = TRUE)#dnearneigh basado en distancias
lista <- nb2listw(reina, style = "S")
moran.test(x, lista)#0.38
moran.mc(x, lista, nsim = 999)#bajo los supuestos del test permutacional
#graficos
# ggplot(Datos_2010) +
#   geom_map(aes(map_id = rosmap$POLY_ID), map = rosmap) +
#   expand_limits(rosmap)
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
  scale_x_continuous("Proporci?n de Hogares con NBI") +
  scale_y_continuous("Retardo Espacial") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 0.4)) +
  ggtitle("Gr?fico de dispersi?n de Moran") +
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
#C?lculo del ?ndice usual utilizando las matrices 
#Se comprueba la igualdad con el obtenido directamente con la sentencia correspondiente
ni <-rosmap$con_NBI#vector que contiene el n?mero de hogares con nbi en cada radio censal
#View(ni)
m <- length(ni)#n?mero de ?reas o radios censales
#View(m)
xi <- rosmap$THOGARES# vector que contiene el n?mero de hogares en cada radio censal
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
View(Q)#La primer columna contiene los pi y la segunda las estimaciones emp?ricas de bayes
#C?lculo de EBI
ebi <- EBImoran.mc(ni, xi, lista, nsim = 999, zero.policy = TRUE)#observamos que se incrementa el valor del ?ndice con respecto al de referencia
print(ebi)
summary(ebi)
View(ebi)
plot(ebi)#----Pensar los graficos que se pueden hacer
plot(Q)
#obtenci?n de la matriz de pesos M
M <- W
diag(M) <- 2#Para todo i=j--->mij=2, tal como se realiza en el estudio
#View(M)
n <- sum(ni)#n?mero totales de hogares con nbi en los 1072 radios censales
#View(n)
x <- sum(xi)#n?mero total de hogares en todos los radios censales
#View(x)
b <- n/x #Proporci?n de hogares con nbi. Distinto al propuesto por assuncao, pero esta bien programado (ver Oden)
#View(b)
ei <- ni/n#Vector que contiene la proporci?n que representa cada radio censal con respecto al total de hogares con nbi
#View(ei)
di <- xi/x#Vector que contiene la proporci?n que representa cada radio censal con respecto al total de hogares
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
var_aprox <- (2 * A ^ 2 + C /2 - E) / (A ^ 2 * x ^ 2)#V?lido para x grande
resultados <-c (Ipop, -1 / (x - 1), stdev ^ 2, var_aprox, stdev, z, 1 - pnorm(z))#pnorm indica la funci?nn de distribuci?n normal, por lo tanto 1-pnorm(Zdesvio) es el p-valor
etiquetas <- c("I*pop", "Media", "Variancia", "Variancia Aproximada", "Desv?o Est?ndar", "Z", "P-valor")
resultados_finales <- cbind(etiquetas, resultados)#concatena los vectores por columna
View(resultados_finales)












#sp.correlogram(listw, x, order = 1, method = "corr", style = "B")
#Extras
#dnearneigh vecinos con criterio de distancia euclidea--> Siniestralidad
probmap(ros_im$THOGARES, ros_im$con_NBI)#investigar m?s para que puede ser ?til
plot.skater(ros_im$Distrito, xy)
#Obtencion de la matriz de pesos W
W <- nb2mat(nb, glist = NULL, style = "B", zero.policy = NULL)

#Example
columbus <- st_read(system.file("shapes/columbus.shp", package="spData")[1], quiet=TRUE)
col.gal.nb <- read.gal(system.file("weights/columbus.gal", package="spData")[1])
coords <- st_coordinates(st_centroid(st_geometry(columbus)))
xx <- poly2nb(as(columbus, "Spatial"))
dxx <- diffnb(xx, col.gal.nb)
plot(st_geometry(columbus), border="grey")
plot(col.gal.nb, coords, add=TRUE)
plot(dxx, coords, add=TRUE, col="red")
title(main=paste("Differences (red) in Columbus GAL weights (black)",
                 "and polygon generated queen weights", sep="\n"), cex.main=0.6)
# poly2nb with sf sfc_MULTIPOLYGON objects
sf_xx <- poly2nb(columbus)
diffnb(sf_xx, xx)
sfc_xx <- poly2nb(st_geometry(columbus))
diffnb(sfc_xx, xx)
xxx <- poly2nb(as(columbus, "Spatial"), queen=FALSE)
dxxx <- diffnb(xxx, col.gal.nb)
plot(st_geometry(columbus), border = "grey")
plot(col.gal.nb, coords, add = TRUE)
plot(dxxx, coords, add = TRUE, col = "red")
title(main=paste("Differences (red) in Columbus GAL weights (black)",
                 "and polygon generated rook weights", sep="\n"), cex.main=0.6)
cards <- card(xx)
maxconts <- which(cards == max(cards))
if(length(maxconts) > 1) maxconts <- maxconts[1]
fg <- rep("grey", length(cards))
fg[maxconts] <- "red"
fg[xx[[maxconts]]] <- "green"
plot(st_geometry(columbus), col=fg)
title(main="Region with largest number of contiguities", cex.main=0.6)
nc.sids <- st_read(system.file("shapes/sids.shp", package="spData")[1], quiet=TRUE)
system.time(xxnb <- poly2nb(nc.sids))
system.time(xxnb <- poly2nb(as(nc.sids, "Spatial")))
plot(st_geometry(nc.sids))
plot(xxnb, coordinates(as(nc.sids, "Spatial")), add=TRUE, col="blue")

#FunciÃ³n para los centroides
#cents <- SpatialPointsDataFrame(coords=rosmap, data=rosmap@data, 
 #                               proj4string=rosmap@proj4string)
