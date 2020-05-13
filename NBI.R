# Carga de paquetes necesarios 
list.of.packages <- c("readxl","ggplot2","sp","rgdal","RColorBrewer","Matrix","lattice","classInt","gtools","sf","spdep","dplyr", "xlsx", "lubridate", "tidyverse","car","raster","rgeos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
for (paquete in list.of.packages) {suppressMessages(library(paquete,character.only = TRUE) ) }

#Poligonos correspondientes a los radios censales de Rosario
rosmap <- readOGR("C:/Users/FerraroS/Desktop/Todo_seba/Cosas/Quinto anio - Tesis/Bases", "Rosario2010")

# df con los centroides de los radios censales
df <- read_excel("C:/Users/FerraroS/Desktop/Todo_seba/Cosas/Quinto anio - Tesis/Bases/Datos_2010.xlsx")

#Grafico antes de pasarlo a la clase spatyal points dataframe
#ggplot(df, aes(X, Y)) +
#  geom_point()

# Se indica que X e Y son las latitudes y longitudes correspondientes
coordinates(df) <- ~X+Y

#Se detectan registros duplicados
duplicados <- zerodist(df)

# Se elimina uno de los duplicados
for (i in 1:nrow(duplicados)){
  rosmap <- rosmap[rosmap$POLY_ID != duplicados[i, 1],]
}

#Es necesario eliminar la observacion 842, ya que no contiene hogares
rosmap <- rosmap[rosmap$POLY_ID != 842, ]

# EDA previo al analisis espacial
# Algunas medidas resumenes interesantes
total_radios <- nrow(rosmap)
total_hogares <- sum(rosmap$THOGARES)
total_hogaresNBI <- sum(rosmap$con_NBI)
proporcion_promedio <- sum(rosmap$con_NBI) / sum(rosmap$THOGARES)

paste0("La cantidad total de radios censales en Rosario es ", total_radios)
paste0("La cantidad total de hogares en Rosario es ", total_hogares)
paste0("La cantidad total de hogares con NBI en Rosario es ", total_hogaresNBI)
paste0("La proporción promedio de hogares con NBI en Rosario es ", round(proporcion_promedio, 4))

# Histograma de la cantidad de hogares por radio censal en Rosario
df_aux <- rosmap$THOGARES
df_aux <- as.data.frame(df_aux)
names(df_aux)[1] = "Total_Hogares"
ggplot(df_aux, aes(x = df_aux$Total_Hogares)) +
  geom_histogram(binwidth = 50,
                 center = 25,
                 aes(col=I("white")), color = "blue") +
  scale_y_continuous("Frecuencia") +
  scale_x_continuous(breaks=seq(0, max(df_aux$Total_Hogares + 50), by = 200), "Total de Hogares") +
  theme(
    plot.title = element_blank()
  )
ggsave("total_hogares.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

# Histograma de la cantidad de hogares  con NBI por radio censal en Rosario
df_aux1 <- rosmap$con_NBI
df_aux1 <- as.data.frame(df_aux1)
names(df_aux1)[1] = "Total_Hogares_NBI"
ggplot(df_aux1, aes(x = df_aux1$Total_Hogares_NBI)) +
  geom_histogram(binwidth = 10,
                 center = 5,
                 aes(col=I("white")), color = "blue") +
  scale_y_continuous("Frecuencia") +
  scale_x_continuous(breaks=seq(0, max(df_aux1$Total_Hogares_NBI + 10), by = 25), "Total de Hogares con NBI") +
  theme(
    plot.title = element_blank()
  )
ggsave("total_hogares_NBI.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

df_aux2 <- rosmap$prop
df_aux2 <- as.data.frame(df_aux2)
names(df_aux2)[1] = "Prop_Hogares_NBI"
ggplot(df_aux2, aes(x = df_aux2$Prop_Hogares_NBI)) +
  geom_histogram(binwidth = 0.02,
                 center = 0.02,
                 aes(col=I("white")), color = "blue") +
  scale_y_continuous("Frecuencia") +
  scale_x_continuous(breaks=seq(0, max(df_aux2$Prop_Hogares_NBI), by = 0.1), "Proporción de Hogares con NBI") +
  theme(
    plot.title = element_blank()
  )
ggsave("proporcion_hogares_NBI.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

# Comienzo del analisis espacial
x <- rosmap$prop #Variable de interes principal
# Grafico exploratorio
plot(rosmap, main="Radios censales de la ciudad de Rosario")

# Grafico de calor que indica el porcentaje de hogares con NBI a lo largo de todos los radios censales en Rosario
arrow <- list("SpatialPolygonsRescale", layout.north.arrow(), offset=c(-60.75, -33), scale(10))
jpeg(file = "nbi_radios.jpg")
spplot(rosmap, "porc", key.space = list(x = 0.62, y = 1, corner = c(0, 1)), sp.layout = list(arrow),
       colorkey = list(space = "right"), scales = list(draw = T),
       main = "Porcentaje de Hogares con NBI en Rosario", as.table = TRUE)
dev.off()
#Definicion del vecindario tipo reina
reina <- poly2nb(rosmap, queen = TRUE)#dnearneigh basado en distancias

# Se escoge el metodo de estandarizacion de filas para la matriz de pesos espaciales
lista <- nb2listw(reina, style = "S")

# Indice de Moran
moran.test(x, lista)# Supuesto de normalidad
moran.mc(x, lista, nsim = 999) # Test permutacional

# Distribucion del numero de vecinos
vecinos <- table(card(reina))
vecinos <- as.data.frame(vecinos)
names(vecinos)[1] = "Vecinos"
names(vecinos)[2] = "Frecuencia"
ggplot(vecinos, aes(x = Vecinos, y = Frecuencia)) +
  geom_bar(color = "blue", stat = "identity") +
  scale_x_discrete("Número de Vecinos") +
  scale_y_continuous("Frecuencia") +
  ggtitle("Distribución del Número de Vecinos") +
  theme(
    plot.title = element_blank()
  )
ggsave("nbi_vecinos.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)
# Grafico de dispersion de moran
rezagos <- cbind(x, lag.listw(lista, x))
rezagos <- as.data.frame(rezagos)
names(rezagos)[1] = "Hogares"
names(rezagos)[2] = "Retardos"
ggplot(rezagos, aes(x = Hogares, y = Retardos)) +
  geom_point(color = "blue", size = 2, shape = 20) +
  stat_smooth(method = "lm", se = F, col = "red") +
  scale_x_continuous("Proporción de Hogares con NBI") +
  scale_y_continuous("Retardo Espacial") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 0.4)) +
  ggtitle("Gráfico de dispersión de Moran") +
  theme(
    plot.title = element_blank()
    )
ggsave("nbi_moran.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

# Obtencion de los centroides de cada radio censal
centroides <- gCentroid(rosmap, byid = TRUE)
plot(centroides)

#ggsave() para guardar el grafico ggsave("miDibujito.jpg", g,width = 8, height = 5, units = "cm") puede ser pdf, etc
#hay opciones para llevarlo a powerpoint y editarlo alli
#ver ggfortify

# Obtencion de la matriz de pesos W
W <- nb2mat(reina, glist = NULL, style = "S", zero.policy = NULL)

# Calculo del indice de Moran utilizando las matrices 
#Se comprueba la igualdad con el obtenido directamente con la sentencia correspondiente
ni <-rosmap$con_NBI #vector que contiene el numero de hogares con nbi en cada radio censal
m <- length(ni) #numero de areas o radios censales
xi <- rosmap$THOGARES #vector que contiene el numero de hogares en cada radio censal
pi <- ni / xi
pmedia <- sum(pi) / m
pdif <- pi - pmedia
denominador <- sum(W) * (t(pdif) %*% pdif)
numerador <- m * (t(pdif) %*% W %*% pdif)
Imoran <- numerador / denominador
print(Imoran) #Coincide con el calculado anteriormente

#EBI: Empirical Bayes Index
Q <- EBest(ni, xi, family = "poisson")
#View(Q)#La primer columna contiene los pi y la segunda las estimaciones empiricas de bayes
# Calculo del EBI
ebi <- EBImoran.mc(ni, xi, lista, nsim = 999, zero.policy = TRUE) #observamos que se incrementa el valor del indice con respecto al de Moran
print(ebi)
summary(ebi)
#View(ebi)
plot(ebi)#----Pensar los graficos que se pueden hacer
plot(Q)

# Grafico mejorado para el EBI
rezagos <- Q
rezagos <- as.data.frame(rezagos)
names(rezagos)[1] = "Hogares"
names(rezagos)[2] = "Retardos"
ggplot(rezagos, aes(x = Hogares, y = Retardos)) +
  geom_point(color = "blue", size = 2, shape = 20) +
  stat_smooth(method = "lm", se = F, col = "red") +
  scale_x_continuous("Proporción de Hogares con NBI") +
  scale_y_continuous("Retardo Espacial") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 0.8)) +
  ggtitle("Gráfico de dispersión de Moran") +
  theme(
    plot.title = element_blank()
  )
ggsave("nbi_ebi.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)
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



seba <- st_read(system.file("shapes/Rosario2010.shp", package="spData")[1], quiet=TRUE)
# Ver, podria ser util
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

