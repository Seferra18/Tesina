# Carga de paquetes necesarios 
list.of.packages <- c("readxl","ggplot2","sp","rgdal","RColorBrewer","Matrix","lattice","classInt","gtools","sf","spdep","dplyr", 
                      "xlsx", "lubridate", "tidyverse","car","raster","rgeos", "spdplyr", "magrittr", "tmap")
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
# Se crea una columna con las clases que segmenta un box plot
RI <- quantile(df$porc, 0.75)-quantile(df$porc, 0.25)
df = df %>% mutate(Referencias = case_when(porc < quantile(df$porc, 0.25) - 1.5*RI ~ '1Outlier Inferior',
                                                      quantile(df$porc, 0.25) - 1.5*RI <= porc & porc < quantile(df$porc, 0.25) ~ '2< 25%',
                                                      quantile(df$porc, 0.25) <= porc & porc < quantile(df$porc, 0.50) ~ '325%-50%',
                                                      quantile(df$porc, 0.50) <= porc & porc < quantile(df$porc, 0.75) ~ '450%-75%',
                                                      quantile(df$porc, 0.75) <= porc & porc < quantile(df$porc, 0.75) + 1.5*RI ~ '5>75%',
                                                      quantile(df$porc, 0.75) + 1.5*RI <= porc ~ '6Outlier Superior'))
dfs <- dplyr::select(df, 'POLY_ID', 'Referencias')
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
ggplot(df_aux, aes(x = Total_Hogares)) +
  geom_histogram(binwidth = 50,
                 center = 25,
                 aes(col=I("white")), color = "blue") +
  scale_y_continuous("Cantidad de radios censales") +
  scale_x_continuous(breaks=seq(0, max(df_aux$Total_Hogares + 50), by = 200), "Cantidad de hogares") +
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
ggplot(df_aux2, aes(x = Prop_Hogares_NBI)) +
  geom_histogram(binwidth = 0.02,
                 center = 0.02,
                 aes(col=I("white")), color = "blue") +
  scale_y_continuous("Cantidad de radios censales") +
  scale_x_continuous(breaks=seq(0, max(df_aux2$Prop_Hogares_NBI), by = 0.1), "Proporción de Hogares con NBI") +
  theme(
    plot.title = element_blank()
  )
ggsave("proporcion_hogares_NBI.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

# Comienzo del analisis espacial
x <- rosmap$prop #Variable de interes principal
# Grafico exploratorio
plot(rosmap, main="Radios censales de la ciudad de Rosario")

# Nos traemos la columna con las clases del boxplot para rosmap
rosmap <- sp::merge(rosmap, dfs, by = c("POLY_ID" = "POLY_ID"))
# Gráfico de calor que indica el porcentaje de hogares con NBI a lo largo de todos los radios censales en Rosario
#arrow <- list("SpatialPolygonsRescale", layout.north.arrow(), offset=c(-60.75, -33), scale(10))
#jpeg(file = "nbi_radios.jpg")
#spplot(rosmap, "class_boxplot", key.space = list(x = 0.62, y = 1, corner = c(0, 1)), sp.layout = list(arrow),
#       colorkey = list(space = "right"), scales = list(draw = T),
#       main = "Porcentaje de Hogares con NBI en Rosario", as.table = TRUE)
#dev.off()

# Inputs para BoxMap 
plotvar <- df_aux2$Prop_Hogares_NBI
nclr <- 5 # Nivel de colores
plotclr <- brewer.pal(nclr, "Blues")
clases <- classIntervals(plotvar, nclr, style = "quantile", digits = 3)
# Se customizan las clases
RI <- quantile(df_aux2$Prop_Hogares_NBI, 0.75) - quantile(df_aux2$Prop_Hogares_NBI, 0.25)
clases$brks[2] <- round(quantile(df_aux2$Prop_Hogares_NBI, 0.25), 3)
clases$brks[3] <- round(quantile(df_aux2$Prop_Hogares_NBI, 0.5), 3)
clases$brks[4] <- round(quantile(df_aux2$Prop_Hogares_NBI, 0.75), 3)
clases$brks[5] <- round(quantile(df_aux2$Prop_Hogares_NBI, 0.75) + 1.5*RI, 3)
clases$brks[6] <- 1
codicol <- findColours(clases, plotclr)

# BoxMap
jpeg(file = "nbi_boxmap_last.jpg", width = 900, height = 700, units = 'px')
plot(rosmap, col=codicol, border="black")
#title(main = "Proporción de Hogares con NBI")
legend("bottomleft", legend=names(attr(codicol, "table")), fill = attr(codicol, "palette"), cex=1.5)
dev.off()
#box()
# Gracias Datacamp
plot <- tm_shape(rosmap) +
  tm_borders() +
  tm_fill(col = 'Referencias') +
  tm_compass() +
  tmap_style("col_blind") #Ver las demas opciones lindas, ejemplo: natural
tmap_save("box_map_NBI.jpg", tm = plot, width = 12, height = 12, units = "cm", dpi = 300)

#Definicion del vecindario tipo reina
reina <- poly2nb(rosmap, queen = TRUE)#dnearneigh basado en distancias

# Se escoge el metodo de estandarizacion de filas para la matriz de pesos espaciales
lista <- nb2listw(reina, style = "W")

# Indice de Moran
moran.test(x, lista)# Supuesto de normalidad
moran.mc(x, lista, nsim = 999) # Test permutacional

#Funcion boxplot
cajaBigotes <- function(df, y, labely, q){
  ggplot(df, aes(x=0, y=y)) + 
    geom_boxplot(width = 0.8, color = "blue", size = 0.5) + 
    scale_y_continuous(labely, breaks = round(seq(0, max(y), by =q),2)) +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}
# BOXPLOTS
# Vecinos
vecinos_bp <- card(reina)
vecinos_bp <- as.data.frame(vecinos_bp)
names(vecinos_bp)[1] = "Vecinos"
cajaBigotes(vecinos_bp, vecinos_bp$Vecinos, "Cantidad de vecinos", 2)
ggsave("vecinos_bp.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

# Total de hogares
cajaBigotes(df_aux, df_aux$Total_Hogares, "Cantidad de hogares", 100)
ggsave("hogares_bp.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

# Total de hogares con nbi
cajaBigotes(df_aux1, df_aux1$Total_Hogares_NBI, "Cantidad de hogares con NBI", 25)
ggsave("hogares_NBI_bp.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

# Proporcion de hogares con nbi
cajaBigotes(df_aux2, df_aux2$Prop_Hogares_NBI, "Proporción de hogares con NBI", 0.1)
ggsave("proporcion_NBI_bp.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)


# Distribucion del numero de vecinos
vecinos <- table(card(reina))
vecinos <- as.data.frame(vecinos)
names(vecinos)[1] = "Vecinos"
names(vecinos)[2] = "Frecuencia"
ggplot(vecinos, aes(x = Vecinos, y = Frecuencia)) +
  geom_bar(color = "blue", stat = "identity") +
  scale_x_discrete("Número de Vecinos") +
  scale_y_continuous("Cantidad de radios censales") +
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
W <- nb2mat(reina, glist = NULL, style = "W", zero.policy = NULL)

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

#Grafico de dispersion del EBI
rezagos_ebi <- cbind(ebi$z, lag.listw(lista, ebi$z))
rezagos_ebi <- as.data.frame(rezagos_ebi)
n0ames(rezagos_ebi)[1] = "Hogares"
names(rezagos_ebi)[2] = "Retardos"
ggplot(rezagos_ebi, aes(x = Hogares, y = Retardos)) +
  geom_point(color = "blue", size = 2, shape = 20) +
  stat_smooth(method = "lm", se = F, col = "red") +
  scale_x_continuous("Proporción de Hogares con NBI") +
  scale_y_continuous("Retardo Espacial") +
  coord_cartesian(xlim = c(min(rezagos_ebi$Hogares), max(rezagos_ebi$Hogares)), ylim = c(min(rezagos_ebi$Retardos) + 0.1, max(rezagos_ebi$Retardos))) +
  ggtitle("Gráfico de dispersión del EBI") +
  theme(
    plot.title = element_blank()
  )
ggsave("nbi_ebi.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

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
#ggsave("nbi_ebi.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)
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

