# Carga de paquetes necesarios 
list.of.packages <- c("readxl","ggplot2","sp","rgdal","RColorBrewer","Matrix","lattice","classInt","gtools","sf","spdep","dplyr", 
                      "xlsx", "lubridate", "tidyverse","car","raster","rgeos", "spdplyr", "magrittr", "tmap")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
for (paquete in list.of.packages) {suppressMessages(library(paquete,character.only = TRUE) ) }

# Poligonos
rosmap <- readOGR("C:/Users/FerraroS/Desktop/Todo_seba/Cosas/Quinto anio - Tesis/Bases", "Rosario2010")

# Conjunto de datos con info de heridos
prohibido <- read_excel("C:/Users/FerraroS/Desktop/Todo_seba/Cosas/Quinto anio - Tesis/Bases/prohibido_1.xlsx")

# Datos de habitantes
habitantes <- read_excel("C:/Users/FerraroS/Desktop/Todo_seba/Cosas/Quinto anio - Tesis/Bases/Datos_2010_seba_tesina.xlsx")
habitantes <- dplyr::select(habitantes, 'POLY_ID', 'Hombres', 'Mujeres', 'Habitantes', 'DUPLICADO')
# Join entre ambos dataframes
prohibido <- merge(prohibido, habitantes, by = c("POLY_ID" = "POLY_ID"))

# Se eliminan columnas para no duplicar en el siguiente merge
prohibido_aux <- dplyr::select(prohibido, c(POLY_ID, Heridos, Hombres, Mujeres, Habitantes, DUPLICADO))

rosmap <- sp::merge(rosmap, prohibido_aux, by = c("POLY_ID" = "POLY_ID"))

# Es necesario eliminar la observacion 842, ya que no contiene hogares
rosmap <- rosmap[rosmap$POLY_ID != 842, ]

# variable de interes
rosmap$Habitantes <- as.numeric(rosmap$Habitantes)
rosmap$her_habitantes <- (rosmap$Heridos / rosmap$Habitantes)*1000
# Se crea una columna con las clases determinadas por percentiles que se consideran convenientes
rosmap = rosmap %>% mutate(Referencias = case_when(her_habitantes < quantile(rosmap$her_habitantes, 0.8) ~ '1< 80% (Sin heridos)',
                                                         quantile(rosmap$her_habitantes, 0.8) <= her_habitantes & her_habitantes < quantile(rosmap$her_habitantes, 0.85) ~ '280%-85%',
                                                         quantile(rosmap$her_habitantes, 0.85) <= her_habitantes & her_habitantes < quantile(rosmap$her_habitantes, 0.90) ~ '385%-90%',
                                                         quantile(rosmap$her_habitantes, 0.90) <= her_habitantes & her_habitantes < quantile(rosmap$her_habitantes, 0.95) ~ '490%-95%',
                                                         quantile(rosmap$her_habitantes, 0.95) <= her_habitantes ~ '5> 95%'))

# Se detectan registros duplicados
coordinates(prohibido) <- ~X+Y
duplicados <- zerodist(prohibido)
for (i in 1:nrow(duplicados)){
  rosmap <- rosmap[rosmap$POLY_ID != duplicados[i, 1],]
}
for (i in 1:nrow(duplicados)){
  prohibido <- prohibido[prohibido$POLY_ID != duplicados[i, 1],]
}

# Por cuestiones de confidencialidad se agrega un ruido aleatorio a las proporciones 
#set.seed(7)
#for (i in 1:nrow(rosmap)){
#  rosmap$Heridos[i] <- (rosmap$Heridos[i] / rosmap$Habitantes[i] + 0.001) * rosmap$Habitantes[i] 
#}

total_heridos <- sum(rosmap$Heridos)
proporcion_promedio <- sum(rosmap$Heridos) / sum(rosmap$Habitantes)

paste0("La cantidad total de Heridos de arma de fuego en Rosario es ", total_heridos)
paste0("La proporción promedio de Heridos de arma de fuego en Rosario es ", round(proporcion_promedio, 6))

# Histograma de la cantidad de habitantes por radio censal en Rosario
df_aux <- rosmap$Habitantes
df_aux <- as.data.frame(df_aux)
names(df_aux)[1] = "Total_Habitantes"
ggplot(df_aux, aes(x = df_aux$Total_Habitantes)) +
  geom_histogram(binwidth = 100,
                 center = 50,
                 aes(col=I("white")), color = "blue") +
  scale_y_continuous("Frecuencia") +
  scale_x_continuous(breaks=seq(0, max(df_aux$Total_Habitantes + 100), by = 500), "Total de Habitantes") +
  theme(
    plot.title = element_blank()
  )
ggsave("total_habitantes.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

# Histograma de la cantidad de heridos por radio censal en Rosario
df_aux1 <- rosmap$Heridos
df_aux1 <- as.data.frame(df_aux1)
names(df_aux1)[1] = "Total_Heridos"
ggplot(df_aux1, aes(x = Total_Heridos)) +
  geom_histogram(binwidth = 1,
                 center = 1,
                 aes(col=I("white")), color = "blue") +
  scale_y_continuous("Frecuencia") +
  scale_x_continuous(breaks=seq(0, max(df_aux1$Total_Heridos), by = 1), "Total de Heridos por arma de fuego") +
  theme(
    plot.title = element_blank()
  )
ggsave("total_heridos.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

df_aux2 <- rosmap$Heridos / rosmap$Habitantes 
df_aux2 <- as.data.frame(df_aux2)
names(df_aux2)[1] = "Prop_Heridos"
ggplot(df_aux2, aes(x = df_aux2$Prop_Heridos)) +
  geom_histogram(binwidth = 0.001,
                 center = 0.001,
                 aes(col=I("white")), color = "blue") +
  scale_y_continuous("Frecuencia") +
  scale_x_continuous(breaks=seq(0, max(df_aux2$Prop_Heridos) , by = 0.002), "Proporción de Heridos de arma de fuego") +
  theme(
    plot.title = element_blank()
  )
ggsave("proporcion_heridos.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

x <- rosmap$her_habitantes  #Variable de interes principal

#jpeg(file = "her_radios.jpg")
#arrow <- list("SpatialPolygonsRescale", layout.north.arrow(), offset=c(-60.75, -33), scale(10))
#spplot(rosmap, "Proporcion", key.space = list(x = 0.62, y = 1, corner = c(0, 1)), sp.layout = list(arrow),
#       colorkey = list(space = "right"), scales = list(draw = T),
#       main = "Porcentaje de heridos de arma de fuego en Rosario", as.table = TRUE)
#dev.off()

# BOX MAP
plot <- tm_shape(rosmap) +
  tm_borders() +
  tm_fill(col = 'Referencias')+
  tm_compass() +
  tmap_style("cobalt") #Ver las demas opciones lindas, ejemplo: natural
tmap_save("box_map_Heridos.jpg", tm = plot, width = 12, height = 12, units = "cm", dpi = 300)

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
ggsave("her_vecinos.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

#Funcion boxplot
cajaBigotes <- function(df, y, labely, q){
  ggplot(df, aes(x=0, y=y)) + 
    geom_boxplot(width = 0.8, color = "blue", size = 0.5) + 
    scale_y_continuous(labely, breaks = round(seq(0, max(y), by =q),2)) +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

# Total de heridos
cajaBigotes(df_aux1, df_aux1$Total_Heridos, "Cantidad de heridos", 1)
ggsave("heridos_bp.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

# Proporcion de heridos
cajaBigotes(df_aux2, df_aux2$Prop_Heridos, "Proporción de heridos", 0.01)
ggsave("prop_heridos_bp.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)


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
ggsave("her_moran.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

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
xi <- rosmap$Habitantes# vector que contiene el numero de hogares en cada radio censal
pi <- ni / xi
pmedia <- sum(pi) / m
pdif <- pi - pmedia
denominador <- sum(W) * (t(pdif) %*% pdif)
numerador <- m * (t(pdif) %*% W %*% pdif)
Imoran <- numerador / denominador
print(Imoran)#Coincide con el calculado anteriormente

# EBI: Empirical Bayes Index
Q <- EBest(ni, xi, family = "poisson")
#View(Q)#La primer columna contiene los pi y la segunda las estimaciones emp?ricas de bayes
# Calculo de EBI
ebi <- EBImoran.mc(ni, xi, lista, nsim = 999, zero.policy = TRUE)#observamos que se incrementa el valor del ?ndice con respecto al de referencia
print(ebi)
summary(ebi)
#View(ebi)
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
ggsave("her_ebi.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

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
var_aprox <- (2 * A ^ 2 + C /2 - E) / (A ^ 2 * x ^ 2) #Aproximacion valida para x grande
resultados <-c (Ipop, -1 / (x - 1), stdev ^ 2, var_aprox, stdev, z, 1 - pnorm(z)) #pnorm indica la funcionn de distribucion normal, por lo tanto 1-pnorm(Zdesvio) es el p-valor
etiquetas <- c("I*pop", "Media", "Variancia", "Variancia Aproximada", "Desvio Estandar", "Z", "P-valor")
resultados_finales <- cbind(etiquetas, resultados)#concatena los vectores por columna
View(resultados_finales)
