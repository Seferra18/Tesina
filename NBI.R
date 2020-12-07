# Carga de paquetes necesarios 
list.of.packages <- c("readxl", "ggplot2", "sp", "rgdal", "RColorBrewer", "classInt", "lattice", "spdep", "dplyr", "lubridate", "tidyverse", "spdplyr", "tmap")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
for (paquete in list.of.packages) {suppressMessages(library(paquete,character.only = TRUE) ) }

# Funciones definidas
source('Funciones.R')

#Poligonos correspondientes a los radios censales de Rosario
rosmap <- readOGR(paste0(getwd(), "/Datos"), "Rosario2010")

#Es necesario eliminar el radio censal 842, ya que no contiene hogares
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

# OUTLIERS NBI PRUEBA
# Inputs para BoxMap 
plotvar <- df_aux2$Prop_Hogares_NBI
nclr <- 2 # Nivel de colores
plotclr <- brewer.pal(nclr, "Blues")
clases <- classIntervals(plotvar, nclr, style = "quantile", digits = 3)
# Se customizan las clases
clases$brks[2] <- 0.99
clases$brks[3] <- 1
codicol <- findColours(clases, plotclr)
# BoxMap
jpeg(file = "nbi_boxmap_out.jpg", width = 900, height = 700, units = 'px')
plot(rosmap, col=codicol, border="black")
title(main = "Proporción de Hogares con NBI")
legend("bottomleft", legend=names(attr(codicol, "table")), fill = attr(codicol, "palette"), cex=1.5)
dev.off()

#Definicion del vecindario tipo reina
reina <- poly2nb(rosmap, queen = TRUE)

# Se escoge el metodo de estandarizacion de filas para la matriz de pesos espaciales
lista <- nb2listw(reina, style = "W")

# Indice de Moran
moran.test(x, lista)
moran.mc(x, lista, nsim = 999) # Test permutacional

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
x_plot <- (x-mean(x))/sd(x) 
rezagos <- cbind(x_plot, lag.listw(lista, x_plot))
rezagos <- as.data.frame(rezagos)
names(rezagos)[1] = "Hogares"
names(rezagos)[2] = "Retardos"
ggplot(rezagos, aes(x = Hogares, y = Retardos)) +
  geom_point(color = "blue", size = 1, shape = 20) +
  stat_smooth(method = "lm", se = F, col = "red") +
  geom_segment(aes(x = 0, y = -1, xend = 0, yend = 5), linetype=2) +
  geom_segment(aes(x = -1, y = 0, xend = 11, yend = 0), linetype=2) +
  scale_x_continuous("Proporción de Hogares con NBI estandarizada") +
  scale_y_continuous("Retardo Espacial") +
  coord_cartesian(xlim = c(min(rezagos$Hogares), max(rezagos$Hogares)), ylim = c(min(rezagos$Retardos) + 0.1, max(rezagos$Retardos))) +
  ggtitle("Gráfico de dispersión de Moran") +
  theme(
    plot.title = element_blank()
    )
ggsave("nbi_moran.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

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

# Calculo del EBI
ebi <- EBImoran.mc(ni, xi, lista, nsim = 999, zero.policy = TRUE)
print(ebi)
summary(ebi)
plot(ebi)

#Grafico de dispersion del EBI
rezagos_ebi <- cbind(ebi$z, lag.listw(lista, ebi$z))
rezagos_ebi <- as.data.frame(rezagos_ebi)
names(rezagos_ebi)[1] = "Hogares"
names(rezagos_ebi)[2] = "Retardos"
ggplot(rezagos_ebi, aes(x = Hogares, y = Retardos)) +
  geom_point(color = "blue", size = 1, shape = 20) +
  stat_smooth(method = "lm", se = F, col = "red") +
  geom_segment(aes(x = 0, y = -1, xend = 0, yend = 6), linetype=2) +
  geom_segment(aes(x = -1, y = 0, xend = 11, yend = 0), linetype=2) +
  scale_x_continuous("Proporción de Hogares con NBI estandarizada (yi)") +
  scale_y_continuous("Retardo Espacial") +
  coord_cartesian(xlim = c(min(rezagos_ebi$Hogares), max(rezagos_ebi$Hogares)), ylim = c(min(rezagos_ebi$Retardos) + 0.1, max(rezagos_ebi$Retardos))) +
  ggtitle("Gráfico de dispersión del EBI") +
  theme(
    plot.title = element_blank()
  )
ggsave("nbi_ebi.jpg", plot = last_plot(), width = 12, height = 7, units = "cm", dpi = 300)

# índice de Oden
oden(matriz_vecindad = W, ni = rosmap$con_NBI, xi = rosmap$THOGARES)

# Exclusión de los 3 radios censales extremos
sin_outliers <- filter(rosmap, rosmap$prop!=1)

#Definicion del vecindario tipo reina
reina <- poly2nb(sin_outliers, queen = TRUE)
# Se escoge el metodo de estandarizacion de filas para la matriz de pesos espaciales
lista <- nb2listw(reina, style = "W")
# Indice de Moran
x <- sin_outliers$prop #Variable de interes principal
moran.test(x, lista)
moran.mc(x, lista, nsim = 999) # Test permutacional

# Calculo del EBI
ni <-sin_outliers$con_NBI #vector que contiene el numero de hogares con nbi en cada radio censal
xi <- sin_outliers$THOGARES #vector que contiene el numero de hogares en cada radio censal
ebi <- EBImoran.mc(ni, xi, lista, nsim = 999, zero.policy = TRUE)
print(ebi)
summary(ebi)
plot(ebi)

# índice de Oden
W <- nb2mat(reina, glist = NULL, style = "W", zero.policy = NULL)
oden(matriz_vecindad = W, ni = sin_outliers$con_NBI, xi = sin_outliers$THOGARES)
