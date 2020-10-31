#En este script se encuentran las funciones más importantes utilizadas en la tesina

#Funcion boxplot
cajaBigotes <- function(df, y, labely, q){
  ggplot(df, aes(x=0, y=y)) + 
    geom_boxplot(width = 0.8, color = "blue", size = 0.5) + 
    scale_y_continuous(labely, breaks = round(seq(0, max(y), by =q),2)) +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

#índice de Oden
oden <- function(matriz_vecindad, ni, xi){
  M <- matriz_vecindad
  diag(M) <- 2 #Para todo i=j--->mij=2, tal como se detalla en el paper de Oden(1995)
  n <- sum(ni) #por ejemplo: numero totales de hogares con nbi en todos los radios censales
  x <- sum(xi) #por ejemplo: numero total de hogares en todos los radios censales
  b <- n/x #por ejemplo: Proporcion de hogares con nbi. Distinto al propuesto por assuncao, pero esta bien programado (ver Oden)
  ei <- ni/n #Vector que contiene por ejemplo: la proporcion que representa cada radio censal con respecto al total de hogares con nbi
  di <- xi/x #Vector que contiene por ejemplo: la proporcion que representa cada radio censal con respecto al total de hogares
  ed <- ei - di #Vector que contiene la diferencias entre las proporciones definidas anteriormente
  m <- length(ni) #numero de areas por ejemplo: radios censales
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
  #Numerador y denominador del indice de Oden
  num <- n ^ 2 * (ed %*% (peso %*% ed)) - n * (1 - 2 * b) * sum(ei * diag(peso)) - n * b * sum(di * diag(peso))
  den <- (b * (1 - b) * s0)
  #Indice de Oden
  Ipop <- num / den
  #Esperanza y variancia
  b2 <- 1 / (b * (1 - b)) - 3
  aux <- x * ((x ^ 2 - 3 * x + 3) * s1 - x * s2 + 3 * s0 ^ 2) - b2 * (x * (x - 1) * s1 - 2 * x *s2 + 6 * s0 ^ 2)
  espcuad <- aux / ((x - 1) * (x - 2) * (x - 3) * s0 ^ 2)
  stdev <- sqrt(espcuad - 1 / (x - 1) ^ 2)
  z <- (Ipop + 1 / (x - 1)) / stdev
  var_aprox <- (2 * A ^ 2 + C /2 - E) / (A ^ 2 * x ^ 2) #La aproximacion es válida para x grande
  resultados <-c (Ipop, -1 / (x - 1), stdev ^ 2, var_aprox, stdev, z, 1 - pnorm(z)) #pnorm indica la funcion de distribucion normal, por lo tanto 1-pnorm(Zdesvio) es el p-valor
  etiquetas <- c("I*pop", "Media", "Variancia", "Variancia Aproximada", "Desvio Estandar", "Z", "P-valor")
  resultados_finales <- cbind(etiquetas, resultados)#concatena los vectores por columna
  return(resultados_finales)
}
