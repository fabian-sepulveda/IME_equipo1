library(ggpubr)
library(rcompanion)
instanciaA <- c(129,109,28,178,74,16,87,108,149,78)
tiempoA <- c(1510394,402929,885722,4428151,48667,834565,70599,783108,210041,37449)
instanciaB <- c(134,193,10,88,142,86,36,190,163,33)
tiempoB <- c(1252837,21962277,120276,4629726,5743260,6701654,6568968,180141,6684497,35974)

data <- data.frame(instanciaA,tiempoA,instanciaB,tiempoB)

g1 <- gghistogram(data, x = "tiempoA", bins = 10,
                  xlab = "Tiempo [milisegundos]", ylab = "Frecuencia",
                  color = "red", fill = "red")

g2 <- gghistogram(data, x = "tiempoB", bins = 10,
                  xlab = "Tiempo [milisegundos]", ylab = "Frecuencia",
                  color = "blue", fill = "blue")

print(g1)
print(g2)

lambda <- transformTukey(tiempoA, start = -4 , end = 4, int = 0.001, plotit = TRUE, statistic = 1, returnLambda = TRUE)

#Transformacion de los datos
tiempoATrans <- tiempoA**lambda
tiempoBTrans <- tiempoB**lambda

print(tiempoATrans)
print(tiempoBTrans)

alfa <- 0.05

#Aplicamos la prueba t para dos muestras independientes
prueba3 <- t.test(x = tiempoATrans,
                  y = tiempoBTrans,
                  paired = FALSE,
                  alternative = "greater",
                  mu = 0,
                  conf.level = 1 - alfa)
print(prueba3)
