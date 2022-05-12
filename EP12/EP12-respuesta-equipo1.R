library(dplyr)
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


#La pregunta de investigacion propuesta para la primera parte del trabajo es la de analizar si, en promedio,
#la cantidad de hijos nacidos vivos es igual en las mujeres residentes en la region del Maule y las 
#mujeres residentes en la region de Los Lagos.

#Hipotesis:
#H0: En promedio, la cantidad de hijos nacidos vivos es igual en las mujeres residentes en la region del Maule y 
#    en las mujeres residentes en la region de Los Lagos.
#HA: En promedio, la cantidad de hijos nacidos vivos es distinta en las mujeres residentes en la region del Maule
#    y en las mujeres residentes en la region de Los Lagos.

#Formula matematica:
#H0: mu.maule - mu.losLagos = 0
#HA: mu.maule - mu.losLagos != 0

library(dplyr)
library(ggpubr)
library(WRS2)
#Lectura de los datos desde el archivo csv entregado.
basename <- "EP11 Datos.csv"
file <- file.path("C:/Users/fabia/Desktop/01-2022/IME/trabajos/IME_equipo1/EP12", basename)
datos <- read.csv2(file = file)

#Se fija el nivel de significacion para este analisis.
alfa <- 0.05

#Se fija la semilla seleccionada al azar para esta prueba
set.seed(420)

#Se obtienen las observaciones correspodientes a mujeres de las dos regiones a estudiar desde el dataframe
#original.
mujeresMaule <- datos %>% filter(region == "Regi�n del Maule" & sexo == "Mujer")
mujeresLagos <- datos %>% filter(region == "Regi�n de Los Lagos" & sexo == "Mujer")

#Se obtiene una muestra aleatoria de 150 observaciones para region.
muestraMaule <- sample_n(mujeresMaule, 150)
muestraLagos <- sample_n(mujeresLagos, 150)

#Se obtiene la columna que contiene la cantidad de hijos nacidos vivos de cada observacion.
hijosMaule <- muestraMaule$s4
hijosLagos <- muestraLagos$s4

hijos <- c(hijosMaule, hijosLagos)
region <- c(rep("Maule", length(hijosMaule)), rep("Los Lagos", length(hijosLagos)))
datos1 <- data.frame(hijos, region)

# Establecer nivel de significación y cantidad de muestras a generar
# con bootstrapping.
alfa <- 0.05
R <- 999

# Aplicar prueba con la media
set.seed(001)

prueba_yuen_media <- pb2gen(hijos ~ region,
                       data = datos1,
                       est = "mean",
                       nboot = R)

cat("\n\nResultado al usar la media como estimador\n\n")
print(prueba_media)

# Propongan una pregunta de investigación original, que involucre la comparación de las medias de más de dos grupos 
# independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior, seleccionen una
# muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta utilizando bootstrapping. Solo 
# por ejercicio académico, aplique un análisis post-hoc con bootstrapping aunque este no sea necesario.

#La pregunta de investigacion planteada es la de analizar si, en promedio, los hombres de las regiones de Atacama,
#Metropolitana y Los Lagos trabajan la misma cantidad de horas a la semana.

#Hipotesis:
#H0: en promedio, los hombres de las regiones de Atacama, Metropolitana y de Los Lagos trabajan la misma cantidad de 
#    horas a la semana.
#HA: en promedio, los hombres de al menos una de las regiones estudiadas trabajan una cantidad distinta de horas 
#    respecto a las demas regiones.

