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
mujeresMaule <- datos %>% filter(region == "Región del Maule" & sexo == "Mujer")
mujeresLagos <- datos %>% filter(region == "Región de Los Lagos" & sexo == "Mujer")

#Se obtiene una muestra aleatoria de 150 observaciones para region.
muestraMaule <- sample_n(mujeresMaule, 150)
muestraLagos <- sample_n(mujeresLagos, 150)

#Se obtiene la columna que contiene la cantidad de hijos nacidos vivos de cada observacion.
hijosMaule <- muestraMaule$s4
hijosLagos <- muestraLagos$s4

hijos <- c(hijosMaule, hijosLagos)
region <- c(rep("Maule", length(hijosMaule)), rep("Los Lagos", length(hijosLagos)))
datos1 <- data.frame(hijos, region)

# Establecer nivel de significaciÃ³n y cantidad de muestras a generar
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



#-------------------------------------------------------------------------------
#PREGUNTA 2

# Una universidad de Chile necesita conocer la situacion economica de los hogares a los que pertenecen 
# sus estudiantes, esto con el fin de disponer de distintas becas que los apoyen durante su carrera. Para ello
# realizaron estudio tomando como ejemplo en 3 regiones correspondientes al sector norte, centro y sur del país respectivamente.
# Lo que se busca con esto es verificar si el ingreso total (variable ytotcorch) medio de los hogares es similar en
# las regiones norte(region Coquimbo), centro(region maule) y sur (region los lagos) del pais.

#Librerias
library(dplyr)
library(ggpubr)
library(WRS2)
library(tidyverse)
set.seed(573)

#Lectura de los datos desde el archivo csv entregado.
basename <- "EP11 Datos.csv"
file <- file.path("C:/Users/fabia/Desktop/01-2022/IME/trabajos/IME_equipo1/EP12", basename)
datos <- read.csv2(file = file)

datosRegion <- datos %>% filter(ytotcorh != "NA")
datosRegion <- filter(datos, region %in% c("Región de Coquimbo", "Región del Maule", "Región de Los Lagos"))

# Se toma una muestra de 600 personas pertenecientes a esas regiones
tam <- 600
muestra <- datosRegion[sample(nrow(datosRegion), tam),]
ingresos <- muestra[["ytotcorh"]]
regiones <- factor(muestra[["region"]])
datos2 <- data.frame(ingresos, regiones)

ingreso_Norte <- sample_n(datos2 %>% filter(regiones == "Región de Coquimbo") %>% select(ingresos),100)
vectorNorte <- as.vector(t(ingreso_Norte))
ingreso_Centro <- sample_n(datos2 %>% filter(regiones == "Región del Maule") %>% select(ingresos),100)
vectorCentro <- as.vector(t(ingreso_Centro))
ingreso_Sur <- sample_n(datos2 %>% filter(regiones == "Región de Los Lagos") %>% select(ingresos),100)
vectorSur <- as.vector(t(ingreso_Sur))

id <- 1:nrow(ingreso_Norte)
datos_Agrupados <- data.frame(id,vectorNorte,vectorCentro,vectorSur)

datos_Agrupados_largo <- datos_Agrupados %>% pivot_longer(c("vectorNorte", "vectorCentro", "vectorSur"), names_to = "region",
                                values_to = "ingresos")

datos_Agrupados_largo[["region"]] <- factor(datos_Agrupados_largo[["region"]])

# Fijar nivel de significacion.
alfa <- 0.05

# Aplicar alternativa robusta para ANOVA de una via con
# muestras correlacionadas.
gamma <- 0.2

prueba <- rmanova(y = datos_Agrupados_largo[["ingresos"]], groups = datos_Agrupados_largo[["region"]],
                  blocks = datos_Agrupados_largo[["id"]], tr = gamma)

print(prueba)

#NO se si se debe aplicar post hoc aqui