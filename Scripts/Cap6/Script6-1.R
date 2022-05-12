library(ggpubr)
library(tidyverse)

# Generar un vector con un rango de valores para la efecto
# de medias.
efecto <- seq(-2.5, 2.5, 0.01)

# Calcular el poder para una prueba t bilareral, para cada tamaño
# del efecto, asumiendo una muestra con desviación estándar igual a 1.
# Se consideran 4 escenarios para calcular el poder:
# 1. Una muestra de tamaño 6 y nivel de significación 0.05.
# 2. Una muestra de tamaño 6 y nivel de significación 0.01.
# 3. Una muestra de tamaño 10 y nivel de significación 0.05.
# 4. Una muestra de tamaño 10 y nivel de significación 0.01.
n_6_alfa_05 <- power.t.test(n = 6,
                      delta = efecto,
                      sd = 1,
                      sig.level = 0.05,
                      type = "one.sample",
                      alternative = "two.sided")$power

n_6_alfa_01 <- power.t.test(n = 6,
                      delta = efecto,
                      sd = 1,
                      sig.level = 0.01,
                      type = "one.sample",
                      alternative = "two.sided")$power

n_10_alfa_05 <- power.t.test(n = 10,
                       delta = efecto,
                       sd = 1,
                       sig.level = 0.05,
                       type = "one.sample",
                       alternative = "two.sided")$power

n_10_alfa_01 <- power.t.test(n = 10,
                             delta = efecto,
                             sd = 1,
                             sig.level = 0.01,
                             type = "one.sample",
                             alternative = "two.sided")$power

# Construir matriz de datos en formato ancho.
datos <- data.frame(efecto, n_6_alfa_05, n_6_alfa_01,
                    n_10_alfa_05, n_10_alfa_01)

# Llevar a formato largo.
datos <- datos %>% pivot_longer(!"efecto", 
                                names_to = "fuente",
                                values_to = "poder")

# Formatear fuente como variable categórica.
niveles <- c("n_6_alfa_05", "n_6_alfa_01", "n_10_alfa_05",
             "n_10_alfa_01")

etiquetas <- c("n=6, alfa=0,05", "n=6, alfa=0,01", "n=10, alfa=0,05",
               "n=10, alfa=0,01")

datos[["fuente"]] <- factor(datos[["fuente"]], levels = niveles,
                            labels = etiquetas)

# Graficar las curvas de poder.
g <- ggplot(datos, aes(efecto, poder,  colour = factor(fuente)))
g <- g + geom_line()
g <- g + labs(colour = "")
g <- g + ylab("Poder estadístico")
g <- g + xlab("Tamaño del efecto")

g <- g + scale_color_manual(values=c("red", "blue", "chartreuse4",
                                     "orange"))

g <- g + theme_pubr()
g <- g + ggtitle("Curvas de poder para prueba t bilateral")
g <- g + geom_vline(xintercept = 0, linetype = "dashed")

print(g)