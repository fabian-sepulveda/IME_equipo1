library(ggpubr)

# Establecer la semilla para generar números aleatorios.
set.seed(9437)

# Generar aleatoriamente una población de tamaño 1500
# (en este caso, con una distribución cercana a la normal).
poblacion <- rnorm(n = 1500, mean = 4.32, sd = 0.98)

# Calcular la media de la población.
media_poblacion <- mean(poblacion)
cat("Media de la población:", media_poblacion, "\n")

# Tomar una muestra de tamaño 1250.
tamano_muestra <- 1250
muestra <- sample(poblacion, tamano_muestra)

# Calcular las medias acumuladas (es decir, con muestras de
# 1, 2, 3, ... elementos).
n <- seq(along = muestra)
media <- cumsum(muestra) / n

# Crear una matriz de datos con los tamaños y las medias muestrales.
datos <- data.frame(n, media)

# Graficar las medias muestrales.
g <- ggline(data = datos,
             x = "n",
             y = "media",
             plot_type = "l",
             color = "blue",
             main = "Media móvil",
             xlab = "Tamaño de la muestra",
             ylab = "Media muestral")

# Añadir al gráfico una recta con la media de la población.
g <- g + geom_hline(aes(yintercept = media_poblacion),
                    color = "red", linetype = 2)

print(g)