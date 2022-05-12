library(ggpubr)

# Generar una muestra donde la media cumpla con la hipótesis nula.
set.seed(872)

media_poblacion_antiguo <- 530
media_muestra_nuevo <- 527.9
desv_est <- 48
n <- 1600
error_est <- desv_est / sqrt(n)

x <- seq(media_poblacion_antiguo - 5.2 * error_est,
         media_poblacion_antiguo + 5.2 * error_est,
         0.01)

y <- dnorm(x, mean = media_poblacion_antiguo, sd = error_est)

datos <- data.frame(x, y)

# Graficar la muestra.
g <- ggplot(data = datos, aes(x))

g <- g + stat_function(fun = dnorm,
                       args = list(mean = media_poblacion_antiguo,
                                   sd = error_est),
                       colour = "steelblue", size = 1)

g <- g + ylab("")
g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_x_continuous(name = "Tiempo de procesamiento [ms]")
g <- g + theme_pubr()

# Colorear el área igual o menor que la media observada.
g <- g + geom_area(data = subset(datos,
                                 x < media_muestra_nuevo),
                   aes(y = y),
                   colour = "steelblue",
                   fill = "steelblue",
                   alpha = 0.5)

# Agregar una línea vertical para el valor nulo.
g <- g + geom_vline(aes(xintercept = media_poblacion_antiguo),
                    color = "red", linetype = 1)

print(g)

# Calcular el valor Z para la muestra.
Z <- (media_muestra_nuevo - media_poblacion_antiguo) / error_est

# Calcular el valor p.
p_1 <- pnorm(Z, lower.tail = TRUE)

cat("Valor p: ", p_1, "\n")

# También se puede calcular el valor p directamente a partir de la
# distribución muestral definida por el valor nulo y el error
# estándar.
p_2 <- pnorm(media_muestra_nuevo, mean = media_poblacion_antiguo,
             sd = est_err)

cat("Valor p: ", p_2)