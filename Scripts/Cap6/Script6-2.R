library(ggpubr)

# Generar un vector con un rango para el tamaño de la muestra.
n <- seq(5, 8000, 5)

# Definir constantes
desv_est <- 6
alfa <- 0.05
tam_efecto <- 0.5

# Se calcula el poder con que se detecta el tamaño del efecto para
# cada tamaño de la muestra, asumiendo una prueba bilateral para
# una sola muestra.
poder <- power.t.test(n = n,
                      delta = tam_efecto,
                      sd = desv_est,
                      sig.level = alfa,
                      type = "two.sample",
                      alternative = "two.sided")$power

# Crear un data frame.
datos <- data.frame(n, poder)

# Graficar la curva de poder.
g <- ggplot(datos, aes(n, poder))
g <- g + geom_line(colour = "red")
g <- g + ylab("Poder estadístico")
g <- g + xlab("Tamaño de la muestra")
g <- g + theme_pubr()
g <- g + ggtitle("Relación entre el poder y el tamaño de la muestra")

print(g)