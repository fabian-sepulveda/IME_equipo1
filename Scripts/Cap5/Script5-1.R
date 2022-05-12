library(TeachingDemos)
library(ggpubr)

# Ingresar los datos.
muestra <- c(19.33, 29.37, 29.14, 32.10, 25.04, 22.22, 31.26, 26.92,
             31.40, 17.66, 22.55, 20.69, 24.68, 28.74, 26.85, 29.68,
             29.27, 26.72, 27.08, 20.62)

# Establecer los datos conocidos.
desv_est <- 2.32
n <- length(muestra)
valor_nulo <- 20

# Crear gráfico Q-Q para verificar la distribución de la muestra,
datos <- data.frame(muestra)

g <- ggqqplot(datos, x = "muestra", color = "SteelBlue")
print(g)

# Verificar distribución muestral usando la prueba de normalidad
# de Shapiro-Wilk.
normalidad <- shapiro.test(muestra)
print(normalidad)

# Fijar un nivel de significación.
alfa <- 0.01

# Calcular la media de la muestra.
cat("\tPrueba Z para una muestra\n\n")
media <- mean(muestra)
cat("Media =", media, "M$\n")

# Calcular el estadístico de prueba.
Z <- (media - valor_nulo) / desv_est
cat("Z =", Z, "\n")

# Calcular el valor p.
p <- 2 * pnorm(Z, lower.tail = FALSE)
cat("p =", p, "\n")

# Hacer la prueba Z con R.
prueba <- z.test(media, mu = valor_nulo, alternative = "two.sided",
                 stdev = desv_est, conf.level = 1-alfa)

print(prueba)