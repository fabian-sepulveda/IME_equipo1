library(ggpubr)

# Cargar los datos.
tiempo <- c(411.5538, 393.2753, 445.8905, 411.4022, 498.8969,
            388.6731, 430.0382, 469.4734, 409.5844, 442.0800,
            418.1169, 408.4110, 463.3733,407.0908, 516.5222)

# Establecer los datos conocidos.
n <- length(tiempo)
grados_libertad <- n - 1
valor_nulo <- 500


# Verificar si la distribución se acerca a la normal.
g <- ggqqplot(data = data.frame(tiempo),
              x = "tiempo",
              color = "steelblue",
              xlab = "Teórico",
              ylab = "Muestra",
              title = "Gráfico Q-Q muestra v/s distr. normal")

print(g)

# Fijar un nivel de significación.
alfa <- 0.025

# Calcular el estadístico de prueba.
cat("\tPrueba t para una muestra\n\n")
media <- mean(tiempo)
cat("Media =", media, "M$\n")
desv_est <- sd(tiempo)
error <- desv_est / sqrt(n)
t <- (media - valor_nulo) / error
cat("t =", t, "\n")

# Calcular el valor p.
p <- pt(t, df = grados_libertad, lower.tail = TRUE)
cat("p =", p, "\n")

# Construir el intervalo de confianza.
t_critico <- qt(alfa, df = grados_libertad, lower.tail = FALSE)
superior <- media + t_critico * error
cat("Intervalo de confianza = (-Inf, ", superior, "]\n", sep = "")

# Aplicar la prueba t de Student con la funcuón de R.
prueba <- t.test(tiempo,
                 alternative = "less",
                 mu = valor_nulo,
                 conf.level = 1 - alfa)

print(prueba)