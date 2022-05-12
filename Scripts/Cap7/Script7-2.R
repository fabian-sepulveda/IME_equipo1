# Fijar valores conocidos
n_hombres <- 48
n_mujeres <- 42
exitos_hombres <- 26
exitos_mujeres <- 20
alfa <- 0.05
valor_nulo <- 0

# Calcular probabilidades de éxito.
p_hombres <- exitos_hombres / n_hombres
p_mujeres <- exitos_mujeres / n_mujeres

# Estimar la diferencia.
diferencia <- p_hombres - p_mujeres
  
# Construcción del intervalo de confianza.
error_hombres <- (p_hombres * (1 - p_hombres)) / n_hombres
error_mujeres <- (p_mujeres * (1 - p_mujeres)) / n_mujeres
error_est <- sqrt(error_hombres + error_mujeres)
Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
inferior <- diferencia - Z_critico * error_est
superior <- diferencia + Z_critico * error_est
cat("Intervalo de confianza = [", inferior, ", ", superior, "]\n", sep = "")

# Prueba de hipótesis.
p_agrupada <- (exitos_hombres + exitos_mujeres) / (n_hombres + n_mujeres)
error_hombres <- (p_agrupada * (1 - p_agrupada)) / n_hombres
error_mujeres <- (p_agrupada * (1 - p_agrupada)) / n_mujeres
error_est_hip <- sqrt(error_hombres + error_mujeres)
Z <- (diferencia - valor_nulo) / error_est_hip
p <- 2 * pnorm(Z, lower.tail = FALSE)
cat("Hipótesis alternativa bilateral\n")
cat("Z =", Z, "\n")
cat("p =", p)