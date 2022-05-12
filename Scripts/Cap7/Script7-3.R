# Fijar valores conocidos
n_hombres <- 89
n_mujeres <- 61
exitos_hombres <- 45
exitos_mujeres <- 21
alfa <- 0.05
valor_nulo <- 0.1

# Calcular probabilidades de éxito.
p_hombres <- exitos_hombres / n_hombres
p_mujeres <- exitos_mujeres / n_mujeres

# Estimar la diferencia.
diferencia <- p_hombres - p_mujeres
  
# Prueba de hipótesis.
p_agrupada <- (exitos_hombres + exitos_mujeres) / (n_hombres + n_mujeres)
error_hombres <- (p_hombres * (1 - p_hombres)) / n_hombres
error_mujeres <- (p_mujeres * (1 - p_mujeres)) / n_mujeres
error_est <- sqrt(error_hombres + error_mujeres)
Z <- (diferencia - valor_nulo) / error_est
p <- pnorm(Z, lower.tail = FALSE)
cat("Hipótesis alternativa bilateral\n")
cat("Z =", Z, "\n")
cat("p =", p)