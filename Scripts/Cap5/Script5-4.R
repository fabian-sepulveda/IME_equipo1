library(ggpubr)

# Cargar los datos.
vacuna_A <- c(6.04, 19.84, 8.62, 13.02, 12.20, 14.78, 4.53, 26.67,
             3.14, 19.14, 10.86, 13.13, 6.34, 11.16, 7.62)

vacuna_B <- c(5.32, 3.31, 5.68, 5.73, 4.86, 5.68, 2.93, 5.48, 6.10,
             2.56, 7.52, 7.41, 4.02)

# Verificar si las muestras se distribuyen de manera cercana
# a la normal.
normalidad_A <- shapiro.test(vacuna_A)
print(normalidad_A)
normalidad_B <- shapiro.test(vacuna_B)
print(normalidad_B)

# Fijar un nivel de significación.
alfa <- 0.01

# Aplicar la prueba t para dos muestras independientes.
prueba <- t.test(x = vacuna_A,
                 y = vacuna_B,
                 paired = FALSE,
                 alternative = "greater",
                 mu = 0,
                 conf.level = 1 - alfa)

print(prueba)

# Calcular la diferencia entre las medias.
media_A <- mean(vacuna_A)
media_B <- mean(vacuna_B)
diferencia <- media_A - media_B
cat("Diferencia de las medias =", diferencia, "[mg/ml]\n")
