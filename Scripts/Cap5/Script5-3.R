# Cargar los datos.
instancia <- seq(1, 35, 1)

t_A <- c(436.5736, 470.7937, 445.8354, 470.9810, 485.9394,
         464.6145, 466.2139, 468.9065, 473.8778, 413.0639,
         496.8705, 450.6578, 502.9759, 465.6358, 437.6397,
         458.8806, 503.1435, 430.0524, 438.5959, 439.7409,
         464.5916, 467.9926, 415.3252, 495.4094, 493.7082,
         433.1082, 445.7433, 515.2049, 441.9420, 472.1396,
         451.2234, 476.5149, 440.7918, 460.1070, 450.1008)

t_B <- c(408.5142, 450.1075, 490.2311, 513.6910, 467.6467,
         484.1897, 465.9334, 502.6670, 444.9693, 456.3341,
         501.1443, 471.7833, 441.1206, 544.1575, 447.8844,
         432.4108, 477.1712, 482.4828, 458.2536, 474.9863,
         496.0153, 485.8112, 457.4253, 483.3700, 510.7131,
         467.5739, 482.5621, 453.5986, 385.9391, 548.7884,
         467.2533, 494.7049, 451.9716, 522.3699, 444.1270)

diferencia <- t_A - t_B

# Verificar si la distribución se acerca a la normal.
normalidad <- shapiro.test(diferencia)
print(normalidad)

# Fijar un nivel de significación.
alfa <- 0.05

# Aplicar la prueba t de Student a la diferencia de medias.
valor_nulo <- 0

prueba_1 <- t.test(diferencia,
                   alternative = "two.sided",
                   mu = valor_nulo,
                   conf.level = 1 - alfa)

print(prueba_1)

# Otra alternativa puede ser aplicar la prueba t de Student
# para dos muestras pareadas.
prueba_2 <- t.test(x = t_A,
                   y = t_B,
                   paired = TRUE,
                   alternative = "two.sided",
                   mu = valor_nulo,
                   conf.level = 1 - alfa)

print(prueba_2)