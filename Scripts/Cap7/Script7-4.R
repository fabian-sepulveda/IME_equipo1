# Fijar valores conocidos
n <- 150
p_exito <- 0.64
alfa <- 0.05
valor_nulo <- 0.7

# Calcular cantidad de éxitos.
exitos <- p_exito * n

# Prueba de Wilson en R.
prueba <- prop.test(exitos, n = n, p = valor_nulo,
                    alternative = "greater", conf.level = 1 - alfa)

print(prueba)