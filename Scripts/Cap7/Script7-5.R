# Fijar valores conocidos (hombres, mujeres)
n <-c(48, 42)
exitos <- c(26, 20)
alfa <- 0.05

# Prueba de Wilson en R.
prueba <- prop.test(exitos, n = n, alternative = "two.sided",
                    conf.level = 1 - alfa)

print(prueba)