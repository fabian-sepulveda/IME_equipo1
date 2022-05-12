library(pwr)

# Fijar valores conocidos.
n <- 36
diferencia <- 4
desv_est <- 12
alfa <- 0.05
poder <- 0.9

# Calcular el poder usando la función power.t.test().
cat("Cálculo del poder con power.t.test()\n")

resultado <- power.t.test(n = n,
                          delta = diferencia,
                          sd = desv_est,
                          sig.level = alfa,
                          power = NULL,
                          type = "paired",
                          alternative = "two.sided")

print(resultado)

# Cálculo del tamaño de la muestra usando la función power.t.test().
cat("Cálculo del tamaño de la muestra con power.t.test()\n")

resultado <- power.t.test(n = NULL,
                          delta = diferencia,
                          sd = desv_est,
                          sig.level = alfa,
                          power = poder,
                          type = "paired",
                          alternative = "two.sided")

n <- ceiling(resultado[["n"]])
cat("n = ", n, "\n")

# Calcular el tamaño del efecto (d de Cohen).
d <- (4 / desv_est) * ((n - 2) / (n - 1.25))

# Calcular el poder usando la función pwr.t.test().
cat("\n\nCálculo del poder con pwr.t.test()\n")

resultado <- pwr.t.test(n = n,
                        d = d,
                        sig.level = alfa,
                        power = NULL,
                        type = "paired",
                        alternative = "two.sided")

print(resultado)

# Cálculo del tamaño de la muestra usando la función pwr.t.test().
cat("\nCálculo del tamaño de la muestra con pwr.t.test()\n")

resultado <- pwr.t.test(n = NULL,
                        d = d,
                        sig.level = alfa,
                        power = poder,
                        type = "paired",
                        alternative = "two.sided")

n <- ceiling(resultado[["n"]])
cat("n = ", n, "\n")