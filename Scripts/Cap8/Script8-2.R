# Crear tabla de contingencia.
nomina <- c(236, 78, 204, 76, 66)
muestra <- c(17, 9, 14, 10, 5)

tabla <- as.table(rbind(nomina, muestra))

dimnames(tabla) <- list(grupo = c("Nómina", "Muestra"),
                        lenguajes = c("C", "Java", "Python", "Ruby", "Otro"))

print(tabla)

# Verificar si se esperan más de 5 observaciones por cada grupo.
n_nomina <- sum(nomina)
n_muestra <- 55
proporciones <- round(nomina/n_nomina, 3)
esperados <- round(proporciones * n_muestra, 3)
print(esperados)

# Hacer prueba chi-cuadrado de homogeneidad.
prueba <- chisq.test(tabla, correct = FALSE)
print(prueba)