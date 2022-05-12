# Crear tabla de contingencia.
programadores <- c(42, 56, 51, 27, 24)
programadoras <- c(25, 24, 27, 15, 9)

tabla <- as.table(rbind(programadores, programadoras))

dimnames(tabla) <- list(sexo = c("programadores", "programadoras"),
                        lenguajes = c("C", "Java", "Python", "Ruby", "Otro"))

print(tabla)

# Hacer prueba chi-cuadrado de homogeneidad.
prueba <- chisq.test(tabla)
print(prueba)