# Construir la tabla de contingencia.
vacuna <- c(rep("Argh", 6), rep("Grrr", 11))
resultado <- c(rep("Humano", 12), rep("Vampiro", 5))
datos <- data.frame(resultado, vacuna)
tabla <- xtabs(~., datos)
print(tabla)

# Aplicar prueba exacta de Fisher.
alfa <- 0.05
prueba <- fisher.test(tabla, 1-alfa)
print(prueba)