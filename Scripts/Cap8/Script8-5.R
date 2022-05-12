# Construir la tabla de contingencia.
alumno <- seq(1:25)
modelo_1 <- c(rep("Correcto", 16), rep("Incorrecto", 9))
modelo_2 <- c(rep("Correcto", 9), rep("Incorrecto", 11), rep("Correcto", 5))
datos <- data.frame(alumno, modelo_2, modelo_1)
tabla <- table(modelo_2, modelo_1)
print(tabla)

# Aplicar prueba de McNemar.
prueba <- mcnemar.test(tabla)
print(prueba)