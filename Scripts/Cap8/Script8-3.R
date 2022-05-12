# Crear tabla de contingencia.
comestible <- c(404, 1948, 32, 228, 1596)
venenoso <- c(48, 1708, 0, 600, 1556)

tabla <- as.table(rbind(comestible, venenoso))

dimnames(tabla) <- list(tipo = c("comestible", "venenoso"),
                        sombrero = c("campana", "convexo", "hundido",
                                     "nudoso", "plano"))


print(tabla)

# Hacer prueba chi-cuadrado de independencia.
prueba <- chisq.test(tabla)
cat("\nLa prueba internamente calcula los valores esperados:\n")
esperados <- round(prueba[["expected"]], 3)
print(esperados)

cat("\nResultado de la prueba:\n")
print(prueba)