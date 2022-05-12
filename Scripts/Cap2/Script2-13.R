library(ggpubr)

# Cargar datos.
datos <- read.csv2("C:/Inferencia/Mtcars.csv", stringsAsFactors = TRUE,
                   row.names = 1)

# Gráfico para variables independientes.
g1 <- ggscatter(datos,
                x = "Peso",
                y = "Cuarto_milla",
                color = "blue",
                title = "Independientes",
                xlab = "Peso [1000 lb]",
                ylab = "Tiempo para recorrer un cuarto de milla [s]")

# Gráfico para variables con asociación positiva.
g2 <- ggscatter(datos,
                x = "Peso",
                y = "Potencia",
                color = "orange",
                title = "Asociación positiva",
                xlab = "Peso [1000 lb]",
                ylab = "Potencia [hp]")

# Gráfico para variables con asociación negativa.
g3 <- ggscatter(datos,
                x = "Peso",
                y = "Rendimiento",
                color = "black",
                title = "Asociación negativa",
                xlab = "Peso [1000 lb]",
                ylab = "Rendimiento [millas/galón]")

# Crear figura con tres gráficos.
g  <- ggarrange(g1 ,g2 ,g3, ncol = 3, nrow = 1, common.legend = TRUE)

print(g)