library(ggpubr)

# Cargar datos.
datos <- read.csv2("C:/Inferencia/Mtcars.csv", stringsAsFactors = TRUE,
                   row.names = 1)

# Crear gráfico de dispersión.
g <- ggscatter(datos,
               x = "Rendimiento",
               y = "Peso",
               color = "red",
               title = "Rendimiento v/s peso",
               xlab = "Rendimiento [millas/galón]",
               ylab = "Peso [1000 lb]")

print(g)