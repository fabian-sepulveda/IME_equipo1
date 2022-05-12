library(dplyr)

datos <- read.csv2("C:/Inferencia/Mtcars.csv", stringsAsFactors = TRUE,
                   row.names = 1)

# Cálculo de varias medidas para la variable Potencia.
medidas_potencia <- datos %>% summarise(Media = mean(Potencia),
                                        Mediana = median(Potencia),
                                        Varianza = var(Potencia),
                                        IQR = IQR(Potencia))

print(medidas_potencia)
cat("\n")

# Cálculo de la media y la desviación estándar para las variables Peso y
# Cuarto_milla.
medidas_varias <- datos %>% summarise(Media_P = mean(Peso),
                                        Media_C = median(Cuarto_milla),
                                        SD_P = sd(Peso),
                                        SD_C = sd(Cuarto_milla))

print(medidas_varias)
cat("\n")