library(dplyr)

# Parte 1.
# Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito
# verificador) del integrante de menor edad del equipo.

set.seed(3473)

# Parte 2.
# Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 50 hombres (si la semilla es impar).

basename <- "EP13 Datos.csv"
file <- file.path("C:/Users/adolf/Desktop/Cosas/IME/IME_equipo1/EP13", basename)
datos <- read.csv2(file = file)

hombres <- datos %>% filter(Gender == 1);
muestra <- sample_n(hombres, 50);

# Parte 3.
# Seleccionar de forma aleatoria ocho posibles variables predictoras.

variables <- colnames(muestra)
variables_aleatorias <- sample(variables, 8, replace = FALSE)

# Parte 4
# Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la variable 
# Peso, justificando bien esta selección.

# Hip.girth

# Parte 5
# Usando el entorno R, construir un modelo de regresión lineal simple con el predictor seleccionado en el
# paso anterior.

modelo <- lm(Weight ~ Hip.Girth, data = muestra)
print(summary(modelo))

# Parte 6