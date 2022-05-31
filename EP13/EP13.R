library(dplyr)
library(car)
library(caret)

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
variables_aleatorias <- c(variables_aleatorias, "Hip.Girth")
fstr <- paste("Weight", paste(variables_aleatorias, collapse = " + "), sep = " ~ ")
formula_m <- formula(fstr)

# Parte 4
# Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la variable 
# Peso, justificando bien esta selección.

# Hip.girth

# Parte 5
# Usando el entorno R, construir un modelo de regresión lineal simple con el predictor seleccionado en el
# paso anterior.

modelo <- lm(Weight ~ Hip.Girth, data = muestra)
#print(summary(modelo))

# Parte 6
# Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco predictores de
# entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de regresión lineal simple 
# obtenido en el paso 5.

modelo_ajustado <- lm(formula_m, data = muestra)

adelante <- step(modelo, scope = list(upper = modelo_ajustado), direction = "forward", trace = 0)
print(summary(adelante))

# Parte 7
# Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que deben cumplir.

alfa <- 0.05

# 1. Independencia de los residuos:
prueba1 <- durbinWatsonTest(adelante)
#print(prueba1)
# Independiente

# 2. Distribución normal de los residuos:
prueba2 <- shapiro.test(adelante$residuals)
#print(prueba2)
#Es normal

# 3. Homocedasticidad de los residuos:
prueba3 <- ncvTest(adelante)
#print(prueba3)
#Se cumple

# 4. Multicolinealidad:
vifs <- vif(adelante)
# print(vifs)
# print(1 / vifs)
# print(mean(vifs))

# 5. Validación cruzada:

modelo <- train(adelante$terms, data = muestra, method = "lm", trControl = trainControl(method = "cv", number = 5))
print(modelo)

# 6. Tamaño de la muestra:

#Ta bien.

# Parte 8
