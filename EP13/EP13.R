library(dplyr)
library(car)
library(caret)

# Parte 1.
# Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito
# verificador) del integrante de menor edad del equipo.

#Se fija la semilla tal como se pidió
set.seed(3473)

# Parte 2.
# Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 50 hombres (si la semilla es impar).

#Se lee la información desde el archivo csv entregado.
basename <- "EP13 Datos.csv"
file <- file.path("C:/Users/adolf/Desktop/Cosas/IME/IME_equipo1/EP13", basename)
datos <- read.csv2(file = file)

#Ya que la semilla es impar se filtran las observaciones correspondientes a hombres.
hombres <- datos %>% filter(Gender == 1)
#Se obtiene una muestra de 100 observaciones, de las cuales se usarán las primeras 50 para entrenar el modelo y las 
#otras 50 para pruebas de validación cruzada.
muestra_larga <- sample_n(hombres, 100)
muestra <- muestra_larga[1:50, ]

# Parte 3.
# Seleccionar de forma aleatoria ocho posibles variables predictoras.

#Para seleccionar las variables de manera aleatoria se obtienen los nombres de las columnas usando la función
#colnames.
variables <- colnames(muestra)
#Se obtiene una muestra de 8 variables desde la lista.
variables_aleatorias <- sample(variables, 8, replace = FALSE)

# Parte 4
# Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la variable 
# Peso, justificando bien esta selección.

#De las variables restantes (que no fueron seleccionadas en la selección al azar) se seleccionó la variable
#Waist.Girth, la cual representa el grosor del cuerpo a la altura de la cintura. Como equipo decidimos seleccionar
#esta variable ya que consideramos que la mayor parte del peso de una persona se concentra en la zona abdominal,
#y que una persona más pesada tendría un mayor grosor en esta zona, y dado que el grosor a la altura del ombligo
#(Navel.Girth) ya fue seleccionada de manera aleatoria, seleccionamos el grosor a la altura de la cintura (Waist.Girth).

# Parte 5
# Usando el entorno R, construir un modelo de regresión lineal simple con el predictor seleccionado en el
# paso anterior.

#Se construye el modelo de regresión lineal simple usando la función lm y la variable Waist.Girth como predictor.
modelo <- lm(Weight ~ Waist.Girth, data = muestra)
print(summary(modelo))

# Parte 6
# Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco predictores de
# entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de regresión lineal simple 
# obtenido en el paso 5.

#Para seleccionar las mejores variables para agregar al modelo se utilizará la selección hacia adelante, la que 
#elige y agrega al modelo la variable que mayor efecto provoca al ser incluida. Para realizar esta selección es
#necesario crear un modelo ajustado que contenga una fórmula que incluya todas las variables que se seleccionaron
#aleatoriamente, junto con la variable seleccionada por el equipo.

#Para crear el modelo ajustado se agrega la variable Waist.Girth a la lista de las variables aleatorias.
variables_aleatorias <- c(variables_aleatorias, "Waist.Girth")
#Se crea un string que sigue la estructura de una fórmula.
fstr <- paste("Weight", paste(variables_aleatorias, collapse = " + "), sep = " ~ ")
#Se transforma el string en una fórmula, para ser usado en el modelo ajustado.
formula_m <- formula(fstr)

#Se crea el modelo ajustado con la fórmula creada.
modelo_ajustado <- lm(formula_m, data = muestra)

#Se realiza la selección hacia adelante usando la función step, ingresando como modelo nulo al modelo de regresión
#lineal simple creado en la parte 5 del trabajo, junto con el modelo ajustado creado anteriormente.
adelante <- step(modelo, scope = list(upper = modelo_ajustado), direction = "forward", trace = 0)
print(summary(adelante))

#Luego de realizar la selección hacia adelante el nuevo modelo de regresión lineal múltiple incluye las siguientes 
#variables: Waist.Girth, Knee.Girth, Height, Forearm.Girth, Thight.Girth y Knees.diameter.

# Parte 7
# Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que deben cumplir.

#Para evaluar los modelo se revisará paso por paso que cumplan las condiciones necesarias.
#Se fijará un nivel de significación igual a 0.05.
alfa <- 0.05

# 1. Independencia de los residuos:
#Para comprobar la independencia de los residuos se usará la prueba de Durbin-Watson, usando la función durbinWatsonTest
#de R.
prueba1 <- durbinWatsonTest(adelante)
#Se imprime el resultado de la prueba.
print(prueba1)

#Del resultado anterior se obtiene un p-valor igual a 0.492, el cual es mayor al nivel de significación, por lo que 
#se falla al rechazar la hipótesis nula, concluyendo con un 95% de confianza que los residuos son independientes.

# 2. Distribución normal de los residuos:
#Para comprobar si los residuos tienen una distribución cercana a la normal se usará la prueba de Shapiro-Wilk.
prueba2 <- shapiro.test(adelante$residuals)
#Se imprime el resultado de la prueba.
print(prueba2)

#De la prueba se obtiene un p-valor igual a 0.178, el cual es mayor al nivel de significación, por lo que se falla
#al rechazar la hipótesis nula, pudiendo concluir con 95% de confianza que los residuos si siguen una distribución 
#cercana a la normal.

# 3. Homocedasticidad de los residuos:
#Para comprobar la homocedasticidad de los residuos se usará la función ncvTest de R.
prueba3 <- ncvTest(adelante)
#Se imprime el resultado
print(prueba3)

#En la prueba se obtiene un p-valor igual a 0.22631, el cual es mayor al nivel de significación, por lo que se falla 
#al rechazar la hipótesis nula, concluyendo con un 95% de confianza que las varianzas de los residuos son iguales, 
#cumpliéndose el principio de homocedasticidad.

# 4. Multicolinealidad:
#Para verificar la multicolinealidad se empleará el factor de inflación de varianza (VIF), que se calcula usando
#la función vif, junto con el estadístico tolerancia (1/VIF).
vifs <- vif(adelante)
#Se imprime por consola el VIF de cada variable.
print(vifs)

#Según la información entregada por la lectura, el VIF de una variable no debe ser mayor o igual a 10, cosa que no 
#ocurre en las variables presentes en el modelo, ya que el VIF de mayor valor corresponde a 3.642660.

#Se imprimen por consola el estadístico tolerancia.
print(1 / vifs)

#También basándose en la información de la lectura, este estadístico no debe ser menor a 0.2, lo que no ocurre en 
#ninguna variable, por lo que se confirma que se cumple la multicolinealidad.

# 5. Validación cruzada:

modelo <- train(adelante$terms, data = muestra, method = "lm", trControl = trainControl(method = "cv", number = 5))
print(modelo)

# 6. Tamaño de la muestra:

#Ta bien.

# Parte 8

muestra2 <- muestra_larga[51:100, ]
modelo <- train(adelante$terms, data = muestra2, method = "lm", trControl = trainControl(method = "cv", number = 5))
print(modelo)
