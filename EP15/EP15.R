library(dplyr)
library(leaps)
library(caret)
library(pROC)

#Función que entrega un 1 o un 0 dependiendo de si la observación presenta sobrepeso o no.
determinarEN <- function(i, dato){
  if(dato[i] >= 25){
    return (1)
  }
  else{
    return (0)
  }
}

#----------------------------------------------------------------------------------------------------------------

#1. Se fija la semilla para trabajar en el primer modelo.
set.seed(1111)

#Se lee la información desde el archivo csv entregado.
basename <- "EP13 Datos.csv"
file <- file.path("C:/Users/adolf/Desktop/Cosas/Progra/IME/IME_equipo1/EP14", basename)
datos <- read.csv2(file = file)

#Se transforma la altura de los datos de centímetros a metros.
altura <- datos$Height / 100
#Se crea un vector con los datos del IMC de cada observación usando la fórmula de peso / altura ** 2.
IMC <- datos$Weight / altura**2
#Se crea un vector de estado nutricional.
EN <- sapply(1:length(IMC), determinarEN, IMC)
#Se agregan al data frame los dos vectores creados (IMC y EN).
datos <- cbind(datos, IMC, EN)

#----------------------------------------------------------------------------------------------------------------

#2. Seleccionar una muestra de 100 personas, asegurando que la mitad tenga estado nutricional “sobrepeso”
#y la otra mitad “no sobrepeso”.
muestra <- rbind(sample_n(datos%>%filter(EN == 1), 50), sample_n(datos%>%filter(EN == 0), 50))
#Se reordena la muestra usando sample_n para que los valores 1 y 0 de EN no estén todos juntos.
muestra <- sample_n(muestra, nrow(muestra))

#----------------------------------------------------------------------------------------------------------------

#3. Usando las herramientas del paquete leaps, realizar una búsqueda exhaustiva para seleccionar entre dos y 
#ocho predictores que ayuden a estimar la variable Peso (Weight), obviamente sin considerar las nuevas 
#variables IMC ni EN, y luego utilizar las funciones del paquete caret para construir un modelo de regresión 
#lineal múltiple con los predictores escogidos y evaluarlo usando bootstrapping.

#Para realizar la búsqueda exhaustiva se obtienen los nombres de todas las columnas del data frame de la muestra.
variables <- colnames(muestra)
#Se eliminan los nombres de las columnas Weight, IMC y EN, ya que no pueden ser utilizadas como predictores del
#modelo.
variables <- variables[-c(23, 26, 27)]
#Se crea un string que sigue la estructura de la fórmula que se usa en la búsqueda exhaustiva.
fstr <- paste("Weight", paste(variables, collapse = " + "), sep = " ~ ")
#Se transforma el string en una fórmula.
formula_m <- formula(fstr)

#Para realizar la búsqueda exhaustiva se usa la función regsubsets, del paquete leaps, que entre una representación
#gráfica de los mejores subconjuntos de predictores para el modelo.
busqueda <- regsubsets(formula_m, data = muestra, method = "exhaustive", nbest = 1, nvmax = 8)
#Se imprime el gráfico de la búsqueda exhaustiva.
print(plot(busqueda))

#En el gráfico se puede ver que el subconjunto con menor bic, y por lo tanto el mejor, es el que se encuentra
#en el siguiente vector.
variables2 <- c("Chest.Girth", "Waist.Girth", "Thigh.Girth", "Bicep.Girth", "Calf.Maximum.Girth", "Height", "Gender")
#Se crea un nuevo string con estructura de fórmula con las variables predictores obtenidas en la búsqueda exhaustiva.
fstr2 <- paste("Weight", paste(variables2, collapse = " + "), sep = " ~ ")
#Se transforma el string en una fórmula.
formula <- formula(fstr2)

#Para crear el modelo de regresión lineal múltiple se utilizará la función train, del paquete caret.
#Primero se define un train control, en el que se especifica que se usará el método de bootstrapping con un total de 
#999 repeticiones.
train.control <- trainControl(method = "boot", number = 999)
#Se crea el modelo usando la función train, entregándole a ésta la fórmula creada con las variables predictoras, 
#la muestra obtenida, definiendo el método en lm para la regresión lineal y definiendo el train control que 
#establecimos previamente.
modelo1 <- train(formula, data = muestra, method = "lm", trControl = train.control)
#Se imprime el modelo obtenido.
print(summary(modelo1))

#----------------------------------------------------------------------------------------------------------------

#4. Haciendo un poco de investigación sobre el paquete caret, en particular cómo hacer Recursive Feature Elimination 
#(RFE), construir un modelo de regresión lineal múltiple para predecir la variable IMC que incluya entre 10 y 20 
#predictores, seleccionando el conjunto de variables que maximice R2 y que use cinco repeticiones de validación 
#cruzada de cinco pliegues para evitar el sobreajuste (obviamente no se debe considerar las variables Peso,  
#Estatura ni estado nutricional –Weight, Height, EN respectivamente).

#Se definen las columnas que se descartarán del data frame de la muestra, que son Weight, Height EN e IMC, ya que 
#estas variables no pueden usarse para predecir el IMC.
drop <- names(muestra) %in% c("Weight", "Height", "EN", "IMC")
#Se asigna el data frame de la muestra quitando las columnas mencionadas.
x2 <- sample(muestra[, !drop], replace = FALSE)
#Se asigna a y la columna de la variable que se quiere predecir, en este caso el IMC.
y2 <- sample(muestra[, 23], replace = FALSE)

#Para poder usar Recursive Feature Elimination primero se establecen los parámetros que tendrá el control de rfe
#Para esto se usa la función rfecontrol, definiendo en las funciones que se use lmFuncs y que el método sea validación
#cruzada de 5 pliegues con 5 repeticiones.
ctrl <- rfeControl(functions = lmFuncs, method = "repeatedcv", number = 5, repeats = 5, verbose = FALSE)
#Para crear el modelo se usa la función rfe, a la que se le entregan las variables x e y definidas anteriormente y
#se le indica en sizes que se quiere entre 10 y 20 predictores.
modelo2 <- rfe(x = x2, y = y2, rfeControl = ctrl, sizes = c(10:20))
#Se selecciona el modelo dentro de los datos retornados por la función rfe.
modelo2 <- modelo2$fit
#Se imprime el modelo obtenido.
print(summary(modelo2))

#----------------------------------------------------------------------------------------------------------------

#5. Usando RFE, construir un modelo de regresión logística múltiple para la variable EN que incluya el conjunto, 
#de entre dos y seis, predictores que entregue la mejor curva ROC y que utilice validación cruzada dejando uno
#fuera para evitar el sobreajuste (obviamente no se debe considerar las variables Peso, Estatura –Weight y 
#Height respectivamente– ni IMC).

#Se seleccionan las variables que se eliminarán porque no pueden ser usadas para predecir el estado nutricional.
drop <- names(muestra) %in% c("Weight", "Height", "IMC", "EN")
#Se asigna el data frame sin las variables seleccionadas anteriormente.
x3 <- muestra[, !drop]
#Se asigna la variable resultado y se transforma en factor.
y3 <- factor(muestra[, 27])

#Se define el control que se usará, indicando que se use lrFuncs, para realizar regresión logística y se selecciona
#el método de validación cruzada dejando uno fuera como se pide en el enunciado.
ctrl3 <- rfeControl(functions = lrFuncs, method = "LOOCV", verbose = FALSE)
#Para crear el modelo se usa la función rfe, a la que se le entregan las variables x e y definidas anteriormente y
#se le indica en sizes que se quiere entre 2 y 6 predictores.
modelo3 <- rfe(x = x3, y = y3, sizes = 2:6, rfeControl = ctrl3)
#Se selecciona el modelo dentro de los datos retornados por la función rfe.
modelo3 <- modelo3$fit
#Se imprime el modelo obtenido.
print(summary(modelo3))


umbral <- 0.5
prob_3 <- predict(modelo3, muestra, type = "response")

preds_3 <- sapply(prob_3, function(p) ifelse(p >= umbral, "1", "0"))
preds_3 <- factor(preds_3, levels = levels(datos[["EN"]]))

ROC_3 <- roc(muestra[["EN"]], prob_3)
plot(ROC_3)
