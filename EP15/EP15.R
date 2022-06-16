library(dplyr)
library(leaps)
library(caret)
set.seed(5888)

#Se lee la información desde el archivo csv entregado.
basename <- "EP13 Datos.csv"
file <- file.path("C:/Users/adolf/Desktop/Cosas/IME/IME_equipo1/EP14", basename)
datos <- read.csv2(file = file)

altura <- datos$Height / 100

IMC <- datos$Weight / altura**2

determinarEN <- function(i, dato){
  if(dato[i] >= 25){
    return (1)
  }
  else{
    return (0)
  }
}

EN <- sapply(1:length(IMC), determinarEN, IMC)
datos <- cbind(datos, IMC, EN)

muestra <- rbind(sample_n(datos%>%filter(EN == 1), 50), sample_n(datos%>%filter(EN == 0), 50))

variables <- colnames(muestra)
variables <- variables[-c(23, 26, 27)]

fstr <- paste("Weight", paste(variables, collapse = " + "), sep = " ~ ")
#Se transforma el string en una fórmula, para ser usado en el modelo ajustado.
formula_m <- formula(fstr)

busqueda <- regsubsets(formula_m, data = muestra, method = "exhaustive", nbest = 1, nvmax = 8)
print(plot(busqueda))

variables2 <- c("Knees.diameter", "Chest.Girth", "Waist.Girth", "Hip.Girth", "Bicep.Girth", "Calf.Maximum.Girth", "Age", "Height")
fstr2 <- paste("Weight", paste(variables2, collapse = " + "), sep = " ~ ")
formula <- formula(fstr2)

train.control <- trainControl(method = "boot", number = 100)
modelo1 <- train(formula, data = muestra, method = "lm", trControl = train.control)
#print(summary(modelo1))

set.seed(3473)

drop <- names(datos) %in% c("Weight", "Height", "EN", "IMC")
x <- datos[, !drop]
y <- datos[, 23]
size <- c(10:20)

ctrl <- rfeControl(functions = lmFuncs, method = "repeatedcv", repeats = 5, verbose = FALSE)
modelo2 <- rfe(x = x, y = y, sizes = size, rfeControl = ctrl)
print(modelo2)

