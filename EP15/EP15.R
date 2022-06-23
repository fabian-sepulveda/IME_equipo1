library(dplyr)
library(leaps)
library(caret)
library(pROC)
set.seed(5888)

#Se lee la información desde el archivo csv entregado.
basename <- "EP13 Datos.csv"
file <- file.path("C:/Users/fabia/Desktop/01-2022/IME/IME_equipo1/EP14", basename)
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
muestra <- sample_n(muestra, nrow(muestra))
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

drop <- names(muestra) %in% c("Weight", "Height", "EN", "IMC")
x2 <- muestra[, !drop]
y2 <- muestra[, 23]

ctrl <- rfeControl(functions = lmFuncs, method = "repeatedcv", repeats = 5, verbose = FALSE)
modelo2 <- rfe(x = x2, y = y2, sizes = c(10:20),data = muestra, rfeControl = ctrl)
modelo2 <- modelo2$fit
print(summary(modelo2))

set.seed(8091)

drop <- names(muestra) %in% c("Weight", "Height", "IMC")
x3 <- muestra[, !drop]
y3 <- muestra[, 27]

ctrl3 <- rfeControl(functions = lrFuncs, method = "LOOCV", verbose = FALSE)
modelo3 <- rfe(x = x3, y = y3, sizes = c(2:6), rfeControl = ctrl3)
modelo3 <- modelo3$fit
print(summary(modelo3))


umbral <- 0.5
prob_3 <- predict(modelo3, muestra, type = "response")

preds_3 <- sapply(prob_3, function(p) ifelse(p >= umbral, "1", "0"))
preds_3 <- factor(preds_3, levels = levels(datos[["EN"]]))

ROC_3 <- roc(muestra[["EN"]], prob_3)
plot(ROC_3)
