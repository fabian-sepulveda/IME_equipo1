library(dplyr)
library(car)
library(mlogit)


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
datos <- cbind(datos, EN)

set.seed(8091)

hombres.sobrepeso <- sample_n(datos%>%filter(Gender == 1 & EN == 1), 60)
hombres.nosobrepeso <- sample_n(datos%>%filter(Gender == 1 & EN == 0), 60)

muestra.entrenamiento <- rbind(hombres.sobrepeso[1:40, ], hombres.nosobrepeso[1:40, ])
muestra.prueba <- rbind(hombres.sobrepeso[41:60, ], hombres.nosobrepeso[41:60, ])

#Knee.Girth, Biiliac.diameter, Thigh.Girth, Height, Calf.Maximum.Girth, Navel.Girth, Knees.diameter, 
#Forearm.Girth.

variables <- c("Knee.Girth", "Biiliac.diameter", "Thigh.Girth", "Calf.Maximum.Girth",
              "Navel.Girth", "Knees.diameter", "Forearm.Girth")

#Se selecciona la variable Waist.Girth.

modelo <- glm(EN ~ Waist.Girth, family = binomial(link = "logit"), data = muestra.entrenamiento)
print(summary(modelo))

variables <- c(variables, "Waist.Girth")
fstr <- paste("EN", paste(variables, collapse = " + "), sep = " ~ ")
#Se transforma el string en una fórmula, para ser usado en el modelo ajustado.
formula_m <- formula(fstr)

modelo.completo <- glm(formula_m, family = binomial(link = "logit"), data = muestra.entrenamiento)

nuevo.modelo <- step(modelo, scope = list(lower = modelo, upper = modelo.completo), direction = "forward", trace = 0)
print(summary(nuevo.modelo))

#Condiciones
#1. Independencia de los residuos:
prueba1 <- durbinWatsonTest(nuevo.modelo)
print(prueba1)

nuevo.modelo <- update(nuevo.modelo, . ~ . - Forearm.Girth)

#pairs()

