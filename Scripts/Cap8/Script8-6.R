library(tidyverse)
library(RVAideMemoire)
library(rcompanion)

# Crear matriz de datos.
instancia <- 1:15
annealing <- c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
hormigas <- c(0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1)
genetico <- c(1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1)
datos <- data.frame(instancia, annealing, hormigas, genetico)

# Llevar matriz de datos a formato largo.
datos <- datos %>% pivot_longer(c("annealing", "hormigas", "genetico"),
                                names_to = "metaheuristica",
                                values_to = "resultado")

datos[["instancia"]] <- factor(datos[["instancia"]])
datos[["metaheuristica"]] <- factor(datos[["metaheuristica"]])

# Hacer prueba Q de Cochran.
prueba <- cochran.qtest(resultado ~ metaheuristica | instancia,
                         data = datos, alpha = 0.05)

print(prueba)

# Procedimiento post-hoc con corrección de Bonferroni.
post_hoc_1 <- pairwiseMcnemar(resultado ~ metaheuristica | instancia,
                              data = datos, method = "bonferroni")

cat("\nProcedimiento post-hoc con corrección de Bonferroni\n")
print(post_hoc_1)
                            
# Procedimiento post-hoc con corrección de Holm.
post_hoc_2 <- pairwiseMcnemar(resultado ~ metaheuristica | instancia,
                              data = datos, method = "holm")

cat("\nProcedimiento post-hoc con corrección de Holm\n")
print(post_hoc_2)