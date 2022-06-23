library(readxl)
library(ggpubr)
TRIAL1 <- read_excel("C:/Users/fabia/Downloads/TRIAL1.xlsx")
TRIAL2 <- read_excel("C:/Users/fabia/Downloads/TRIAL2.xlsx")
TRIAL3 <- read_excel("C:/Users/fabia/Downloads/TRIAL3.xlsx")

borrar <- c("Tiempo")

TRIAL2 <- TRIAL2[ , !(names(TRIAL2) %in% borrar)]
TRIAL3 <- TRIAL3[ , !(names(TRIAL3) %in% borrar)]

#PALMAR LARGO
tiempo1 <- as.vector(TRIAL1$Tiempo)
palmar1 <- as.vector(TRIAL1$Extensor_del_carpo)
palmar2 <- as.vector(TRIAL2$Extensor_del_carpo)
palmar3 <- as.vector(TRIAL3$Extensor_del_carpo)
datos1 <- data.frame(tiempo1, palmar1)
datos2 <- data.frame(tiempo1, palmar2)
datos3 <- data.frame(tiempo1, palmar3)

datos <- data.frame(palmar1, palmar2, palmar3)

color <- c("red", "green", "blue")
g_Palmar <- matplot(x = tiempo1, y = datos, type = "l",xlab = "Tiempo", ylab = "Extensor radial del carpo", col = color)

