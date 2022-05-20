#ópcíón
library(ggpubr)

ingreso <- c(4250,8500,12800,19800,25500)
cmp <- c(4.12,4.19,4.35,4.66,5.26)
datos <- data.frame(ingreso,cmp)
modelo <- lm(cmp ~ ingreso, data = datos)
print(summary(modelo))

p <- ggscatter(datos, x = "ingreso", y = "cmp", color = "blue", fill = "blue",
               xlab = "USD/año", ylab = "%")
p <- p + geom_smooth(method = lm, se = FALSE, colour = "red")
print(p)
plot(modelo)