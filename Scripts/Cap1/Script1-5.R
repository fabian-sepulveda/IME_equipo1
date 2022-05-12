library(dplyr)

# Cargar dataframe iris incluido en R.
datos <- iris

# Seleccionar observaciones correspondientes a la especie versicolor.
versicolor <- datos %>% filter(Species == "versicolor")

# Seleccionar observaciones de la especie versicolor cuyos sépalos tengan una
# longitud igual o superior a 6 cm.
largas <- datos %>% filter(Species == "versicolor" & Sepal.Length >= 6)

# Seleccionar la especie y variables relativas a los pétalos.
petalos <- datos %>% select(Species, starts_with("Petal"))

# Seleccionar variables de ancho y la especie.
anchos <- datos %>% select(ends_with("Width"), Species)

# Agregar al conjunto de datos de los pétalos una nueva variable con la razón
# entre el largo y el ancho de éstos.
petalos <- petalos %>% mutate(Species, Petal.Width,
                              Petal.Ratio = Petal.Length / Petal.Width)

# Ordenar el conjunto de datos de pétalos en forma descendente según la razón
# de los pétalos.
petalos <- petalos %>% arrange(desc(Petal.Ratio))

# Ordenar el conjunto de datos de pétalos en forma ascendente según el largo de
# los pétalos.
petalos <- petalos %>% arrange(Petal.Length)