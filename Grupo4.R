#------- Preparación de los datos -----------

library("tidyverse")
library("tidygraph")
library("igraph")

rm(list = ls()) #limpiar environment

# Cargamos el dataset entero

excel <- read.csv("puestos_depto_priv_por_clae2.csv", sep = ",")

# Filtramos para dejar solo los registros de 2022 y por mes
# Tener en cuenta: cada dato es sobre el 1er día de cada mes

puestos22 <- excel %>% 
  mutate(año = year(fecha), .after = fecha) %>% 
  filter(año==2022) %>% 
  mutate(mes = month(fecha), .after = fecha)
  

# Cambiamos nombres y atributos de las columnas para facilitar el análisis

colnames(excel)

colnames(puestos22)[4] = "departamento"
colnames(puestos22)[5] = "provincia"

summary(puestos22)
glimpse(puestos22)

# Joineamos tablas con nombre de provincias y descripcion de cada actividad

diccionario_clae2 <- read.csv(file = "diccionario_clae2.csv")
diccionario_cod_depto <- read.csv("diccionario_cod_depto.csv")

colnames(diccionario_cod_depto)[1] <- "departamento"
diccionario_cod_depto <- diccionario_cod_depto[-c(3)]

puestos22 <- left_join(puestos22, diccionario_clae2, by = "clae2")
puestos22 <- left_join(puestos22, diccionario_cod_depto, by = "departamento")

# Puestos de Trabajo bajo secreto estadístico: 
# reemplazamos los casilleros que tenían -99 en la columna puestos por 0
# DESARROLLAR Y JUSTIFICAR

length(which(puestos22$puestos == -99))
puestos22$puestos[which(puestos22$puestos == -99)] <- 0

# Contamos cuántos puestos hay bajo secreto estadístico por provincia

puestos22 %>% 
  group_by(puestos, provincia) %>% 
  filter(puestos == 0) %>% 
  count(provincia)
#VER REPRESENTATIVIDAD

# Agrupamos provincias por regiones:
# tomamos a CABA y BS AS como parte de la region PAMPA, 
# aunque el indec cuenta al gran buenos aires como una region aparte

puestos22 <- puestos22 %>% 
  mutate(region = case_when(provincia %in% c(66, 38, 90, 10, 46, 86) ~ "NOA",
                            provincia %in% c(34, 22, 18, 54) ~ "NEA",
                            provincia %in% c(50, 70, 74) ~ "CUYO",
                            provincia %in% c(14, 82, 42, 30) ~ "PAMPA",
                            provincia %in% c(58, 62, 26, 78, 94) ~ "PATAGONIA",
                            provincia %in% c(6, 2) ~ "BSAS"))

# Contamos la cantidad de NAs por columna

cantidadNA <- colSums(is.na(puestos22))
#VER REPRESENTATIVIDAD

# PROMEDIO ANUAL
# calculamos la cantidad de puestos por provincia y actividad en el año
# los resultados están redondeados para arriba

promedioAnual <- puestos22 %>% 
  group_by(provincia, letra) %>% 
  summarise(totalpuestos = sum(puestos, na.rm = TRUE)) %>% 
  mutate(promedio = totalpuestos/12)


# Gráfico Red de Flujos
# Seleccionar solo las dos columnas deseadas
columnas_deseadas <- puestos22[, c("nombre_provincia_indec", "letra_desc")]

# Crear la red de flujos
red_flujos <- graph_from_data_frame(columnas_deseadas, directed = TRUE)
# undirected?

# Personalizar los nodos y los bordes
V(red_flujos)$color <- "lightblue"  # Color de los nodos
V(red_flujos)$size <- 10  # Tamaño de los nodos
E(red_flujos)$color <- "grey"  # Color de los bordes

# Visualizar la red de flujos
plot(red_flujos)
