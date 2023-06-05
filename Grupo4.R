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
  count(provincia) %>% 
  print(n=25)

#VER REPRESENTATIVIDAD

#Asigno la letra X a aquellas actividades clasificadas como Otros
puestos22 <- puestos22 %>%
  mutate(letra = ifelse(letra_desc == "OTROS", "X", letra))

#4177 observaciones con la letra X
puestos22 %>% 
  count(letra)

# Agrupamos provincias por regiones:

#Averiguo el codigo de aquellos departamentos (24) que pertenecen al AMBA segun INDEC 
#Fuente: https://www.indec.gob.ar/dbindec/folleto_gba.pdf
glimpse(diccionario_cod_depto)
diccionario_cod_depto %>% 
  filter(nombre_provincia_indec=="Buenos Aires") %>% 
  filter(nombre_departamento_indec %in% c("Lomas de Zamora", "Quilmes", "Lanús",
                                          "General San Martín", "Tres de Febrero",
                                          "Avellaneda", "Morón", "San Isidro", 
                                          "Malvinas Argentinas", "Vicente López",
                                          "San Miguel", "José C. Paz", "Hurlingham",
                                          "Ituzaingó", "La Matanza", "Almirante Brown",
                                          "Merlo", "Moreno", "Florencio Varela",
                                          "Tigre", "Berazategui", "Esteban Echeverría",
                                          "San Fernando", "Ezeiza"))
#verifico que sea integer
class(puestos22$departamento)

#creo vector con el codigo de deptos del AMBA por simplicidad.
deptosAMBA <- c(2000, 6028, 6035, 6091,6260, 6270, 
                6274, 6371, 6408, 6410, 6412, 
                6427, 6434, 6490, 6515, 6539, 
                6560, 6568, 6658, 6749, 6756, 
                6760, 6805, 6840, 6861)

#Agrupo por regiones, respetando la clasificacion del INDEC. 
#La region pampeana incluye a todos los departamentos de Bs.As que no pertencen al AMBA.

puestos22 <- puestos22 %>% 
  mutate(region = case_when(provincia %in% c(66, 38, 90, 10, 46, 86) ~ "NOA",
                            provincia %in% c(34, 22, 18, 54) ~ "NEA",
                            provincia %in% c(50, 70, 74) ~ "CUYO",
                            provincia %in% c(58, 62, 26, 78, 94) ~ "PATAGONIA",
                            provincia %in% c(14, 82, 42, 30, 6) &  
                              !(departamento %in% deptosAMBA) ~ "PAMPA",
                            departamento %in% deptosAMBA ~ "AMBA"))

unique(puestos22$region)

# Contamos la cantidad de NAs por columna

cantidadNA <- colSums(is.na(puestos22))
#los omitimos ya que no podemos analizar de donde provienen

puestos22 <- puestos22 %>% 
  na.omit()

#VER REPRESENTATIVIDAD

# PROMEDIO ANUAL
# calculamos la cantidad de puestos por provincia y actividad en el año
# los resultados están redondeados para arriba

#agrupacion por provincia y letra 
promedioAnual <- puestos22 %>% 
  group_by(provincia, letra) %>% 
  summarise(totalpuestos = sum(puestos, na.rm = TRUE)) %>% 
  mutate(promedio = ceiling(totalpuestos/12)) #lo redondeo para arriba

#agrupacion por departamento y clae2. 
promedioAnual2 <- puestos22 %>% 
  group_by(departamento, clae2) %>% 
  summarise(totalpuestos = sum(puestos, na.rm = TRUE)) %>% 
  mutate(promedio = ceiling(totalpuestos/12))

#creamos un nuevo data frame con los promedios incluidos  
puestos22_mean <-
  left_join(puestos22, promedioAnual2, by= c("departamento","clae2")) %>% 
  filter(fecha=="2022-02-01") %>% #me quedo con una fecha sola porque tengo el promedio
  select(-puestos, -totalpuestos, -fecha, -mes, -año) %>% 
  group_by(departamento, clae2)

cantNAA <- colSums(is.na(puestos22_mean))

glimpse(puestos22_mean)


#intento grafico 2--------
puestos22_red <- puestos22_mean %>%
  group_by(region, letra) %>% 
  summarise(weight = sum(promedio))

#normalizo para que el weight este entre 0 y 1 con media y desvio.
sum(puestos22_red$weight)

media_weight <- mean(puestos22_red$weight)
desvio_weight <- sd(puestos22_red$weight)

#agrego constante arbitraria para no tener valores negativos

puestos22_red_norm <- puestos22_red %>% 
  mutate(weight = ((weight-media_weight)/ desvio_weight) + 1 )


glimpse(puestos22_red_norm)


# prueba de red region-letra con weight --------

# Seleccionar solo las dos columnas deseadas
columnas_deseadas <- puestos22_red_norm[, c("region", "letra", "weight")]

# Crear la red de flujos
g <- graph_from_data_frame(columnas_deseadas, directed = FALSE)
# undirected?

E(g)$weight

bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type
g
# Personalizar los nodos y los bordes
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$size <- ifelse(V(g)$type, 15, 21)
V(g)$label.font <-  ifelse(V(g)$type, 1, 2) #negrita para regiones
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"

#Como no entra patagonia en el grafico, le asigno una abreviatura
V(g)$short_label <- ifelse(V(g)$name == "PATAGONIA", "PTG", V(g)$name)
is.weighted(g)
V(g)$label <- V(g)$short_label
# Visualizar la red de flujos

#establezco seed para que sea siempre el mismo grafico
seed_bipartita1 <- 1236
set.seed(seed_bipartita1)
plot(g, layout= layout_with_graphopt,
     vertex.label.cex = 1,
     vertex.label.color = "black",
     edge.width = E(g)$weight,
     main="Red Bipartita de Regiones y Tipo de Actividad Económica",
     main.y=-4,
     margin=c(0, 0, 0.1, 0))
   



