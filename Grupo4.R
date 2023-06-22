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
  mutate(weight = ((weight-media_weight)/ desvio_weight)+1)

#nuevo intento de normalizacion / x maximo
puestos22_red_norm2 <- puestos22_red %>% 
  mutate(weight = weight/ max(puestos22_red$weight))


max(puestos22_red$weight)

glimpse(puestos22_red_norm) %>% print(n=200)


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

#Proyecciones-----
#Creo proyeccion de actividades (muestro regiones)
proy_region <- bipartite.projection(g, which=FALSE)

#Creo proyeccion de regiones (muestro actividades)
proy_activ <- bipartite.projection(g, which=TRUE)

#Atributos de la proyeccion

#color
#Busco colores
colors()[grep("yellow", colors())]
colors()[grep("blue", colors())]
colors()[grep("violet", colors())]
colors()[grep("pink", colors())]
colors()[grep("grey", colors())]
V(proy_region)$color <- case_when(V(proy_region)$name=="NEA"~"greenyellow",
                                  V(proy_region)$name=="PATAGONIA"~"royalblue",
                                  V(proy_region)$name=="AMBA"~"orangered",
                                  V(proy_region)$name=="CUYO"~"violetred4",
                                  V(proy_region)$name=="NOA"~"gold",
                                  V(proy_region)$name=="PAMPA"~"lightpink")

## Proyeccion actividades------
#Ploteo la proyeccion de regiones. Red Completa
#podriamos hacer que los edges respeten el color de los nodos de los cuales salen¿?

plot(proy_region,
     vertex.label.cex = 1.25,
     vertex.label.color = "black",
     vertex.size=40,
     edge.color= "grey",
     edge.width = 2,
     margin=c(0, 0, 0.1, 0),
     main="¿Cómo se relacionan las regiones segun las actividades que realizan?",
     cex.main=1.5) #no cambia tamaño, tendria que estar un poco mas abajo, (main.y)

##Proyeccion Regiones-----
colors()[grep("light", colors())]

#podriamos hacer algun tipo de sectorizacion por color y agregar leyenda con activ.
plot(proy_activ,
     vertex.label.cex = 1.25,
     vertex.label.color = "black",
     vertex.label.font=2,
     vertex.color="lightskyblue",
     vertex.size=12,
     edge.color= "grey",
     edge.width = E(g)$weight, #el weight del original
     margin=c(0, 0, 0.1, 0),
     main="¿Cómo se relacionan las actividades segun las regiones donde se realizan?",
     cex.main=1.5)#no cambia tamaño, tendria que estar un poco mas abajo, (main.y))

## Comparación de nuestro dataset vs. los datos recopilados por el Ministerio de Economía----

# Filtrar campos específicos en las dos primeras columnas
datos_filtrados <- subset(puestos22,
                          "nombre_provincia_indec" %in% c("catamarca",
                                                          "jujuy", 
                                                          "la rioja",
                                                          "salta", 
                                                          "santiago del estero", 
                                                          "tucuman")
                          & "fecha" %in% c("2022-07-01"))

# Crear la tabla dinámica y obtener el total
total <- aggregate("puestos" ~ "nombre_provincia_indec" + "fecha" + "letra_desc", 
                   data = datos_filtrados, FUN = sum)

#Metricas------
degree(g)
closeness(g, V(g)) %>% sort()
betweenness(g) 
edge.betweenness(g)

E(g)[[57]]

g_sin_x

#elimino las actividades clasificadas como otros para que no distorsione analisis.
g_sin_x <- delete.vertices(g, 25)
degree(g_sin_x) #red completa
betweenness(g_sin_x, weights=NULL
            
#NEA-B es el que mas betweenness tiene
edge.betweenness(proy_activ, weights=NULL) 

##NEA-B es el que tiene el weight mas bajo tambien.
E(g_sin_x)[[38]]
min(E(g_sin_x)$weight)
            
all_shortest_paths(g_sin_x, from=3)
shortest_paths(g_sin_x, from = 3, weights=NA)
            
transitivity(proy_activ)
            
min(E(g_sin_x)$weight)
edge_density(g_sin_x)
#Analis de proyecciones por provincia y letra---------------------
            
#Creamos tabla con provincia y letra
puestos22_red_prov_letra<- puestos22_mean %>%
  group_by(nombre_provincia_indec, letra) %>% 
  summarise(weight = sum(promedio))
            
#Estandarizamos weight
puestos22_red_norm_prov_letra <- puestos22_red_prov_letra %>% 
              mutate(weight = ((weight-media_weight)/ desvio_weight)+1)
            
#Selecciono columnas
columnas_deseadas_prov_letra <- puestos22_red_norm_prov_letra[, c("nombre_provincia_indec", "letra", "weight")]
            
# Crear la red de flujos
gprov_letra <- graph_from_data_frame(columnas_deseadas_prov_letra, directed = FALSE)
gprov_letra <- delete.vertices(gprov_letra, 43) #elimino la categoria X
            
#Grafo bipartito
bipartite_mapping(gprov_letra)
V(gprov_letra)$type <- bipartite_mapping(gprov_letra)$type
            
#Abrevio nombres de provincias
V(gprov_letra)$short_label <- case_when(V(gprov_letra)$name == "Santiago del Estero" ~ "Sgo. del Estero",  
                                                    V(gprov_letra)$name == "Tierra del Fuego" ~ "T. del Fuego",
                                                    V(gprov_letra)$name == "Buenos Aires" ~ "Bs. As.",
                                                    !V(gprov_letra)$name %in% c("Santiago del Estero", "Tierra del Fuego", "Buenos Aires") ~ V(gprov_letra)$name)
E(gprov_letra)$weight
V(gprov_letra)$label <- V(gprov_letra)$short_label
            
##Proyecciones-------
###Proyeccion de provincias----
            
proy_prov_letra <- bipartite.projection(gprov_letra, which="false")
            
#Abreviatura para que entre en la forma seleccionada
V(proy_prov_letra)$short_label <- case_when(V(proy_prov_letra)$name == "Santiago Del Estero" ~ "S. Del Estero",  
                                                        V(proy_prov_letra)$name == "Tierra Del Fuego" ~ "T. Del Fuego",
                                                        V(proy_prov_letra)$name == "Buenos Aires" ~ "Bs. As.",
                                                        !V(proy_prov_letra)$name %in% c("Santiago Del Estero", "Tierra Del Fuego", "Buenos Aires") ~ V(proy_prov_letra)$name)
            
#Definimos el color de los nodos de acuerdo a la suma del weight por provincia
            
#Suma de los pesos  por provincia 
suma_pesos <- aggregate(weight ~ from, data = get.data.frame(gprov_letra), sum)
print(suma_pesos)
            
E(proy_prov_letra)$weight
#Normalizacion para poder usarlos en el plot
suma_pesos_norm <- (suma_pesos$weight - min(suma_pesos$weight)) / (max(suma_pesos$weight) - min(suma_pesos$weight))
            
#Busco paleta de colores
colores_intensidad <- heat.colors(length(suma_pesos_norm))
            
            
V(proy_prov_letra)$color <- colores_intensidad[match(V(proy_prov_letra)$name, suma_pesos$from)]
            
####Ploteo----
plot(proy_prov_letra, 
                 vertex.color= V(proy_prov_letra)$color,
                 vertex.frame.color= "black", 
                 vertex.shape= "rectangle",
                 vertex.size=27,
                 vertex.label= V(proy_prov_letra)$short_label,
                 vertex.label.cex=0.8,
                 vertex.label.color="black",
                 vertex.label.family="Helvetica",
                 vertex.label.font=2,
                 edge.width= E(gprov_letra)$weight*1.85, 
                 margin= c(0,0,0,0.6), 
                 asp=0.7,
                 main= "Relación entre provincias de acuerdo a las act. económicas clasificadas por letra")
            
#install.packages("fields")
library("fields")
            
#agrego leyenda con degrade de colores
image.plot(1, 1:length(colores_intensidad), matrix(1:length(colores_intensidad), nrow = 1),
                       col = colores_intensidad, axes = FALSE,
                       legend.only = TRUE, legend.shrink=0.4,
                       lab.breaks=c("Mayor", "", "", "","", "", "","", "", "","", "", "","", "", "",
                                    "", "", "","", "", "","", "","Menor"), 
                       horizontal = FALSE, 
                       key.width = 0.3)
            
# Agregar el título a la leyenda 
text(1.05,1.2, "Cantidad de puestos", cex = 1, font = 1, adj=0)
            
            
##Proyeccion de actividades por letra--------
proy_activ_prov <- bipartite.projection(gprov_letra, which="true")
            
#Suma de pesos y Normalizacion para poder usarlos en el plot
suma_pesos_letra <- aggregate(weight ~ to, data = get.data.frame(gprov_letra), sum)
            
print(suma_pesos_letra)
            
suma_pesos_letra_norm <- (suma_pesos_letra$weight - min(suma_pesos_letra$weight)) / (max(suma_pesos_letra$weight) - min(suma_pesos_letra$weight))
            
#Busco paleta de colores
colores_intensidad_letra <- heat.colors(length(suma_pesos_letra_norm))
            
#Asigno colores
V(proy_activ_prov)$color <- colores_intensidad_letra[match(V(proy_activ_prov)$name, suma_pesos_letra$to)]
            
###Ploteo-----
plot(proy_activ_prov, 
                 vertex.label.cex = 1.25,
                 vertex.label.color = "grey2",
                 vertex.label.font=2,
                 vertex.size=20,
                 edge.color= "grey",
                 edge.width=E(gprov_letra)$weight*2,
                 margin=c(0, 0, 0.1, 0),
                 main="¿Cómo se relacionan las actividades entre si?",  
                 cex.main=1.5)
            
#agrego leyenda con degrade de colores
image.plot(1, 1:length(colores_intensidad_letra), matrix(1:length(colores_intensidad_letra), nrow = 1),
                       col = colores_intensidad_letra, axes = FALSE,
                       legend.only = TRUE, legend.shrink=0.4,
                       lab.breaks=c("Mayor", "", "", "","", "", "","", "", "","", "", "","", "","", "", "",
                                    "Menor"), 
                       horizontal = FALSE, 
                       key.width = 0.3)
            
# Agregar el título a la leyenda 
text(0.55,0.75, "Cantidad de puestos", cex = 1, font = 1, adj=0)







#Red Bipartita con Centralidad Autovector---------------------------------
library(igraph)
library(ggplot2)

# Crear nodos
nodos_region <- unique(puestos22$region)
nodos_letra_desc <- unique(puestos22$letra_desc)

# Crear edges
edges <- puestos22[, c("region", "letra_desc", "puestos")]

# Crear vector de nombres de nodos
nombres_nodos <- c(nodos_region, nodos_letra_desc)

# Crear red bipartita
red_bipartita <- graph_from_data_frame(edges, directed = FALSE, vertices = nombres_nodos)

# Calcular centralidad autovector
centralidad_autovector <- eigen_centrality(red_bipartita)$vector

# Agregar la centralidad autovector como atributo a los nodos
V(red_bipartita)$centralidad_autovector <- centralidad_autovector

# Crear el objeto del gráfico de red
grafo <- red_bipartita

# Establecer los colores de los nodos para regiones y actividades económicas
colores_nodos <- c("region" = "blue", "letra_desc" = "red")

# Calcular la posición de los nodos utilizando el algoritmo Fruchterman-Reingold
posiciones_nodos <- layout_with_fr(grafo)

# Convertir los datos de los enlaces en un data.frame
enlaces <- as.data.frame(get.edgelist(grafo))

# Crear el gráfico de red utilizando ggplot2
grafico_red <- ggplot() +
  theme_void() +
  geom_segment(aes(x = posiciones_nodos[enlaces$x, 1], y = posiciones_nodos[enlaces$x, 2],
                   xend = posiciones_nodos[enlaces$y, 1], yend = posiciones_nodos[enlaces$y, 2]),
               arrow = arrow(length = unit(0.15, "cm")), alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(x = posiciones_nodos[, 1], y = posiciones_nodos[, 2], color = V(grafo)$centralidad_autovector),
             size = 5) +
  scale_color_gradient(low = "blue", high = "red", name = "Centralidad Autovector") +
  labs(title = "Red Bipartita con Centralidad Autovector",
       x = "", y = "") +
  theme(plot.title = element_text(size = 16, face = "bold"))

# Mostrar el gráfico de red
print(grafico_red)


            