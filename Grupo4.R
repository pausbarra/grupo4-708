library("tidyverse")

excel <- read.csv("puestos_depto_priv_por_clae2 (1).csv", sep = ",")
colnames(excel)

puestos22 <- excel %>% 
  mutate(año = year(fecha), .after = fecha) %>% 
  filter(año=="2022")

colnames(puestos22)[3] = "departamento"
colnames(puestos22)[4] = "provincia"

summary(puestos22)

puestos22 %>% 
  count(provincia)

which(puestos22$puestos == "-99")
puestos22$puestos[which(puestos22$puestos == "-99")] <- "0"

