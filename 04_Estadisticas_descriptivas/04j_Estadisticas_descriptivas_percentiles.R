#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 13 feb, 2022
# Estadisticas descriptivas por region, tamano de la tierra y cadena productiva
# UPA: Caracteristicas productor, agroecologicas; Municipal: entorno, institucional, estructura economica
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readr, readxl, janitor)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
graficas <- "03_Graficas"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Percentiles ----
#-------------------------------------------------------#

### Para estadisticas 
# Se separa por region y tamano de la tierra (tipo_upa)
# Variables: ls2 (productividad Cobb-Douglas), rend_cos (Rendimiento por hectarea sembrada),
# edad, anios educacion (por calcular), temperatura (centigrados), lluvias (mm), distancia mercado mayorista (dismdo),
# distancia mercado alimentos (distancia_mercado), transferencias sistema general de participaciones per capita (sgp_pc),
# indice desempeno institucional (ind_desempeno_int), valor_agregado, indice diversidad economica (ind_div_econ)

# Analizar jefaturas compartidas por aparte (tablas aparte)
# Stand-by: sexo productor (% mujeres)

test <- cna_prod %>% dplyr::filter(clasificacion_utilizada == "Arroz") %>% distinct(ls2)
test <- cna_prod %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)") %>% distinct(ls2)
p10 <- quantile(test$ls2, 0.10, na.rm = T) 
p90 <- quantile(test$ls2, 0.90, na.rm = T) 
100*(p90/p10)

p10 <- quantile(prod$ls2, 0.10, na.rm = T) 
p90 <- quantile(prod$ls2, 0.90, na.rm = T) 
100*(p90/p10)