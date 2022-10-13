#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 16 marzo, 2022
# Agrupar cultivos Censo Nacional Agropecuario
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readr, readxl)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales/CNA"
datos <- "02_Datos"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Agrupar cultivos ----
#-------------------------------------------------------#

# Base de grupos de cultivos
grupos_cul <- read_excel(glue("{datos_ori}/Crops classification.xlsx")) %>%
  drop_na(`...3`) %>% 
  janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  rename(tipo_cul_lote = p_s6p46, uso_suelo = nom_tipo) %>%
  mutate(tipo_cul_lote = as.numeric(tipo_cul_lote)) %>%
  select(-na)

# Abrir base CNA
cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds")) %>%
  mutate(tipo_cul_lote = as.numeric(tipo_cul_lote)) %>% 
  dplyr::select(-c(starts_with("pesca"), starts_with("acui"), starts_with("captura"))) 

# Unir CNA con grupos cultivos
cna <- cna %>% left_join(grupos_cul, by = "tipo_cul_lote") 

# Organizar clasificaciones cultivos
cna$clasificacion_utilizada[str_detect(cna$nombre_cultivo, "Café")] <- "Café"
cna$clasificacion_utilizada[str_detect(cna$nombre_cultivo, "Palma africana")] <- "Palma africana"
cna$clasificacion_utilizada[str_detect(cna$nombre_cultivo, "Caña panelera")] <- "Caña panelera"
cna$clasificacion_utilizada[str_detect(cna$nombre_cultivo, "Caña de azúcar")] <- "Caña de azúcar"
cna$clasificacion_utilizada[str_detect(cna$clasificacion_utilizada, "legumbres nueces y semillas oleaginosas")] <- "Legumbres, nueces y semillas oleaginosas"
cna$clasificacion_utilizada[str_detect(cna$clasificacion_utilizada, "Arboles maderables")] <- "Árboles maderables"
cna$clasificacion_utilizada[str_detect(cna$clasificacion_utilizada, "Arboles no maderables")] <- "Árboles no maderables"

table(cna$clasificacion_utilizada)

# Guardamos base desagregada
saveRDS(cna, glue("{datos}/CNA/base_cna_all_maq_cultivos.rds"))

# Hay 26/496 cultivos que no pegan en sample
colSums(is.na(cna))
# test <- grupos_cul %>% anti_join(cna, by = "tipo_cul_lote")

#-------------------------------------------------------#
# 2. Base agregada por cultivo ----
#-------------------------------------------------------#

cna_agro <- cna %>% drop_na(tipo_cul_lote)

# Agregamos datos a nivel de cultivo: solo lotes con cultivos (excluidos pecuarios)
cna_agregado <- cna_agro %>%
  mutate(lote = 1) %>%
  group_by(clasificacion_utilizada) %>%
  summarise(num_lote = sum(lote, na.rm = T), area_upa = sum(area_upa, na.rm = T), 
            area_agropecuario = sum(area_agropecuario, na.rm = T), area_sembrada = sum(area_sembrada, na.rm = T), 
            area_cosechada = sum(area_cosechada, na.rm = T), cant_cosecha = sum(cant_cosecha, na.rm = T),
            total_jor = sum(jornales, na.rm = T), ing_cosecha = sum(ing_cosecha, na.rm = T)) %>%
  ungroup() %>%
  rename(clas_cultivo = clasificacion_utilizada)

# No tenemos precios para categorias de arboles maderables y forrajes
cna_agregado$ing_cosecha[cna_agregado$ing_cosecha == 0] <- NA

# Guardar base
saveRDS(cna_agregado, glue("{datos}/CNA/base_cna_grupos_cultivos.rds"))
