#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 1 dic, 2021
# Lista de precios cultivos CNA
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales/CNA"
datos <- "02_Datos"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Unir lista precios ----
#-------------------------------------------------------#

# Lista cultivos CNA
lista_precios <- readRDS(glue("{datos}/CNA/base_cultivos.rds")) %>%
  dplyr::rename(cod_cultivo = p_s6p46) %>%
  dplyr::select(cod_cultivo) %>% distinct()

# Precios por cultivo
# precios <- readxl::read_excel(glue("{datos_ori}/precios_por_cultivo_Hamman_2018.xlsx")) %>%
#   dplyr::rename(cod_cultivo = cod_cul) 

# Precios por cultivos completos
precios <- readxl::read_excel(glue("{datos}/CNA/precios/lista_precios_cultivos_area_cna.xlsx")) %>%
  dplyr::select(cod_cultivo, precio, cultivo, tipo_cul, tipo_fuente, fuente, unidad)

# Unimos precios que ya existen
precios_all <- left_join(lista_precios, precios, by = "cod_cultivo") %>%
  mutate(unidad = ifelse(!is.na(precio), "pesos por kg", NA))

colSums(is.na(precios_all))

# Exportar
# writexl::write_xlsx(precios_all, glue("{datos}/CNA/precios/lista_precios_cultivos_cna.xlsx"))
# saveRDS(precios_all, glue("{datos}/CNA/precios/lista_precios_cultivos_cna.rds"))
haven::write_dta(precios_all, glue("{datos}/CNA/precios/lista_precios_cultivos_cna.dta"))

#-------------------------------------------------------#
# 2. Area sembrada por cultivo ----
#-------------------------------------------------------#

# Abrimos base CNA limpia
cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds")) %>% 
  drop_na(tipo_maq) %>%
  distinct(cod_vereda, encuesta, tipo_cul_lote, area_sembrada, .keep_all = T) 

# Area sembrada por cultivo (participacion acumulativa)
cultivos <- cna %>% 
  drop_na(tipo_cul_lote, area_sembrada) %>%
  group_by(tipo_cul_lote) %>%
  summarise(area_sembrada = sum(area_sembrada, na.rm = T)) %>%
  ungroup() %>%
  mutate(area_total = sum(area_sembrada), part = 100*(area_sembrada/area_total)) %>%
  arrange(-part) %>%
  mutate(part_cum = cumsum(part)) %>%
  dplyr::rename(cod_cultivo = tipo_cul_lote)

# Unimos cultivos con precios y organizamos
cultivos <- cultivos %>% 
  left_join(precios_all, by = "cod_cultivo") %>%
  dplyr::select(cod_cultivo, cultivo, tipo_cul, part, part_cum, precio, tipo_fuente, fuente, unidad)

colSums(is.na(cultivos))

# Exportamos base
writexl::write_xlsx(cultivos, glue("{datos}/CNA/precios/lista_precios_cultivos_area_cna.xlsx"))
saveRDS(cultivos, glue("{datos}/CNA/precios/lista_precios_cultivos_area_cna.rds"))

#-------------------------------------------------------#
# 3. Nombres y codigos cultivos ----
#-------------------------------------------------------#

dic <- readxl::read_excel(glue("{datos_ori}/TEMATICA_DISENO DE REGISTRO CNA2014.xlsx"), 
                  sheet = "TABLAS_REFERENCIA", range = "AM2:AN501") %>%
  janitor::clean_names() %>%
  dplyr::rename(cod_cultivo = p_s6p46_p_s6p64_p_s6p74, nombre_cultivo = cultivo_o_plantacion_forestal) %>%
  distinct(cod_cultivo, .keep_all = T)

haven::write_dta(dic, glue("{datos}/CNA/limpieza_censo/lista_codigos_cultivos_cna.dta"))

# Lista de precios con nombres completos y grupos 
grupos_cul <- readxl::read_excel(glue("{datos_ori}/Crops classification.xlsx")) %>%
  drop_na(`...3`) %>% 
  janitor::row_to_names(row_number = 1) %>% janitor::clean_names() %>%
  rename(cod_cultivo = p_s6p46, grupo_cultivo = clasificacion_utilizada) %>%
  select(cod_cultivo, grupo_cultivo) %>%
  mutate(cod_cultivo = as.numeric(cod_cultivo))

cultivos <- readRDS(glue("{datos}/CNA/precios/lista_precios_cultivos_area_cna.rds")) %>% 
  dplyr::select(cod_cultivo, part, part_cum) %>% mutate(cod_cultivo = as.numeric(cod_cultivo))

dic_cul <- dic %>% 
  mutate(cod_cultivo = as.numeric(cod_cultivo)) %>%
  left_join(grupos_cul, by = "cod_cultivo") %>%
  left_join(cultivos, by = "cod_cultivo") %>%
  dplyr::arrange(part_cum)

# Exportamos base
writexl::write_xlsx(dic_cul, glue("{datos}/CNA/precios/lista_cultivos_area_nombres_cna.xlsx"))

# Con precios
precios_all <- haven::read_dta(glue("{datos}/CNA/precios/lista_precios_cultivos_cna.dta")) %>%
  mutate(cod_cultivo = as.numeric(cod_cultivo)) %>%
  dplyr::select(cod_cultivo, precio)

dic_cul <- dic_cul %>% left_join(precios_all, by = "cod_cultivo") %>%
  dplyr::select(-part_cum) %>% dplyr::filter(is.na(precio)) %>%
  mutate(part_all = sum(part, na.rm = T))

# Exportamos base
writexl::write_xlsx(dic_cul, glue("{datos}/CNA/precios/lista_precios_cultivos_area_precios_cna.xlsx"))
