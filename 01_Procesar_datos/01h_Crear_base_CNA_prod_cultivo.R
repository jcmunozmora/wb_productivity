#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 1 dic, 2021
# Crear bases de datos para visualizacion 
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue)
path <- getwd()
path <- gsub("BM_productividad.*", "", path)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
graficas <- "03_Graficas"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Organizar datos ----
#-------------------------------------------------------#

# Base CNA con clasificacion de cultivos
base_cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq_cultivos.rds"))

base_cna <- base_cna %>%
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, tipo_cul_lote, nombre_cultivo, 
                clasificacion_utilizada, area_upa, cant_cosecha_kg, jornales) %>% 
  drop_na(tipo_cul_lote, area_upa) %>%
  distinct() %>%
  mutate(area_upa = area_upa/10000)

# Identificamos maquinaria por finca
cna_maq <- readRDS(glue("{datos}/CNA/base_maquinaria_agro.rds")) %>%
  dplyr::rename(cod_dpto = p_depto, cod_mpio = p_munic, 
                tipo_maq = p_s9p118, maq_menor5 = p_s9p119, maq_mayor5 = p_s9p120) %>%
  mutate(cod_dpto = as.numeric(cod_dpto), cod_mpio = as.numeric(cod_mpio), cod_vereda = as.numeric(cod_vereda),
         maq_mayor5 = ifelse(maq_mayor5 == 99999, NA, maq_mayor5),
         maq_menor5 = ifelse(maq_menor5 == 99999, NA, maq_menor5),
         num_maq = maq_menor5 + maq_mayor5, 
         num_maq = ifelse(is.na(maq_menor5), maq_mayor5, num_maq),
         num_maq = ifelse(is.na(maq_mayor5), maq_menor5, num_maq),
         num_maq = ifelse(is.na(maq_mayor5) & is.na(maq_menor5), NA, num_maq)) %>%
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, num_maq) %>%
  group_by(cod_dpto, cod_mpio, cod_vereda, encuesta) %>%
  summarise(num_maq = sum(num_maq, na.rm = T)) %>%
  ungroup()

# Unimos datos de CNA con maquinaria por finca
base_maq <- base_cna %>%
  left_join(cna_maq, by = c("cod_dpto", "cod_mpio", "cod_vereda", "encuesta"))

# Identificamos maiz y arroz por separado
base_maq$clasificacion_utilizada[base_maq$nombre_cultivo == "Maíz Blanco" | base_maq$nombre_cultivo == "Maíz Amarillo"] <- "Maíz (blanco o amarillo)"
base_maq$clasificacion_utilizada[base_maq$nombre_cultivo == "Arroz verde"] <- "Arroz"

#-------------------------------------------------------#
# 2. Bases por cultivos ----
#-------------------------------------------------------#

# Agrupamos base de datos a nivel de grupo de cultivo
base_cul <- base_maq %>%
  dplyr::rename(grupo_cultivo = clasificacion_utilizada) %>%
  distinct(cod_dpto, cod_mpio, cod_vereda, encuesta, grupo_cultivo,
           area_upa, jornales, num_maq, tipo_cul_lote, cant_cosecha_kg, .keep_all = T) %>%
  group_by(cod_dpto, cod_mpio, cod_vereda, encuesta, grupo_cultivo,
           area_upa, jornales, num_maq) %>%
  summarise(cant_cosecha_kg = sum(cant_cosecha_kg, na.rm = T)) %>%
  ungroup() %>%
  # Creamos ID de cultivo
  group_by(grupo_cultivo) %>%
  mutate(id_cul = cur_group_id()) %>%
  ungroup() %>%
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, id_cul, grupo_cultivo,
                area_upa, jornales, num_maq, cant_cosecha_kg)

# Organizamos labels
base_cul$grupo_cultivo <- ordered(base_cul$grupo_cultivo,
                                levels = c("Árboles maderables", "Árboles no maderables", "Arroz",
                                           "Café", "Caña panelera", "Cereales", "Forrajes", "Frutas y vegetales",
                                           "Legumbres, nueces y semillas oleaginosas", "Maíz (blanco o amarillo)", 
                                           "Palma africana", "Plantas"),
                                labels = c("Timber trees", "Non-timber trees", "Rice", "Coffee", "Sugar cane",
                                           "Cereals", "Forages", "Fruits and vegetables", "Legumes, nuts and oilseeds",
                                           "Corn (yellow or white)", "African palm", "Plants"))

cultivos <- base_cul %>% distinct(id_cul) %>% unique() %>% unlist() %>% as.numeric()
cultivos_id <- base_cul %>% distinct(id_cul, grupo_cultivo)

# Guardamos una base de datos por cultivo
lapply(1:12, function(x){
  
  # Filtramos base para tener cultivo de interes
  df <- base_cul %>% dplyr::filter(id_cul == x)
  cul <- df %>% distinct(grupo_cultivo) %>% unique() %>% unlist()
  print(glue("Guardando cultivo: {cul}"))
  
  # Guardamos base de datos
  saveRDS(df, glue("{path}BM_Efficiency/00_Rawdata/base_cna_cultivo_{x}.rds"))
})
