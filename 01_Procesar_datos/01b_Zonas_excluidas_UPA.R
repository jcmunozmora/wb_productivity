#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 22 nov, 2021
# Identificar UPAS del CNA que estan en zonas de exclusion segun la UPRA, resguardos indigenas y consejos comunitarios
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readr, sf)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Centroides UPAs ----
#-------------------------------------------------------#

# Abrimos base georreferenciada del CNA y escogemos UPAs (no UPNAs)
upas <- read_csv(glue("{datos_ori}/CNA/georreferenciados/BASE_CNA_UPAS_PNA_COLOMBIA.csv")) %>%
  janitor::clean_names() %>%
  dplyr::filter(tipo_uc == 1) %>%
  dplyr::rename(longitud = x_geo, latitud = y_geo, encuesta = encuesta_i) %>%
  dplyr::select(cod_vereda, encuesta, longitud, latitud)

# Convertimos upas a objeto espacial
upas <- sf::st_as_sf(upas, coords = c("longitud", "latitud"), crs = 4326)

# Exportar shp de UPAs
sf::write_sf(upas, glue("{datos}/CNA/mapas/centroides_upa_cna.shp"))

# Guardamos codigos de encuesta en string (base georreferenciada viene numerica)
cna_gen <- readRDS(glue("{datos}/CNA/base_upas.rds")) %>% 
  dplyr::select(encuesta) %>% 
  distinct() %>% 
  mutate(encuesta_s = encuesta, encuesta = as.numeric(encuesta_s))

saveRDS(cna_gen, glue("{datos}/CNA/limpieza_censo/lista_codigos_upa.rds"))

#-------------------------------------------------------#
# 2. Zonas excluidas por UPRA ----
#-------------------------------------------------------#

# Clasificacion de suelo en zonas por dentro y fuera de la frontera agricola
upra <- st_read(glue("{datos_ori}/UPRA/geo_export_1d9d23ad-baa0-4bdc-b922-d1abac7d08d9.shp")) %>%
  janitor::clean_names() %>% dplyr::rename(cod_dpto = cod_depart) %>%
  dplyr::select(cod_dpto, elemento, area_ha)

exclusion <- upra %>% dplyr::filter(elemento == "Exclusiones legales")
exclusion <- st_make_valid(exclusion)

# Identificamos las UPA que estan en veredas excluidas
data_vereda <- readRDS(glue("{datos}/UPRA/veredas_zona_exclusion_legal.rds"))
upas <- st_read(glue("{datos}/CNA/mapas/centroides_upa_cna.shp"))
upas_ex <- upas %>% dplyr::filter(cod_vereda %in% data_vereda$cod_vereda)
upas_ex <- st_transform(upas_ex, crs = st_crs(exclusion))

# Separamos UPAs en 5 subgrupos para agilizar el proceso
num_groups <- 5
lista_ex <- upas_ex %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)

# Identificamos UPAs cuyo centroide se intersecta con zonas de exclusion legal
data_upas <- lapply(1:num_groups, function(x){
  
  print(glue("Grupo {x}"))
  
  # Identificamos upas en zona exclusion
  data <- lista_ex[[x]]
  data <- st_intersection(data, exclusion)
  
  # Organizamos base
  data <- data %>% 
    mutate(zona_exclusion = 1) %>% 
    dplyr::select(cod_vereda, encuesta, zona_exclusion)
  
  st_geometry(data) <- NULL
  saveRDS(data, glue("{datos}/UPRA/upas_zona_exclusion_legal_p{x}.rds"))
}) 

# Abrimos y unimos las bases de UPAs en zonas de exclusion
data_upas <- lapply(1:num_groups, function(x){
  df <- readRDS(glue("{datos}/UPRA/upas_zona_exclusion_legal_p{x}.rds"))
  return(df)
}) %>% bind_rows()

# Pegamos codigos en string
data_upas <- data_upas %>% left_join(cna_gen, by = "encuesta") %>%
  dplyr::rename(encuesta_n = encuesta, encuesta = encuesta_s) %>%
  dplyr::select(-encuesta_n)

# Exportamos UPAs a excluir
saveRDS(data_upas, glue("{datos}/UPRA/upas_zona_exclusion_legal.rds"))
rm(data_upas, upas_ex, lista_ex, upra, exclusion)

#-------------------------------------------------------#
# 3. Resguardos indigenas y consejos comunitarios ----
#-------------------------------------------------------#

# UPAs
upas <- st_read(glue("{datos}/CNA/mapas/centroides_upa_cna.shp"))

# Veredas con resguardos indigenas/consejos comunitarios
data_res <- readRDS(glue("{datos}/UPRA/veredas_zona_resguardo_indigena.rds"))
data_con <- readRDS(glue("{datos}/UPRA/veredas_zona_consejo_comunitario.rds"))

#--------------------------#
# A. Resguardos ----
#--------------------------#

# Shp resguardos indigenas
resguardo <- st_read(glue("{datos_ori}/resguardos_consejos/RESGUARDO_INDIGENA_LEGALIZADO.shp")) %>%
  janitor::clean_names() %>% 
  dplyr::select(id_resguar, nombre_res)

resguardo <- st_make_valid(resguardo)

upas_res <- upas %>% dplyr::filter(cod_vereda %in% data_res$cod_vereda)
upas_res <- st_transform(upas_res, crs = st_crs(resguardo))

# Separamos UPAs en 5 subgrupos para agilizar el proceso
num_groups <- 5
lista_res <- upas_res %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)

# Identificamos UPAs cuyo centroide se intersecta con resguardos
lapply(1:num_groups, function(x){
  
  print(glue("Grupo {x}"))
  
  # Identificamos upas en zona exclusion
  data <- lista_res[[x]]
  data <- st_intersection(data, resguardo)
  
  # Organizamos base
  data <- data %>% 
    mutate(zona_resguardo = 1) %>% 
    dplyr::select(cod_vereda, encuesta, zona_resguardo)
  
  st_geometry(data) <- NULL
  saveRDS(data, glue("{datos}/UPRA/upas_zona_resguardo_indigena_p{x}.rds"))
}) 

# Abrimos y unimos las bases de UPAs en zonas de resguardos indigenas
data_res_upa <- lapply(1:num_groups, function(x){
  df <- readRDS(glue("{datos}/UPRA/upas_zona_resguardo_indigena_p{x}.rds"))
  return(df)
}) %>% bind_rows()

# Pegamos codigos en string
data_res_upa <- data_res_upa %>% left_join(cna_gen, by = "encuesta") %>%
  dplyr::rename(encuesta_n = encuesta, encuesta = encuesta_s) %>%
  dplyr::select(-encuesta_n)

# Exportamos UPAs a excluir
saveRDS(data_res_upa, glue("{datos}/UPRA/upas_zona_resguardo_indigena.rds"))
rm(data_res_upa, lista_res, upas_res, resguardo, data_res)

#--------------------------#
# B. Consejos ----
#--------------------------#

# Shp consejos comunitarios
consejos <- st_read(glue("{datos_ori}/resguardos_consejos/Comunidad_Negra_Titulada.shp")) %>%
  janitor::clean_names() %>% 
  dplyr::select(objectid, nombre_com)

consejos <- st_make_valid(consejos)

upas_con <- upas %>% dplyr::filter(cod_vereda %in% data_con$cod_vereda)
upas_con <- st_transform(upas_con, crs = st_crs(consejos))

# Separamos UPAs en 5 subgrupos para agilizar el proceso
num_groups <- 5
lista_con <- upas_con %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)

# Identificamos UPAs cuyo centroide se intersecta con consejos
lapply(1:num_groups, function(x){
  
  print(glue("Grupo {x}"))
  
  # Identificamos upas en consejos comunitarios
  data <- lista_con[[x]]
  data <- st_intersection(data, consejos)
  
  # Organizamos base
  data <- data %>% 
    mutate(zona_consejo = 1) %>% 
    dplyr::select(cod_vereda, encuesta, zona_consejo)
  
  st_geometry(data) <- NULL
  saveRDS(data, glue("{datos}/UPRA/upas_zona_consejo_comunitario_p{x}.rds"))
}) 

# Abrimos y unimos las bases de UPAs en zonas de consejo comunitario
data_con_upa <- lapply(1:num_groups, function(x){
  df <- readRDS(glue("{datos}/UPRA/upas_zona_consejo_comunitario_p{x}.rds"))
  return(df)
}) %>% bind_rows()

# Pegamos codigos en string
data_con_upa <- data_con_upa %>% left_join(cna_gen, by = "encuesta") %>%
  dplyr::rename(encuesta_n = encuesta, encuesta = encuesta_s) %>%
  dplyr::select(-encuesta_n)

# Exportamos UPAs a excluir
saveRDS(data_con_upa, glue("{datos}/UPRA/upas_zona_consejo_comunitario.rds"))

# Revisar match de las UPA (Buen match en ambas)
# cna_noagro <- readRDS(glue("{datos}/CNA/base_no_agropecuarias.rds")) %>%
#   distinct(ENCUESTA, COD_VEREDA) %>%
#   mutate(encuesta = as.numeric(ENCUESTA), cod_vereda = as.numeric(COD_VEREDA))
# 
# test <- anti_join(upas_ex, cna_noagro, by = c("encuesta"))
