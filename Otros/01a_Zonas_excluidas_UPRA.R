#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 28 oct, 2021
# Identificar UPAS del CNA que estan en zonas de exclusion segun la UPRA
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

datos_ori <- "01_Datos_originales/"
datos <- "02_Datos"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Organizar datos ----
#-------------------------------------------------------#

# Shp de veredas
veredas <- st_read(glue("{datos_ori}/DANE/mgn/VEREDAS_V27.shp")) %>% 
  janitor::clean_names() %>% dplyr::select(cod_dpto, dptompio, codigo_ver) %>%
  dplyr::rename(cod_mpio = dptompio, cod_vereda = codigo_ver)

# Clasificacion de suelo en zonas por dentro y fuera de la frontera agricola
upra <- st_read(glue("{datos_ori}/UPRA/geo_export_1d9d23ad-baa0-4bdc-b922-d1abac7d08d9.shp")) %>%
  janitor::clean_names() %>% dplyr::rename(cod_dpto = cod_depart) %>%
  dplyr::select(cod_dpto, elemento, area_ha)

table(upra$elemento)

# Zonas fuera de la frontera
exclusion <- upra %>% dplyr::filter(elemento == "Exclusiones legales")

# Identificamos veredas que estan en zonas por fuera de la frontera agricola
veredas <- st_transform(veredas, crs = st_crs(exclusion))
veredas_ex <- st_join(veredas, exclusion)
st_geometry(veredas_ex) <- NULL
veredas_ex <- distinct(veredas_ex)

# Veredas a excluir
veredas_ex <- veredas_ex %>%
  mutate(zona_exclusion = ifelse(!is.na(elemento), 1, 0)) %>%
  distinct(cod_vereda, zona_exclusion, .keep_all = T) %>%
  dplyr::select(-c(cod_dpto.y, area_ha)) %>%
  dplyr::rename(cod_dpto = cod_dpto.x)

table(veredas_ex$zona_exclusion)

# Exportamos veredas a excluir
saveRDS(veredas_ex, glue("{datos}/UPRA/veredas_zona_exclusion_legal.rds"))

