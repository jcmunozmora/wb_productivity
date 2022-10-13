#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 21 nov, 2021
# Identificar veredas del CNA que estan en zonas de exclusion segun la UPRA, resguardos indigenas y consejos comunitarios
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
# 1. Zonas excluidas por UPRA ----
#-------------------------------------------------------#

# Clasificacion de suelo en zonas por dentro y fuera de la frontera agricola
upra <- st_read(glue("{datos_ori}/UPRA/geo_export_1d9d23ad-baa0-4bdc-b922-d1abac7d08d9.shp")) %>%
  janitor::clean_names() %>% dplyr::rename(cod_dpto = cod_depart) %>%
  dplyr::select(cod_dpto, elemento, area_ha)

# table(upra$elemento)

# Zonas fuera de la frontera
# Calculamos areas en m^2
exclusion <- upra %>% dplyr::filter(elemento == "Exclusiones legales")
exclusion <- st_make_valid(exclusion)
exclusion <- exclusion %>% mutate(area_ex = st_area(geometry))
exclusion$area_ex <- units::set_units(exclusion$area_ex, NULL)

# Shp de veredas
veredas <- st_read(glue("{datos_ori}/DANE/mgn/VEREDAS_V27.shp")) %>% 
  janitor::clean_names() %>% dplyr::select(cod_dpto, dptompio, codigo_ver) %>%
  dplyr::rename(cod_mpio = dptompio, cod_vereda = codigo_ver)

# Calculamos areas en m^2
veredas <- st_transform(veredas, crs = st_crs(exclusion))
veredas <- st_make_valid(veredas)
veredas <- veredas %>% mutate(area_ver = st_area(geometry))
veredas$area_ver <- units::set_units(veredas$area_ver, NULL)

# Identificamos veredas que estan en zonas por fuera de la frontera agricola
veredas_ex <- st_intersection(veredas, exclusion)

data_vereda <- veredas_ex %>% mutate(area_ex_v = st_area(geometry))
data_vereda$area_ex_v <- units::set_units(data_vereda$area_ex_v, NULL)
st_geometry(data_vereda) <- NULL

data_vereda <- data_vereda %>%
  group_by(cod_dpto, cod_mpio, cod_vereda, elemento, area_ver) %>%
  summarise(area_ex_v = sum(area_ex_v)) %>%
  ungroup() %>%
  mutate(part_ex = 100*(area_ex_v/area_ver))

# Veredas a excluir
data_vereda <- data_vereda %>%
  mutate(zona_exclusion = ifelse(part_ex >= 10, 1, 0)) %>%
  distinct(cod_vereda, zona_exclusion, .keep_all = T) %>%
  dplyr::select(cod_vereda, part_ex, zona_exclusion)

table(data_vereda$zona_exclusion)

# Exportamos veredas a excluir
saveRDS(data_vereda, glue("{datos}/UPRA/veredas_zona_exclusion_legal.rds"))
rm(data_vereda, exclusion, veredas_ex, upra, veredas)

#-------------------------------------------------------#
# 2. Resguardos indigenas y consejos comunitarios ----
#-------------------------------------------------------#

# UPAs
upas <- st_read(glue("{datos}/CNA/mapas/centroides_upa_cna.shp"))

# Organizar shp veredas; areas en m^2
veredas <- st_read(glue("{datos_ori}/DANE/mgn/VEREDAS_V27.shp")) %>% 
  janitor::clean_names() %>% dplyr::select(cod_dpto, dptompio, codigo_ver) %>%
  dplyr::rename(cod_mpio = dptompio, cod_vereda = codigo_ver)

# Areas en m^2
resguardo <- st_read(glue("{datos_ori}/resguardos_consejos/RESGUARDO_INDIGENA_LEGALIZADO.shp")) %>%
  janitor::clean_names() %>% 
  dplyr::select(id_resguar, nombre_res, area_acto) %>% 
  mutate(area_res = st_area(geometry))

resguardo$area_res <- units::set_units(resguardo$area_res, NULL)
resguardo <- st_make_valid(resguardo)

# Areas en m^2
consejos <- st_read(glue("{datos_ori}/resguardos_consejos/Comunidad_Negra_Titulada.shp")) %>%
  janitor::clean_names() %>% 
  dplyr::select(objectid, nombre_com, area_titul) %>% 
  mutate(area_con = st_area(geometry))

consejos$area_con <- units::set_units(consejos$area_con, NULL)
consejos <- st_make_valid(consejos)

#--------------------------#
# A. Veredas: Resguardos indigenas ----
#--------------------------#

veredas <- st_transform(veredas, crs = st_crs(resguardo))
veredas <- st_make_valid(veredas)
veredas <- veredas %>% mutate(area_ver = st_area(geometry))
veredas$area_ver <- units::set_units(veredas$area_ver, NULL)

# Identificamos veredas que estan en resguardos indigenas
veredas_res <- st_intersection(veredas, resguardo)

data_resguardo <- veredas_res %>% mutate(area_res_v = st_area(geometry))
data_resguardo$area_res_v <- units::set_units(data_resguardo$area_res_v, NULL)
st_geometry(data_resguardo) <- NULL

data_vereda <- data_resguardo %>%
  group_by(cod_dpto, cod_mpio, cod_vereda, area_ver) %>%
  summarise(area_res_v = sum(area_res_v)) %>%
  ungroup() %>%
  mutate(part_res = 100*(area_res_v/area_ver))

# Veredas a excluir
data_vereda <- data_vereda %>%
  mutate(zona_resguardo = ifelse(part_res >= 10, 1, 0)) %>%
  distinct(cod_vereda, zona_resguardo, .keep_all = T) %>%
  dplyr::select(cod_vereda, part_res, zona_resguardo)

table(data_vereda$zona_resguardo)

# Exportamos veredas a excluir
saveRDS(data_vereda, glue("{datos}/UPRA/veredas_zona_resguardo_indigena.rds"))
rm(data_vereda, data_resguardo, veredas, veredas_res)

#--------------------------#
# B. Veredas: Consejos comunitarios ----
#--------------------------#

veredas <- st_transform(veredas, crs = st_crs(consejos))
veredas <- st_make_valid(veredas)
veredas <- veredas %>% mutate(area_ver = st_area(geometry))
veredas$area_ver <- units::set_units(veredas$area_ver, NULL)

# Identificamos veredas que estan en consejos comunitarios
veredas_con <- st_intersection(veredas, consejos)

data_consejo <- veredas_con %>% mutate(area_con_v = st_area(geometry))
data_consejo$area_con_v <- units::set_units(data_consejo$area_con_v, NULL)
st_geometry(data_consejo) <- NULL

data_vereda <- data_consejo %>%
  group_by(cod_dpto, cod_mpio, cod_vereda, area_ver) %>%
  summarise(area_con_v = sum(area_con_v)) %>%
  ungroup() %>%
  mutate(part_con = 100*(area_con_v/area_ver))

# Veredas a excluir
data_vereda <- data_vereda %>%
  mutate(zona_consejo = ifelse(part_con >= 10, 1, 0)) %>%
  distinct(cod_vereda, zona_consejo, .keep_all = T) %>%
  dplyr::select(cod_vereda, part_con, zona_consejo)

table(data_vereda$zona_consejo)

# Exportamos veredas a excluir
saveRDS(data_vereda, glue("{datos}/UPRA/veredas_zona_consejo_comunitario.rds"))
rm(data_vereda, data_consejo, veredas, veredas_con)



