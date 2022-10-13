#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 18 noviembre, 2021
# Calculo de precipitaciones y temperatura a nivel de vereda 
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, sf, raster)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
graficas <- "03_Graficas"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Rain and temperature zonal statistics ----
#-------------------------------------------------------#

# Abrir shape de veredas de Colombia
poly_veredas <- st_read(glue("{datos_ori}/DANE/mgn/VEREDAS_V27.shp")) %>% 
  janitor::clean_names() %>% dplyr::select(cod_dpto, dptompio, codigo_ver) %>%
  dplyr::rename(cod_mpio = dptompio, cod_vereda_dane = codigo_ver)

poly_veredas <- st_make_valid(poly_veredas)

# Hay discrepancias entre codigos de veredas, identificamos los codigos CNA con un st_join
# Entre centroides CNA y shp de veredas

# Organizamos shp de centroides UPA
upas <- st_read(glue("{datos}/CNA/mapas/centroides_upa_cna.shp")) 
veredas <- upas
st_geometry(veredas) <- NULL
veredas <- veredas %>% distinct(cod_vereda, .keep_all = T)
upas <- upas %>% dplyr::filter(encuesta %in% veredas$encuesta)
upas <- st_transform(upas, crs = st_crs(poly_veredas))

# Encontrar codigos de UPA en mgn DANE
codigos <- st_intersection(upas, poly_veredas)
st_geometry(codigos) <- NULL
codigos <- codigos %>% distinct(cod_vereda, .keep_all = T)

# Pegamos codigos de vereda del CNA a shp del DANE
poly_veredas <- poly_veredas %>% left_join(codigos, by = "cod_vereda_dane")
colSums(is.na(poly_veredas))

# Extraer ID de las veredas
df <- data.frame(cod_vereda = as.numeric(poly_veredas$cod_vereda),
                 cod_vereda_dane = as.numeric(poly_veredas$cod_vereda_dane))

# Abrir rasters de precipitaciones y temperatura, y extraer promedio municipal de 2013
# Datos de clima provienen de ERA5, los rasters originales se procesaron en el paper de Cafe y Cambio climatico
raster_preci <- raster(glue("{datos_ori}/ERA5/precip_col_2013.tif"))
raster_temp <- raster(glue("{datos_ori}/ERA5/temperature_col_2013.tif"))
df$rain <- exactextractr::exact_extract(raster_preci, poly_veredas, "mean")
df$temp <- exactextractr::exact_extract(raster_temp, poly_veredas, "mean")
df$year <- 2013
df <- df %>% dplyr::select(starts_with("cod_vereda"), year, temp, rain)

# Revisar balance del panel
df$year %>% table()

# Transformar temperatura de Kelvin a Celsius
data_clima <- df %>% mutate(temp = temp - 273.15) %>% distinct(cod_vereda, .keep_all = T)
hist(data_clima$temp)

# Guardar datos de clima
saveRDS(data_clima, glue("{datos}/base_clima_veredas_2013.rds"))
