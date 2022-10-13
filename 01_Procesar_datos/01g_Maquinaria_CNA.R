#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 1 dic, 2021
# Precios de maquinaria CNA
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
# 1. Abrir datos ----
#-------------------------------------------------------#

# Abrimos base de CNA en modulo maquinaria y exportamos lista de maquinas
cna <- readRDS(glue("{datos}/CNA/base_cna_modulos.rds")) %>%
  dplyr::select(encuesta, cod_dpto, cod_mpio, existe_maq, tipo_maq, maq_menor5, maq_mayor5)

# Identificamos las UPA que tienen maquinaria
cna$existe_maq[is.na(cna$existe_maq)] <- 0
cna$existe_maq[cna$existe_maq == 2] <- 0
cna$existe_maq[cna$existe_maq == 9] <- 0
cna$tipo_maq[is.na(cna$tipo_maq)] <- 0
cna$maq_menor5[is.na(cna$maq_menor5)] <- 0
cna$maq_mayor5[is.na(cna$maq_mayor5)] <- 0

# Contamos la maquinaria que existe en todas las UPA
num_maq <- cna %>%
  mutate(tiene_maq = ifelse(existe_maq + tipo_maq + maq_menor5 + maq_mayor5 > 0, 1, 0),
         total_maq = maq_menor5 + maq_mayor5) %>%
  distinct(encuesta, total_maq, .keep_all = T) %>%
  dplyr::filter(tiene_maq == 1) %>%
  distinct(encuesta, total_maq) %>%
  mutate(upas = 1) %>%
  dplyr::filter(total_maq > 0) %>%
  summarise(total_maq = sum(total_maq, na.rm = T), upas = sum(upas)) 

# Solo nos quedamos con la lista de maquinas en las UPA
cna <- cna %>%
  mutate(tiene_maq = ifelse(existe_maq + tipo_maq + maq_menor5 + maq_mayor5 > 0, 1, 0)) %>%
  dplyr::filter(tiene_maq == 1) %>%
  distinct(tipo_maq)

# Abrimos etiquetas de maquinaria del Censo
label_maq <- readxl::read_excel(glue("{datos_ori}/TEMATICA_DISENO DE REGISTRO CNA2014.xlsx"), sheet = "Maquinaria") %>%
  janitor::clean_names() %>%
  dplyr::select(-x3) %>% dplyr::rename(tipo_maq = p_s9p118)

# Unimos maquinaria en CNA con sus nombres
cna_maq <- cna %>% left_join(label_maq, by = "tipo_maq")
colSums(is.na(cna_maq))

# Guardar muestra
saveRDS(cna_maq, glue("{datos}/CNA/maquinaria/lista_maquinaria_cna_completo.rds"))
writexl::write_xlsx(cna_maq, glue("{datos}/CNA/maquinaria/lista_maquinaria_cna_completo.xlsx"))

#-------------------------------------------------------#
# 2. Area sembrada por maquinaria ----
#-------------------------------------------------------#

# Abrimos base de maquinaria
maq <- readRDS(glue("{datos}/CNA/maquinaria/lista_maquinaria_cna_completo.rds"))

# Abrimos base CNA limpia
cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds")) %>% 
  drop_na(tipo_maq, tiene_maq, area_sembrada) %>%
  distinct(cod_vereda, encuesta, tipo_maq, tiene_maq, area_sembrada, .keep_all = T) 

# Area sembrada por maquinaria (participacion acumulativa)
maquinas <- cna %>% 
  drop_na(tipo_maq, area_sembrada) %>%
  dplyr::filter(tipo_maq != 0) %>%
  group_by(tipo_maq) %>%
  summarise(area_sembrada = sum(area_sembrada, na.rm = T)) %>%
  ungroup() %>%
  mutate(area_total = sum(area_sembrada), part = 100*(area_sembrada/area_total)) %>%
  arrange(-part) %>%
  mutate(part_cum = cumsum(part)) 

# Unimos maquinaria con precios y organizamos
maquinas <- maquinas %>% left_join(maq, by = "tipo_maq") %>%
  mutate(precio = 0, fuente = NA, unidad = NA) %>%
  dplyr::select(tipo_maq, maquinaria, part, part_cum, precio, fuente, unidad)

colSums(is.na(cultivos))

# Exportamos base
saveRDS(maquinas, glue("{datos}/CNA/maquinaria/lista_maquinaria_cna_area_completo.rds"))
writexl::write_xlsx(maquinas, glue("{datos}/CNA/maquinaria/lista_maquinaria_cna_area_completo.xlsx"))

# Area con informacion disponible:
maquinas <- readRDS(glue("{datos}/CNA/maquinaria/lista_maquinaria_cna_area_completo.rds"))

hamman <- readxl::read_excel("01_Datos_originales/BR/datos/input/precios_maquinaria.xlsx") %>%
  dplyr::rename(tipo_maq = p_s9p118) %>% dplyr::select(tipo_maq, pmin_agro)

maq_data <- maquinas %>% 
  left_join(hamman, by = "tipo_maq") %>% 
  drop_na(pmin_agro) %>%
  mutate(part_all = sum(part))

nomaq_data <- maquinas %>% 
  left_join(hamman, by = "tipo_maq") %>% 
  dplyr::filter(is.na(pmin_agro)) %>%
  mutate(part_all = sum(part))

writexl::write_xlsx(nomaq_data, glue("{datos}/CNA/maquinaria/lista_maquinaria_cna_sin_precios.xlsx"))

# Capital imputado
prod <- haven::read_dta(glue("{datos}/Productividad/base_productividad_hamman.dta")) %>%
  drop_na(kminbar, ls2) %>% dplyr::select(cod_vereda, encuesta, kminbar, kmin_agro)

# Numero de UPAs que reportan # de maquinaria
num_cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds"))

maq_cna <- num_cna %>% 
  mutate(num_maq = maq_menor5 + maq_mayor5, 
         encuesta = as.numeric(encuesta), cod_vereda = as.numeric(cod_vereda)) %>%
  distinct(cod_vereda, encuesta, .keep_all = T) %>%
  dplyr::select(cod_vereda, encuesta, tiene_maq, tipo_maq, num_maq, ing_cosecha) %>%
  left_join(prod, by = c("cod_vereda", "encuesta")) %>%
  mutate(price = ifelse(!is.na(kminbar) | !is.na(kmin_agro), 1, 0))

# %>%
#   dplyr::filter(num_maq > 0 | !is.na(tipo_maq))

