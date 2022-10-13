#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 23 noviembre, 2021
# Indice de diversidad de cultivos por UPA
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
# 0. Abrir datos ----
#-------------------------------------------------------#

# Abrimos base CNA e identificamos UPA con cultivos
cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq_cultivos.rds")) %>%
  dplyr::select(-c(ends_with(c("maq", "pesca", "acui", "captura", "zona", "especie")))) %>%
  drop_na(area_cosechada, ing_cosecha) %>%
  distinct()

#-------------------------------------------------------#
# 1. Diversidad UPA ----
#-------------------------------------------------------#

# Calculamos el % del area cosechada y de la produccion (en $) por tipo de cultivo
# Usamos area cosechada y no sembrada porque area_sembrada NO varia por cultivo
data_upa <- cna %>%
  group_by(cod_vereda, encuesta, tipo_cul_lote, nombre_cultivo, clasificacion_utilizada) %>%
  summarise(area_cosechada = sum(area_cosechada), ing_cosecha = sum(ing_cosecha, na.rm = T)) %>%
  ungroup() %>%
  group_by(cod_vereda, encuesta) %>%
  mutate(total_area = sum(area_cosechada), part_cul = (area_cosechada/total_area),
         total_ing = sum(ing_cosecha, na.rm = T), part_prod = (ing_cosecha/total_ing)) %>%
  ungroup()

# Indice de Herfindahl-Hirschman: sum(participacion^2)
div_upa <- data_upa %>%
  mutate(part_cul = ifelse(total_area + area_cosechada == 0, NA, part_cul),
         part_prod = ifelse(ing_cosecha + total_ing == 0, NA, part_prod),
    part_cul_sq = part_cul*part_cul, part_prod_sq = part_prod*part_prod) %>%
  group_by(encuesta) %>%
  summarise(ind_div_cul = sum(part_cul_sq),
            ind_div_prod = sum(part_prod_sq)) %>%
  ungroup() %>%
  mutate(ind_div_cul = 100*ind_div_cul, ind_div_prod = 100*ind_div_prod)

# Guardamos indice de diversidad
saveRDS(div_upa, glue("{datos}/CNA/base_ind_diversidad_upa.rds"))

#-------------------------------------------------------#
# 2. Diversidad municipio ----
#-------------------------------------------------------#

# Agregamos datos de UPA a nivel municipal
# Calculamos el % del area cosechada y de la produccion (en $) por tipo de cultivo
data_mpio <- cna %>%
  group_by(cod_mpio, tipo_cul_lote, nombre_cultivo, clasificacion_utilizada) %>%
  summarise(area_cosechada = sum(area_cosechada), ing_cosecha = sum(ing_cosecha, na.rm = T)) %>%
  ungroup() %>%
  group_by(cod_mpio) %>%
  mutate(total_area = sum(area_cosechada), part_cul = (area_cosechada/total_area),
         total_ing = sum(ing_cosecha, na.rm = T), part_prod = (ing_cosecha/total_ing)) %>%
  ungroup()

# Indice de Herfindahl-Hirschman: sum(participacion^2)
div_mpio <- data_mpio %>%
  mutate(part_cul = ifelse(total_area + area_cosechada == 0, NA, part_cul),
         part_prod = ifelse(ing_cosecha + total_ing == 0, NA, part_prod),
         part_cul_sq = part_cul*part_cul, part_prod_sq = part_prod*part_prod) %>%
  group_by(cod_mpio) %>%
  summarise(ind_div_cul = sum(part_cul_sq),
            ind_div_prod = sum(part_prod_sq)) %>%
  ungroup() %>%
  mutate(ind_div_cul = 100*ind_div_cul, ind_div_prod = 100*ind_div_prod)

# Guardamos indice de diversidad
saveRDS(div_mpio, glue("{datos}/CNA/base_ind_diversidad_mpio.rds"))

