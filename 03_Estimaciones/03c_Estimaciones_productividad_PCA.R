#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 12 dic, 2021
# Regresiones de la productividad a nivel de UPA  
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
graficas <- "03_Graficas"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Organizar datos ----
#-------------------------------------------------------#

#--------------------------#
# A. Productividad por vereda ----
#--------------------------#

# Abrimos datos productividad agricola
prod <- haven::read_dta(glue("{datos}/Productividad/base_productividad_hamman.dta")) %>%
  mutate(log_area_sembrada = log(area_sembrada), log_jornales = log(jornales),
         ing_hectarea = revenue/area_sembrada, log_ingreso = log(ing_hectarea),
         log_ing_jornal = log(revenue_j)) %>%
  drop_na(ls2)

# UPAs con capital con y sin imputar
k <- prod %>% drop_na(kmin_agro) %>% distinct(cod_vereda, encuesta)
k_im <- prod %>% drop_na(kminbar) %>% distinct(cod_vereda, encuesta)

# Agregamos productividad a nivel de vereda
prod_vereda <- prod %>% 
  group_by(cod_vereda) %>%
  summarise(ls2 = mean(ls2)) %>%
  ungroup()

# Abrimos datos PCA
pca <- readRDS(glue("02_Datos/Productividad/base_PCA_censo.rds")) %>%
  janitor::clean_names() %>% mutate(cod_vereda = as.numeric(cod_vereda)) %>%
  distinct(cod_vereda, .keep_all = T)

# Unimos productividad y pca
prod_pca <- prod_vereda %>% left_join(pca, by = "cod_vereda")

#--------------------------#
# B. Region y clima ----
#--------------------------#

# Abrimos datos de regiones
base_ver <- readRDS(glue("{datos}/Productividad/base_productividad_vereda.rds")) %>%
  dplyr::select(region, provincia, cod_provincia, cod_dpto, cod_mpio, cod_vereda) %>%
  distinct(cod_vereda, .keep_all = T)

# Abrimos datos de clima
base_clima <- readRDS(glue("{datos}/base_clima_veredas_2013.rds")) %>%
  distinct(cod_vereda, .keep_all = T)

#--------------------------#
# C. Cultivo principal ----
#--------------------------#

# Calculamos principal cultivo por vereda segun area sembrada
cultivo <- readRDS(glue("{datos}/CNA/base_cna_all_maq_cultivos.rds")) %>%
  dplyr::select(cod_vereda, encuesta, tipo_cul_lote, nombre_cultivo,
                uso_suelo, clasificacion_utilizada, area_cosechada, area_sembrada) %>%
  drop_na(tipo_cul_lote, area_sembrada) %>% 
  distinct() %>%
  mutate(encuesta = as.numeric(encuesta))

# Identificamos maiz y arroz por separado
cultivo$clasificacion_utilizada[cultivo$nombre_cultivo == "Maíz Blanco" | cultivo$nombre_cultivo == "Maíz Amarillo"] <- "Maíz (blanco o amarillo)"
cultivo$clasificacion_utilizada[cultivo$nombre_cultivo == "Arroz verde"] <- "Arroz"

# Identificar principal cultivo de la UPA: % del area sembrada mas alto de la finca
# Si no queda 1 cultivo claro con % de area sembrada, usamos area cosechada
cultivo <- cultivo %>%
  group_by(cod_vereda) %>%
  mutate(area_total = sum(area_sembrada), part = 100*(area_sembrada/area_total),
         test = sum(part), principal = ifelse(part == max(part), 1, 0)) %>%
  dplyr::filter(principal == 1) %>%
  mutate(area_total = sum(area_cosechada), part = 100*(area_cosechada/area_total),
         test = sum(part), principal2 = ifelse(part == max(part), 1, 0)) %>%
  dplyr::filter(principal == 1 | principal2 == 1) %>%
  ungroup() %>%
  distinct(cod_vereda, .keep_all = T) %>%
  dplyr::select(cod_vereda, clasificacion_utilizada)

# Unimos datos de productividad y pca con regiones, cultivo principal y clima
data_reg <- prod_pca %>%
  left_join(base_ver, by = c("cod_vereda")) %>%
  left_join(base_clima, by = c("cod_vereda")) %>%
  left_join(cultivo, by = c("cod_vereda")) %>%
  dplyr::rename(grupo_cultivo = clasificacion_utilizada) %>%
  dplyr::select(region, provincia, cod_provincia, cod_dpto, cod_mpio, 
                cod_vereda, grupo_cultivo, everything())

#-------------------------------------------------------#
# 1. Regresiones PCA y productividad -----
#-------------------------------------------------------#

reg <- lfe::felm(ls2 ~ f01_water + f02_soil + f03_energy + f04_assit + f05_credit + f06_workers + 
     f07_mag_crops + f08_others + f09_plaques + temp + rain | region + grupo_cultivo, 
     data = data_reg)

summary(reg, robust = T)

# Organizar tabla en HTML
labels_dependientes = c("Acceso y uso del agua", "Prácticas del suelo", "Fuentes de energía", 
                        "Asistencia técnica", "Acceso al crédito agropecuario", "Mano de obra", 
                        "Manejo de cultivos (fertilizantes)", "Manejo de desechos y recursos naturales", 
                        "Prácticas de control de plagas", "Temperatura media","Precipitación media")

# Tabla descriptivas
# Docx
stargazer::stargazer(reg,  
                     header=FALSE, type= 'html',
                     keep = c("f01_water", "f02_soil", "f03_energy", "f04_assit", "f05_credit",
                              "f06_workers", "f07_mag_crops", "f08_others", "f09_plaques",
                              "temp", "rain"),
                     digits = 2,
                     dep.var.labels.include = TRUE,
                     model.numbers = TRUE,
                     omit.stat = c("ser", "adj.rsq", "rsq", "f"),
                     label = "reg_prod_pca",
                     dep.var.labels = c("Log(Productividad agropecuaria)"),
                     title = "Principales determinantes de la productividad agropecuaria",
                     column.separate = c(2),
                     covariate.labels = labels_dependientes, 
                     table.placement = "H", 
                     column.sep.width = "-7pt",
                     df = FALSE,
                     notes = "",
                     notes.append = FALSE,
                     out = glue("04_Tablas/reg_productividad_pca.doc"))

# Latex
stargazer::stargazer(reg,  
                     header=FALSE, type= 'latex',
                     keep = c("f01_water", "f02_soil", "f03_energy", "f04_assit", "f05_credit",
                              "f06_workers", "f07_mag_crops", "f08_others", "f09_plaques",
                              "temp", "rain"),
                     digits = 2,
                     dep.var.labels.include = TRUE,
                     model.numbers = TRUE,
                     omit.stat = c("ser", "adj.rsq", "rsq", "f"),
                     label = "reg_prod_pca",
                     dep.var.labels = c("Productividad agrícola (Log)"),
                     title = "Principales determinantes de la productividad agrícola",
                     # column.labels = c("First group"),
                     column.separate = c(2),
                     covariate.labels = labels_dependientes, 
                     table.placement = "H", 
                     column.sep.width = "-7pt",
                     df = FALSE,
                     # se = list(reg1[["rse"]], reg2[["rse"]], reg11[['rse']], reg21[['rse']]),
                     # add.lines = list(c("Media de la variable dependiente", mean_dep1, mean_dep2, mean_dep11, mean_dep21),
                     #                  c("Media mujeres productoras (\\%)", mean_ind1, mean_ind2, mean_ind11, mean_ind21),
                     #                  c("Periodo", "2014", "2014", "2014", "2014")),
                     notes = "",
                     notes.append = FALSE,
                     out = glue("04_Tablas/reg_productividad_pca.tex"))

