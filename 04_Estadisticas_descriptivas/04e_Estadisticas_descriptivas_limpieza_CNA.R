#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 7 dic, 2021
# Estadisticas descriptivas de la muestra de UPAs 
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
tablas <- "04_Tablas"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Filtro #1 ----
#-------------------------------------------------------#

# Construimos descriptivas para cada etapa de limpieza del CNA

# Abrir datos originales CNA (Filtro #1)
cna1 <- readRDS(glue("{datos}/CNA/base_cna_modulos.rds")) %>%
  dplyr::filter(tipo_uc == 1) %>%
  mutate(area_agricola = area_upa - (area_pasto_nat + area_pasto_sem + area_inf_noagro + area_otros_usos),
         area_agricola = ifelse(area_agricola < 0, 0, area_agricola))

cna1_desc <- cna1 %>%
  # Calcular jornales
  mutate(tpj = total_trab*(6*4*12), ja = total_jorn*12, jornales = tpj+ja) %>%
  group_by(cod_vereda, encuesta) %>%
  distinct(area_sembrada, area_upa, area_agricola, area_cosechada, jornales) %>%
  # Area sembrada y cosechada varian por cultivo-UPA, promediamos
  summarise(area_sembrada = mean(area_sembrada, na.rm = T), 
            area_upa = mean(area_upa, na.rm = T),
            area_agricola = mean(area_agricola, na.rm = T),
            area_cosechada = mean(area_cosechada, na.rm = T),
            jornales = mean(jornales, na.rm = T)) %>%
  ungroup() %>%
  # Pasamos areas a hectareas (sembrada y cosechada vienen en ha)
  mutate(area_upa = area_upa/10000, area_agricola = area_agricola/10000) %>%
  dplyr::select(area_upa, area_agricola, area_sembrada, area_cosechada, jornales) %>%
  as.data.frame()

labels <- c("Farm area (ha)", "Farm agricultural area (ha)", 
            "Farm planted area (ha)", "Farm harvested area (ha)",
            "Total workers (year)")

# Tabla descriptivas
stargazer::stargazer(cna1_desc, summary.stat = c("mean", "sd", "min", "max", "n"),
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = labels,
                     title = "General descriptive statistics",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_descriptive_stats_filter1.doc"))

# Area sembrada total
area <- cna1_desc %>% dplyr::select(area_sembrada) %>% drop_na(area_sembrada)
sum(area$area_sembrada, na.omit = T)

rm(cna1, cna1_desc)

#-------------------------------------------------------#
# 2. Filtro #2 y #3 ----
#-------------------------------------------------------#

# Abrir datos tras filtrar por area y zonas restringidas (Filtros #2 y 3)
cna2 <- readRDS(glue("{datos}/CNA/limpieza_censo/base_cna_modulos_clean.rds")) %>%
  mutate(area_agricola = area_upa - (area_pasto_nat + area_pasto_sem + area_inf_noagro + area_otros_usos),
         area_agricola = ifelse(area_agricola < 0, 0, area_agricola))

cna2_desc <- cna2 %>%
  # Calcular jornales
  mutate(tpj = total_trab*(6*4*12), ja = total_jorn*12, jornales = tpj+ja) %>%
  group_by(cod_vereda, encuesta) %>%
  distinct(area_sembrada, area_upa, area_agricola, area_cosechada, jornales) %>%
  # Area sembrada y cosechada varian por cultivo-UPA, promediamos
  summarise(area_sembrada = mean(area_sembrada, na.rm = T), 
            area_upa = mean(area_upa, na.rm = T),
            area_agricola = mean(area_agricola, na.rm = T),
            area_cosechada = mean(area_cosechada, na.rm = T),
            jornales = mean(jornales, na.rm = T)) %>%
  ungroup() %>%
  # Pasamos areas a hectareas (sembrada y cosechada vienen en ha)
  mutate(area_upa = area_upa/10000, area_agricola = area_agricola/10000) %>%
  dplyr::select(area_upa, area_agricola, area_sembrada, area_cosechada, jornales) %>%
  as.data.frame()

labels <- c("Farm area (ha)", "Farm agricultural area (ha)", 
            "Farm planted area (ha)", "Farm harvested area (ha)",
            "Total workers (year)")

# Tabla descriptivas
stargazer::stargazer(cna2_desc, summary.stat = c("mean", "sd", "min", "max", "n"),
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = labels,
                     title = "General descriptive statistics",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_descriptive_stats_filter2.doc"))

rm(cna2, cna2_desc)

#-------------------------------------------------------#
# 3. Filtro #4 ----
#-------------------------------------------------------#

# Abrir datos tras filtrar por informacion de cultivos (Filtro #4)
cna3 <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds")) %>% drop_na(precio)

cna3_desc <- cna3 %>%
  group_by(cod_vereda, encuesta) %>%
  distinct(area_sembrada, area_upa, area_agricola, area_cosechada, jornales, ing_cosecha) %>%
  # Area sembrada y cosechada varian por cultivo-UPA, promediamos
  summarise(area_sembrada = mean(area_sembrada, na.rm = T), 
            area_upa = mean(area_upa, na.rm = T),
            area_agricola = mean(area_agricola, na.rm = T),
            area_cosechada = mean(area_cosechada, na.rm = T),
            jornales = mean(jornales, na.rm = T),
            ing_cosecha = mean(ing_cosecha, na.rm = T)) %>%
  ungroup() %>%
  # Pasamos areas a hectareas 
  mutate(area_upa = area_upa/10000, area_agricola = area_agricola/10000,
         area_sembrada = area_sembrada/10000, area_cosechada = area_cosechada/10000,
         ing_cosecha = ing_cosecha/1000000) %>%
  dplyr::select(area_upa, area_agricola, area_sembrada, area_cosechada, jornales, ing_cosecha) %>%
  as.data.frame()

labels <- c("Farm area (ha)", "Farm agricultural area (ha)", 
            "Farm planted area (ha)", "Farm harvested area (ha)",
            "Total workers (year)", "Output (\\$ millions)")

# Tabla descriptivas
stargazer::stargazer(cna3_desc, summary.stat = c("mean", "sd", "min", "max", "n"),
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = labels,
                     title = "General descriptive statistics",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_descriptive_stats_filter4.doc"))

rm(cna3, cna3_desc)

#-------------------------------------------------------#
# 4. Filtro #5 ----
#-------------------------------------------------------#

# Abrir datos tras filtrar por maquinaria existente (Filtro #5)
cna4 <- readRDS(glue("{datos}/CNA/base_cna_maq.rds")) %>% drop_na(precio)

cna4_desc <- cna4 %>%
  group_by(cod_vereda, encuesta) %>%
  distinct(area_sembrada, area_upa, area_agricola, area_cosechada, jornales, ing_cosecha) %>%
  # Area sembrada y cosechada varian por cultivo-UPA, promediamos
  summarise(area_sembrada = mean(area_sembrada, na.rm = T), 
            area_upa = mean(area_upa, na.rm = T),
            area_agricola = mean(area_agricola, na.rm = T),
            area_cosechada = mean(area_cosechada, na.rm = T),
            jornales = mean(jornales, na.rm = T),
            ing_cosecha = mean(ing_cosecha, na.rm = T)) %>%
  ungroup() %>%
  # Pasamos areas a hectareas 
  mutate(area_upa = area_upa/10000, area_agricola = area_agricola/10000,
         area_sembrada = area_sembrada/10000, area_cosechada = area_cosechada/10000,
         ing_cosecha = ing_cosecha/1000000) %>%
  dplyr::select(area_upa, area_agricola, area_sembrada, area_cosechada, jornales, ing_cosecha) %>%
  as.data.frame()

labels <- c("Farm area (ha)", "Farm agricultural area (ha)", 
            "Farm planted area (ha)", "Farm harvested area (ha)",
            "Total workers (year)", "Output (\\$ millions)")

# Tabla descriptivas
stargazer::stargazer(cna4_desc, summary.stat = c("mean", "sd", "min", "max", "n"),
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = labels,
                     title = "General descriptive statistics",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_descriptive_stats_filter5.doc"))

rm(cna4, cna4_desc)

#-------------------------------------------------------#
# 5. Filtro #6 ----
#-------------------------------------------------------#

# Abrir datos tras filtrar por maquinaria imputada 
prod <- haven::read_dta(glue("{datos}/Productividad/base_productividad_hamman.dta")) %>% 
  drop_na(ls2) %>% dplyr::select(p_munic, cod_vereda, encuesta, ls2) %>%
  dplyr::rename(cod_mpio = p_munic)

cna6 <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds")) %>% 
  drop_na(precio, area_upa, area_agricola, jornales) %>%
  mutate(encuesta = as.numeric(encuesta)) 

cna6_desc <- cna6 %>% 
  left_join(prod, by = c("cod_vereda", "encuesta")) %>%
  drop_na(ls2) %>%
  dplyr::group_by(cod_vereda, encuesta) %>%
  distinct(area_sembrada, area_upa, area_agricola, area_cosechada, jornales, ing_cosecha) %>%
  # Area sembrada y cosechada varian por cultivo-UPA, promediamos
  summarise(area_sembrada = mean(area_sembrada, na.rm = T), 
            area_upa = mean(area_upa, na.rm = T),
            area_agricola = mean(area_agricola, na.rm = T),
            area_cosechada = mean(area_cosechada, na.rm = T),
            jornales = mean(jornales, na.rm = T),
            ing_cosecha = mean(ing_cosecha, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Pasamos areas a hectareas 
  dplyr::mutate(area_upa = area_upa/10000, area_agricola = area_agricola/10000,
         area_sembrada = area_sembrada/10000, area_cosechada = area_cosechada/10000,
         ing_cosecha = ing_cosecha/1000000) %>%
  dplyr::select(area_upa, area_agricola, area_sembrada, area_cosechada, jornales, ing_cosecha) %>%
  as.data.frame()

labels <- c("Farm area (ha)", "Farm agricultural area (ha)", 
            "Farm planted area (ha)", "Farm harvested area (ha)",
            "Total workers (year)", "Output (\\$ millions)")

# Tabla descriptivas
stargazer::stargazer(cna6_desc, summary.stat = c("mean", "sd", "min", "max", "n"),
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = labels,
                     title = "General descriptive statistics",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_descriptive_stats_filter6.doc"))

# Area sembrada total
area <- cna6_desc %>% dplyr::select(area_sembrada) %>% drop_na(area_sembrada)
sum(area$area_sembrada, na.omit = T)


rm(cna6, cna6_desc)
