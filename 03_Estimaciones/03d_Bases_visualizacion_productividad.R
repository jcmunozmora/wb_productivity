#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 16 marzo, 2022
# Crear bases de datos para visualizacion
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

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
graficas <- "03_Graficas"
options(scipen = 999)

#-------------------------------------------------------#
# 0. Abrir datos ----
#-------------------------------------------------------#

# Abrimos datos productividad agricola: enfasis en productividad con area sembrada
prod <- haven::read_dta(glue("{datos}/Productividad/base_productividad_hamman.dta")) %>%
  drop_na(ls2) %>% mutate(ing_hectarea = revenue/area_sembrada)

# Panel CEDE con regiones
cede <- readxl::read_excel(glue("{datos_ori}/CEDE/PANEL_CARACTERISTICAS_GENERALES(2019).xlsx")) %>%
  dplyr::select(codprovincia, codmpio, provincia, gcaribe, gpacifica, gorinoquia, gamazonia, gandina) %>%
  dplyr::rename(p_munic = codmpio, cod_provincia = codprovincia) %>%
  distinct()

# Unimos productividad con clasificacion de regiones
base_prod <- prod %>%
  left_join(cede, by = "p_munic") %>%
  dplyr::rename(cod_dpto = p_depto, cod_mpio = p_munic) %>%
  dplyr::select(-c(s1, yield1, ls1, s3, yield3, ls3)) %>%
  mutate(region = ifelse(gcaribe == 1, "Caribe", 
                         ifelse(gpacifica == 1, "Pacifica", 
                                ifelse(gorinoquia == 1, "Orinoquia",
                                       ifelse(gamazonia == 1, "Amazonia",
                                              ifelse(gandina == 1, "Andina", 0)))))) %>%
  dplyr::select(provincia, cod_provincia, cod_dpto, cod_mpio, cod_vereda, encuesta, everything()) 
                
table(base_prod$region)
colSums(is.na(base_prod))

#--------------------------#
# A. Identificar cultivo principal ----
# Para calcular estadisticas de grupo de cultivo, asignamos el grupo de cultivo
# A toda la UPA segun el cultivo principal
#--------------------------#

# Base de agrupaciones de cultivos
cultivo <- readRDS(glue("{datos}/CNA/base_cna_all_maq_cultivos.rds")) %>%
  dplyr::select(cod_vereda, encuesta, tipo_cul_lote, nombre_cultivo,
                uso_suelo, clasificacion_utilizada, area_cosechada, area_sembrada) %>%
  drop_na(tipo_cul_lote, area_sembrada) %>% 
  distinct() 

# Identificamos maiz y arroz por separado
cultivo$clasificacion_utilizada[cultivo$nombre_cultivo == "Maíz Blanco" | cultivo$nombre_cultivo == "Maíz Amarillo"] <- "Maíz (blanco o amarillo)"
cultivo$clasificacion_utilizada[cultivo$nombre_cultivo == "Arroz verde"] <- "Arroz"

# Identificar principal cultivo de la UPA: % del area sembrada mas alto de la finca
# Si no queda 1 cultivo claro con % de area sembrada, usamos area cosechada
cultivo <- cultivo %>%
  group_by(cod_vereda, encuesta) %>%
  mutate(area_total = sum(area_sembrada), part = 100*(area_sembrada/area_total),
         test = sum(part), principal = ifelse(part == max(part), 1, 0)) %>%
  dplyr::filter(principal == 1) %>%
  mutate(area_total = sum(area_cosechada), part = 100*(area_cosechada/area_total),
         test = sum(part), principal2 = ifelse(part == max(part), 1, 0)) %>%
  dplyr::filter(principal == 1 | principal2 == 1) %>%
  ungroup() %>%
  distinct(cod_vereda, encuesta, .keep_all = T) %>%
  dplyr::select(cod_vereda, encuesta, clasificacion_utilizada)

table(cultivo$clasificacion_utilizada)

# Guardamos base de cultivo principal por UPA
saveRDS(cultivo, glue("{datos}/CNA/limpieza_censo/lista_cultivo_principal_upa.rds"))
 
#-------------------------------------------------------#
# 1. Nivel de UPA ----
#-------------------------------------------------------#

# Guardamos base de caracteristicas CNA solo para UPAs de la muestra de productividad
cna_final <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds")) %>% 
  drop_na(precio, area_upa, area_agricola, jornales) 

cod_encuesta <- cna_final %>% 
  distinct(cod_vereda, encuesta) %>%
  mutate(encuesta_s = encuesta, encuesta = as.numeric(encuesta))

prod_final <- prod %>% dplyr::select(cod_vereda, encuesta, ls2) 

cna_prod <- cna_final %>% 
  mutate(encuesta = as.numeric(encuesta)) %>%
  left_join(prod_final, by = c("cod_vereda", "encuesta")) %>%
  drop_na(ls2) %>%
  # Pasamos areas a hectareas 
  dplyr::mutate(area_upa = area_upa/10000, area_agricola = area_agricola/10000,
                area_sembrada = area_sembrada/10000, area_cosechada = area_cosechada/10000,
                ing_cosecha = ing_cosecha/1000000) %>%
  as.data.frame() %>%
  dplyr::select(-c(`...10`,`80.299851663846511`))

# Pegamos los codigos de encuesta en caracter a la base
# Esto facilita emparejar con las demas bases de datos
cna_prod <- cna_prod %>%
  left_join(cod_encuesta, by = c("cod_vereda", "encuesta")) %>%
  dplyr::rename(encuesta_n = encuesta, encuesta = encuesta_s) %>%
  dplyr::relocate(encuesta, .before = "tipo_uc") %>%
  dplyr::select(-encuesta_n)

test <- cna_prod %>% distinct(cod_vereda, encuesta)
colSums(is.na(cna_prod))

# Guardamos base
saveRDS(cna_prod, glue("{datos}/CNA/base_cna_productividad.rds"))

#--------------------------#
# A. Cultivo ----
#--------------------------#

# Colapsamos base a nivel de grupo de cultivo, usando el cultivo principal por UPA
cna_prod <- readRDS(glue("{datos}/CNA/base_cna_productividad.rds"))
cultivo <- readRDS(glue("{datos}/CNA/limpieza_censo/lista_cultivo_principal_upa.rds"))

cna_prod <- cna_prod %>% left_join(cultivo, by = c("cod_vereda", "encuesta")) 

# Organizar clasificaciones cultivos
cna_prod$clasificacion_utilizada[str_detect(cna_prod$clasificacion_utilizada, "legumbres nueces y semillas oleaginosas")] <- "Legumbres, nueces y semillas oleaginosas"
cna_prod$clasificacion_utilizada[str_detect(cna_prod$clasificacion_utilizada, "Arboles maderables")] <- "Árboles maderables"
cna_prod$clasificacion_utilizada[str_detect(cna_prod$clasificacion_utilizada, "Arboles no maderables")] <- "Árboles no maderables"
table(cna_prod$clasificacion_utilizada)

# Agregamos datos a nivel de cultivo: solo lotes con cultivos (excluidos pecuarios)
cna_agregado <- cna_prod %>%
  drop_na(tipo_cul_lote) %>%
  mutate(lote = 1) %>%
  group_by(clasificacion_utilizada) %>%
  summarise(num_lote = sum(lote, na.rm = T), area_upa = sum(area_upa, na.rm = T), 
            area_agricola = sum(area_agricola, na.rm = T), area_sembrada = sum(area_sembrada, na.rm = T), 
            area_cosechada = sum(area_cosechada, na.rm = T), cant_cosecha = sum(cant_cosecha, na.rm = T),
            total_jor = sum(jornales, na.rm = T), ing_cosecha = sum(ing_cosecha, na.rm = T)) %>%
  ungroup() %>%
  rename(clas_cultivo = clasificacion_utilizada)

# Guardar base
saveRDS(cna_agregado, glue("{datos}/CNA/base_cna_grupos_cultivos_principal.rds"))

#-------------------------------------------------------#
# 2. Nivel de vereda ----
#-------------------------------------------------------#

# Agregamos la base a nivel de vereda
base_ver <- base_prod %>%
  dplyr::select(-c(starts_with(c("log", "g")), ls2)) %>%
  group_by(cod_vereda, provincia, cod_provincia, cod_dpto, cod_mpio, region) %>%
  summarise(across(.cols = everything(), .fns = mean)) %>%
  ungroup() %>%
  haven::zap_formats() %>% haven::zap_labels() %>% haven::zap_label() %>%
  mutate(log_area_sembrada = log(area_sembrada), log_jornales = log(jornales),
         log_ingreso = log(ing_hectarea), log_ing_jornal = log(revenue_j), ls2 = log(s2),
         ing_maq = revenue/kminbar)

base_ver <- base_ver %>%
  dplyr::select(region, provincia, cod_provincia, cod_dpto, cod_mpio, cod_vereda, everything()) %>%
  dplyr::rename(potencial_prod = mpoty, potencial_prod_j = mpoty_j, 
                ing_cosecha = revenue, ing_jornal = revenue_j, productividad = s2, 
                log_productividad = ls2) %>% 
  dplyr::select(-encuesta)

# Exportar datos
saveRDS(base_ver, glue("{datos}/Productividad/base_productividad_vereda.rds"))
rm(base_ver, cede)

#-------------------------------------------------------#
# 3. Nivel de vereda-cultivo-tamano ----
#-------------------------------------------------------#

# Base CNA con clasificacion de cultivos
base_cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq_cultivos.rds")) %>%
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, tipo_cul_lote, nombre_cultivo, 
                uso_suelo, clasificacion_utilizada, ing_cosecha, 
                area_upa, area_sembrada, area_cosechada, area_agricola, jornales) %>% 
  drop_na(tipo_cul_lote, area_upa) %>%
  distinct() %>%
  mutate(area_upa = area_upa/10000, area_sembrada = area_sembrada/10000,
         area_agricola = area_agricola/10000, area_cosechada = area_cosechada/10000,
         encuesta = as.numeric(encuesta), cat_tierra = NA)

base_cna$cat_tierra[base_cna$area_upa <= 5] <- "Micro"
base_cna$cat_tierra[base_cna$area_upa > 5 & base_cna$area_upa <= 10] <- "Pequeno"
base_cna$cat_tierra[base_cna$area_upa > 10 & base_cna$area_upa <= 20] <- "Mediano"
base_cna$cat_tierra[base_cna$area_upa > 20] <- "Grande"

# Abrimos base de cultivos principales por UPA y se le asigna clasificacion a datos
cultivo <- readRDS(glue("{datos}/limpieza_censo/lista_cultivo_principal_upa.rds")) %>%
  mutate(encuesta = as.numeric(encuesta))

base_cul <- base_cna %>% 
  left_join(cultivo, by = c("cod_vereda", "encuesta")) %>% 
  dplyr::rename(clasificacion_utilizada = clasificacion_utilizada.y) %>% 
  dplyr::select(-clasificacion_utilizada.x) %>%
  mutate(ing_cosecha = ing_cosecha + 0.001)

# Unimos con ingreso por jornal e ingreso por maquinaria
# Como ambas son a nivel de UPA se extraen de base productividad 
base_cul <- base_cul %>% 
  left_join(prod, by = c("encuesta", "cod_vereda")) %>% 
  dplyr::select(starts_with("cod"), encuesta, tipo_cul_lote, nombre_cultivo, uso_suelo,
                ing_cosecha, area_upa, area_sembrada.x, area_cosechada, area_agricola,
                jornales.y, cat_tierra, clasificacion_utilizada, revenue_j, kminbar, ing_hectarea) %>%
  dplyr::rename(area_sembrada = area_sembrada.x, ing_jornal = revenue_j, jornales = jornales.y) %>%
  mutate(ing_maq = ing_cosecha/kminbar)

colSums(is.na(base_cul))

#--------------------------#
# B. Cultivo ----
#--------------------------#

# Productividad por vereda
base_prod <- readRDS(glue("{datos}/Productividad/base_productividad_vereda.rds"))

# Base por cultivo
base_cultivo <- base_cul %>%
  group_by(cod_dpto, cod_mpio, cod_vereda, clasificacion_utilizada) %>%
  summarise(area_upa = mean(area_upa, na.rm = T),
            area_agricola = mean(area_agricola, na.rm = T), 
            area_sembrada = mean(area_sembrada, na.rm = T),
            area_cosechada = mean(area_cosechada, na.rm = T),
            jornales = mean(jornales, na.rm = T), 
            ing_cosecha = mean(ing_cosecha, na.rm = T),
            ing_hectarea = mean(ing_hectarea, na.rm = T),
            ing_jornal = mean(ing_jornal, na.rm = T),
            ing_maq = mean(ing_maq, na.rm = T)) %>%
  ungroup() %>%
  dplyr::rename(grupo_cultivo = clasificacion_utilizada)

# Unimos estadisticas y productividad por vereda
base_ver_cul <- base_cultivo %>%
  left_join(base_prod, by = c("cod_vereda", "cod_mpio", "cod_dpto")) %>%
  dplyr::select(region, provincia, cod_provincia, cod_dpto, cod_mpio, cod_vereda,
                grupo_cultivo, area_upa, area_sembrada.x, area_cosechada, area_agricola, 
                ing_cosecha.x, jornales.y, ing_hectarea.x, ing_jornal.x, ing_maq.y, productividad) %>%
  dplyr::rename(jornales = jornales.y, area_sembrada = area_sembrada.x, ing_cosecha = ing_cosecha.x,
                ing_hectarea = ing_hectarea.x, ing_jornal = ing_jornal.x, ing_maq = ing_maq.y) %>%
  dplyr::select(-ends_with(".y")) %>%
  mutate(log_productividad = log(productividad), log_area_sembrada = log(area_sembrada), 
         log_jornales = log(jornales), log_ingreso = log(ing_cosecha), log_ing_hectarea = log(ing_hectarea), 
         log_ing_jornal = log(ing_jornal), log_ing_maq = log(ing_maq)) %>%
  drop_na(productividad)

colSums(is.na(base_ver_cul))
base_ver_cul$ing_hectarea[is.nan(base_ver_cul$ing_hectarea)] <- NA
base_ver_cul$ing_jornal[is.nan(base_ver_cul$ing_jornal)] <- NA
base_ver_cul$ing_maq[is.nan(base_ver_cul$ing_maq)] <- NA
base_ver_cul$log_ingreso[is.nan(base_ver_cul$log_ingreso)] <- NA
base_ver_cul$ing_cosecha[is.nan(base_ver_cul$ing_cosecha)] <- NA
base_ver_cul$log_ing_jornal[is.nan(base_ver_cul$log_ing_jornal)] <- NA
base_ver_cul$log_ing_maq[is.nan(base_ver_cul$log_ing_maq)] <- NA
base_ver_cul$log_ing_hectarea[is.nan(base_ver_cul$log_ing_hectarea)] <- NA

# Pasamos la base de wide a long
base_ver_cul_long <- base_ver_cul %>%
  pivot_longer(cols = starts_with(c("area", "ing", "jornales", "productividad", "log")), 
               names_to = "variable", values_to = "value")

# Exportar datos
saveRDS(base_ver_cul, glue("{datos}/Productividad/base_prod_vereda_cultivo_wide.rds"))
saveRDS(base_ver_cul_long, glue("{datos}/Productividad/base_prod_vereda_cultivo_long.rds"))

#--------------------------#
# C. Cultivo-tamano ----
#--------------------------#

# Agregamos todas las estadisticas a nivel de grupo cultivo-tamano
base_cul_tam <- base_cul %>%
  group_by(cod_dpto, cod_mpio, cod_vereda, clasificacion_utilizada, cat_tierra) %>%
  summarise(area_upa = mean(area_upa, na.rm = T),
            area_agricola = mean(area_agricola, na.rm = T), 
            area_sembrada = mean(area_sembrada, na.rm = T),
            area_cosechada = mean(area_cosechada, na.rm = T),
            jornales = mean(jornales, na.rm = T), 
            ing_cosecha = mean(ing_cosecha, na.rm = T),
            ing_hectarea = mean(ing_hectarea, na.rm = T),
            ing_jornal = mean(ing_jornal, na.rm = T),
            ing_maq = mean(ing_maq, na.rm = T)) %>%
  ungroup() %>%
  dplyr::rename(grupo_cultivo = clasificacion_utilizada)

# Unimos estadisticas y productividad por vereda
base_ver <- base_cul_tam %>%
  left_join(base_prod, by = c("cod_vereda", "cod_mpio", "cod_dpto")) %>%
  dplyr::select(region, provincia, cod_provincia, cod_dpto, cod_mpio, cod_vereda,
                grupo_cultivo, cat_tierra, area_upa, area_sembrada.x,
                area_cosechada, area_agricola, ing_cosecha.x, ing_hectarea.x, jornales.y, productividad,
                ing_jornal, ing_maq) %>%
  dplyr::rename(jornales = jornales.y, area_sembrada = area_sembrada.x, ing_cosecha = ing_cosecha.x,
                ing_hectarea = ing_hectarea.x) %>%
  dplyr::select(-ends_with(".y")) %>%
  mutate(log_productividad = log(productividad), log_area_sembrada = log(area_sembrada), 
         log_jornales = log(jornales), log_ingreso = log(ing_hectarea)) %>%
  drop_na(productividad)

colSums(is.na(base_ver))
base_ver$ing_hectarea[is.nan(base_ver$ing_hectarea)] <- NA
base_ver$ing_jornal[is.nan(base_ver$ing_jornal)] <- NA
base_ver$ing_maq[is.nan(base_ver$ing_maq)] <- NA
base_ver$log_ingreso[is.nan(base_ver$log_ingreso)] <- NA
base_ver$ing_cosecha[is.nan(base_ver$ing_cosecha)] <- NA

# Pasamos la base de wide a long
base_ver_long <- base_ver %>%
  pivot_longer(cols = starts_with(c("area", "ing", "jornales", "productividad", "log")), 
               names_to = "variable", values_to = "value")

# Exportar datos
saveRDS(base_ver, glue("{datos}/Productividad/base_prod_vereda_tamano_cultivo_wide.rds"))
saveRDS(base_ver_long, glue("{datos}/Productividad/base_prod_vereda_tamano_cultivo_long.rds"))











