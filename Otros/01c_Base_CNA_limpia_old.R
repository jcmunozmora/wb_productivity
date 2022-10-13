#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 28 oct, 2021
# Base datos final CNA 
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
# 1. Limpiar datos ----
#-------------------------------------------------------#

#--------------------------#
# A. Datos atipicos ----
#--------------------------#

# Se eliminan UPAs que:
# 0. Sean no agropecuarias
# 1. area > mean_upa + 3*sd_area_upa, donde mean_upa viene del universo de UPAs
# 2. UPA en resguardo/asentamiento indigena o territorio raizal
# 3. UPA en veredas llamadas "RESGUARDO"
# 4. UPA en terrenos clasificados como "excluido", "condicionado" en mapa areas protegidas de UPRA
# 5. UPA area < 100m2

# Borrar UPAs no agropecuarias (tipo_uc == 2, UPNA), se borran 543,064 obs
cna <- readRDS(glue("{datos}/CNA/base_cna_modulos.rds")) %>%
  dplyr::filter(tipo_uc == 1)

# Obtener area upas (excluimos UPNAs)
cna_gen <- readRDS(glue("{datos}/CNA/base_upas.rds")) %>%
  dplyr::filter(tipo_uc == 1) %>%
  rename(cod_dpto = p_depto, cod_mpio = p_munic, area_upa = p_s5pautos) %>%
  select(starts_with("cod_"), area_upa) %>%
  drop_na(area_upa) 

area_upa <- cna_gen %>% 
  summarise(mean_area = mean(area_upa), sd_area = sd(area_upa), 
            min_area = min(area_upa), max_area = max(area_upa)) %>%
  mutate(area_atipico = mean_area + sd_area*3)

saveRDS(area_upa, glue("{datos}/CNA/limpieza_censo/descriptivas_area_upas.rds"))

# Filtrar por area
area_upa <- readRDS(glue("{datos}/CNA/limpieza_censo/descriptivas_area_upas.rds"))
atipico <- area_upa$area_atipico
cna <- cna %>% dplyr::filter(area_upa >= 100) %>% dplyr::filter(area_upa <= atipico)

# Eliminar UPA con poblacion predominantemente indigena o raizal
cna <- cna %>% 
  filter(pred_etnica != 1 & pred_etnica != 2 & pred_etnica != 3 & pred_etnica != 4 & pred_etnica != 9)

# Pegamos nombre de las veredas a las UPA y eliminamos las llamadas resguardo
nom_vereda <- read_excel("01_Datos_originales/CNA/nombres_veredas_CNA.xlsx") %>%
  clean_names() %>% mutate(cod_vereda = as.numeric(cod_vereda))

resguardos <- nom_vereda %>% filter(str_detect(nombre_de_la_vereda, "RESGUARDO"))

cna <- cna %>% 
  left_join(nom_vereda, by = "cod_vereda") %>%
  filter(!(nombre_de_la_vereda %in% resguardos$nombre_de_la_vereda))

# Tenemos 2'157,523 upas sin mapa upra; hamman reporta 1.840.998 en muestra final
test <- cna %>% distinct()
upas <- test %>% distinct(cod_vereda, encuesta)

#--------------------------#
# B. Zonas de exclusion ----
# Eliminamos veredas ubicadas en zonas de exclusion legal segun UPRA
#--------------------------#

veredas_ex <- readRDS(glue("{datos}/UPRA/veredas_zona_exclusion_legal.rds")) %>%
  mutate(cod_dpto = as.numeric(cod_dpto), cod_mpio = as.numeric(cod_mpio), 
         cod_vereda = as.numeric(cod_vereda)) 

cna_clean <- cna %>% left_join(veredas_ex, by = c("cod_dpto", "cod_mpio", "cod_vereda")) %>%
  mutate(zona_exclusion = ifelse(is.na(zona_exclusion), 0, zona_exclusion)) %>%
  dplyr::filter(zona_exclusion == 0)

# Tras eliminar zonas de exclusion quedan 1'573,288 upas
test <- cna_clean %>% distinct()
upas <- test %>% distinct(cod_vereda, encuesta)

rm(test, upas, veredas_ex, nom_vereda, resguardos, area_upa)

# El match entre zonas de exclusion y veredas CNA no es perfecto, CNA tiene 30.296 veredas 
# y MGN 2015 tiene 32.305
# test <- veredas_ex %>% anti_join(cna, by = c("cod_dpto", "cod_mpio", "cod_vereda")) %>%
#   dplyr::filter(zona_exclusion == 0)
# No pegan 954 veredas del CNA
# test <- nom_vereda %>% anti_join(veredas_ex, by = c("cod_vereda"))

#-------------------------------------------------------#
# 2. Modulos Pesca y Acuicultura ----
# Por el peso de la base no es posible pegar modulos antes
#-------------------------------------------------------#

# Agregar modulos de pesca y acuilcultura
cna_pesca <- readRDS(glue("{datos}/CNA/base_pesca_artesanal.rds")) %>% 
  dplyr::rename(cod_dpto = p_depto, cod_mpio = p_munic) %>%
  mutate(cod_dpto = as.numeric(cod_dpto), cod_mpio = as.numeric(cod_mpio), 
         cod_vereda = as.numeric(cod_vereda)) 

cna_acui <- readRDS(glue("{datos}/CNA/base_acuicultura.rds")) %>% 
  dplyr::rename(cod_dpto = p_depto, cod_mpio = p_munic) %>%
  mutate(cod_dpto = as.numeric(cod_dpto), cod_mpio = as.numeric(cod_mpio), 
         cod_vereda = as.numeric(cod_vereda))

# Unir modulos con base limpia
cna2 <- cna_clean %>% 
  left_join(cna_pesca, by = c("cod_dpto", "cod_mpio", "encuesta", "cod_vereda")) %>%
  left_join(cna_acui, by = c("cod_dpto", "cod_mpio", "encuesta", "cod_vereda")) %>%
  dplyr::rename(pesca_agua_dulce = p_s8_numpezdul, especies_dulce = p_s8p1141,
                captura_vul_dulce1 = p_s8p1151, captura_vul_dulce2 = p_s8p1161,
                pesca_agua_sal = p_s8_numpezsal, especies_sal = p_s8p1142,
                captura_vul_sal1 = p_s8p1152, captura_vul_sal2 = p_s8p1162,
                acui_nombre_pez = p_s7pnumpez, acui_num_cos = p_s7p96,
                acui_pez_cos = p_s7p97, acui_num_pez = p_s7p98,
                acui_peso_pez = p_s7p99, acui_prod_ano = p_s7p100) %>%
  dplyr::select(-starts_with(c("p_s8", "p_s7", "tipo_reg")))

cna2 <- cna2 %>% distinct()
test <- cna2 %>% distinct(cod_vereda, encuesta)

# Guardamos cna con pesca
saveRDS(cna2, glue("{datos}/CNA/limpieza_censo/base_cna_modulos_clean.rds"))

summary(cna2$area_upa)

#-------------------------------------------------------#
# 3. Calculo jornales e ingresos ----
#-------------------------------------------------------#

# Calculo de jornales viene de Hamman (2018)
# Pasamos produccion de toneladas a kilogramos
cna <- readRDS(glue("{datos}/CNA/limpieza_censo/base_cna_modulos_clean.rds")) %>%
  mutate(tpj = total_trab*(6*4*12), ja = total_jorn*12, jornales = tpj+ja,
         cant_cosecha_kg = cant_cosecha*1000)

# Precios por cultivo
precios <- readxl::read_excel(glue("{datos_ori}/precios_por_cultivo_Hamman_2018.xlsx")) %>%
  dplyr::rename(tipo_cul_lote = cod_cul) %>% dplyr::select(-c("tipo_fuente", "fuente"))

cna <- cna %>% left_join(precios, by = "tipo_cul_lote")

# Calcular ingreso por cultivo de la UPA
cna <- cna %>% 
  mutate(ing_cosecha = precio*cant_cosecha_kg) %>%
  # Factores de conversion a cultivos especificos (Hamman, 2018)
  mutate(ing_cosecha = ifelse(tipo_cul_lote == "00180201001", ing_cosecha*(1/0.115), 
                              ifelse(tipo_cul_lote == "00149102001", ing_cosecha*(1/0.187),
                                     ifelse(tipo_cul_lote == "00149102001", ing_cosecha*(0.664), ing_cosecha)))) 

# Sumamos ingresos por UPA (ingreso en pesos por kg cosechado)
# Hay informacion de ingresos para 532.779 upa
ingresos_upa <- cna %>%
  drop_na(cant_cosecha, precio) %>%
  distinct(cod_vereda, encuesta, tipo_cul_lote, cant_cosecha, .keep_all = T) %>%
  group_by(cod_vereda, encuesta) %>%
  summarise(ing_cul_upa = sum(ing_cosecha, na.rm = T)) %>%
  ungroup()

# Guardar CNA con ingresos de toda la UPA
# cna_ing <- cna %>% left_join(ingresos_upa, by = c("cod_vereda", "encuesta")) %>% 
#   dplyr::select(-c(tipo_uc, zona_exclusion))
# colSums(is.na(cna_ing))

# Exportar 
saveRDS(ingresos_upa, glue("{datos}/CNA/limpieza_censo/base_cna_ing_jornal_upa.rds"))

#-------------------------------------------------------#
# 4. Eliminar UPAs sin maquinaria ----
#-------------------------------------------------------#

cna <- cna %>% mutate(tiene_maq = ifelse(!is.na(tipo_maq), 1, 0))

# UPAs con maquinaria vs sin maquinaria
maq <- cna %>% distinct(cod_vereda, encuesta, .keep_all = T) %>% dplyr::filter(tiene_maq == 1)
maq <- nrow(maq)
upas <- cna %>% distinct(cod_vereda, encuesta)
upas <- nrow(upas)

# 17% de las UPA que tenemos (limpias) reportan maquinaria
100*(maq/upas)

# Eliminar UPAs sin maquinaria
cna_maq <- cna %>% dplyr::filter(tiene_maq == 1)
test <- cna_maq %>% distinct(cod_vereda, encuesta)

saveRDS(cna_maq, glue("{datos}/CNA/base_cna_maq.rds"))

#--------------------------#
# A. Guardar muestra ----
#--------------------------#

# 10% de la base completa
# cna <- readRDS(glue("{datos}/CNA/limpieza_censo/base_cna_modulos_clean.rds"))
# rows <- round(nrow(cna2)/10)
# cna_sample <- cna2[1:rows,]
# saveRDS(cna_sample, glue("{datos}/CNA/limpieza_censo/base_cna_modulos_sample.rds"))
