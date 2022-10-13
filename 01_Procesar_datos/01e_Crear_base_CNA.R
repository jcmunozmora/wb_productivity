#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 22 nov, 2021
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
# A. UPNA y areas UPA ----
#--------------------------#

# Se eliminan UPAs que:
# 0. Sean no agropecuarias
# 1. area > mean_upa + 3*sd_area_upa, donde mean_upa viene del universo de UPAs
# 2. UPA en resguardo/asentamiento indigena o territorio raizal
# 3. UPA en terrenos clasificados como "excluido", "condicionado" en mapa areas protegidas de UPRA
# 4. UPA area < 100m2

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

#--------------------------#
# B. Poblacion indigena/raizal ----
#--------------------------#

# Eliminar UPA con poblacion predominantemente indigena o raizal
cna <- cna %>% 
  dplyr::filter(pred_etnica != 1 & pred_etnica != 2 & pred_etnica != 3 & pred_etnica != 4 & pred_etnica != 9) 

# Abrimos lista de veredas ubicadas en resguardos y consejos comunitarios
# resguardos <- readRDS(glue("{datos}/UPRA/veredas_zona_resguardo_indigena.rds")) %>% 
resguardos <- readRDS(glue("{datos}/UPRA/upas_zona_resguardo_indigena.rds")) %>%
  dplyr::select(-cod_vereda) %>%
  distinct()

consejos <- readRDS(glue("{datos}/UPRA/upas_zona_consejo_comunitario.rds")) %>% 
# consejos <- readRDS(glue("{datos}/UPRA/veredas_zona_consejo_comunitario.rds")) %>% 
  dplyr::select(-cod_vereda) %>%
  distinct()

cna_pob <- cna %>%
  left_join(resguardos, by = c("encuesta")) %>%
  left_join(consejos, by = c("encuesta"))

cna_pob$zona_resguardo[is.na(cna_pob$zona_resguardo)] <- 0
cna_pob$zona_consejo[is.na(cna_pob$zona_consejo)] <- 0

# Quedamos con 11'770,597 obs 
cna_pob <- cna_pob %>% dplyr::filter(zona_consejo == 0 & zona_resguardo == 0)

# Tenemos 2'056,558 upas sin mapa upra; hamman reporta 1.840.998 en muestra final
test <- cna_pob %>% distinct()
upas <- test %>% distinct(cod_vereda, encuesta)
rm(test, upas, consejos, resguardos)

#--------------------------#
# B. Zonas de exclusion ----
# Eliminamos veredas ubicadas en zonas de exclusion legal segun UPRA
#--------------------------#

upas_ex <- readRDS(glue("{datos}/UPRA/upas_zona_exclusion_legal.rds")) %>%
# veredas_ex <- readRDS(glue("{datos}/UPRA/veredas_zona_exclusion_legal.rds")) %>%
  dplyr::select(-cod_vereda) %>%
  distinct()

cna_pob <- cna_pob %>% left_join(upas_ex, by = c("encuesta")) %>%
  mutate(zona_exclusion = ifelse(is.na(zona_exclusion), 0, zona_exclusion)) %>%
  dplyr::filter(zona_exclusion == 0)

# Tras eliminar zonas de exclusion quedan 1'680,623 upas
test <- cna_pob %>% distinct()
upas <- test %>% distinct(encuesta)

rm(test, upas, upas_ex, area_upa, cna)

# Guardamos base de CNA
# saveRDS(cna_pob, glue("{datos}/"))

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
cna2 <- cna_pob %>% 
  left_join(cna_pesca, by = c("cod_dpto", "cod_mpio", "encuesta", "cod_vereda")) %>%
  left_join(cna_acui, by = c("cod_dpto", "cod_mpio", "encuesta", "cod_vereda")) %>%
  dplyr::rename(pesca_agua_dulce = p_s8_numpezdul, especies_dulce = p_s8p1141,
                captura_vul_dulce1 = p_s8p1151, captura_vul_dulce2 = p_s8p1161,
                pesca_agua_sal = p_s8_numpezsal, especies_sal = p_s8p1142,
                captura_vul_sal1 = p_s8p1152, captura_vul_sal2 = p_s8p1162,
                acui_nombre_pez = p_s7pnumpez, acui_num_cos = p_s7p96,
                acui_pez_cos = p_s7p97, acui_num_pez = p_s7p98,
                acui_peso_pez = p_s7p99, acui_prod_ano = p_s7p100) %>%
  dplyr::select(-starts_with(c("p_s8", "p_s7", "tipo_reg"))) %>% 
  distinct()

# Guardamos cna con pesca
saveRDS(cna2, glue("{datos}/CNA/limpieza_censo/base_cna_modulos_clean.rds"))

#-------------------------------------------------------#
# 3. Calculos adicionales ----
#-------------------------------------------------------#

# Area agricola
cna <- readRDS(glue("{datos}/CNA/limpieza_censo/base_cna_modulos_clean.rds")) %>%
  mutate(area_agricola = area_upa - (area_pasto_nat + area_pasto_sem + area_inf_noagro + area_otros_usos),
         area_agricola = ifelse(area_agricola < 0, 0, area_agricola))

#--------------------------#
# A. Jornales e ingresos ----
#--------------------------#

# Calculo de jornales viene de Hamman (2018)
# Pasamos produccion de toneladas a kilogramos
cna <- cna %>%
  mutate(tpj = total_trab*(6*4*12), ja = total_jorn*12, jornales = tpj+ja,
         cant_cosecha_kg = cant_cosecha*1000)

# Precios por cultivo
precios <- readxl::read_excel(glue("{datos}/CNA/precios/lista_precios_cultivos_area_cna.xlsx")) %>%
  dplyr::select(-c("tipo_fuente", "fuente")) %>% dplyr::rename(tipo_cul_lote = cod_cultivo)

precios_info <- precios %>% 
  drop_na(precio) %>%
  mutate(part_all = sum(part))

cna <- cna %>% left_join(precios, by = "tipo_cul_lote")

# Calcular ingreso por cultivo de la UPA
cna <- cna %>% 
  mutate(ing_cosecha = precio*cant_cosecha_kg) %>%
  # Factores de conversion a cultivos especificos (Hamman, 2018)
  mutate(ing_cosecha = ifelse(tipo_cul_lote == "00180201001", ing_cosecha*(1/0.115), 
                              ifelse(tipo_cul_lote == "00149102001", ing_cosecha*(1/0.187),
                                     ifelse(tipo_cul_lote == "00149102001", ing_cosecha*(0.664), ing_cosecha)))) 

# Sumamos ingresos por UPA (ingreso en pesos por kg cosechado)
# En total tenemos 1'680,623 UPA y hay informacion de ingresos para 529,268 upa
ingresos_upa <- cna %>%
  drop_na(cant_cosecha, precio) %>%
  distinct(cod_vereda, encuesta, tipo_cul_lote, cant_cosecha, .keep_all = T) %>%
  group_by(cod_vereda, encuesta) %>%
  summarise(ing_cul_upa = sum(ing_cosecha, na.rm = T)) %>%
  ungroup()

# % del Area de los cultivos con informacion de precios (69.2%)
info_precios <- cna %>% 
  mutate(tiene_precio = ifelse(!is.na(precio), 1, 0)) %>%
  group_by(tiene_precio) %>%
  summarise(area_sembrada = sum(area_sembrada, na.rm = T)) %>%
  ungroup() %>%
  mutate(total = sum(area_sembrada), part = 100*(area_sembrada/total))

# Descriptivas
desc_upa <- cna %>% 
  drop_na(area_upa) %>% 
  dplyr::filter(area_upa > 0) %>% 
  distinct(encuesta, .keep_all = T)

round(min(desc_upa$area_upa)/10000, 2)
round(max(desc_upa$area_upa)/10000, 2)
round(mean(desc_upa$area_upa)/10000, 2)
round(median(desc_upa$area_upa)/10000, 2)
round(sd(desc_upa$area_upa)/10000, 2)
rm(desc_upa)

#--------------------------#
# B. % UPA dedicado a cultivo ----
#--------------------------#

# Calcular area de la upa dedicada a cada cultivo
# Pasamos areas sembrada y cosechada de has a m^2 (area upa esta en m^2)
cna <- cna %>% 
  mutate(area_sembrada = area_sembrada*10000, area_cosechada = area_cosechada*10000,
    prc_sem = 100*(area_sembrada/area_agricola), prc_cos = 100*(area_cosechada/area_agricola))

# Tenemos 4833 UPA que reportan areas sembradas/cosechadas mayores al area agricola de la UPA
test <- cna %>% 
  filter(prc_cos > 101 | prc_sem > 101) %>%
  dplyr::select(starts_with(c("cod", "encuesta", "area", "prc"))) %>% 
  distinct(encuesta)

# Si area mayor esta entre 100 y 101%, se reemplaza por 100, otros casos son reemplazados con NA
cna <- cna %>%
  mutate(prc_sem = ifelse(prc_sem > 100 & prc_sem <= 101, 100, prc_sem),
         prc_sem = ifelse(prc_sem > 101, NA, prc_sem),
         prc_cos = ifelse(prc_cos > 100 & prc_cos <= 101, 100, prc_cos),
         prc_cos = ifelse(prc_cos > 101, NA, prc_cos))

#-------------------------------------------------------#
# 4. Maquinaria ----
#-------------------------------------------------------#

# Identificamos las UPA que tienen maquinaria
cna$existe_maq[is.na(cna$existe_maq)] <- 0
cna$existe_maq[cna$existe_maq == 2] <- 0
cna$existe_maq[cna$existe_maq == 9] <- 0
cna$tipo_maq[is.na(cna$tipo_maq)] <- 0
cna$maq_menor5[is.na(cna$maq_menor5)] <- 0
cna$maq_mayor5[is.na(cna$maq_mayor5)] <- 0

cna <- cna %>% mutate(tiene_maq = ifelse(existe_maq + tipo_maq + maq_menor5 + maq_mayor5 > 0, 1, 0))

# UPAs con maquinaria vs sin maquinaria
maq <- cna %>% distinct(cod_vereda, encuesta, .keep_all = T) %>% dplyr::filter(tiene_maq == 1)
maq <- nrow(maq)
upas <- cna %>% distinct(encuesta)
upas <- nrow(upas)

# 17% de las UPA que tenemos (limpias) reportan maquinaria
100*(maq/upas)

# Eliminar UPAs sin maquinaria: quedan 314,678 UPAs
cna_maq <- cna %>% dplyr::filter(tiene_maq == 1)
test <- cna_maq %>% distinct(encuesta)

# Exportar datos
saveRDS(cna_maq, glue("{datos}/CNA/base_cna_maq.rds"))
saveRDS(cna, glue("{datos}/CNA/base_cna_all_maq.rds"))

# Exportar lista de UPAs de interes
lista_cna <- cna %>% 
  distinct(cod_dpto, cod_mpio, cod_vereda, encuesta) %>%
  dplyr::rename(p_depto = cod_dpto, p_munic = cod_mpio)

haven::write_dta(lista_cna, glue("{datos}/CNA/limpieza_censo/lista_codigos_upa_cna.dta"))

# Upas con cultivos
cul <- cna_maq %>% drop_na(tipo_cul_lote) %>% distinct(encuesta)

#--------------------------#
# A. Guardar muestra ----
#--------------------------#

# 10% de la base completa
cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds"))
rows <- round(nrow(cna)/10)
cna_sample <- cna[1:rows,]
saveRDS(cna_sample, glue("{datos}/CNA/base_cna_all_maq_sample.rds"))
