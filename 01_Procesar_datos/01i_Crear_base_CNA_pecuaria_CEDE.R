#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 22 feb, 2022
# Procesamiento de datos Censo Nacional Agropecuario (base_cna_all_maq) y
# Panel CEDE
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readr, readxl, janitor)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales/"
datos <- "02_Datos"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Limpiar datos ----
#-------------------------------------------------------#

#--------------------------#
# A. CNA ----
#--------------------------#
#*Cambiar base_cna_all_maq_sample.rds por base_cna_all_maq.rds
cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds")) %>%
#cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq_sample.rds")) %>% 
  dplyr::select(
    # Identificadoras
    cod_dpto, cod_mpio, cod_vereda, encuesta, tipo_uc,
    #areas pecuarias
    area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem,
    #cultivos
    tipo_cul_lote,
    #leche
    hembras_bov_ord, cant_leche, 
    #construccion y maquinaria
    tipo_cons_agropecuario, tipo_maq, existe_maq,maq_menor5, maq_mayor5, hay_cons_pec, 
    #trabajadores
    total_trab, total_trab_h, total_trab_m, total_trab_hogar, total_trab_hogar_h, 
    total_trab_hogar_m, total_jorn, jornales
  ) %>% clean_names()

#--------------------------#
# B. CEDE ----
#--------------------------#

cede <- read_excel(glue("{datos_ori}CEDE/PANEL_CARACTERISTICAS_GENERALES(2019).xlsx"), sheet = "Hoja1" , col_names = T) %>%
  dplyr::select(
    # Identificador
    codmpio,
    # ano
    ano,
    #poblacion
    pobl_rur, pobl_urb, pobl_tot,
    #distancia a mercado
    dismdo, distancia_mercado
  ) %>% 
  clean_names() %>%
  filter(ano == 2013)

#----------------------------------#
# C. DATOS PRODUCTOR PECUARIO ----
#----------------------------------#

# ElegiR variables de interes de orientacion ganadera

#*Cambiar S01_15_Unidad_productora_.csv por S01_15(Unidad_productora).csv
cna_gen <- read_csv(glue("{datos_ori}CNA/Total_nacional(csv)/S01_15(Unidad_productora).csv")) %>%
#cna_gen <- read_csv(glue("{datos_ori}CNA/Total_nacional(csv)/S01_15_Unidad_productora_.csv")) %>% 
  dplyr::select(
    # Identificadoras
    TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA, 
    #Agricolas
    P_S6P43, P_S6P44, P_S3P9, P_S3P10,
    #Ganado bovino
    P_S7P78, P_S7P82, P_S7P83A, P_S7P84A, 
    #Porcinos en confinamiento
    P_S7P86, P_S7P89A,P_S7P89B, P_S7P89D,
    #Avicultura
    P_S7P90, P_S7P92A, P_S7P92B, P_S7P93A, P_S7P93B,
    #Acuicultura
    P_S7P94,
    #Bufalos, equinos, ovinos y caprinos
    P_S7P101,P_S7P102A, P_S7P102B, P_S7P102C, P_S7P102D, 
    P_S7P102E, P_S7P102F,P_S7P102G, P_S7P102H, P_S7P102I, 
    P_S7P102J, P_S7P102K, P_S7P102L,
    #Otros animales
    P_S7P105,
    #Orientación lechera
    P_S7P79_SP2) %>% 
  clean_names()

head(cna_gen)

# Organizar nombres variables
cna_pecuario <- cna_gen %>%
  rename(cod_dpto = p_depto, cod_mpio = p_munic,
         #Agricolas
         tiene_tuvo_cultivos = p_s6p43, tiene_tuvo_plantaciones = p_s6p44,
         siembra_consumo = p_s3p9, siembra_venta = p_s3p10,
         # Ganaderia
         tiene_bov_12 = p_s7p78, tiene_bov_hoy = p_s7p82, num_bov_macho = p_s7p83a, 
         num_bov_hembra = p_s7p84a,
         #Porcinos en confinamiento
         tenido_cerdos = p_s7p86, num_cer_m_rep = p_s7p89a, num_cer_h = p_s7p89b,
         num_cer_des = p_s7p89d,
         #Avicultura
         criado_gall_poll = p_s7p90, num_av_e_piso = p_s7p92a, num_av_s_piso = p_s7p92b,
         num_av_e_jaula = p_s7p93a, num_av_s_jaula = p_s7p93b,
         #Acuicultura
         criado_peces = p_s7p94,
         #Bufalos, equinos, ovinos y caprinos
         tiene_b_e_o_c = p_s7p101, num_buf_m = p_s7p102a, num_buf_h = p_s7p102b, 
         num_caballos = p_s7p102c, num_yeguas = p_s7p102d, 
         num_mulos = p_s7p102e, num_mulas = p_s7p102f, num_burros = p_s7p102g, 
         num_burras = p_s7p102h, num_cabros = p_s7p102i, num_cabras = p_s7p102j,
         num_ovejos = p_s7p102k, num_ovejas = p_s7p102l,
         #Otros animales
         tiene_otras_esps = p_s7p105,
         #Orientacion lechera
         act_prin_leche = p_s7p79_sp2) %>%
  mutate(cod_dpto = as.numeric(cod_dpto), cod_mpio = as.numeric(cod_mpio), cod_vereda = as.numeric(cod_vereda)) %>%
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, starts_with("tiene"), starts_with("num"), 
                starts_with("siembra"), tenido_cerdos, starts_with("criado"), act_prin_leche) %>%
  distinct(cod_dpto, cod_mpio, encuesta, cod_vereda, .keep_all = T)

head(cna_pecuario)
rm(cna_gen)

#--------------------------------------#
# D. DATOS CARACTERISTICAS ----
#--------------------------------------#

caracteristicas <- readRDS(glue("{datos}/CNA/caracteristicas_upa.rds")) %>% 
  dplyr::select(
    #Identificador
    cod_mpio,
    #Trasferencias SGP
    sgp_pc,
    #Indice de desempeno institucional
    ind_desempeno_int,
    #Valor agregado
    valor_agregado,
    #indice diversidad economica
    ind_div_econ) %>% 
  distinct(cod_mpio, .keep_all = T)

#--------------------------------------#
# E. UNIR CNA, CEDE, CARACTERISTICAS y CNA_PECUARIO ----
#--------------------------------------#

cna_cede <- cna %>%
  left_join(cede, by = c("cod_mpio" = "codmpio")) %>%
  left_join(caracteristicas, by= c("cod_mpio")) %>% 
  left_join(cna_pecuario, by = c("cod_dpto", "cod_mpio", "encuesta", "cod_vereda")) %>% 
  mutate(tiene_animales = if_else((tiene_bov_12 == 1 | tiene_bov_hoy == 1 | tenido_cerdos == 1 | criado_gall_poll == 1 |
                                     criado_peces == 1 | tiene_b_e_o_c == 1 | tiene_otras_esps == 1), 1, 0),
         pecuaria = if_else(tiene_animales == 1 & siembra_consumo == 2 & siembra_venta == 2, 1, 0))

saveRDS(cna_cede, glue("{datos}/CNA/cna_cede.rds"))

#------------------------------------------------------#
# E.  Guardar base de datos de productores de leche----
#------------------------------------------------------#

cna_cede <- readRDS(glue("{datos}/CNA/cna_cede.rds"))
cna_cede_lecheros <- cna_cede %>% dplyr::filter(cant_leche > 0)

# Exportar datos
dir <- getwd()
dir <- gsub("BM_productividad", "BM_Efficiency", dir)
saveRDS(cna_cede_lecheros, glue("{dir}/00_Rawdata/cna_cede_lecheros.rds"))
saveRDS(cna_cede_lecheros, glue("{datos}/CNA/cna_cede_lecheros.rds"))

#UPA pecuarias con codigos de cultivo
test <- cna_cede %>% 
       select(starts_with("tiene"), starts_with("siembra"), starts_with("criado"), tenido_cerdos, tipo_cul_lote, tiene_animales,
              pecuaria) %>% filter(pecuaria == 1) 
sum(is.na(test$tipo_cul_lote))


# cna_cede <- readRDS(glue("{datos}/CNA/cna_cede.rds"))

