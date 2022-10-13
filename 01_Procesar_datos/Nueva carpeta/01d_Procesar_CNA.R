#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 13 feb, 2022
# Procesamiento de datos Censo Nacional Agropecuario
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
packageList<-c("tidyverse", "glue", "readr", "readxl", "janitor")
lapply(packageList,require,character.only=TRUE)

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales/CNA/Total_nacional(csv)"
datos <- "02_Datos/CNA"
graficas <- "03_Graficas"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Procesar datos originales ----
#-------------------------------------------------------#

# #--------------------------#
# # A. Organizar modulos ----
# #--------------------------#
# 
# # Elegimos variables de interes por modulo y guardamos
# # General (UPAs)
# cna_gen <- read_csv(glue("{datos_ori}/S01_15(Unidad_productora).csv")) %>% 
#   dplyr::select(
#     # Identificadoras
#     TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA, P_S5PAUTOS, PRED_ETNICA,
#     # Capital
#     P_S7P84F, P_S10P121,
#     # Trabajo
#     P_S11P138, P_S11P138A, P_S11P138B, P_S11P139, P_S11P139A, P_S11P139B, P_S11P140,
#     # Tierra
#     P_S12P142, P_S12P148, P_S12P150A,
#     # Tierra pecuaria (pastos y forrajes)
#     P_S6P66, P_S6P68, 
#     # Produccion
#     P_S7P85B,
#     # No agropecuarias
#     TIPO_UC, P_S12P148, P_S12P149,
#     # Maquinaria
#     P_S9P117,
#     # Agricolas
#     P_S6P43, P_S6P44, P_S3P9, P_S3P10,
#     # Ganado bovino
#     P_S7P78, P_S7P82, P_S7P83A, P_S7P84A, 
#     # Porcinos en confinamiento
#     P_S7P86, P_S7P89A,P_S7P89B, P_S7P89D,
#     # Avicultura
#     P_S7P90, P_S7P92A, P_S7P92B, P_S7P93A, P_S7P93B,
#     # Acuicultura
#     P_S7P94,
#     # Bufalos, equinos, ovinos y caprinos
#     P_S7P101,P_S7P102A, P_S7P102B, P_S7P102C, P_S7P102D, 
#     P_S7P102E, P_S7P102F,P_S7P102G, P_S7P102H, P_S7P102I, 
#     P_S7P102J, P_S7P102K, P_S7P102L,
#     # Otros animales
#     P_S7P105) %>% 
#   clean_names()
# 
# saveRDS(cna_gen, glue("{datos}/CNA/base_upas.rds"))
# 
# # Cultivos
# cna_cul <- read_csv(glue("{datos_ori}/S06A(Cultivos).csv")) %>%
#   dplyr::select(
#     # Identificadoras
#     TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA,
#     # Cultivos
#     P_S6P46, AREA_SEMBRADA, AREA_COSECHADA, P_S6P57A, P_S6P59_UNIF 
#     ) %>% clean_names()
# 
# saveRDS(cna_cul, glue("{datos}/CNA/base_cultivos.rds"))
# 
# # Construcciones agropecuarias
# cna_cons <- read_csv(glue("{datos_ori}/S10(Construcciones_uso_agropecuario).csv")) %>%
#   dplyr::select(
#     # Identificadoras
#     TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA,
#     # Construcciones
#     P_S10P122, P_S10P123
#   ) %>% clean_names()
# 
# saveRDS(cna_cons, glue("{datos}/CNA/base_construcciones_agro.rds"))
# 
# # Maquinaria agropecuaria
# cna_maq <- read_csv(glue("{datos_ori}/S09(Maquinaria_uso_agropecuario).csv")) %>%
#   dplyr::select(
#     # Identificadoras
#     TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA, UC_UO,
#     # Capital
#     P_S9P118, P_S9P119, P_S9P120,
#     ) %>% clean_names()
# 
# saveRDS(cna_maq, glue("{datos}/CNA/base_maquinaria_agro.rds"))
# writexl::write_xlsx(cna_maq, glue("{datos}/CNA/base_maquinaria_agro.xlsx"))
# haven::write_dta(cna_maq, glue("{datos}/CNA/base_maquinaria_agro.dta"))
# 
# # Pesca
# cna_pesca <- read_csv(glue("{datos_ori}/S08(Pesca_artesanal).csv")) %>%
#   dplyr::select(
#     # Identificadoras
#     TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA,
#     # Pesca de agua dulce y salada
#     everything()
#   ) %>% clean_names() %>% dplyr::select(-c(pais, uc_uo))
# saveRDS(cna_pesca, glue("{datos}/CNA/base_pesca_artesanal.rds"))
# 
# # Acuicultura
# cna_acui <- read_csv(glue("{datos_ori}/S07D(Acuicultura).csv")) %>%
#   dplyr::select(
#     # Identificadoras
#     TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA,
#     # Acuicultura
#     everything()
#   ) %>% clean_names() %>% dplyr::select(-c(pais, uc_uo))
# saveRDS(cna_acui, glue("{datos}/CNA/base_acuicultura.rds"))
# 
# # Dispersos y viveros
# cna_dis <- read_csv(glue("{datos_ori}/S06BD(Frutales_y_forestales_dispersos,_y_viveros).csv")) %>%
# dplyr::select(
#   # Identificadoras
#   TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA,
#   # Acuicultura
#   everything()
# ) %>% clean_names() %>% dplyr::select(-c(pais, uc_uo))
# saveRDS(cna_dis, glue("{datos}/CNA/base_frutales_dispersos.rds"))
# 
# # No agropecuaria
# cna_noagro <- read_csv(glue("{datos_ori}/S14(Actividad_no_agropecuaria).csv")) %>%
#   dplyr::select(
#     # Identificadoras
#     TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA,
#     # Act no agro
#     P_S14P157_SP1
#     ) %>% clean_names()
# 
# saveRDS(cna_noagro, glue("{datos}/CNA/base_no_agropecuarias.rds"))

#--------------------------#
# B. Unir modulos ----
#--------------------------#

# Abrimos modulo general (UPAs) y pegamos los demas
# Solo nos quedamos con UPAs (eliminamos UPNAs)
cna_gen <- readRDS(glue("{datos}/CNA/base_upas.rds")) 
cna_cul <- readRDS(glue("{datos}/CNA/base_cultivos.rds")) 
cna_cons <- readRDS(glue("{datos}/CNA/base_construcciones_agro.rds"))
cna_maq <- readRDS(glue("{datos}/CNA/base_maquinaria_agro.rds")) 

# Organizamos nombres variables
cna <- cna_gen %>%
  left_join(cna_cul, by = c("p_depto", "p_munic", "encuesta", "cod_vereda")) %>%
  left_join(cna_cons, by = c("p_depto", "p_munic", "encuesta", "cod_vereda")) %>%
  left_join(cna_maq, by = c("p_depto", "p_munic", "encuesta", "cod_vereda")) %>%
  select(-c(starts_with("tipo_reg"), p_s12p150a)) %>%
  rename(cod_dpto = p_depto, cod_mpio = p_munic,
         # Agricola 
         tipo_cul_lote = p_s6p46, tipo_cons_agropecuario = p_s10p122, cant_cosecha = p_s6p57a, 
         rend_cosecha = p_s6p59_unif, tiene_tuvo_cultivos = p_s6p43, tiene_tuvo_plantaciones = p_s6p44, 
         siembra_consumo = p_s3p9, siembra_venta = p_s3p10,
         # Ganaderia
         tiene_bov_12 = p_s7p78, tiene_bov_hoy = p_s7p82, num_bov_macho = p_s7p83a, 
         num_bov_hembra = p_s7p84a, hembras_bov_ord = p_s7p84f, hay_cons_pec = p_s10p121, 
         cant_leche = p_s7p85b,
         # Porcinos en confinamiento
         tenido_cerdos = p_s7p86, num_cer_m_rep = p_s7p89a, num_cer_h = p_s7p89b,
         num_cer_des = p_s7p89d,
         # Avicultura
         criado_gall_poll = p_s7p90, num_av_e_piso = p_s7p92a, num_av_s_piso = p_s7p92b,
         num_av_e_jaula = p_s7p93a, num_av_s_jaula = p_s7p93b,
         # Acuicultura
         criado_peces = p_s7p94,
         # Bufalos, equinos, ovinos y caprinos
         tiene_b_e_o_c = p_s7p101, num_buf_m = p_s7p102a, num_buf_h = p_s7p102b, 
         num_caballos = p_s7p102c, num_yeguas = p_s7p102d, 
         num_mulos = p_s7p102e, num_mulas = p_s7p102f, num_burros = p_s7p102g, 
         num_burras = p_s7p102h, num_cabros = p_s7p102i, num_cabras = p_s7p102j,
         num_ovejos = p_s7p102k, num_ovejas = p_s7p102l,
         # Otros animales
         tiene_otras_esps = p_s7p105,
         # Areas
         area_inf_noagro = p_s12p148, area_otros_usos = p_s12p149, area_agropecuario = p_s12p142, 
         area_pasto_nat = p_s6p66, area_pasto_sem = p_s6p68, area_upa = p_s5pautos,
         # Trabajo
         total_trab = p_s11p138, total_trab_h = p_s11p138a, total_trab_m = p_s11p138b, total_jorn = p_s11p140,
         total_trab_hogar = p_s11p139, total_trab_hogar_h = p_s11p139a, total_trab_hogar_m = p_s11p139b,
         # Maquinaria
         existe_maq = p_s9p117, tipo_maq = p_s9p118, maq_menor5 = p_s9p119, maq_mayor5 = p_s9p120) %>%
  mutate(cod_dpto = as.numeric(cod_dpto), cod_mpio = as.numeric(cod_mpio), cod_vereda = as.numeric(cod_vereda))

# Guardamos una base agricola y otra pecuaria
# Por temas de tamano no es posible tener todo en una base grande
cna_mod <- cna %>% 
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, tipo_uc, area_upa, pred_etnica, tipo_cul_lote,
                starts_with("area"), cant_cosecha, rend_cosecha, hembras_bov_ord, cant_leche,
                tipo_cons_agropecuario, tipo_maq, existe_maq, maq_menor5, maq_mayor5, hay_cons_pec)

cna_pec <- cna %>%
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, starts_with("tiene"), starts_with("num"), 
                starts_with("siembra"), tenido_cerdos, starts_with("criado")) %>%
  distinct(cod_dpto, cod_mpio, encuesta, cod_vereda, .keep_all = T)

rm(cna_gen, cna_cons, cna_cul, cna_maq, cna_mod, cna_pec)

# Guardamos cna 
saveRDS(cna_mod, glue("{datos}/CNA/base_cna_modulos.rds"))
saveRDS(cna_pec, glue("{datos}/CNA/base_cna_pecuario.rds"))

# Match perfecto entre general y todas las bases 
# cna <- cna_cul %>%
#   anti_join(cna_gen, by = c("p_depto", "p_munic", "encuesta", "cod_vereda"))

