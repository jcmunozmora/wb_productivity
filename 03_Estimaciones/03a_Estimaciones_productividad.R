#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 23 nov, 2021
# Medicion de la productividad a nivel de UPA  
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
# 0. Opciones graficas ----
#-------------------------------------------------------#

# Tamanos graficas (ancho, alto, texto, resolucion)
w <- 4.5*2.5
h <- 3.2*2.5
text <- 14
d <- 900
s <- 1.2
a_line <- 0.6
a_dot <- 0.7

# Opciones tema
legend_pos <- "bottom"
label_ang.x <- 45
label_h.x <- 1
label_v.x <- 1
label_ang.y <- 0
label_h.y <- 1
label_v.y <- 1
legend_title <- element_blank()
plot_title <- element_blank()
theme_style <- theme_classic(base_size = text*1.5)
text_color <- "black"

# Tema modificable
custom_theme <- list(
  theme_style,
  theme(legend.position = legend_pos, legend.title = legend_title,
        plot.title = plot_title,
        axis.title.x = element_text(colour = text_color),
        axis.title.y = element_text(colour = text_color),
        axis.text.x = element_text(angle = label_ang.x, vjust = label_h.x, hjust = label_v.x, colour = text_color, size = text*0.8),
        axis.text.y = element_text(angle = label_ang.y, vjust = label_h.y, hjust = label_v.y, colour = text_color))
)

theme <- list(
  theme_classic(base_size = text*1.5),
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()))

#-------------------------------------------------------#
# 1. Organizar datos ----
#-------------------------------------------------------#

# Abrimos base CNA
cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds")) %>% 
  dplyr::select(cod_vereda, encuesta, tipo_cul_lote, area_agricola, area_cosechada,
                jornales, ing_cosecha, tiene_maq, tipo_maq, maq_menor5, maq_mayor5) %>%
  dplyr::rename(cod_cultivo = tipo_cul_lote) %>%
  drop_na(cod_cultivo) %>%
  distinct() 

#--------------------------#
# A. Agregar a nivel de UPA ----
#--------------------------#

# Agregamos base cultivos a nivel de UPA
cna_upa <- cna %>%
  distinct(cod_vereda, encuesta, area_agricola, area_cosechada, jornales, .keep_all = T) %>%
  group_by(cod_vereda, encuesta, area_agricola, jornales) %>%
  summarise(ing_cosecha = sum(ing_cosecha, na.rm = T)) %>%
  ungroup()

# Puede haber observaciones repetidas por el tipo de maquinaria
# Tratamos estas variables aparte para no tener duplicados por cultivo
cna_upa_maq <- cna %>%
  distinct(cod_vereda, encuesta, tiene_maq, tipo_maq, maq_menor5, maq_mayor5) %>%
  group_by(cod_vereda, encuesta, tiene_maq) %>%
  summarise(maq_menor5 = sum(maq_menor5, na.rm = T),
            maq_mayor5 = sum(maq_mayor5, na.rm = T)) %>%
  ungroup()

# Unimos informacion de area, labor, ingresos con maquinaria
cna_all <- cna_upa %>% left_join(cna_upa_maq, by = c("cod_vereda", "encuesta"))

#--------------------------#
# B. Potencial y maquinaria ----
#--------------------------#

# Abrimos base de potencial productivo  y maquinaria de Hamman (2018)
maquinaria <- haven::read_dta(glue("01_Datos_originales/BR/datos/output/capital_valuation.dta")) %>%
  haven::zap_labels() %>% haven::zap_formats() %>%
  dplyr::select(encuesta, kmin_agro) %>%
  dplyr::filter(encuesta %in% cna_all$encuesta) %>%
  dplyr::filter(encuesta != "999999999") %>%
  distinct()

# Codigos de UPA de potencial estan numericos, los pegamos con su string
lista_upa <- readRDS(glue("{datos}/CNA/limpieza_censo/lista_codigos_upa.rds"))

potencial <- haven::read_dta(glue("01_Datos_originales/BR/datos/input/potential_yield_farm.dta")) %>%
  haven::zap_labels() %>% haven::zap_formats() %>% 
  left_join(lista_upa, by = "encuesta") %>%
  dplyr::select(encuesta_s, mpoty) %>% 
  dplyr::rename(encuesta = encuesta_s, potencial = mpoty) %>%
  dplyr::filter(encuesta %in% cna_all$encuesta) %>%
  dplyr::filter(encuesta != "999999999") %>%
  distinct()

cna_maq <- cna_all %>% 
  dplyr::left_join(maquinaria, by = "encuesta") %>%
  dplyr::left_join(potencial, by = "encuesta")

test <- maquinaria %>% distinct(encuesta)

rm(potencial, maquinaria, cna, lista_upa, cna_upa_maq)

#--------------------------#
# C. Variables climaticas ----
#--------------------------#

# Abrir temperatura y precipitaciones promedio de veredas (2013)
data_clima <- readRDS(glue("{datos}/base_clima_veredas_2013.rds")) %>% 
  distinct(cod_vereda, .keep_all = T)

# Unimos datos de UPA con variables climaticas: hay 7695 UPA que no tienen informacion climatica
# Son UPA que quedan en los limites de Colombia y no caen dentro del raster 
cna_maq <- cna_maq %>% left_join(data_clima, by = "cod_vereda")

rm(cna, cna_upa, cna_upa_maq, data_clima)

# Guardamos base a nivel de UPA
saveRDS(cna_maq, glue("{datos}/CNA/base_cna_upa.rds"))

#--------------------------#
# D. Imputacion capital ----
#--------------------------#

# crop_max_area = area sembrada mas grande de la upa
# dummy_monocultivo = area_sembrada de un cultivo es > 80% area sembrada
# felm(log(kmin_agro) ~ log(area_upa) + dummy_monocultivo + trab_perm +
#        jorn_adicional + dummy_ganado + dummy_poulty + dummy_pez + dummy_pesca | cod_dpto + crop_maxarea)

#-------------------------------------------------------#
# 2. Estimaciones 1 ----
# Siguiendo a Aragon, Restuccia y Rud (2021)
#-------------------------------------------------------#

# Parametros Aragon (2021)
alpha <- 0.526
gamma <- 0.708

# Abrir base UPAs: area en m^2, calculamos logaritmos
# A todas las variables sumarle 0.01
cna_all <- readRDS(glue("{datos}/CNA/base_cna_upa.rds")) %>% 
  dplyr::filter(area_agricola > 0) %>%
  mutate(log_area = log(area_agricola),
         log_ingreso = ing_cosecha + 0.001,
         log_ingreso = log(log_ingreso),
         log_labor = jornales + 0.001,
         log_labor = log(log_labor))

# Estimaciones Cobb Douglas
prod <- cna_all %>%
  mutate(land = alpha*gamma*log_area,
         labor = (1-alpha)*gamma*log_labor,
         ln_si = log_ingreso - land - labor - rain - temp)

# Distribucion de la productividad 
ggplot(prod, aes(x = ln_si)) +
  geom_histogram(fill = "#6FBDD1", color = "white") +
  labs(x = "\nLogaritmo de la productividad agrícola - Ln(S_i)", y = "Frecuencia\n") +
  theme
ggsave(glue("{graficas}/estimacion_prod_upa_aragon.jpeg"), height = h, width = w, dpi = d)

#-------------------------------------------------------#
# 3. Estimaciones 2 ----
# Siguiendo a Hamman (2018) 
#-------------------------------------------------------#

# Parametros Hamman (2018), tomados de Adamopoulos (2017), Restuccia (2015), Fuglie (2010)
s <- 1
theta_l <- 0.36
theta_k <- 0.18
theta_x <- 0.07

# Abrir base UPAs y pasar variable a per capita
cna_all <- readRDS(glue("{datos}/CNA/base_cna_upa.rds")) %>% 
  dplyr::select(-c(maq_menor5, maq_mayor5, cod_vereda_dane, year)) %>%
  dplyr::filter(area_agricola >= 0.01 & kmin_agro > 0 & ing_cosecha > 0) %>%
  mutate(area_agricola = area_agricola/10000,
         area_agricola_j = area_agricola/jornales,
         kmin_agro_j = kmin_agro/jornales,
         ing_cosecha_j = ing_cosecha/jornales,
         potencial_j = potencial/jornales,
         ln_area_agricola_j = log(area_agricola))

# Estimar productividad
prod_hamman <- cna_all %>%
  mutate(si = ing_cosecha_j/((kmin_agro_j^theta_k)*((potencial_j*jornales)^theta_l)),
         ln_si = log(si))
  
summary(prod_hamman$area_agricola)

# Exportar base productividad
saveRDS(prod_hamman, glue("{datos}/Productividad/base_cna_productividad_upa.rds"))

# Distribucion de la productividad
ggplot(prod_hamman, aes(x = log(area_agricola), y = ln_si)) +
  geom_point()

ggplot(prod_hamman, aes(x = ln_si)) +
  geom_histogram(fill = "#6FBDD1", color = "white") +
  labs(x = "\nLogaritmo de la productividad agrícola - Ln(S_i)", y = "Frecuencia\n") +
  theme
ggsave(glue("{graficas}/estimacion_prod_upa_hamman.jpeg"), height = h, width = w, dpi = d)

