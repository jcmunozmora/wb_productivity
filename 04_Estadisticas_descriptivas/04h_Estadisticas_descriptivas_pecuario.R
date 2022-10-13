#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 21 feb, 2022
# Estadisticas descriptivas variables CNA pecuarias y CEDE 
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readr, janitor, stargazer, ggplot2, skimr, writexl)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos <- "02_Datos"
tablas <- "04_Tablas"
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
legend_pos_t <- "right"
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
        legend.position = legend_pos_t,
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()))

# Color
cr= "#6FBDD1"
cr1= "#5BCFD2"
cr2= "#62E0C4"

# Tamano del punto
s_dot=3 

#-------------------------------------------------------#
# 1. Estadisticas descriptivas ----
#-------------------------------------------------------#

#Abrir base de datos
# Areas de metros a hectareas (1ha = 10,000m)
cna_cede <- readRDS(glue("{datos}/CNA/cna_cede.rds")) %>%
  mutate(area_upa = area_upa/10000, area_agropecuario = area_agropecuario/10000, 
         area_pasto_nat = area_pasto_nat/10000, area_pasto_sem = area_pasto_sem/10000,
         cant_leche = cant_leche/1000)
  
#--------------------------#
# A. Estadisticas descriptivas UPA agropecuarias ----
#--------------------------#

#filtrar variables de interes 
cna_cede_1 <- cna_cede %>%
  group_by(cod_vereda, encuesta) %>%
  distinct(area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem, hembras_bov_ord,
           cant_leche, jornales, pobl_tot, pobl_rur, 
           pobl_urb, dismdo, distancia_mercado, num_bov_macho, num_bov_hembra, num_cer_m_rep,
           num_cer_h, num_cer_des, num_av_e_piso, num_av_e_jaula,
           sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ) %>% 
  ungroup() %>%
  dplyr::select(area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem, hembras_bov_ord,
                cant_leche, jornales, pobl_tot, pobl_rur, 
                pobl_urb, dismdo, distancia_mercado, num_bov_macho, num_bov_hembra, num_cer_m_rep,
                num_cer_h, num_cer_des, num_av_e_piso, num_av_e_jaula,
                sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ) %>%
  as.data.frame() %>% 
  relocate(area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem, num_bov_macho,
           num_bov_hembra, hembras_bov_ord, cant_leche, num_cer_m_rep,num_cer_h,
           num_cer_des, num_av_e_piso, num_av_e_jaula, jornales, pobl_tot, pobl_rur, 
           pobl_urb, dismdo, distancia_mercado,
           sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ)

labels <- c("Area Upa (ha)", "Area Agropecuaria (ha)", "Area Pastos nat (ha)", 
            "Area Pastos sem (ha)","Machos bovinos", "Hembras bovinas", 
            "Hembras bovinas ordeño", "Leche (miles de lts)", "Machos reproductores Porcinos",
            "Hembras Porcinas", "Porcinos de descarte", "Aves entrada g piso", 
            "Aves entrada g jaula", "Jornales", "Poblacion Total", "Poblacion Rural", 
            "Poblacion Urbana", "Distancia Mercado Principal (km)", "Distancia Mercado Cercano (km)",
            "Transferencias SGP percapita (millones)", "Indice desempeño institucional", "Valor agregado (millones)",
            "Indice diversidad economica")

# Tabla descriptivas
stargazer::stargazer(cna_cede_1, summary.stat = c("mean", "sd", "min", "max", "n"),
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = labels,
                     title = "Estadisticas Descriptivas UPA Agropecuarias",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_cede_descriptive_stats_1.doc"))

rm(cna_cede_1)

#--------------------------#
# B. Estadisticas descriptivas UPA Pecurias ----
#--------------------------#

cna_cede_2 <- cna_cede %>%
  group_by(cod_vereda, encuesta) %>%
  filter(pecuaria == 1)%>%
  distinct(area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem, hembras_bov_ord,
           cant_leche, jornales, pobl_tot, pobl_rur, 
           pobl_urb, dismdo, distancia_mercado, num_bov_macho, num_bov_hembra, num_cer_m_rep,
           num_cer_h, num_cer_des, num_av_e_piso, num_av_e_jaula,
           sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ) %>% 
  ungroup() %>%
  dplyr::select(area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem, hembras_bov_ord,
                cant_leche, jornales, pobl_tot, pobl_rur, 
                pobl_urb, dismdo, distancia_mercado, num_bov_macho, num_bov_hembra, num_cer_m_rep,
                num_cer_h, num_cer_des, num_av_e_piso, num_av_e_jaula,
                sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ) %>%
  as.data.frame() %>% 
  relocate(area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem, num_bov_macho,
           num_bov_hembra, hembras_bov_ord, cant_leche, num_cer_m_rep,num_cer_h,
           num_cer_des, num_av_e_piso, num_av_e_jaula, jornales, pobl_tot, pobl_rur, 
           pobl_urb, dismdo, distancia_mercado,
           sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ)

labels <- c("Area Upa (ha)", "Area Agropecuaria (ha)", "Area Pastos nat (ha)", 
            "Area Pastos sem (ha)","Machos bovinos", "Hembras bovinas", 
            "Hembras bovinas ordeño", "Leche (miles de lts)", "Machos reproductores Porcinos",
            "Hembras Porcinas", "Porcinos de descarte", "Aves entrada g piso", 
            "Aves entrada g jaula", "Jornales", "Poblacion Total", "Poblacion Rural", 
            "Poblacion Urbana", "Distancia Mercado Principal (km)", "Distancia Mercado Cercano (km)",
            "Transferencias SGP percapita (millones)", "Indice desempeño institucional", "Valor agregado (millones)",
            "Indice diversidad economica")

# Tabla descriptivas
stargazer::stargazer(cna_cede_2, summary.stat = c("mean", "sd", "min", "max", "n"),
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = labels,
                     title = "Estadisticas Descriptivas UPA Pecuarias",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_cede_descriptive_stats_2.doc"))
rm(cna_cede_2)

#--------------------------#
# C. Estadisticas descriptivas UPA pecuaria con bovinos ----
#--------------------------#

cna_cede_3 <- cna_cede %>%
  group_by(cod_vereda, encuesta) %>%
  filter(pecuaria == 1 & (tiene_bov_12 == 1 | tiene_bov_hoy == 1))%>%
  distinct(area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem, hembras_bov_ord,
           cant_leche, jornales, pobl_tot, pobl_rur, 
           pobl_urb, dismdo, distancia_mercado, num_bov_macho, num_bov_hembra, num_cer_m_rep,
           num_cer_h, num_cer_des, num_av_e_piso, num_av_e_jaula,
           sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ) %>% 
  ungroup() %>%
  dplyr::select(area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem, hembras_bov_ord,
                cant_leche, jornales, pobl_tot, pobl_rur, 
                pobl_urb, dismdo, distancia_mercado, num_bov_macho, num_bov_hembra, num_cer_m_rep,
                num_cer_h, num_cer_des, num_av_e_piso, num_av_e_jaula,
                sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ) %>%
  as.data.frame() %>% 
  relocate(area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem, num_bov_macho,
           num_bov_hembra, hembras_bov_ord, cant_leche, num_cer_m_rep,num_cer_h,
           num_cer_des, num_av_e_piso, num_av_e_jaula, jornales, pobl_tot, pobl_rur, 
           pobl_urb, dismdo, distancia_mercado,
           sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ)

labels <- c("Area Upa (ha)", "Area Agropecuaria (ha)", "Area Pastos nat (ha)", 
            "Area Pastos sem (ha)","Machos bovinos", "Hembras bovinas", 
            "Hembras bovinas ordeño", "Leche (miles de lts)", "Machos reproductores Porcinos",
            "Hembras Porcinas", "Porcinos de descarte", "Aves entrada g piso", 
            "Aves entrada g jaula", "Jornales", "Poblacion Total", "Poblacion Rural", 
            "Poblacion Urbana", "Distancia Mercado Principal (km)", "Distancia Mercado Cercano (km)",
            "Transferencias SGP percapita (millones)", "Indice desempeño institucional", "Valor agregado (millones)",
            "Indice diversidad economica")

# Tabla descriptivas
stargazer::stargazer(cna_cede_3, summary.stat = c("mean", "sd", "min", "max", "n"),
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = labels,
                     title = "Estadisticas Descriptivas UPA Pecuarias Bovinos",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_cede_descriptive_stats_3.doc"))
rm(cna_cede_3)

#----------------------------------------------------------------#
# D. Estadisticas descriptivas UPA lecheras    ----
#----------------------------------------------------------------#

cna_cede_4 <- cna_cede %>%
  group_by(cod_vereda, encuesta) %>%
  filter(cant_leche > 0)%>%
  distinct(area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem, hembras_bov_ord,
           cant_leche, jornales, pobl_tot, pobl_rur, 
           pobl_urb, dismdo, distancia_mercado, num_bov_macho, num_bov_hembra, num_cer_m_rep,
           num_cer_h, num_cer_des, num_av_e_piso, num_av_e_jaula,
           sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ) %>% 
  ungroup() %>%
  dplyr::select(area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem, hembras_bov_ord,
                cant_leche, jornales, pobl_tot, pobl_rur, 
                pobl_urb, dismdo, distancia_mercado, num_bov_macho, num_bov_hembra, num_cer_m_rep,
                num_cer_h, num_cer_des, num_av_e_piso, num_av_e_jaula,
                sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ) %>%
  as.data.frame() %>% 
  relocate(area_upa, area_agropecuario, area_pasto_nat, area_pasto_sem, num_bov_macho,
           num_bov_hembra, hembras_bov_ord, cant_leche, num_cer_m_rep,num_cer_h,
           num_cer_des, num_av_e_piso, num_av_e_jaula, jornales, pobl_tot, pobl_rur, 
           pobl_urb, dismdo, distancia_mercado,
           sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ)

labels <- c("Area Upa (m)", "Area Agropecuaria (m)", "Area Pastos nat (m)", 
            "Area Pastos sem (m)","Machos bovinos", "Hembras bovinas", 
            "Hembras bovinas ordeño", "Leche (l)", "Machos reproductores Porcinos",
            "Hembras Porcinas", "Porcinos de descarte", "Aves entrada g piso", 
            "Aves entrada g jaula", "Jornales", "Poblacion Total", "Poblacion Rural", 
            "Poblacion Urbana", "Distancia Mercado Principal (km)", "Distancia Mercado Cercano (km)",
            "Transferencias SGP percapita", "Indice desempeño institucional", "Valor agregado",
            "Indice diversidad economica")

# Tabla descriptivas
stargazer::stargazer(cna_cede_4, summary.stat = c("mean", "sd", "min", "max", "n"),
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = labels,
                     title = "Estadisticas Descriptivas UPA Lecheras",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_cede_descriptive_stats_4.doc"))
rm(cna_cede_4)


#-------------------------------------------------------#
# 2. Graficas   ----
#-------------------------------------------------------#

rm(cna_cede)
.rs.restartR()

#--------------------------#
# A. Grafica de distribucion de las UPA y produccion de leche por rangos de tamano ----
#--------------------------#

# A1. Datos ----

# Unidades originales:
# Areas: metros

# Unidades transformadas
# Areas: hectareas

# Abrir censo y crear variables de interes

# Base all_maq

#*Cambiar base_cna_all_maq_sample.rds por base_cna_all_maq.rds
#cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq_sample.rds"))
cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds"))

# Se crea variable cantidad de tierra y se pasa el area a hectareas -> solo para all_maq
cna <- cna %>% mutate(cat_tierra = NA, area_upa = area_upa*0.0001)

# Se llena la variables cantidad de tierra de acuerdo con los rangos de Hamman
cna$cat_tierra[cna$area_upa < 5] <- "0-5"
cna$cat_tierra[cna$area_upa >=5 & cna$area_upa< 20] <- "5-20"
cna$cat_tierra[cna$area_upa >=20 & cna$area_upa< 28] <- "20-28"
cna$cat_tierra[cna$area_upa >=28 & cna$area_upa< 40] <- "28-40"  
cna$cat_tierra[cna$area_upa >=40 & cna$area_upa< 57] <- "40-57"
cna$cat_tierra[cna$area_upa >=57 & cna$area_upa< 73] <- "57-73"
cna$cat_tierra[cna$area_upa >=73 & cna$area_upa< 89] <- "73-89"
cna$cat_tierra[cna$area_upa >=89 & cna$area_upa< 105] <- "89-105"
cna$cat_tierra[cna$area_upa >=105 & cna$area_upa< 202] <- "105-202"
cna$cat_tierra[cna$area_upa >=202 & cna$area_upa< 405] <- "202-405"
cna$cat_tierra[cna$area_upa >=405 & cna$area_upa< 809] <- "405-809"
cna$cat_tierra[cna$area_upa >=809] <- "809 +"
table(cna$cat_tierra)
sum(is.na(cna$cat_tierra))

# Se calcula la cantidad de upas y produccion de leche de acuerdo al tamaño
tamano <- cna %>%
  distinct(area_upa, .keep_all = T) %>%
  mutate(upa = 1) %>%
  group_by(cat_tierra) %>%
  summarise(upa = sum(upa, na.rm = T), leche = sum(cant_leche, na.rm = T)/1000) %>%
  ungroup() %>%
  mutate(total = sum(upa), part = 100*(upa/total), total_leche = sum(leche),
         part_leche = 100*(leche/total_leche))

tamano$cat_tierra <- factor(tamano$cat_tierra, 
                            levels = c("0-5", "5-20", "20-28", "28-40", 
                                       "40-57", "57-73", "73-89", "89-105",
                                       "105-202", "202-405", "405-809", "809 +"))


# A2.  Grafica distribucion UPA por rango de tamano ----

# Usando definicion de la Federacion Nacional de Cafeteros
# Pequena: 0-5 has, mediana 5-15 has, grande +15 has
tamano <- tamano %>%
  mutate(tipo_upa = ifelse(cat_tierra == "0-5", "Pequeña",
                           ifelse(cat_tierra == "5-20", "Mediana", "Grande")))

tamano$tipo_upa <- factor(tamano$tipo_upa, levels = c("Pequeña", "Mediana", "Grande"))

custom_theme[[2]][["axis.text.x"]][["size"]] <- 15
ggplot(data=tamano, aes(x = cat_tierra, y = part, fill = tipo_upa))+
  geom_bar(stat="identity")+
  geom_text(aes(label= round(part, 2)), vjust=-0.3, size=3.5)+
  scale_fill_manual(values = c(cr2, "#6FBDD1", "#9983B5")) + 
  labs(title = "Distribución de UPAs por rango de tamaño", caption="")+
  ylab("Participación (%)") + 
  xlab("Área (ha)") + 
  custom_theme
# ggsave(glue("{graficas}/distribucion_upa_tamano.pdf"), height = h, width = w, dpi = d)  
ggsave(glue("{graficas}/distribucion_upa_tamano_pecuario.svg"), height = h, width = w, dpi = d)  
custom_theme[[2]][["axis.text.x"]][["size"]] <- text*1.7

# A3.  Grafica produccion de leche por tamano UPA ----
custom_theme[[2]][["axis.text.x"]][["size"]] <- 15
ggplot(data=tamano, aes(x = cat_tierra, y = leche, fill = tipo_upa))+
  geom_bar(stat="identity")+
  geom_text(aes(label= round(leche, 2)), vjust=-0.3, size=3.5)+
  scale_fill_manual(values = c(cr2, "#6FBDD1", "#9983B5")) + 
  labs(title = "Produccion de leche por rango de tamaño UPA", caption="")+
  ylab("Producción de Leche (miles de L)") + 
  xlab("Área (ha)") + 
  custom_theme
ggsave(glue("{graficas}/leche_upa_tamano.svg"), height = h, width = w, dpi = d)  
custom_theme[[2]][["axis.text.x"]][["size"]] <- text*1.7

# A4.  Grafica participacion de leche por tamano UPA ----
custom_theme[[2]][["axis.text.x"]][["size"]] <- 15
custom_theme[[2]][["axis.title.y"]][["size"]] <- 15

ggplot(data=tamano, aes(x = cat_tierra, y = part_leche, fill = tipo_upa))+
  geom_bar(stat="identity")+
  geom_text(aes(label= round(part_leche, 2)), vjust=-0.3, size=3.5)+
  scale_fill_manual(values = c(cr2, "#6FBDD1", "#9983B5")) + 
  labs(title = "Distribución de UPAs por rango de tamaño", caption="")+
  ylab("Participación producción de leche (%)") + 
  xlab("Área (ha)") + 
  custom_theme
ggsave(glue("{graficas}/distribucion_leche_upa_tamano.svg"), height = h, width = w, dpi = d)  
custom_theme[[2]][["axis.text.x"]][["size"]] <- text*1.7

#--------------------------#
# B. Produccion de leche por cabeza de ganado bovino (Hembras) ----
#--------------------------#

# B1.Datos ----

# Se crea variable de cantidad de hembras bovinas de ordeno
cna <- cna %>% mutate(cat_h_ordeno = NA)

# Se llena la variables cantidad de hembras de ordeno con un criterio arbitrario
cna$cat_h_ordeno[cna$hembras_bov_ord >= 1 & cna$hembras_bov_ord < 5] <- "1-5"
cna$cat_h_ordeno[cna$hembras_bov_ord >= 5 & cna$hembras_bov_ord < 20] <- "5-20"
cna$cat_h_ordeno[cna$hembras_bov_ord >= 20 & cna$hembras_bov_ord < 50] <- "20-50"
cna$cat_h_ordeno[cna$hembras_bov_ord >= 50 & cna$hembras_bov_ord < 100] <- "50-100"
cna$cat_h_ordeno[cna$hembras_bov_ord >= 100 & cna$hembras_bov_ord < 300] <- "100-300"
cna$cat_h_ordeno[cna$hembras_bov_ord >= 300 & cna$hembras_bov_ord < 500] <- "300-500"
cna$cat_h_ordeno[cna$hembras_bov_ord >= 500 & cna$hembras_bov_ord < 1000] <- "500-1000"
cna$cat_h_ordeno[cna$hembras_bov_ord >= 1000] <- "1000 +"
table(cna$cat_h_ordeno)
sum(is.na(cna$cat_h_ordeno))

# Se calcula la produccion le leche por categoria de cantidad de hembras bovinas de ordeno

produccion <- cna %>%
  distinct(cod_vereda, encuesta, .keep_all = T) %>%
  group_by(cat_h_ordeno) %>%
  summarise(produccion_leche = (sum(cant_leche, na.rm = T)/1000)) %>%
  ungroup() %>% 
  mutate(total = sum(produccion_leche), part = 100*(produccion_leche/total)) %>% 
  filter(is.na(cat_h_ordeno) == F ) 

produccion$cat_h_ordeno <- factor(produccion$cat_h_ordeno, 
                            levels = c("1-5", "5-20", "20-50", "50-100", 
                                       "100-300", "300-500", "500-1000", "89-105", "1000 +"))
# B2.  Grafica ----

custom_theme[[2]][["axis.title.y"]][["size"]] <- 20
custom_theme[[2]][["axis.text.x"]][["size"]] <- 15

ggplot(produccion, 
       aes(x = cat_h_ordeno, y = produccion_leche)) +
  geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, fill = "#9983B5") +
  geom_text(aes(label = round(produccion_leche, 1)), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=text/3) +
  ylab("Producción de Leche (miles de L)") +
  xlab("Hembras de ordeno") +
  custom_theme
ggsave(glue("{graficas}/leche_hembras_ordeno.svg"), height = h, width = w, dpi = d)

#B3. Grafica participacion produccion de leche por tamano hato ----
ggplot(produccion, 
       aes(x = cat_h_ordeno, y = part)) +
  geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, fill = "#9983B5") +
  geom_text(aes(label = round(part, 1)), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=text/3) +
  ylab("Participacion producción de Leche") +
  xlab("Hembras de ordeno") +
  custom_theme
ggsave(glue("{graficas}/par_leche_hembras_ordeno.svg"), height = h, width = w, dpi = d)

#------------------------------------------------------------------#
# C. Produccion de leche por departamentos ----
#------------------------------------------------------------------#

leche_dpto <- cna %>% 
  group_by(cod_dpto) %>% 
  summarise(produccion_leche = (sum(cant_leche, na.rm = T)/1000)) %>%
  ungroup() %>% 
  arrange(-produccion_leche)

writexl::write_xlsx(leche_dpto, glue("{datos}/CNA/prod_leche_dpto.xlsx"))

