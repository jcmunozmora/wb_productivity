#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 13 enero, 2022
# Estadisticas descriptivas replicacion graficas Hamman
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, ggplot2, skimr, reshape2)
# .rs.restartR()
# gc()

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
# 1. Abrir datos ----
#-------------------------------------------------------#

# Unidades originales:
# Areas: metros
# Ingresos: pesos

# Unidades transformadas
# Areas: hectareas

# Abrir censo agrupado por cultivo, y crear variables de interes

# Base all_maq
# cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds"))

# Base productividad
cna <- readRDS(glue("{datos}/CNA/base_cna_productividad.rds"))

# Se crea variable cantidad de tierra y se pasa el area a hectareas -> solo para all_maq
cna <- cna %>% mutate(cat_tierra = NA)

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

# Se calcula la cantidad de upas de acuerdo al tamaño
tamano <- cna %>%
  distinct(area_upa, .keep_all = T) %>%
  mutate(upa = 1) %>%
  group_by(cat_tierra) %>%
  summarise(upa = sum(upa, na.rm = T)) %>%
  ungroup() %>%
  mutate(total = sum(upa), part = 100*(upa/total))

tamano$cat_tierra <- factor(tamano$cat_tierra, 
                            levels = c("0-5", "5-20", "20-28", "28-40", 
                                       "40-57", "57-73", "73-89", "89-105",
                                       "105-202", "202-405", "405-809", "809 +"))

# Se crean variables de trabajadores promedio y de jornales promedio
trabajadores <- cna %>%
  distinct(cod_vereda, encuesta, .keep_all = T) %>%
  group_by(cat_tierra) %>%
  summarise(total_jorn = mean(total_jorn, na.rm = T),
            total_trab = mean(total_trab, na.rm = T),
            total_trab_hogar = mean(total_trab_hogar, na.rm = T)) %>%
  ungroup() %>%
  mutate(total_trab_perm= total_trab - total_trab_hogar,
         total_trab_perm = round(total_trab_perm, digits = 1),
         total_trab_hogar = round(total_trab_hogar, digits = 1))

trabajadores$cat_tierra <- factor(trabajadores$cat_tierra, 
                            levels = c("0-5", "5-20", "20-28", "28-40", 
                                       "40-57", "57-73", "73-89", "89-105",
                                       "105-202", "202-405", "405-809", "809 +"))

# Se crean variables del ingreso por hectarea e ingreso por jornal
ingresos <- cna%>%
  drop_na(cant_cosecha, precio, total_jorn) %>%
  distinct(cod_vereda, encuesta, tipo_cul_lote, cant_cosecha,.keep_all = T) %>%
  filter(ing_cosecha > 0) %>%
  group_by(cod_vereda, encuesta,jornales, area_agricola) %>%
  summarise(ing_cul_upa = sum(ing_cosecha, na.rm = T)) %>%
  ungroup() %>%
  mutate(ing_jor_upa = ing_cul_upa/jornales,
         log_ing = log(ing_cul_upa/area_agricola),
         log_ing_jor = log(ing_jor_upa),
         log_area = log(area_agricola))

#-------------------------------------------------------#
# 2. Graficas ----
#-------------------------------------------------------#

# A. Distribucion de UPA por rangos de tamaño

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
ggsave(glue("{graficas}/distribucion_upa_tamano.svg"), height = h, width = w, dpi = d)  
custom_theme[[2]][["axis.text.x"]][["size"]] <- text*1.7

# B. Relacion entre el area y el numero de trabajadores de la UPA
custom_theme[[2]][["axis.text.x"]][["size"]] <- 18
custom_theme[[1]][["text"]][["size"]] <- 23

svg(glue("{graficas}/trabajadores_perm_jornales_adi.svg"), height = h*1.1, width = w*1.5)
# jpeg(glue("{graficas}/trabajadores_perm_jornales_adi.jpeg"), height = h*1000, width = w*1500, res = d)
Rmisc::multiplot(
  
  # i. Jornales adicionales (mes)
  ggplot(data=trabajadores)+
    geom_segment(aes(x=cat_tierra, xend=cat_tierra, y=total_jorn, yend=0), 
                 color="gray", size = 0.8) +
    geom_point(aes(x=cat_tierra, y=total_jorn, color=cr), size=s_dot*1.2)+
    scale_color_identity(labels = c("Jornales adicionales"), guide = "legend") +
    labs(title = "Jornales adicionales (mes)", caption="")+
    ylab("Número de jornales adicionales promedio")+
    xlab("Área (ha)") + 
    custom_theme,
  
  # ii. Trabajadores permanentes
  ggplot(data=trabajadores)+
    geom_segment(aes(x=cat_tierra, xend=cat_tierra,
                     y=total_trab_perm, yend=0), color="gray", size = 0.8) +
    geom_point(aes(x=cat_tierra, y=total_trab_hogar, color=cr), size=s_dot*1.2) +
    geom_point(aes(x=cat_tierra, y=total_trab_perm, color=cr2), size=s_dot*1.2)+
    scale_color_identity(labels = c("Trabajadores permanentes (total)", "Trabajadores del hogar"), 
                         guide = "legend") +
    labs(title = "Trabajadores permanentes", caption="", color = "Legend")+
    ylab("Número de trabajadores promedio") +
    xlab("Área (ha)") + 
    custom_theme,
  
  cols = 2
)
dev.off()

# Regresar letra a tamano original
custom_theme[[2]][["axis.text.x"]][["size"]] <- text*1.7
custom_theme[[1]][["text"]][["size"]] <- text*1.5

# ggsave(glue("{graficas}/jornales_adicionales.pdf"), height = h, width = w, dpi = d)
# ggsave(glue("{graficas}/trabajadores_permanentes.pdf"), height = h, width = w, dpi = d)

# Ingresos 

theme[[1]][["text"]][["size"]] <- 20
# pdf(glue("{graficas}/ingresos_tierra_trab.pdf"), height = h*0.95, width = w*1.4)
jpeg(glue("{graficas}/ingresos_tierra_trab.jpeg"), height = h*950, width = w*1400, res = d)
Rmisc::multiplot(
  # D. Ingresos por hectárea
  ggplot(data=ingresos, aes(x=log_area, y=log_ing))+
    geom_point(size=s_dot, color=cr)+
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F)+
    labs(title = "Ingresos por hectárea", caption="")+ 
    #coord_cartesian(xlim = c(-10,10), ylim = c(0,30))+
    ylab("log (producción/ha)")+ xlab("log (área)")+ theme,
    
  # E. Ingresos por jornal
  ggplot(data=ingresos, aes(x=log_area, y=log_ing_jor))+
    geom_point(size=s_dot, color=cr)+
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F)+
    labs(title = "Ingresos por jornal", caption="")+
    #coord_cartesian(xlim = c(-10,10), ylim = c(-10,25))+
    ylab("log (producción/jornal)")+ xlab("log (área)")+ theme,
    
  cols = 2
)
dev.off()
theme[[1]][["text"]][["size"]] <- text*1.5

#ggsave(glue("{graficas}/ingresos_por_area.pdf"), height = h, width = w, dpi = d)
#ggsave(glue("{graficas}/ingresos_por_jornal.pdf"), height = h, width = w, dpi = d)
