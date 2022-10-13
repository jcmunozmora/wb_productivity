#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 20 abril, 2022
# Estadisticas descriptivas por grupos de cultivos Censo Nacional Agropecuario
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
theme_style <- theme_classic(base_size = text*1.8)
text_color <- "black"

# Tema modificable
custom_theme <- list(
  theme_style,
  theme(legend.position = legend_pos, legend.title = legend_title,
        plot.title = plot_title,
        axis.title.x = element_text(colour = text_color),
        axis.title.y = element_text(colour = text_color),
        axis.text.x = element_text(angle = label_ang.x, vjust = label_h.x, hjust = label_v.x, colour = text_color, size = text*1.7),
        axis.text.y = element_text(angle = label_ang.y, vjust = label_h.y, hjust = label_v.y, colour = text_color, size = text*1.7))
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
# 1. Abrir datos ----
#-------------------------------------------------------#

# Unidades de la base:
# Cantidad cosechada: toneladas 
# Areas: hectareas
# Jornaleros: numero de trabajadores permanentes + jornales adicionales, anual
# Maquinaria: dummy (1=tiene, 0=notiene), esta muestra tiene maquinaria (100%)
# Ingresos: millones de pesos

# Unidades transformadas
# Rendimientos cosecha y trabajador: tonelada/hectarea, trabajador/hectarea, tonelada/trabajador

# Abrir censo agrupado por cultivo principal de la UPA, y crear variables de interes
cna_agro <- readRDS(glue("{datos}/CNA/base_cna_grupos_cultivos_principal.rds")) %>%
  # Variables para estadisticas
  mutate(mean_jornal = total_jor/num_lote,
         mean_upa = area_upa/num_lote, mean_agro = area_agricola/num_lote, 
         mean_sembrada = area_sembrada/num_lote, mean_area_cos = area_cosechada/num_lote,
         mean_cosecha = cant_cosecha/num_lote, rend_cos = cant_cosecha/area_cosechada,
         tierra_trab = area_upa/total_jor, cos_trab = cant_cosecha/total_jor,
         mean_ingreso = ing_cosecha/num_lote, rend_ind = ing_cosecha/cant_cosecha,
         ing_jor = ing_cosecha/total_jor)

cna_agro$clas_cultivo[cna_agro$clas_cultivo == "Legumbres, nueces y semillas oleaginosas"] <- "Legumbres, nueces, oleaginosas"

#-------------------------------------------------------#
# 2. Estadisticas ----
#-------------------------------------------------------#

#--------------------------#
# A. Areas ----
#--------------------------#

# Area UPA, area agropecuaria, area sembrada, area cosechada
areas <- cna_agro %>% 
  select(clas_cultivo, mean_upa, mean_agro, mean_sembrada, mean_area_cos) %>% 
  pivot_longer(cols = starts_with("mean"), names_to = "tipo_area", values_to = "area")

areas$tipo_area <- factor(areas$tipo_area, levels = c("mean_upa", "mean_agro", "mean_sembrada", "mean_area_cos"))

# Area UPA y agricola
ggplot(areas %>% filter(tipo_area == "mean_upa" | tipo_area == "mean_agro"), 
       aes(x = reorder(factor(clas_cultivo), - area), y = area, fill = factor(tipo_area))) +
  geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, aes(fill = factor(tipo_area))) +
  ylab("Área promedio (has)\n") +
  xlab("") +
  scale_fill_manual(labels = c("mean_upa" = "Área UPA", "mean_agro" = "Área agrícola"), 
                    values = c("#6FBDD1", "#9983B5")) +
  custom_theme
ggsave(glue("{graficas}/area_upa_agro_cultivos.pdf"), height = h, width = w, dpi = d)

# Aumentar tamano letra
theme[[1]][["text"]][["size"]] <- 30

# Area sembrada y cosechada
writexl::write_xlsx(areas, path = glue("{datos}/CNA/area_sembrada_cosechada_cultivo.xlsx"))

svg(glue("{graficas}/area_sembrada_cosechada_cultivos.svg"), height = h*1.2, width = w*2.2)
# pdf(glue("{graficas}/area_sembrada_cosechada_cultivos.pdf"), height = h, width = w*1.5)
Rmisc::multiplot(
  ggplot(areas %>% filter(tipo_area == "mean_sembrada" | tipo_area == "mean_area_cos") %>%
           filter(clas_cultivo != "Palma africana"), 
         aes(x = reorder(factor(clas_cultivo), - area), y = area, fill = factor(tipo_area))) +
    geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, aes(fill = factor(tipo_area))) +
    ylab("Área promedio (has)\n") +
    xlab("") +
    scale_fill_manual(labels = c("mean_sembrada" = "Área sembrada", "mean_area_cos" = "Área cosechada"), 
                      values = c("#6FBDD1", "#9983B5")) +
    custom_theme,
  
  ggplot(areas %>% filter(tipo_area == "mean_sembrada" | tipo_area == "mean_area_cos") %>%
           filter(clas_cultivo == "Palma africana"), 
         aes(x = reorder(factor(clas_cultivo), - area), y = area, fill = factor(tipo_area))) +
    geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, aes(fill = factor(tipo_area))) +
    ylab("Área promedio (has)\n") +
    xlab("") +
    scale_fill_manual(labels = c("mean_sembrada" = "Área sembrada", "mean_area_cos" = "Área cosechada"), 
                      values = c("#6FBDD1", "#9983B5")) +
    theme,
  
  cols = 2)
dev.off()

# Regresar tamano letra al original
theme[[1]][["text"]][["size"]] <- text*1.5

rm(areas)

#--------------------------#
# B. Trabajo ----
#--------------------------#

# Jornales totales
trab <- cna_agro %>% 
  select(clas_cultivo, mean_jornal) %>% 
  pivot_longer(cols = starts_with("mean"), names_to = "tipo_trab", values_to = "trabajadores")

ggplot(trab, aes(x = reorder(factor(clas_cultivo), -trabajadores), y = trabajadores, fill = factor(tipo_trab))) +
  geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, aes(fill = factor(tipo_trab))) +
  ylab("Promedio jornales\n") +
  xlab("") +
  geom_text(aes(label = round(trabajadores, 1)), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=text/3) +
  scale_fill_manual(labels = c("mean_jornal" = "Total jornales anuales"), 
                    values = c("#6FBDD1", "#9983B5")) +
  custom_theme 
ggsave(glue("{graficas}/trabajadores_cultivos.pdf"), height = h, width = w, dpi = d)
rm(trab)

# Hectareas por trabajador (tierra/trabajo) y cosecha por trabajador (produccion/trabajo)
rend_trab <- cna_agro %>% 
  select(clas_cultivo, tierra_trab, cos_trab) %>% 
  pivot_longer(cols = c("tierra_trab", "cos_trab"), names_to = "rend_trab", values_to = "rendimiento")

rend_trab$rend_trab <- factor(rend_trab$rend_trab, levels = c("tierra_trab", "cos_trab"))

pdf(glue("{graficas}/rendimiento_trabajador_cultivos.pdf"), height = h*0.8, width = w*1.2)
Rmisc::multiplot(
  
  ggplot(rend_trab %>% filter(rend_trab == "tierra_trab"), 
         aes(x = reorder(factor(clas_cultivo), -rendimiento), y = rendimiento)) +
    geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, fill = "#6FBDD1") +
    ylab("Hectáreas por jornal\n") +
    xlab("") +
    geom_text(aes(label = round(rendimiento, 2)), vjust=-0.5, color="black",
              position = position_dodge(0.9), size=text/3) +
    custom_theme,
  
  ggplot(rend_trab %>% filter(rend_trab == "cos_trab"), 
         aes(x = reorder(factor(clas_cultivo), -rendimiento), y = rendimiento)) +
    geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, fill = "#9983B5") +
    ylab("Cosecha por jornal (ton)\n") +
    xlab("") +
    geom_text(aes(label = round(rendimiento, 3)), vjust=-0.5, color="black",
              position = position_dodge(0.9), size=text/3) +
    custom_theme,
  
  cols = 2
)
dev.off()

rm(rend_trab, trab)

#--------------------------#
# C. Cosechas ----
#--------------------------#

# Cosecha por hectarea y cosecha total
cos <- cna_agro %>% 
  select(clas_cultivo, mean_cosecha, rend_cos) %>% 
  pivot_longer(cols = c("mean_cosecha", "rend_cos"), names_to = "cosecha", values_to = "total")

cos$cosecha <- factor(cos$cosecha, levels = c("mean_cosecha", "rend_cos"))

# Cosecha por hectárea
writexl::write_xlsx(cos %>% filter(cosecha == "rend_cos"), path = glue("{datos}/CNA/rendimiento_cultivo.xlsx"))

custom_theme[[2]][["axis.title.y"]][["size"]] <- 26
custom_theme[[2]][["axis.text.x"]][["size"]] <- 15

ggplot(cos %>% filter(cosecha == "rend_cos"), 
       aes(x = reorder(factor(clas_cultivo), -total), y = total)) +
  geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, fill = "#9983B5") +
  geom_text(aes(label = round(total, 1)), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=text/3) +
  ylab("Ton cosechadas por hectárea\n") +
  xlab("") +
  custom_theme
# ggsave(glue("{graficas}/cosecha_hectarea_cultivos.jpeg"), height = h, width = w, dpi = d)
ggsave(glue("{graficas}/cosecha_hectarea_cultivos.svg"), height = h, width = w, dpi = d)

custom_theme[[2]][["axis.text.x"]][["size"]] <- 18
svg(glue("{graficas}/cosecha_total_rend_cultivos.svg"), height = h*1.2, width = w*1.8)
# pdf(glue("{graficas}/cosecha_total_rend_cultivos.pdf"), height = h*1.2, width = w*1.5)
Rmisc::multiplot(
  
  ggplot(cos %>% filter(cosecha == "mean_cosecha"), 
         aes(x = reorder(factor(clas_cultivo), -total), y = total)) +
    geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, fill = "#6FBDD1") +
    ylab("Toneladas promedio\n") +
    xlab("") +
    custom_theme,
  
  ggplot(cos %>% filter(cosecha == "rend_cos"), 
         aes(x = reorder(factor(clas_cultivo), -total), y = total)) +
    geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, fill = "#9983B5") +
    ylab("Toneladas por hectárea\n") +
    xlab("") +
    custom_theme,
  
  cols = 2
)
dev.off()

# Regresar letra a tamano original
custom_theme[[2]][["axis.text.x"]][["size"]] <- text*1.7

#--------------------------#
# D. Ingresos por cultivo ----
#--------------------------#

ingreso <- cna_agro %>% 
  dplyr::select(clas_cultivo, mean_ingreso, rend_ind, ing_jor) %>%
  pivot_longer(cols = c("mean_ingreso", "rend_ind", "ing_jor"), names_to = "variable", values_to = "total")

custom_theme[[2]][["axis.title.y"]][["size"]] <- 20
custom_theme[[2]][["axis.text.x"]][["size"]] <- 18

writexl::write_xlsx(ingreso %>% dplyr::filter(variable != "ing_jor"), path = glue("{datos}/CNA/ingresos_cultivo.xlsx"))

svg(glue("{graficas}/ingreso_promedio_cultivos_cosecha.svg"), height = h*1.1, width = w*1.5)
# pdf(glue("{graficas}/ingreso_promedio_cultivos_cosecha.pdf"), height = h, width = w*1.35)
Rmisc::multiplot(
  
  # Ingreso promedio UPA
  ggplot(ingreso %>% dplyr::filter(variable == "mean_ingreso"), 
         aes(x = reorder(factor(clas_cultivo), -total), y = total)) +
    geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, fill = "#6FBDD1") +
    ylab("Ingreso promedio por cultivo (millones)\n") +
    xlab("") +
    geom_text(aes(label = round(total, 1)), vjust=-0.5, color="black",
              position = position_dodge(0.9), size=text/3) +
    custom_theme,
  
  # Ingreso por kg cosechado
  ggplot(ingreso %>% dplyr::filter(variable == "rend_ind"), 
         aes(x = reorder(factor(clas_cultivo), -total), y = total)) +
    geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, fill = "#6FBDD1") +
    ylab("Ingreso por ton cosechadas (millones)\n") +
    xlab("") +
    geom_text(aes(label = round(total, 1)), vjust=-0.5, color="black",
              position = position_dodge(0.9), size=text/3) +
    custom_theme,
  
  cols = 2
)
dev.off()

# ggsave(glue("{graficas}/ingreso_promedio_cultivos.pdf"), height = h, width = w, dpi = d)
# ggsave(glue("{graficas}/ingreso_promedio_cosecha.pdf"), height = h, width = w, dpi = d)

# Ingreso por jornal
ggplot(ingreso %>% dplyr::filter(variable == "ing_jor"), 
       aes(x = reorder(factor(clas_cultivo), -total), y = total)) +
  geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, fill = "#6FBDD1") +
  ylab("Ingreso por jornal (millones)\n") +
  xlab("") +
  geom_text(aes(label = round(total, 3)), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=text/3) +
  custom_theme
ggsave(glue("{graficas}/ingreso_jornal_cultivo.pdf"), height = h, width = w, dpi = d)

