#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 12 julio, 2022
# Estadisticas descriptivas de la productividad a nivel de UPA  
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

# Productividad calculada con Do Hamman et al (2018)
prod <- haven::read_dta(glue("{datos}/Productividad/base_productividad_hamman.dta")) %>%
  mutate(log_area_sembrada = log(area_sembrada), log_jornales = log(jornales),
         ing_hectarea = revenue/area_sembrada, log_ingreso = log(ing_hectarea),
         log_ing_jornal = log(revenue_j))

# Area sembrada total
area <- prod %>% drop_na(ls2) %>% dplyr::select(area_sembrada)
sum(area$area_sembrada)

p10 <- quantile(prod$ls2, 0.10, na.rm = T) 
p90 <- quantile(prod$ls2, 0.90, na.rm = T) 
100*(p90/p10)

# Ingresos por ha segun prc
ing10 <- prod %>% 
  drop_na(ls2) %>% dplyr::filter(ls2 >= p10) %>% dplyr::select(log_ingreso)

ing90 <- prod %>% 
  drop_na(ls2) %>% dplyr::filter(ls2 >= p90) %>% dplyr::select(log_ingreso)

100*(mean(ing90$log_ingreso)/mean(ing10$log_ingreso))

# Ingresos por jornal segun prc
jor10 <- prod %>% 
  drop_na(ls2) %>% dplyr::filter(ls2 >= p10) %>% dplyr::select(log_ing_jornal)

jor90 <- prod %>% 
  drop_na(ls2) %>% dplyr::filter(ls2 >= p90) %>% dplyr::select(log_ing_jornal)

100*(mean(jor90$log_ing_jornal)/mean(jor10$log_ing_jornal))

#-------------------------------------------------------#
# A. Distribuciones ----
#-------------------------------------------------------#

### Distribucion de la productividad

# Histogramas
svg(glue("{graficas}/productividad_hamman.svg"), height = h*0.8, width = w*1.2)
# pdf(glue("{graficas}/productividad_hamman.pdf"), height = h*0.8, width = w*1.2)
Rmisc::multiplot(
  
  # Area agricola
  ggplot(prod, aes(x = ls1)) +
    geom_histogram(fill = "#9983B5", color = "white") +
    labs(x = "\nLog (productividad agropecuaria)", y = "Frecuencia \n") +
    ggtitle("Área agrícola") +
    coord_cartesian(xlim = c(-5, 20), ylim = c(0, 160000)) +
    theme,
  
  # Area sembrada
  ggplot(prod, aes(x = ls2)) +
    geom_histogram(fill = "#6FBDD1", color = "white") +
    labs(x = "\nLog (productividad agropecuaria)", y = "") +
    ggtitle("Área sembrada") +
    coord_cartesian(xlim = c(-5, 20), ylim = c(0, 160000)) +
    theme,
  
  cols = 2
  
)
dev.off()

# # Boxplot (diagrama de caja -- Nivel de vereda)
# prod_vereda <- prod %>%
#   distinct(cod_vereda, encuesta, ls2) %>%
#   drop_na(ls2) %>%
#   group_by(cod_vereda) %>%
#   summarise(ls2 = mean(ls2)) %>%
#   ungroup()
#   
# ggplot(prod_vereda, aes(y = ls2)) +
#   geom_boxplot(fill = "white", colour = "#6FBDD1",
#                outlier.colour = "#6FBDD1", outlier.shape = 16, outlier.size = 1, notch=FALSE) +
#   labs(y = "\nLog (productividad agropecuaria)", x = "\n") +
#   coord_cartesian(ylim = c(1, max(prod_vereda$ls2))) +
#   theme
# ggsave(glue("{graficas}/box_plot_productividad_all.jpeg"), height = h, width = w*0.9)

# Diagramas de dispersion
jpeg(glue("{graficas}/scatter_prod_hamman.jpeg"), height = h*1000, width = w*1200, res = d)
# pdf(glue("{graficas}/scatter_prod_hamman.pdf"), height = h*1.2, width = w*1.2)
Rmisc::multiplot(
  
  # Productividad contra area sembrada (logaritmo)
  ggplot(prod %>% drop_na(ls2), aes(x = ls2, y = log_area_sembrada)) +
    geom_point(color = "#6FBDD1", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    coord_cartesian(xlim = c(-5, 20)) +
    labs(x = "\nLog (productividad agropecuaria)", y = "Log (área sembrada)\n"),
  
  # Productividad contra jornales (logaritmo)
  ggplot(prod %>% drop_na(ls2), aes(x = ls2, y = log_jornales)) +
    geom_point(color = "#6FBDD1", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    coord_cartesian(xlim = c(-5, 20)) +
    labs(x = "\nLog (productividad agropecuaria)", y = "Log (jornales)\n"),
  
  # Productividad contra ingreso por hectarea (log)
  ggplot(prod %>% drop_na(ls2), aes(x = ls2, y = log_ingreso)) +
    geom_point(color = "#6FBDD1", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    coord_cartesian(xlim = c(-5, 20)) +
    labs(x = "\nLog (productividad agropecuaria)", y = "Log (ingreso/ha.)\n"),
  
  # Productividad contra ingreso por jornal (log)
  ggplot(prod %>% drop_na(ls2), aes(x = ls2, y = log_ing_jornal)) +
    geom_point(color = "#6FBDD1", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    coord_cartesian(xlim = c(-5, 20)) +
    labs(x = "\nLog (productividad agropecuaria)", y = "Log (ingreso/jornal)\n"),
  
  cols = 2
  
)
dev.off()

# ggsave(glue("{graficas}/scatter_prod_area_sembrada.pdf"), height = h, width = w, dpi = d)
# ggsave(glue("{graficas}/scatter_prod_jornales.pdf"), height = h, width = w, dpi = d)
# ggsave(glue("{graficas}/scatter_prod_ingreso.pdf"), height = h, width = w, dpi = d)
# ggsave(glue("{graficas}/scatter_prod_ing_jornales.pdf"), height = h, width = w, dpi = d)

#-------------------------------------------------------#
# B. Promedios municipales y departamentales ----
#-------------------------------------------------------#

# Nombres de los municipios: Shp de mpios
mpios <- readr::read_csv(glue("01_Datos_originales/DANE/base_nombres_codigos_dpto_mpio.csv")) 

# Municipal
mean_mpio <- prod %>% 
  drop_na(p_munic, p_depto) %>%
  group_by(p_depto, p_munic) %>%
  summarise(mean_productividad = mean(ls2, na.rm = T)) %>%
  dplyr::rename(cod_mpio = p_munic, cod_dpto = p_depto) %>%
  ungroup() %>% 
  left_join(mpios, by = c("cod_mpio", "cod_dpto")) %>%
  dplyr::select(cod_dpto, cod_mpio, nom_dpto, nom_mpio, mean_productividad)

# Departamental
mean_dpto <- prod %>% 
  drop_na(p_depto) %>%
  # Excluimos San Andres
  dplyr::filter(p_depto != 88) %>%
  group_by(p_depto) %>%
  summarise(mean_productividad = mean(ls2, na.rm = T)) %>%
  ungroup() %>%
  dplyr::rename(cod_dpto = p_depto) %>%
  ungroup() %>% 
  left_join(mpios %>% distinct(cod_dpto, .keep_all = T), by = "cod_dpto") %>%
  dplyr::select(cod_dpto,nom_dpto, mean_productividad)

colSums(is.na(mean_dpto))

writexl::write_xlsx(mean_mpio, path = glue("{datos}/Productividad/productividad_promedio_municipal.xlsx"))
writexl::write_xlsx(mean_dpto, path = glue("{datos}/Productividad/productividad_promedio_departamental.xlsx"))
