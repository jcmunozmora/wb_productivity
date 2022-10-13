#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 12 julio, 2022
# Estadisticas descriptivas de las UPAs de las cadenas productivas de arroz y maiz
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, sf)
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
text <- 10
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

map_theme <- list(
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", size = rel(1)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(vjust = -0.2,colour = "black"),
    axis.text = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ))

theme <- list(
  theme_classic(base_size = text*1.5),
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()))

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

# Color
cr = "#6FBDD1"
cr1 = "#5BCFD2"
cr2 = "#62E0C4"
col_palette <- c("#6FBDD1", "#65A8D0", "#7090C6", "#8476B1", "#935A90", "#974066")

# Tamano del punto
s_dot = 3 

#-------------------------------------------------------#
# 1. Abrir datos ----
#-------------------------------------------------------#

# Base de muestra productividad por cultivo principal de la UPA, filtramos por maiz y arroz
cna_prod <- readRDS(glue("{datos}/CNA/base_cna_productividad.rds"))
cultivo <- readRDS(glue("{datos}/CNA/limpieza_censo/lista_cultivo_principal_upa.rds"))

cna_prod <- cna_prod %>% 
  left_join(cultivo, by = c("cod_vereda", "encuesta")) %>%
  dplyr::filter(clasificacion_utilizada == "Arroz" | clasificacion_utilizada == "Maíz (blanco o amarillo)")

# UPA en arroz y maiz
arroz <- cna_prod %>% dplyr::filter(clasificacion_utilizada == "Arroz") %>% distinct(ls2)
maiz <- cna_prod %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)") %>% distinct(ls2)

#-------------------------------------------------------#
# 2. Estadisticas descriptivas ----
#-------------------------------------------------------#

#--------------------------#
# A. Distribucion tierra ----
#--------------------------#

# Se crea variable cantidad de tierra y se pasa el area a hectareas 
cna <- cna_prod %>% mutate(cat_tierra = NA)

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
  group_by(cat_tierra, clasificacion_utilizada) %>%
  summarise(upa = sum(upa, na.rm = T)) %>%
  ungroup() %>%
  group_by(clasificacion_utilizada) %>%
  mutate(total = sum(upa), part = 100*(upa/total)) %>%
  ungroup()

tamano$cat_tierra <- factor(tamano$cat_tierra, 
                            levels = c("0-5", "5-20", "20-28", "28-40", 
                                       "40-57", "57-73", "73-89", "89-105",
                                       "105-202", "202-405", "405-809", "809 +"))  

# Usando definicion de la Federacion Nacional de Cafeteros
# Pequena: 0-5 has, mediana 5-15 has, grande +15 has
tamano <- tamano %>%
  mutate(tipo_upa = ifelse(cat_tierra == "0-5", "Pequeña",
                           ifelse(cat_tierra == "5-20", "Mediana", "Grande")))

tamano$tipo_upa <- factor(tamano$tipo_upa, levels = c("Pequeña", "Mediana", "Grande"))

# Letras
custom_theme[[2]][["plot.title"]] <- element_text(colour = "black", hjust = 0.5)
custom_theme[[2]][["axis.text.x"]][["size"]] <- 15
custom_theme[[1]][["text"]][["size"]] <- 20

svg(glue("{graficas}/distribucion_upa_tamano_cadenas.svg"), height = h*1, width = w*1.5)
Rmisc::multiplot(
  
  # Arroz
  ggplot(data=tamano %>% dplyr::filter(clasificacion_utilizada == "Arroz"), 
         aes(x = cat_tierra, y = part, fill = tipo_upa))+
    geom_bar(stat="identity", position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = c(cr2, "#6FBDD1", "#9983B5"), na.value = "#ededed", na.translate = F) +
    geom_text(aes(label= round(part, 2)), vjust= -0.3, size=text*0.5, position = position_dodge(width = .9))+
    labs(title = "Arroz", caption = "") +
    ylab("Participación (%)\n") + xlab("Área (ha)") + 
    custom_theme,
  
  # Maiz
  ggplot(data=tamano %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)"), 
         aes(x = cat_tierra, y = part, fill = tipo_upa))+
    geom_bar(stat="identity", position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = c(cr2, "#6FBDD1", "#9983B5"), na.value = "#ededed", na.translate = F) +
    geom_text(aes(label= round(part, 2)), vjust= -0.3, size=text*0.5, position = position_dodge(width = .9))+
    labs(title = "Maíz (blanco o amarillo)", caption = "") +
    ylab("Participación (%)\n") + xlab("Área (ha)") + 
    custom_theme,
  
  cols = 2
)
dev.off()

# ggsave(glue("{graficas}/distribucion_upa_tamano_cadenas.pdf"), height = h*1.1, width = w, dpi = d)  

#--------------------------#
# B. Scatter plots ----
#--------------------------#

# Se crean variables del ingreso por hectarea e ingreso por jornal
ingresos <- cna %>%
  drop_na(cant_cosecha, precio, total_jorn) %>%
  distinct(cod_vereda, encuesta, tipo_cul_lote, cant_cosecha, .keep_all = T) %>%
  filter(ing_cosecha > 0) %>%
  group_by(cod_vereda, encuesta,jornales, area_agricola, clasificacion_utilizada) %>%
  summarise(ing_cul_upa = sum(ing_cosecha, na.rm = T)) %>%
  ungroup() %>%
  mutate(ing_jor_upa = ing_cul_upa/jornales,
         log_ing = log(ing_cul_upa/area_agricola),
         log_ing_jor = log(ing_jor_upa),
         log_area = log(area_agricola))

# Ingresos 
theme[[1]][["text"]][["size"]] <- 18
theme[[2]][["axis.text.x"]][["size"]] <- 18
theme[[2]][["axis.text.y"]][["size"]] <- 18

jpeg(glue("{graficas}/ingresos_tierra_trab_cadenas.jpeg"), height = h*1050, width = w*1400, res = d)
# pdf(glue("{graficas}/ingresos_tierra_trab_cadenas.pdf"), height = h*1.05, width = w*1.4)
Rmisc::multiplot(
  # Ingresos por hectárea: Arroz
  ggplot(data = ingresos %>% dplyr::filter(clasificacion_utilizada == "Arroz"), aes(x=log_area, y=log_ing))+
    geom_point(size=s_dot, color=cr)+
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F)+
    coord_cartesian(xlim = c(-5, 10), ylim = c(-12,10)) +
    labs(title = "Arroz", caption="")+ 
    ylab("log (producción/ha)")+ xlab("log (área)") + theme,

  # Ingresos por jornal: Arroz
  ggplot(data=ingresos %>% dplyr::filter(clasificacion_utilizada == "Arroz"), aes(x=log_area, y=log_ing_jor))+
    geom_point(size=s_dot, color=cr)+
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F)+
    # labs(title = "Arroz", caption="")+
    coord_cartesian(xlim = c(-5, 10), ylim = c(-12,10)) +
    scale_color_identity(labels = c("Arroz"), guide = "legend") +
    ylab("log (producción/jornal)")+ xlab("log (área)") + theme,
  
  # Ingresos por jornal: Maiz
  ggplot(data = ingresos %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)"), aes(x=log_area, y=log_ing))+
    geom_point(size=s_dot, color= "#9983B5")+
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F)+
    coord_cartesian(xlim = c(-5, 10), ylim = c(-12,10)) +
    labs(title = "Maíz (blanco o amarillo)", caption="")+ 
    ylab("log (producción/ha)")+ xlab("log (área)") + theme,
  
  # Ingresos por jornal: Maiz 
  ggplot(data=ingresos %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)"), aes(x=log_area, y=log_ing_jor))+
    geom_point(size=s_dot, color="#9983B5")+
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F)+
    # labs(title = "Maíz (blanco o amarillo)", caption="")+
    coord_cartesian(xlim = c(-5, 10), ylim = c(-12,10)) +
    scale_color_identity(labels = c("Maíz (blanco o amarillo)"), guide = "legend") +
    ylab("log (producción/jornal)")+ xlab("log (área)") + theme,
  
  cols = 2
)
dev.off()

# Regresar tema a propiedades predeterminadas
custom_theme[[2]][["plot.title"]] <- element_blank()
custom_theme[[2]][["axis.text.x"]][["size"]] <- text*0.8
custom_theme[[1]][["text"]][["size"]] <- text*1.5
theme[[1]][["text"]][["size"]] <- text*1.5
theme[[2]][["axis.text.y"]][["size"]] <- text*0.8
theme[[2]][["axis.text.x"]][["size"]] <- text*0.8

rm(ingresos, tamano, cna)

#-------------------------------------------------------#
# 3. Productividad ----
#-------------------------------------------------------#

# test <- cna_prod %>% dplyr::filter(clasificacion_utilizada == "Arroz") %>% distinct(ls2)
test <- cna_prod %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)") %>% distinct(ls2)
p10 <- quantile(test$ls2, 0.10, na.rm = T) 
p90 <- quantile(test$ls2, 0.90, na.rm = T) 
100*(p90/p10)

#--------------------------#
# A. Histograma (distribucion) ----
#--------------------------#

theme[[1]][["text"]][["size"]] <- 20
svg(glue("{graficas}/productividad_hamman_cadenas.svg"), height = h*0.9, width = w*1.35)
# pdf(glue("{graficas}/productividad_hamman_cadenas.pdf"), height = h*0.9, width = w*1.35)
Rmisc::multiplot(
  
  # Arroz
  ggplot(cna_prod %>% dplyr::filter(clasificacion_utilizada == "Arroz") %>% distinct(ls2), aes(x = ls2)) +
    geom_histogram(fill = "#6FBDD1", color = "white") +
    labs(x = "\nLog (productividad agropecuaria)", y = "Frecuencia\n") +
    ggtitle("Arroz") +
    coord_cartesian(xlim = c(0, 16)) +
    theme,
  
  # Maiz
  ggplot(cna_prod %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)") %>% distinct(ls2), aes(x = ls2)) +
    geom_histogram(fill = "#9983B5", color = "white") +
    labs(x = "\nLog (productividad agropecuaria)", y = "Frecuencia\n") +
    ggtitle("Maíz (blanco o amarillo)") +
    coord_cartesian(xlim = c(0, 16)) +
    theme,
  
  cols = 2
)
dev.off()
theme[[1]][["text"]][["size"]] <- text*1.5

# ggplot(cna_prod %>% distinct(ls2), aes(x = ls2)) +
#   geom_histogram(fill = "#6FBDD1", color = "white") +
#   labs(x = "\nLog (productividad agropecuaria)", y = "Frecuencia\n") +
#   ggtitle("Área sembrada") +
#   # coord_cartesian(xlim = c(0, 20), ylim = c(0, 170000)) +
#   theme
# ggsave(glue("{graficas}/productividad_hamman_cadenas.pdf"), height = h, width = w, dpi = d)  

#--------------------------#
# B. Scatter plots ----
#--------------------------#

scatter_prod <- cna_prod %>%
  mutate(ing_hectarea = ing_cosecha/area_sembrada, ing_jornal = ing_cosecha/jornales) %>%
  group_by(cod_vereda, encuesta, clasificacion_utilizada, ls2) %>%
  summarise(area_upa = mean(area_upa, na.rm = T),
            jornales = mean(jornales, na.rm = T),
            area_sembrada = mean(area_sembrada, na.rm = T),
            ing_jornal = mean(ing_jornal, na.rm = T),
            ing_hectarea = mean(ing_hectarea, na.rm = T)) %>%
  ungroup() %>%
  mutate(log_ing_jornal = log(ing_jornal), log_ingreso = log(ing_hectarea), 
         log_jornales = log(jornales), log_area_sembrada = log(area_sembrada))

# Letras
theme[[1]][["text"]][["size"]] <- 21
theme[[2]][["axis.text.x"]][["size"]] <- 21
theme[[2]][["axis.text.y"]][["size"]] <- 21

# Arroz
jpeg(glue("{graficas}/scatter_prod_hamman_cadena_arroz.jpeg"), height = h*1200, width = w*1200, res = d)
# pdf(glue("{graficas}/scatter_prod_hamman_cadena_arroz.pdf"), height = h*1.2, width = w*1.2)
Rmisc::multiplot(
  
  # Productividad contra area sembrada (logaritmo)
  ggplot(scatter_prod %>% dplyr::filter(clasificacion_utilizada == "Arroz") %>% drop_na(ls2), 
         aes(x = ls2, y = log_area_sembrada)) +
    geom_point(color = "#6FBDD1", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    # coord_cartesian(xlim = c(-5, 20)) +
    labs(x = "\nLog (productividad agropecuaria)", y = "Log (área sembrada)"),
  
  # Productividad contra jornales (logaritmo)
  ggplot(scatter_prod %>% dplyr::filter(clasificacion_utilizada == "Arroz") %>% drop_na(ls2), 
         aes(x = ls2, y = log_jornales)) +
    geom_point(color = "#6FBDD1", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    # coord_cartesian(xlim = c(-5, 20)) +
    labs(x = "\nLog (productividad agropecuaria)", y = "Log (jornales)"),
  
  # Productividad contra ingreso por hectarea (log)
  ggplot(scatter_prod %>% dplyr::filter(clasificacion_utilizada == "Arroz") %>% drop_na(ls2), 
         aes(x = ls2, y = log_ingreso)) +
    geom_point(color = "#6FBDD1", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    # coord_cartesian(xlim = c(-5, 20)) +
    labs(x = "\nLog (productividad agropecuaria)", y = "Log (ingreso/ha)"),
  
  # Productividad contra ingreso por jornal (log)
  ggplot(scatter_prod %>% dplyr::filter(clasificacion_utilizada == "Arroz") %>% drop_na(ls2), 
         aes(x = ls2, y = log_ing_jornal)) +
    geom_point(color = "#6FBDD1", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    # coord_cartesian(xlim = c(-5, 20)) +
    labs(x = "\nLog (productividad agropecuaria)", y = "Log (ingreso/jornal)"),
  
  cols = 2
  
)
dev.off()

# Maiz
jpeg(glue("{graficas}/scatter_prod_hamman_cadena_maiz.jpeg"), height = h*1200, width = w*1200, res = d)
# pdf(glue("{graficas}/scatter_prod_hamman_cadena_maiz.pdf"), height = h*1.2, width = w*1.2)
Rmisc::multiplot(
  
  # Productividad contra area sembrada (logaritmo)
  ggplot(scatter_prod %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)") %>% drop_na(ls2), 
         aes(x = ls2, y = log_area_sembrada)) +
    geom_point(color = "#9983B5", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    # coord_cartesian(xlim = c(-5, 20)) +
    labs(x = "\nLog (productividad agropecuaria)", y = "Log (área sembrada)"),
  
  # Productividad contra jornales (logaritmo)
  ggplot(scatter_prod %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)") %>% drop_na(ls2), 
         aes(x = ls2, y = log_jornales)) +
    geom_point(color = "#9983B5", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    # coord_cartesian(xlim = c(-5, 20)) +
    labs(x = "\nLog (productividad agropecuaria)", y = "Log (jornales)"),
  
  # Productividad contra ingreso por hectarea (log)
  ggplot(scatter_prod %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)") %>% drop_na(ls2), 
         aes(x = ls2, y = log_ingreso)) +
    geom_point(color = "#9983B5", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    # coord_cartesian(xlim = c(-5, 20)) +
    labs(x = "\nLog (productividad agropecuaria)", y = "Log (ingreso/ha)"),
  
  # Productividad contra ingreso por jornal (log)
  ggplot(scatter_prod %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)") %>% drop_na(ls2), 
         aes(x = ls2, y = log_ing_jornal)) +
    geom_point(color = "#9983B5", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    # coord_cartesian(xlim = c(-5, 20)) +
    labs(x = "\nLog (productividad agropecuaria)", y = "Log (ingreso/jornal)"),
  
  cols = 2
  
)
dev.off()

#--------------------------#
# C. Box Plots ----
#--------------------------#

prod_vereda <- cna_prod %>%
  distinct(cod_vereda, encuesta, ls2) %>%
  drop_na(ls2) %>%
  group_by(cod_vereda) %>%
  summarise(ls2 = mean(ls2)) %>%
  ungroup()

prod_cadena <- cna_prod %>%
  distinct(cod_vereda, encuesta, clasificacion_utilizada, ls2) %>%
  drop_na(ls2) %>%
  group_by(cod_vereda, clasificacion_utilizada) %>%
  summarise(ls2 = mean(ls2)) %>%
  ungroup()

jpeg(glue("{graficas}/boxplot_productividad.jpeg"), height = h*1000, width = w*1700, res = d)
Rmisc::multiplot(
  
  # Todos
  ggplot(prod_vereda, aes(y = ls2)) +
    geom_boxplot(fill = "white", colour = "#6FBDD1",
                 outlier.colour = "#6FBDD1", outlier.shape = 16, outlier.size = 1, notch=FALSE) +
    labs(y = "\nLog (productividad agropecuaria)", x = "\n Todos los cultivos") +
    coord_cartesian(ylim = c(1, max(prod_vereda$ls2))) +
    theme,
  
  # Maiz
  ggplot(prod_cadena %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)") %>% drop_na(ls2), 
         aes(y = ls2)) +
    geom_boxplot(fill = "white", colour = "#9983B5",
                 outlier.colour = "#9983B5", outlier.shape = 16, outlier.size = 2, notch=FALSE) +
    labs(y = "\n", x = "\n Maíz (blanco o amarillo)") +
    coord_cartesian(ylim = c(1, max(prod_vereda$ls2))) +
    theme,
  
  # Arroz
  ggplot(prod_cadena %>% dplyr::filter(clasificacion_utilizada == "Arroz") %>% drop_na(ls2), 
         aes(y = ls2)) +
    geom_boxplot(fill = "white", colour = cr2,
                 outlier.colour = cr1, outlier.shape = 16, outlier.size = 2, notch=FALSE) +
    labs(y = "\n", x = "\n Arroz") +
    coord_cartesian(ylim = c(1, max(prod_vereda$ls2))) +
    theme,

  cols = 3
)
dev.off()

theme[[1]][["text"]][["size"]] <- text*1.5
theme[[2]][["axis.text.y"]][["size"]] <- text*0.8
theme[[2]][["axis.text.x"]][["size"]] <- text*0.8

#--------------------------#
# D. Mapa ----
#--------------------------#

# Shp de mpios
mpios <- st_read(glue("01_Datos_originales/DANE/mapas/mapa_municipios_colombia.shp")) %>% 
  dplyr::rename(cod_mpio = nivl_vl, nom_mpio = nvl_lbl) 

# Shp de regiones
region <- st_read(glue("01_Datos_originales/DANE/mapas/regiones.shp"))

# Extraemos coordenadas (para etiquetas de region)
region <- cbind(region, st_coordinates(st_centroid(region)))
region <- region %>% dplyr::rename(x_reg = X, y_reg = Y)

# Pegamos identificador de region a municipios
mpios <- mpios %>% st_join(region)

# Calculamos productividad promedio por municipio
prod_mpio <- cna_prod %>%
  group_by(cod_dpto, cod_mpio, clasificacion_utilizada) %>%
  summarise(ls2 = mean(ls2, na.rm = T)) %>%
  ungroup()

# Emparejamos # de UPAs y su maquinaria con shp de municipios, borramos San Andres y Providencia

# Arroz: Partir datos en percentiles
mpios_arroz <- mpios %>% 
  left_join(prod_mpio %>% dplyr::filter(clasificacion_utilizada == "Arroz"), by = c("cod_mpio")) %>% 
  dplyr::filter(cod_mpio != 88001 & cod_mpio != 88564) 

classes <- 6
q1 <- quantile(mpios_arroz$ls2, na.rm=T, probs = seq(0, 1, length.out = classes + 1))
mpios_arroz$q_value <- cut(mpios_arroz$ls2, breaks = q1, include.lowest = T, dig.lab = 2)
table(mpios_arroz$q_value)
rm(q1)

# Maiz: Partir datos en percentiles
mpios_maiz <- mpios %>% 
  left_join(prod_mpio %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)"), by = c("cod_mpio")) %>% 
  dplyr::filter(cod_mpio != 88001 & cod_mpio != 88564) 

q1 <- quantile(mpios_maiz$ls2, na.rm=T, probs = seq(0, 1, length.out = classes + 1))
mpios_maiz$q_value <- cut(mpios_maiz$ls2, breaks = q1, include.lowest = T, dig.lab = 2)
table(mpios_maiz$q_value)

# Aumentar tamano letra
theme[[1]][["text"]][["size"]] <- 20

# Distribucion espacial del numero de UPAs
svg(glue("{graficas}/mapa_productividad_mpio_cadenas.svg"), height = h, width = w*1.5)
# pdf(glue("{graficas}/mapa_productividad_mpio_cadenas.pdf"), height = h, width = w*1.2)
Rmisc::multiplot(
  
  # Arroz
  ggplot() +
    geom_sf(data = mpios_arroz, aes(fill = q_value), color = "#2b2b2b", size = 0.05, alpha = 0.65) +
    geom_sf(data = region, color = "#2b2b2b", size = 0.35, alpha = 1, fill = NA) +
    geom_text(data = region, aes(x_reg, y_reg, label = Subregion), size = 5, alpha = 1, color = "black") +
    scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = T) +
    guides(fill = guide_legend(ncol = 4)) +
    ggtitle("Arroz") +
    xlab("Log (productividad agropecuaria)") +
    theme + map_theme,
  
  # Maiz
  ggplot() +
    geom_sf(data = mpios_maiz, aes(fill = q_value), color = "#2b2b2b", size = 0.05, alpha = 0.65) +
    geom_sf(data = region, color = "#2b2b2b", size = 0.35, alpha = 1, fill = NA) +
    geom_text(data = region, aes(x_reg, y_reg, label = Subregion), size = 5, alpha = 1, color = "black") +
    scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = T) +
    guides(fill = guide_legend(ncol = 4)) +
    ggtitle("Maíz (blanco o amarillo)") +
    xlab("Log (productividad agropecuaria)") +
    theme + map_theme,
  
  cols = 2
)
dev.off()

# Regresar tamano letra al original
theme[[1]][["text"]][["size"]] <- text*1.5

# ggsave(glue("{graficas}/mapa_productividad_mpio_cadenas.pdf"), height = h, width = w*0.8, dpi = d)
# ggsave(glue("{graficas}/mapa_productividad_mpio_cadenas.pdf"), height = h, width = w*0.8, dpi = d)

# Municipios que producen arroz/maiz
arroz <- mpios_arroz %>% drop_na(ls2) %>% distinct(cod_mpio)
maiz <- mpios_maiz %>% drop_na(ls2) %>% distinct(cod_mpio)
mpios_cna <- prod_mpio %>% drop_na(ls2) %>% distinct(cod_mpio)
nrow(arroz)/nrow(mpios_cna)
nrow(maiz)/nrow(mpios_cna)

# Productividad por departamento y region
dpto <- readxl::read_excel("01_Datos_originales/DANE/base_nombre_regiones.xlsx") %>%
  dplyr::rename(cod_dpto = nivel_value, nom_dpto = nivel_label)

prod_dpto <- cna_prod %>%
  distinct(ls2, clasificacion_utilizada, .keep_all = T) %>%
  left_join(dpto, by = "cod_dpto") %>%
  group_by(cod_dpto, nom_dpto, clasificacion_utilizada) %>%
  summarise(ls2 = mean(ls2, na.rm = T)) %>%
  ungroup()

prod_reg <- cna_prod %>%
  distinct(ls2, clasificacion_utilizada, .keep_all = T) %>%
  left_join(dpto, by = "cod_dpto") %>%
  group_by(region, clasificacion_utilizada) %>%
  summarise(ls2 = mean(ls2, na.rm = T)) %>%
  ungroup()

writexl::write_xlsx(prod_dpto, glue("{datos}/CNA/limpieza_censo/productividad_dpto_cadenas.xlsx"))
writexl::write_xlsx(prod_reg, glue("{datos}/CNA/limpieza_censo/productividad_region_cadenas.xlsx"))

# Diferencia en productividad
# Maiz
test <- prod_dpto %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)")
dpto_dif <- 100*(max(test$ls2)/min(test$ls2))

# Arroz
test <- prod_dpto %>% dplyr::filter(clasificacion_utilizada == "Arroz")
dpto_dif <- 100*(max(test$ls2)/min(test$ls2))
