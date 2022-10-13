#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 20 abril, 2022
# Estadisticas descriptivas de la eficiencia por grupo de cultivo
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
col_palette2 <- c("#6FBDD1", "#7090C6", "#8476B1", "#974066")

# Tamano del punto
s_dot = 3 

#-------------------------------------------------------#
# 1. Abrir datos ----
#-------------------------------------------------------#

# Cargamos datos de eficiencia
wb <- getwd()
wb <- gsub("BM_productividad", "BM_Efficiency", wb)
load(glue("{wb}/01_out/base_prod_efficiency.rda"))

# Pasamos nombres de cultivos de ingles a espanol
ds_final$grupo_cultivo <- ordered(ds_final$grupo_cultivo,
                                  labels = c("Árboles maderables", "Árboles no maderables", "Arroz",
                                             "Café", "Caña panelera", "Cereales", "Forrajes", "Frutas y vegetales",
                                             "Legumbres, nueces y semillas oleaginosas", "Maíz (blanco o amarillo)", 
                                             "Palma africana", "Plantas"),
                                  levels = c("Timber trees", "Non-timber trees", "Rice", "Coffee", "Sugar cane",
                                             "Cereals", "Forages", "Fruits and vegetables", "Legumes, nuts and oilseeds",
                                             "Corn (yellow or white)", "African palm", "Plants"))
table(ds_final$grupo_cultivo)

# Eficiencia esta por cultivo-UPA, pasamos a wide y agregamos a nivel de vereda
data_ef <- ds_final %>% 
  # distinct(cod_vereda, encuesta, grupo_cultivo, Efficiency, .keep_all = T) %>%
  distinct() %>%
  janitor::clean_names() %>%
  pivot_wider(id_cols = c("cod_dpto", "cod_mpio", "cod_vereda", "encuesta", "grupo_cultivo", "id_cul", "efficiency"),
              names_from = "var", values_from = "value") %>%
  arrange(cod_vereda, encuesta)

data_ef_ag <- data_ef %>%
  distinct(cod_vereda, encuesta, efficiency) %>%
  group_by(cod_vereda) %>%
  summarise(efficiency = mean(efficiency, na.rm = T)) %>%
  ungroup()
  
# Lista de UPAs en muestra eficiencia
# Revisar porque solo tenemos 94,204 UPAs
upas <- data_ef %>% distinct(cod_vereda, encuesta)
  
# Abrir caracteristicas UPA
base_cna <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds")) %>%
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, tipo_cul_lote, 
                area_upa, area_sembrada, area_agricola, ing_cosecha, cant_cosecha_kg, jornales) %>% 
  drop_na(tipo_cul_lote, area_upa) %>%
  distinct() %>%
  # Pasamos areas a hectareas e ingresos a millones
  dplyr::mutate(area_upa = area_upa/10000, area_agricola = area_agricola/10000,
                area_sembrada = area_sembrada/10000, ing_cosecha = ing_cosecha/1000000)
  
# Agregamos caracteristicas UPA a nivel de vereda
cna_ver <- base_cna %>%
  group_by(cod_vereda) %>%
  summarise(area_upa = mean(area_upa, na.rm = T), area_agricola = mean(area_agricola, na.rm = T),
            area_sembrada = mean(area_sembrada, na.rm = T), ing_cosecha = mean(ing_cosecha, na.rm = T),
            jornales = mean(jornales, na.rm = T), cant_cosecha_kg = mean(cant_cosecha_kg, na.rm = T)) %>%
  ungroup()

# Unimos eficiencia y caracteristicas UPA
data_ver <- data_ef_ag %>% left_join(cna_ver, by = "cod_vereda")
colSums(is.na(data_ver))

#-------------------------------------------------------#
# 2. Estadisticas descriptivas ----
#-------------------------------------------------------#

#--------------------------#
# A. Distribucion ----
#--------------------------#

# Exportar distribucion eficiencia
efi <- data_ver %>% count(efficiency) %>% rename(num_veredas = n) %>% arrange(-efficiency)
writexl::write_xlsx(efi, path = glue("{datos}/CNA/distribucion_eficiencia_vereda.xlsx"))

theme[[1]][["text"]][["size"]] <- 20

ggplot(data_ver, aes(x = efficiency)) +
  geom_histogram(fill = "#6FBDD1", color = "white") +
  labs(x = "\nEficiencia", y = "Frecuencia\n") +
  theme
# ggsave(glue("{graficas}/distribucion_eficiencia.pdf"), height = h, width = w, dpi = d)
ggsave(glue("{graficas}/distribucion_eficiencia.svg"), height = h*0.8, width = w*0.8)

# Regresar tamano letra al original
theme[[1]][["text"]][["size"]] <- text*1.5

#--------------------------#
# B. Scatter plots ----
#--------------------------#

scatter_ef <- data_ver %>%
  drop_na() %>%
  mutate(ing_cosecha = ing_cosecha + 0.001,
         ing_hectarea = ing_cosecha/area_sembrada, ing_jornal = ing_cosecha/jornales,
         log_ing_jornal = log(ing_jornal), log_ingreso = log(ing_hectarea), 
         log_jornales = log(jornales), log_area_sembrada = log(area_sembrada))

summary(scatter_ef)
sum(is.infinite(scatter_ef$log_ingreso))

theme[[1]][["text"]][["size"]] <- 20
jpeg(glue("{graficas}/scatter_eficiencia.jpeg"), height = h*1200, width = w*1200, res = d)
# pdf(glue("{graficas}/scatter_eficiencia.pdf"), height = h*1.2, width = w*1.2)
Rmisc::multiplot(
  
  # Eficiencia contra area sembrada (logaritmo)
  ggplot(scatter_ef, 
         aes(x = efficiency, y = log_area_sembrada)) +
    geom_point(color = "#6FBDD1", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    labs(x = "\nEficiencia", y = "Log (área sembrada)\n"),
  
  # Eficiencia contra jornales (logaritmo)
  ggplot(scatter_ef, 
         aes(x = efficiency, y = log_jornales)) +
    geom_point(color = "#6FBDD1", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    labs(x = "\nEficiencia", y = "Log (jornales)\n"),
  
  # Eficiencia contra ingreso por hectarea (log)
  ggplot(scatter_ef, 
         aes(x = efficiency, y = log_ingreso)) +
    geom_point(color = "#6FBDD1", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    labs(x = "\nEficiencia", y = "Log (ingreso/ha)\n"),
  
  # Eficiencia contra ingreso por jornal (log)
  ggplot(scatter_ef, 
         aes(x = efficiency, y = log_ing_jornal)) +
    geom_point(color = "#6FBDD1", alpha = 0.9) + 
    geom_smooth(color = "black", alpha = 0.5, size = 0.6, linetype = "dashed", se = F) +
    theme +
    labs(x = "\nEficiencia", y = "Log (ingreso/jornal)\n"),
  
  cols = 2
  
)
dev.off()
# Regresar tamano letra al original
theme[[1]][["text"]][["size"]] <- text*1.5


#--------------------------#
# C. Mapa general ----
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

# Calculamos eficiencia promedio por municipio
ef_mpio <- data_ef %>%
  distinct(cod_mpio, encuesta, efficiency) %>%
  group_by(cod_mpio) %>%
  summarise(efficiency = mean(efficiency, na.rm = T)) %>%
  ungroup()

writexl::write_xlsx(ef_mpio, path = glue("{datos}/CNA/eficiencia_municipal.xlsx"))

# Partir datos en percentiles
mpios_ef <- mpios %>% 
  left_join(ef_mpio, by = c("cod_mpio")) %>% 
  dplyr::filter(cod_mpio != 88001 & cod_mpio != 88564) 

classes <- 6
q1 <- quantile(mpios_ef$efficiency, na.rm=T, probs = seq(0, 1, length.out = classes + 1))
mpios_ef$q_value <- cut(mpios_ef$efficiency, breaks = q1, include.lowest = T, dig.lab = 2)
table(mpios_ef$q_value)

# Distribucion municipal de la eficiencia
ggplot() +
  geom_sf(data = mpios_ef, aes(fill = q_value), color = "#2b2b2b", size = 0.05, alpha = 0.65) +
  geom_sf(data = region, color = "#2b2b2b", size = 0.35, alpha = 1, fill = NA) +
  geom_text(data = region, aes(x_reg, y_reg, label = Subregion), size = 5, alpha = 1, color = "black") +
  scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = T) +
  guides(fill = guide_legend(ncol = 4)) +
  xlab("Eficiencia") +
  theme + map_theme
ggsave(glue("{graficas}/mapa_eficiencia_mpio.svg"), height = h, width = w*0.8, dpi = d)
# ggsave(glue("{graficas}/mapa_eficiencia_mpio.pdf"), height = h, width = w*0.8, dpi = d)

test <- ef_mpio %>% distinct(efficiency)
p10 <- quantile(test$efficiency, 0.10, na.rm = T) 
p90 <- quantile(test$efficiency, 0.90, na.rm = T) 
100*(p90/p10)

# Calculos departamentales
dpto <- readxl::read_excel("01_Datos_originales/DANE/base_nombre_regiones.xlsx") %>%
  dplyr::rename(cod_dpto = nivel_value, nom_dpto = nivel_label)

ef_dpto <- data_ef %>%
  distinct(cod_dpto, cod_mpio, encuesta, efficiency) %>%
  left_join(dpto, by = "cod_dpto") %>%
  group_by(cod_dpto, nom_dpto, region) %>%
  summarise(efficiency = mean(efficiency, na.rm = T)) %>%
  ungroup()

dpto_dif <- 100*(max(ef_dpto$efficiency)/min(ef_dpto$efficiency))

#--------------------------#
# D. Mapa cadenas (maiz, arroz) ----
#--------------------------#

# Calculamos eficiencia promedio por municipio y cadena
ef_cadenas <- data_ef %>%
  dplyr::filter(grupo_cultivo == "Arroz" | grupo_cultivo == "Maíz (blanco o amarillo)") %>%
  distinct(cod_mpio, encuesta, grupo_cultivo, efficiency) %>%
  group_by(cod_mpio, grupo_cultivo) %>%
  summarise(efficiency = mean(efficiency, na.rm = T)) %>%
  ungroup()

# Arroz: Partir datos en percentiles
mpios_arroz <- mpios %>% 
  left_join(ef_cadenas %>% dplyr::filter(grupo_cultivo == "Arroz"), by = c("cod_mpio")) %>% 
  dplyr::filter(cod_mpio != 88001 & cod_mpio != 88564) 

classes <- 4
q1 <- quantile(mpios_arroz$efficiency, na.rm=T, probs = seq(0, 1, length.out = classes + 1))
mpios_arroz$q_value <- cut(mpios_arroz$efficiency, breaks = q1, include.lowest = T, dig.lab = 2)
table(mpios_arroz$q_value)
rm(q1)

# Maiz: Partir datos en percentiles
mpios_maiz <- mpios %>% 
  left_join(ef_cadenas %>% dplyr::filter(grupo_cultivo == "Maíz (blanco o amarillo)"), by = c("cod_mpio")) %>% 
  dplyr::filter(cod_mpio != 88001 & cod_mpio != 88564) 

q1 <- quantile(mpios_maiz$efficiency, na.rm=T, probs = seq(0, 1, length.out = classes + 1))
mpios_maiz$q_value <- cut(mpios_maiz$efficiency, breaks = q1, include.lowest = T, dig.lab = 2)
table(mpios_maiz$q_value)

# Aumentar tamano letra
theme[[1]][["text"]][["size"]] <- 20

# Creamos mapas de la eficiencia municipal promedio para las cadenas de maiz y arroz
svg(glue("{graficas}/mapa_eficiencia_mpio_cadenas.svg"), height = h, width = w*1.5)
# pdf(glue("{graficas}/mapa_eficiencia_mpio_cadenas.pdf"), height = h, width = w*1.2)
Rmisc::multiplot(
  
  # Arroz
  ggplot() +
    geom_sf(data = mpios_arroz, aes(fill = q_value), color = "#2b2b2b", size = 0.05, alpha = 0.65) +
    geom_sf(data = region, color = "#2b2b2b", size = 0.35, alpha = 1, fill = NA) +
    geom_text(data = region, aes(x_reg, y_reg, label = Subregion), size = 5, alpha = 1, color = "black") +
    scale_fill_manual(values =  rep(col_palette2, 40), na.value = "#ededed", na.translate = T) +
    guides(fill = guide_legend(ncol = 4)) +
    ggtitle("Arroz") +
    xlab("Eficiencia") +
    theme + map_theme,
  
  # Maiz
  ggplot() +
    geom_sf(data = mpios_maiz, aes(fill = q_value), color = "#2b2b2b", size = 0.05, alpha = 0.65) +
    geom_sf(data = region, color = "#2b2b2b", size = 0.35, alpha = 1, fill = NA) +
    geom_text(data = region, aes(x_reg, y_reg, label = Subregion), size = 5, alpha = 1, color = "black") +
    scale_fill_manual(values =  rep(col_palette2, 40), na.value = "#ededed", na.translate = T) +
    guides(fill = guide_legend(ncol = 4)) +
    ggtitle("Maíz (blanco o amarillo)") +
    xlab("Eficiencia") +
    theme + map_theme,
  
  cols = 2
)
dev.off()

# Regresar tamano letra al original
theme[[1]][["text"]][["size"]] <- text*1.5