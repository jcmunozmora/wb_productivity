#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 13 enero, 2022
# Mapas ubicacion geografica UPAs con y sin maquinaria, distribución productividad
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

#-------------------------------------------------------#
# 1. Abrir datos ----
#-------------------------------------------------------#

# Abrimos datos CNA con y sin maquinaria
cna_maq <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds"))

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

# Abrimos datos productividad agricola
prod <- haven::read_dta(glue("{datos}/Productividad/base_productividad_hamman.dta")) %>%
  drop_na(ls2) %>% dplyr::rename(cod_dpto = p_depto, cod_mpio = p_munic)

# Abrir tamano total de UPAs por municipio
cna_gen <- readRDS(glue("{datos}/CNA/base_upas.rds")) %>%
  dplyr::select(p_munic, cod_vereda, encuesta, p_s5pautos) %>%
  dplyr::rename(cod_mpio = p_munic, area_upa = p_s5pautos) %>%
  drop_na(area_upa) %>%
  group_by(cod_mpio) %>%
  summarise(area_upa_ori = sum(area_upa)) %>%
  ungroup() %>%
  mutate(area_upa_ori = area_upa_ori/10000, cod_mpio = as.numeric(cod_mpio))

#--------------------------#
# A. Area con maquinaria ----
#--------------------------#

# % del area agropecuaria con y sin maquinaria
area_maq <- cna_maq %>%
  dplyr::select(encuesta, tiene_maq, area_agropecuario, area_upa) %>%
  distinct() %>%
  group_by(tiene_maq) %>%
  summarise(area_agropecuario = sum(area_agropecuario, na.rm = T),
            area_upa = sum(area_upa, na.rm = T)) %>%
  ungroup() %>%
  mutate(total_upa = sum(area_upa), total_agro = sum(area_agropecuario),
         part_upa = 100*(area_upa/total_upa), 
         part_agro = 100*(area_agropecuario/total_agro))
  
area_maq$tiene_maq[area_maq$tiene_maq == 1] <- "Con maquinaria"
area_maq$tiene_maq[area_maq$tiene_maq == 0] <- "Sin maquinaria"

area_maq <- area_maq %>% 
  dplyr::select(tiene_maq, starts_with("part")) %>%
  pivot_longer(cols = c("part_upa", "part_agro"), values_to = "part", names_to = "area")

area_maq$area[area_maq$area == "part_upa"] <- "Área UPAs"
area_maq$area[area_maq$area == "part_agro"] <- "Área agropecuaria"

ggplot(area_maq, aes(x = area, y = part, fill = factor(tiene_maq))) +
  geom_bar(stat = "identity", position=position_dodge(), 
           alpha = 0.8, aes(fill = factor(tiene_maq))) +
  geom_text(aes(label = round(part, 1)), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=text/2) +
  ylab("Porcentaje (%)\n") +
  xlab("") +
  coord_cartesian(ylim = c(0, 70)) +
  scale_fill_manual(values = c("#6FBDD1", "#9983B5")) +
  theme
ggsave(glue("{graficas}/prc_area_con_sin_maquinaria.pdf"), height = h, width = w, dpi = d)

rm(area_maq)
  
#-------------------------------------------------------#
# 2. Ubicacion UPAs con maquinaria ----
#-------------------------------------------------------#

#--------------------------#
# A. Calculos ----
#--------------------------#

# Contamos % de UPA por municipio que tienen precios de maquinaria sin imputar
prod_maq <- prod %>% 
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, kmin_agro, kminbar) %>%
  mutate(tiene_maq = ifelse(!is.na(kmin_agro), 1, 0),
         tiene_maq_im = ifelse(!is.na(kminbar), 1, 0)) 

# Contamos las UPA por municipio que tienen (o no) maquinaria
geo_upa <- cna_maq %>%
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, area_upa) %>%
  distinct() %>%
  mutate(encuesta = as.numeric(encuesta), upas = 1) %>%
  left_join(prod_maq, by = c("cod_dpto", "cod_mpio", "cod_vereda", "encuesta")) %>%
  drop_na(area_upa) %>%
  mutate(tiene_maq = ifelse(is.na(tiene_maq), 0, tiene_maq),
         tiene_maq_im = ifelse(is.na(tiene_maq_im), 0, tiene_maq_im),
         area_maq = ifelse(tiene_maq == 1, area_upa, 0), 
         area_maq_im = ifelse(tiene_maq_im == 1, area_upa, 0)) %>%
  group_by(cod_dpto, cod_mpio) %>%
  summarise(area_maq = sum(area_maq), area_maq_im = sum(area_maq_im), 
            area_upa = sum(area_upa), upas = sum(upas)) %>%
  ungroup() %>%
  mutate(part = 100*(area_maq/area_upa), part_im = 100*(area_maq_im/area_upa))

# Emparejamos # de UPAs, area total mpio y su maquinaria con shp de municipios
# Borramos San Andres y Providencia
mpios_upa <- mpios %>% 
  left_join(geo_upa, by = "cod_mpio") %>% 
  left_join(cna_gen, by = "cod_mpio") %>%
  dplyr::filter(cod_mpio != 88001 & cod_mpio != 88564) %>%
  mutate(part = ifelse(is.na(part), 0, part), part_im = ifelse(is.na(part_im), 0, part_im),
         upas = upas/1000, area_upa = area_upa/10000, part_area = 100*(area_upa/area_upa_ori),
         part_loss = 100 - part_area)

#--------------------------#
# A. UPAS por mpio ----
#--------------------------#

# Partir datos en percentiles
classes <- 6
q0 <- quantile(mpios_upa$upas, na.rm=T, probs = seq(0, 1, length.out = classes + 1))
mpios_upa$q_value <- cut(mpios_upa$upas, breaks = q0, include.lowest = T, dig.lab = 2)
table(mpios_upa$q_value)

col_palette <- c("#6FBDD1", "#65A8D0", "#7090C6", "#8476B1", "#935A90", "#974066")

# Distribucion espacial del numero de UPAs
ggplot() +
  geom_sf(data = mpios_upa, aes(fill = q_value), color = "#2b2b2b", size = 0.05, alpha = 0.65) +
  geom_sf(data = region, color = "#2b2b2b", size = 0.35, alpha = 1, fill = NA) +
  geom_text(data = region, aes(x_reg, y_reg, label = Subregion), size = 5, alpha = 1, color = "black") +
  scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = T) +
  guides(fill = guide_legend(ncol = 4)) +
  xlab("Número de UPAs por municipio (miles)") +
  theme + map_theme
ggsave(glue("{graficas}/mapa_upas_mpio.svg"), height = h, width = w*0.8, dpi = d)


# ggsave(glue("{graficas}/mapa_upas_mpio.pdf"), height = h, width = w*0.8, dpi = d)

# % de Area total de UPAs por municipio con informacion
q1 <- quantile(mpios_upa$part_loss, na.rm=T, probs = seq(0, 1, length.out = classes + 1))
mpios_upa$q_value1 <- cut(mpios_upa$part_loss, breaks = q1, include.lowest = T, dig.lab = 3)
table(mpios_upa$q_value1)

ggplot() +
  geom_sf(data = mpios_upa, aes(fill = q_value1), color = "#2b2b2b", size = 0.05, alpha = 0.65) +
  geom_sf(data = region, color = "#2b2b2b", size = 0.35, alpha = 1, fill = NA) +
  geom_text(data = region, aes(x_reg, y_reg, label = Subregion), size = 5, alpha = 1, color = "black") +
  scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = T) +
  guides(fill = guide_legend(ncol = 4)) +
  xlab("Pérdida de muestra como % del área de las UPA") +
  theme + map_theme
ggsave(glue("{graficas}/mapa_upas_area_filtrada_mpio.svg"), height = h, width = w*0.8, dpi = d)
# ggsave(glue("{graficas}/mapa_upas_area_filtrada_mpio.pdf"), height = h, width = w*0.8, dpi = d)

#--------------------------#
# B. Maquinaria ----
# Con y sin imputar
#--------------------------#

# Partir datos en percentiles
mpios_upa$part[mpios_upa$part == 0] <- NA
mpios_upa$part_im[mpios_upa$part_im == 0] <- NA
q2 <- quantile(mpios_upa$part, na.rm=T, probs = seq(0, 1, length.out = classes + 1))
q3 <- quantile(mpios_upa$part_im, na.rm=T, probs = seq(0, 1, length.out = classes + 1))
mpios_upa$q_value2 <- cut(mpios_upa$part, breaks = q2, include.lowest = T, dig.lab = 3)
mpios_upa$q_value3 <- cut(mpios_upa$part_im, breaks = q3, include.lowest = T, dig.lab = 3)

# Aumentar tamano letra
theme[[1]][["text"]][["size"]] <- 20

# pdf(glue("{graficas}/mapa_upas_maquinaria_mpio.pdf"), height = h*0.8, width = w*1.25)
svg(glue("{graficas}/mapa_upas_maquinaria_mpio.svg"), height = h*1, width = w*1.50)
Rmisc::multiplot(
  
  # Maquinaria
  ggplot() +
    geom_sf(data = mpios_upa, aes(fill = q_value2), color = "#2b2b2b", size = 0.05, alpha = 0.65) +
    geom_sf(data = region, color = "#2b2b2b", size = 0.35, alpha = 1, fill = NA) +
    geom_text(data = region, aes(x_reg, y_reg, label = Subregion), size = 5, alpha = 1, color = "black") +
    scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = T) +
    guides(fill = guide_legend(ncol = 4)) +
    xlab("% del área agropecuaria con maquinaria") +
    theme + map_theme,
  
  # Maquinaria imputada
  ggplot() +
    geom_sf(data = mpios_upa, aes(fill = q_value3), color = "#2b2b2b", size = 0.05, alpha = 0.65) +
    geom_sf(data = region, color = "#2b2b2b", size = 0.35, alpha = 1, fill = NA) +
    geom_text(data = region, aes(x_reg, y_reg, label = Subregion), size = 5, alpha = 1, color = "black") +
    scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = T) +
    guides(fill = guide_legend(ncol = 4)) +
    xlab("% del área agropecuaria con maquinaria (imputación)") +
    theme + map_theme,
  
  cols = 2
)
dev.off()

# Regresar tamano letra al original
theme[[1]][["text"]][["size"]] <- text*1.5

# ggsave(glue("{graficas}/mapa_upas_maq_mpio.pdf"), height = h, width = w*0.8, dpi = d)
# ggsave(glue("{graficas}/mapa_upas_maq_imputada_mpio.pdf"), height = h, width = w*0.8, dpi = d)

rm(mpios_upa, prod_maq, geo_upa, cna_maq)

#-------------------------------------------------------#
# 3. Distribucion productividad ----
#-------------------------------------------------------#

# Calculamos productividad promedio por municipio
prod_mpio <- prod %>%
  group_by(cod_dpto, cod_mpio) %>%
  summarise(ls2 = mean(ls2, na.rm = T)) %>%
  ungroup()

# Emparejamos # de UPAs y su maquinaria con shp de municipios, borramos San Andres y Providencia
mpios_prod <- mpios %>% left_join(prod_mpio, by = c("cod_mpio")) %>% 
  dplyr::filter(cod_mpio != 88001 & cod_mpio != 88564) 

# Partir datos en percentiles
classes <- 6
q1 <- quantile(mpios_prod$ls2, na.rm=T, probs = seq(0, 1, length.out = classes + 1))
mpios_prod$q_value <- cut(mpios_prod$ls2, breaks = q1, include.lowest = T, dig.lab = 2)

# Percentiles
p10 <- quantile(mpios_prod$ls2, 0.10, na.rm = T) 
p90 <- quantile(mpios_prod$ls2, 0.90, na.rm = T) 
100*(p90/p10)

# Distribucion espacial del numero de UPAs
ggplot() +
  geom_sf(data = mpios_prod, aes(fill = q_value), color = "#2b2b2b", size = 0.05, alpha = 0.65) +
  geom_sf(data = region, color = "#2b2b2b", size = 0.35, alpha = 1, fill = NA) +
  geom_text(data = region, aes(x_reg, y_reg, label = Subregion), size = 5, alpha = 1, color = "black") +
  scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = T) +
  guides(fill = guide_legend(ncol = 4)) +
  xlab("Log. de la productividad agropecuaria") +
  theme + map_theme
ggsave(glue("{graficas}/mapa_productividad_mpio.svg"), height = h, width = w*0.8, dpi = d)

# Productividad departamental
dpto <- readxl::read_excel("01_Datos_originales/DANE/base_nombre_regiones.xlsx") %>%
  dplyr::rename(cod_dpto = nivel_value, nom_dpto = nivel_label)

prod_dpto <- prod %>%
  left_join(dpto, by = "cod_dpto") %>%
  group_by(cod_dpto, nom_dpto) %>%
  summarise(ls2 = mean(ls2, na.rm = T)) %>%
  ungroup()

prod_reg <- prod %>%
  left_join(dpto, by = "cod_dpto") %>%
  group_by(region) %>%
  summarise(ls2 = mean(ls2, na.rm = T)) %>%
  ungroup()

writexl::write_xlsx(prod_dpto, glue("{datos}/CNA/limpieza_censo/productividad_dpto.xlsx"))
writexl::write_xlsx(prod_reg, glue("{datos}/CNA/limpieza_censo/productividad_region.xlsx"))

# Diferencia entre el mas y el menos productivo
dpto_dif <- 100*(max(prod_dpto$ls2)/min(prod_dpto$ls2))
