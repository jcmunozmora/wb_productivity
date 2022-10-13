#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 12 dic, 2021
# Regresiones de la productividad a nivel de UPA  
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue,jtools,ggstance,fastDummies)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_raw <- "02_Datos/CNA"
datos <- "02_Datos"
graficas <- "03_Graficas"
options(scipen = 999)

out <- "/Users/jcmunoz/OneDrive - Universidad EAFIT/Projects/2021_WB_Productividad/01_Analisis/BM_productividad/"
datos_ori <- "01_Datos_originales/CNA/Total_nacional(csv)"
#-------------------------------------------------------#
# 1. Organizar datos ----
#-------------------------------------------------------#

# Abrimos base limpia Censo e identificamos encuestas de interes
cna <- readRDS(glue("{datos_raw}/base_cna_all_maq.rds")) %>%
  distinct(cod_dpto, cod_mpio, cod_vereda, encuesta)

# No tenemos estas preguntas en base limpia, abrimos CNA original y solo tomamos UPAs en muestra final
und_produc <- read_csv(glue("{datos_ori}/S01_15(Unidad_productora).csv")) %>% 
  dplyr::filter(ENCUESTA %in% cna$encuesta) 

#--------------------------#
# A. Productividad
#--------------------------#

# Abrimos datos productividad agricola
prod <- haven::read_dta(glue("{datos}/Productividad/base_productividad_hamman.dta")) %>%
      mutate(log_area_sembrada = log(area_sembrada), 
          log_jornales = log(jornales),
          ing_hectarea = revenue/area_sembrada, 
          log_ingreso = log(ing_hectarea),
          log_ing_jornal = log(revenue_j),
          landsize=Tplot_area) %>% drop_na(ls2)

      # UPAs con capital con y sin imputar
      #k <- prod %>% drop_na(kmin_agro) %>% distinct(cod_vereda, encuesta)
      #k_im <- prod %>% drop_na(kminbar) %>% distinct(cod_vereda, encuesta)
      
      # Agregamos productividad a nivel de vereda
      #prod_vereda <- prod %>% 
      #  group_by(cod_vereda) %>%
      #  summarise(ls2 = mean(ls2)) %>%
      #  ungroup()

# Abrimos datos PCA
pca <- readRDS(glue("{datos}/CNA/PCA/base_PCA_censo.rds")) %>%
  janitor::clean_names() %>%
  dplyr::mutate_at(c("p_depto","p_munic","cod_vereda","encuesta"),as.numeric)

### Create main data
prod_pca <- prod %>% left_join(pca, 
                               by = c("p_depto","p_munic","cod_vereda","encuesta")) %>% 
            dplyr::select("p_depto","p_munic","cod_vereda","encuesta",
                                       ls2,f01_water:f09_plaques,
                                       tlu,landsize)
#--------------------------#
# B. Cultivo principal ----
#--------------------------#

# Calculamos principal cultivo por vereda segun area sembrada
cultivo <- readRDS(glue("{datos}/CNA/base_cna_all_maq_cultivos.rds")) %>%
  dplyr::select(cod_vereda, encuesta, tipo_cul_lote, nombre_cultivo,
                uso_suelo, clasificacion_utilizada, area_cosechada, area_sembrada) %>%
  drop_na(tipo_cul_lote, area_sembrada) %>% 
  distinct() %>%
  mutate(encuesta = as.numeric(encuesta))

# Identificamos maiz y arroz por separado
cultivo$clasificacion_utilizada[
              cultivo$nombre_cultivo == "Maíz Blanco" | 
                cultivo$nombre_cultivo == "Maíz Amarillo"] <- "Maíz (blanco o amarillo)"

cultivo$clasificacion_utilizada[cultivo$nombre_cultivo == "Arroz verde"] <- "Arroz"

# Identificar principal cultivo de la UPA: % del area sembrada mas alto de la finca
# Si no queda 1 cultivo claro con % de area sembrada, usamos area cosechada

cultivo <- cultivo %>%
  group_by(encuesta) %>%
  mutate(area_total = sum(area_sembrada), part = 100*(area_sembrada/area_total),
         test = sum(part), principal = ifelse(part == max(part), 1, 0)) %>%
  dplyr::filter(principal == 1) %>%
  mutate(area_total = sum(area_cosechada), part = 100*(area_cosechada/area_total),
         test = sum(part), principal2 = ifelse(part == max(part), 1, 0)) %>%
  dplyr::filter(principal == 1 | principal2 == 1) %>%
  ungroup() %>%
  distinct(encuesta, .keep_all = T) %>%
  dplyr::select(encuesta, clasificacion_utilizada)


#--------------------------#
# C. Nuevas Variables ----
#--------------------------#

  fv <- function(x) { ifelse(is.na(x),0,1) }

  fv1 <- function(x) { ifelse(x>=1,1,0) }
  
  mo_obra <- function(x,y) { (ifelse(is.na(x),0,x)/ifelse(is.na(y),0,y))}
  maq <- function(x,y) { ifelse(is.na(x),0,x)/(ifelse(is.na(x),0,x)+ifelse(is.na(y),0,y)) }
  
  # Formas Riego
  ds_cna <- und_produc %>%
            transmute(encuesta=as.numeric(ENCUESTA),
              ### Formas de Riego
              f1=fv(P_S11P124_SP1),
              f2=fv1(fv(P_S11P124_SP2)+fv(P_S11P124_SP3)+fv(P_S11P124_SP4)+fv(P_S11P124_SP7)),
              f3=fv1(fv(P_S11P124_SP8)+fv(P_S11P124_SP9)),
              f4=fv1(fv(P_S11P124_SP6)+fv(P_S11P124_SP5)+fv(P_S11P124_SP10)),
              ## Asistencia Técnica
              a1=fv(P_S11P135),
              ## Crédito
              c1=fv(P_S11P136A),
              c2=fv1(fv(P_S11P136B_SP1)+fv(P_S11P136B_SP2)),
              c3=fv(P_S11P136B_SP3),
              c4=fv1(fv(P_S11P136B_SP4)+fv(P_S11P136B_SP8)),
              ## Manejo Suelos
              s1=fv1(fv(P_S11P127_SP1)+fv(P_S11P127_SP10)),
              s2=fv(P_S11P127_SP2),
              s3=fv1(fv(P_S11P127_SP3)+fv(P_S11P127_SP4)+fv(P_S11P127_SP5)),
              s4=fv(P_S11P127_SP9),
              ## Fuentes de Energia
              e1=fv(P_S11P133_SP1),
              e2=fv1(fv(P_S11P133_SP3)+fv(P_S11P133_SP4)),
              e3=fv1(fv(P_S11P133_SP2)+fv(P_S11P133_SP6)),
              ## Manejo de Cultivo
              m1=fv1(fv(P_S6P76_SP1)+fv(P_S6P76_SP2)),
              m2=fv(P_S6P76_SP3),
              # Control de plagas
              p1=fv(P_S6P77_SP2),
              p2=fv(P_S6P77_SP3),
              p3=fv1(fv(P_S6P77_SP4)+fv(P_S6P77_SP6)),
              p4=fv(P_S6P77_SP7),
              # Infraestructura
              i1=fv(P_S10P121),
              # Mercados
              d1=fv1(fv(P_S6P61_SP1)+fv(P_S6P61_SP2)),
              d2=fv1(fv(P_S6P61_SP3)+fv(P_S6P61_SP4)+fv(P_S6P61_SP6)+fv(P_S6P61_SP7)),
              d3=fv1(fv(P_S6P61_SP5)+fv(P_S6P61_SP8)),
              d4=fv(P_S6P61_SP9),
              d5=fv(P_S6P61_SP10),
              # Mano de Obra
              mo1=ifelse(mo_obra(P_S11P139,P_S11P138)>=1,1,mo_obra(P_S11P139,P_S11P138)),
              mo2=fv(P_S11P140))
            
#--------------------------#
# B. Build Data set
#--------------------------#

prod_pca1 <- prod_pca %>%
            left_join(cultivo, by = c("encuesta")) %>%
            dplyr::rename(grupo_cultivo = clasificacion_utilizada) %>%
            left_join(ds_cna, by = c("encuesta")) 

#-------------------------------------------------------#
# 1. Regresiones PCA y productividad -----
#-------------------------------------------------------#

get_reg <- function(i){
  reg_small <- lfe::felm(ls2 ~ 
                           f01_water + f02_soil + f03_energy + 
                           f04_assit + P_S11P135 + f06_workers + 
                           f07_mag_crops + f08_others + f09_plaques+
                           tlu| p_depto , 
                         data = subset(prod_pca1,landsize<=5 & grupo_cultivo==i))
  
  reg_medium <- lfe::felm(ls2 ~ 
                            f01_water + f02_soil + f03_energy + 
                            f04_assit + P_S11P135 + f06_workers + 
                            f07_mag_crops + f08_others + f09_plaques+
                            tlu| p_depto , 
                          data = subset(prod_pca1,landsize>5 & landsize<=20 & grupo_cultivo==i ))
  
  reg_grande <- lfe::felm(ls2 ~ 
                            f01_water + f02_soil + f03_energy + 
                            f04_assit + P_S11P135 + f06_workers + 
                            f07_mag_crops + f08_others + f09_plaques+
                            tlu| p_depto , 
                          data = subset(prod_pca1,landsize>20 & grupo_cultivo==i ))
  
 
  
  p <- plot_summs(reg_small,reg_medium,reg_grande, 
             legend.title = "Tamaño \n Finca",
             coefs = c("Acceso y uso del agua" = "f01_water", 
                       "Prácticas del suelo" = "f02_soil",
                       "Fuentes de energía" = "f03_energy",
                       "Asistencia técnica" ="f04_assit",
                       "Crédito agropecuario" ="f05_credit",
                       "Mano de obra"="f06_workers", 
                       "Manejo de cultivos \n (fertilizantes)"= "f07_mag_crops",
                       "Manejo de desechos \n y recursos naturales"="f08_others",
                       "Control de plagas"="f09_plaques",
                       "Ganado"="tlu"
             ),
             facet.label.pos="bottom",
             scale = TRUE,robust = list("HC0","HC0","HC0"),
             model.names = c("Pequeños \n (<=5ha)", 
                             "Medianos \n (>5ha - <=20)", "Grandes \n (>20ha)"))
  
  
 return(p)
}





  i= unique(prod_pca$grupo_cultivo)[1]
  tiff(file=paste0(out,"saving_plot",i,".tiff"),
      width=8, height=4, units="in", res=200)
  get_reg(i)
dev.off()


i= unique(prod_pca$grupo_cultivo)[2]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca$grupo_cultivo)[3]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca$grupo_cultivo)[4]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca$grupo_cultivo)[5]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca$grupo_cultivo)[6]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca$grupo_cultivo)[7]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca$grupo_cultivo)[8]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca$grupo_cultivo)[9]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca$grupo_cultivo)[10]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca$grupo_cultivo)[11]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca$grupo_cultivo)[12]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()




  






#--------------------------#
# B. Region y clima ----
#--------------------------#

# Abrimos datos de regiones
base_ver <- readRDS(glue("{datos}/Productividad/base_productividad_vereda.rds")) %>%
  dplyr::select(region, provincia, cod_provincia, cod_dpto, cod_mpio, cod_vereda) %>%
  distinct(cod_vereda, .keep_all = T)

# Abrimos datos de clima
base_clima <- readRDS(glue("{datos}/base_clima_veredas_2013.rds")) %>%
  distinct(cod_vereda, .keep_all = T)

#--------------------------#
# C. Cultivo principal ----
#--------------------------#

# Calculamos principal cultivo por vereda segun area sembrada
cultivo <- readRDS(glue("{datos}/CNA/base_cna_all_maq_cultivos.rds")) %>%
  dplyr::select(cod_vereda, encuesta, tipo_cul_lote, nombre_cultivo,
                uso_suelo, clasificacion_utilizada, area_cosechada, area_sembrada) %>%
  drop_na(tipo_cul_lote, area_sembrada) %>% 
  distinct() %>%
  mutate(encuesta = as.numeric(encuesta))

# Identificamos maiz y arroz por separado
cultivo$clasificacion_utilizada[cultivo$nombre_cultivo == "Maíz Blanco" | cultivo$nombre_cultivo == "Maíz Amarillo"] <- "Maíz (blanco o amarillo)"
cultivo$clasificacion_utilizada[cultivo$nombre_cultivo == "Arroz verde"] <- "Arroz"

# Identificar principal cultivo de la UPA: % del area sembrada mas alto de la finca
# Si no queda 1 cultivo claro con % de area sembrada, usamos area cosechada
cultivo <- cultivo %>%
  group_by(cod_vereda) %>%
  mutate(area_total = sum(area_sembrada), part = 100*(area_sembrada/area_total),
         test = sum(part), principal = ifelse(part == max(part), 1, 0)) %>%
  dplyr::filter(principal == 1) %>%
  mutate(area_total = sum(area_cosechada), part = 100*(area_cosechada/area_total),
         test = sum(part), principal2 = ifelse(part == max(part), 1, 0)) %>%
  dplyr::filter(principal == 1 | principal2 == 1) %>%
  ungroup() %>%
  distinct(cod_vereda, .keep_all = T) %>%
  dplyr::select(cod_vereda, clasificacion_utilizada)

# Unimos datos de productividad y pca con regiones, cultivo principal y clima
data_reg <- prod_pca %>%
  left_join(base_ver, by = c("cod_vereda")) %>%
  left_join(base_clima, by = c("cod_vereda")) %>%
  left_join(cultivo, by = c("cod_vereda")) %>%
  dplyr::rename(grupo_cultivo = clasificacion_utilizada) %>%
  dplyr::select(region, provincia, cod_provincia, cod_dpto, cod_mpio, 
                cod_vereda, grupo_cultivo, everything())




plot_summs(fit, fit2, scale = TRUE, plot.distributions = TRUE)

# Organizar tabla en HTML
labels_dependientes = c("Acceso y uso del agua", "Prácticas del suelo", "Fuentes de energía", 
                        "Asistencia técnica", "Acceso al crédito agropecuario", "Mano de obra", 
                        "Manejo de cultivos (fertilizantes)", "Manejo de desechos y recursos naturales", 
                        "Prácticas de control de plagas", "Temperatura media","Precipitación media")

# Tabla descriptivas
# Docx
stargazer::stargazer(reg,  
                     header=FALSE, type= 'html',
                     keep = c("f01_water", "f02_soil", "f03_energy", "f04_assit", "f05_credit",
                              "f06_workers", "f07_mag_crops", "f08_others", "f09_plaques",
                              "temp", "rain"),
                     digits = 2,
                     dep.var.labels.include = TRUE,
                     model.numbers = TRUE,
                     omit.stat = c("ser", "adj.rsq", "rsq", "f"),
                     label = "reg_prod_pca",
                     dep.var.labels = c("Log(Productividad agropecuaria)"),
                     title = "Principales determinantes de la productividad agropecuaria",
                     column.separate = c(2),
                     covariate.labels = labels_dependientes, 
                     table.placement = "H", 
                     column.sep.width = "-7pt",
                     df = FALSE,
                     notes = "",
                     notes.append = FALSE,
                     out = glue("04_Tablas/reg_productividad_pca.doc"))

# Latex
stargazer::stargazer(reg,  
                     header=FALSE, type= 'latex',
                     keep = c("f01_water", "f02_soil", "f03_energy", "f04_assit", "f05_credit",
                              "f06_workers", "f07_mag_crops", "f08_others", "f09_plaques",
                              "temp", "rain"),
                     digits = 2,
                     dep.var.labels.include = TRUE,
                     model.numbers = TRUE,
                     omit.stat = c("ser", "adj.rsq", "rsq", "f"),
                     label = "reg_prod_pca",
                     dep.var.labels = c("Productividad agrícola (Log)"),
                     title = "Principales determinantes de la productividad agrícola",
                     # column.labels = c("First group"),
                     column.separate = c(2),
                     covariate.labels = labels_dependientes, 
                     table.placement = "H", 
                     column.sep.width = "-7pt",
                     df = FALSE,
                     # se = list(reg1[["rse"]], reg2[["rse"]], reg11[['rse']], reg21[['rse']]),
                     # add.lines = list(c("Media de la variable dependiente", mean_dep1, mean_dep2, mean_dep11, mean_dep21),
                     #                  c("Media mujeres productoras (\\%)", mean_ind1, mean_ind2, mean_ind11, mean_ind21),
                     #                  c("Periodo", "2014", "2014", "2014", "2014")),
                     notes = "",
                     notes.append = FALSE,
                     out = glue("04_Tablas/reg_productividad_pca.tex"))

