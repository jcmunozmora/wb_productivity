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
# Laptop JC
out <- "/Users/jcmunoz/Library/CloudStorage/OneDrive-UniversidadEAFIT/Projects/2021_WB_Productividad/01_Analisis/BM_productividad/"

datos_ori <- "01_Datos_originales/CNA/Total_nacional(csv)"
#-------------------------------------------------------#
# 1. Organizar datos ----
#-------------------------------------------------------#

# Abrimos base limpia Censo e identificamos encuestas de interes
cna <- readRDS(glue("{datos_raw}/base_cna_all_maq.rds")) %>%
  distinct(cod_dpto, cod_mpio, cod_vereda, encuesta)

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
          landsize=Tplot_area) %>% drop_na(ls2) %>%
      dplyr::select(cod_vereda,encuesta,ls2,landsize)

      # UPAs con capital con y sin imputar
      #k <- prod %>% drop_na(kmin_agro) %>% distinct(cod_vereda, encuesta)
      #k_im <- prod %>% drop_na(kminbar) %>% distinct(cod_vereda, encuesta)
      
      # Agregamos productividad a nivel de vereda
      #prod_vereda <- prod %>% 
      #  group_by(cod_vereda) %>%
      #  summarise(ls2 = mean(ls2)) %>%
      #  ungroup()

# Abrimos datos PCA
#pca <- readRDS(glue("{datos}/CNA/PCA/base_PCA_censo.rds")) %>%
#  janitor::clean_names() %>%
#  dplyr::mutate_at(c("p_depto","p_munic","cod_vereda","encuesta"),as.numeric)

### Create main data
#prod_pca <- prod %>% left_join(pca, 
#                               by = c("p_depto","p_munic","cod_vereda","encuesta")) %>% 
#            dplyr::select("p_depto","p_munic","cod_vereda","encuesta",
#                                       ls2,f01_water:f09_plaques,
#                                       tlu,landsize)

#--------------------------#
# A. Productividad
#--------------------------#

# No tenemos estas preguntas en base limpia, abrimos CNA original y solo tomamos UPAs en muestra final
und_produc_raw <- read_csv(glue("{datos_ori}/S01_15(Unidad_productora).csv")) %>%
  mutate(cod_vereda=as.numeric(COD_VEREDA),encuesta=as.numeric(ENCUESTA))

und_produc <- merge(prod[,c("cod_vereda","encuesta")],und_produc_raw,by=c("cod_vereda","encuesta"))

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
  group_by(cod_vereda,encuesta) %>%
  mutate(area_total = sum(area_sembrada), part = 100*(area_sembrada/area_total),
         test = sum(part), principal = ifelse(part == max(part), 1, 0)) %>%
  dplyr::filter(principal == 1) %>%
  mutate(area_total = sum(area_cosechada), part = 100*(area_cosechada/area_total),
         test = sum(part), principal2 = ifelse(part == max(part), 1, 0)) %>%
  dplyr::filter(principal == 1 | principal2 == 1) %>%
  ungroup() %>%
  distinct(encuesta, .keep_all = T) %>%
  dplyr::select(cod_vereda,encuesta, clasificacion_utilizada)

#--------------------------#
# C. Nuevas Variables ----
#--------------------------#

  fv <- function(x) { ifelse(ifelse(is.na(x),0,x)==1,1,0) }

  fv1 <- function(x) { ifelse(x>=1,1,0) }
  
  mo_obra <- function(x,y) { (ifelse(is.na(x),0,x)/ifelse(is.na(y),0,y))}
  maq <- function(x,y) { ifelse(is.na(x),0,x)/(ifelse(is.na(x),0,x)+ifelse(is.na(y),0,y)) }
  
  # Formas Riego
  ds_cna <- und_produc %>%
            transmute(cod_vereda,encuesta,
              ### Formas de Riego
              f1=fv(P_S11P124_SP1),
              f2=fv1(fv(P_S11P124_SP2)+fv(P_S11P124_SP3)+fv(P_S11P124_SP4)+fv(P_S11P124_SP7)),
              f3=fv1(fv(P_S11P124_SP8)+fv(P_S11P124_SP9)),
              f4=fv1(fv(P_S11P124_SP6)+fv(P_S11P124_SP5)+fv(P_S11P124_SP10)),
              ## Asistencia Técnica
              a1=fv(P_S11P135),
              ## Crédito
              c10=fv(P_S11P136),
              c11=fv(P_S11P136A),
              c2=fv1(fv(P_S11P136B_SP1)+fv(P_S11P136B_SP2)),
              c20=fv(P_S11P136B_SP1),
              c21=fv(P_S11P136B_SP2),
              c3=fv(P_S11P136B_SP3),
              c4=fv1(fv(P_S11P136B_SP4)+fv(P_S11P136B_SP8)),
              c5=ifelse(c11==1 & c2==0 & c3==0 & c4==0,1,0),
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
              mo2=fv(P_S11P140)) %>%
              distinct(cod_vereda, encuesta, .keep_all = T)


  
  
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
# B. Build Data set
#--------------------------#

prod_pca1 <- prod %>%
            left_join(base_ver, by = c("cod_vereda")) %>%
            left_join(base_clima, by = c("cod_vereda")) %>%
            left_join(cultivo, by = c("cod_vereda","encuesta")) %>%
            dplyr::rename(grupo_cultivo = clasificacion_utilizada)

prod_pca1 <- merge(prod_pca1,ds_cna, by = c("cod_vereda","encuesta"))

write_rds(prod_pca1,"/Users/jcmunoz/Library/CloudStorage/OneDrive-UniversidadEAFIT/Projects/2021_WB_Productividad/01_Analisis/BM_Escenarios/DS_Escenarios_productividad.rds")

#-------------------------------------------------------#
# 1. Regresiones PCA y productividad -----
#-------------------------------------------------------#

eq <- formula(ls2 ~ 
                ### Formas de Riego
                f1+f2+f3+f4+
                ## Asistencia Técnica
                a1+
                ## Crédito
                #c1+
                c11+c2+c3+c4+
                #c2+c3+c4+
                ## Manejo Suelos
                s1+s2+s3+s4+
                ## Fuentes de Energia
                e1+e2+e3+
                ## Manejo de Cultivo
                m1+m2+
                # Control de plagas
                p1+p2+p3+
                #p4+
                # Infraestructura
                i1+
                # Mercados
                d1+d2+d3+d5+
                #d4+
                #d5+
                # Mano de Obra
                mo1+mo2 +
                # Vereda
                temp+rain| cod_mpio | 0 | cod_vereda )

#-------------------------------------------------------#
# 1. Main
#-------------------------------------------------------#
reg_small <- lfe::felm(eq , 
                       data = subset(prod_pca1,landsize<=5 ))

reg_medium <- lfe::felm(eq , 
                        data = subset(prod_pca1,landsize>5 & landsize<=20  ))

reg_grande <- lfe::felm(eq , 
                        data = subset(prod_pca1,landsize>20 ))

p <- plot_summs(reg_small,reg_medium,reg_grande, 
                ci_level = 0.95,
                legend.title = "Tamaño \n Finca",
                coefs = c(### Formas de Riego
                  "Riego - Lluvia"="f1",
                  "Riego - Fuentes Naturales"="f2",
                  "Riego - Fuentes Artificiales"="f3",
                  "Riego - Acumulación"="f4",
                  ## Asistencia Técnica
                  "Asist. Técnica"="a1",
                  ## Crédito
                  "Crédito - Solicitud"="c10",
                  "Crédito - Aprobado"="c11",
                  "Crédito - B. Agrario"="c20",
                  "Crédito - Otros Bancos"="c21",
                  "Crédito - Aprobado"="c1",
                  "Crédito - Sect. Financiero"="c2",
                  "Crédito - Cooperativas"="c3",
                  "Crédito - No bancario"="c4",
                  "Crédito - Otras Fuentes"="c5",
                  ## Manejo Suelos
                  "Suelos - Labranza"="s1",
                  "Suelos - Siembra manual"="s2",
                  "Suelos - Conservación"="s3",
                  "Suelos - Rotación"="s4",
                  ## Fuentes de Energia
                  "Energía - Red Eléctrica"="e1",
                  "Energía - Alternativas"="e2",
                  "Energía - Tradicionales"="e3",
                  ## Manejo de Cultivo
                  "Cultivos - Fertilizante"="m1",
                  "Cultivos - Enmienda"="m2",
                  # Control de plagas
                  "Plagas - Control orgánico"="p1",
                  "Plagas - Control químico"="p2",
                  "Plagas - Control biológico"="p3",
                  #"Plagas - Plantas GMO"="p4",
                  # Infraestructura
                  "Infraestructura"="i1",
                  # Mercados
                  "Destino - Autoconsumo"="d1",
                  "Destino - Venta con Intermediario"="d2",
                  "Destino - Venta directa"="d3",
                  "Destino - Mcdo. Internacional"="d4",
                  "Destino - Industria"="d5",
                  # Mano de Obra
                  "Mcdo. Laboral - % Mano obra familiar"="mo1",
                  "Mcdo. Laboral - Jornales adicionales"="mo2"
                ),
                facet.label.pos="bottom",
                scale = TRUE,robust = list("HC0","HC0","HC0"),
                model.names = c("Pequeños \n (<=5ha)", 
                                "Medianos \n (>5ha - <=20)", "Grandes \n (>20ha)"))


tiff(file=paste0(out,"saving_plot0.tiff"),
     width=8, height=5, units="in", res=200)

p  + theme_minimal()+
theme(legend.position="bottom",
      #panel.grid.major=element_blank(),
           panel.grid.minor=element_blank(),
           panel.border=element_blank(),
           axis.line=element_line(),
           text=element_text(size=5),
           legend.title=element_blank(), 
           axis.text=element_text(size=7),
           axis.title=element_text(size=7),
           legend.text = element_text(size = 7),
            axis.text.y = element_text(color = "grey20", size = 7, angle = 0, hjust = 1, vjust = 0, face = "plain"))+guides(colour = guide_legend(override.aes = list(size=0.05)),size=1)+
  ylab("")
  
dev.off()

#### Export

# Tabla descriptivas
# Docx
stargazer::stargazer(reg_small,reg_medium,reg_grande,  
                     header=FALSE, type= 'html',
                     #keep = c("f01_water", "f02_soil", "f03_energy", "f04_assit", "f05_credit",
                    #          "f06_workers", "f07_mag_crops", "f08_others", "f09_plaques",
                    #          "temp", "rain"),
                     digits = 2,
                     dep.var.labels.include = TRUE,
                     model.numbers = TRUE,
                     omit.stat = c("ser", "adj.rsq", "rsq", "f"),
                     label = "reg_prod_pca",
                     dep.var.labels = c("Log(Productividad agropecuaria)"),
                     title = "Principales determinantes de la productividad agropecuaria",
                     column.separate = c(2),
                     #covariate.labels = labels_dependientes, 
                     table.placement = "H", 
                     column.sep.width = "-7pt",
                     df = FALSE,
                     notes = "",
                     notes.append = FALSE,
                     out = glue("04_Tablas/reg_productividad_pca.doc"))


#-------------------------------------------------------#
# 1. Regresiones PCA y productividad -----
#-------------------------------------------------------#

get_reg <- function(i){
  reg_small <- lfe::felm(eq , 
                         data = subset(prod_pca1,landsize<=5 & grupo_cultivo==i))
  
  reg_medium <- lfe::felm(eq , 
                          data = subset(prod_pca1,landsize>5 & landsize<=20 & grupo_cultivo==i ))
  
  reg_grande <- lfe::felm(eq , 
                          data = subset(prod_pca1,landsize>20 & grupo_cultivo==i ))
  
  p <- plot_summs(reg_small,reg_medium,reg_grande, 
             legend.title = "Tamaño \n Finca",
             coefs = c(### Formas de Riego
               "Riego - Lluvia"="f1",
               "Riego - Fuentes Naturales"="f2",
               "Riego - Fuentes Artificiales"="f3",
               "Riego - Acumulación"="f4",
               ## Asistencia Técnica
               "Asist. Técnica"="a1",
               ## Crédito
               "Crédito - Aprobado"="c1",
               "Crédito - Sect. Financiero"="c2",
               "Crédito - Cooperativas"="c3",
               "Crédito - No bancario"="c4",
               ## Manejo Suelos
               "Suelos - Labranza"="s1",
               "Suelos - Siembra manual"="s2",
               "Suelos - Conservación"="s3",
               "Suelos - Rotación"="s4",
               ## Fuentes de Energia
               "Energía - Red Eléctrica"="e1",
               "Energía - Alternativas"="e2",
               "Energía - Tradicionales"="e3",
               ## Manejo de Cultivo
               "Cultivos - Fertilizante"="m1",
               "Cultivos - Enmienda"="m2",
               # Control de plagas
               "Plagas - Control orgánico"="p1",
               "Plagas - Control químico"="p2",
               "Plagas - Control biológico"="p3",
               "Plagas - Plantas GMO"="p4",
               # Infraestructura
               "Infraestructura"="i1",
               # Mercados
               "Destino - Autoconsumo"="d1",
               "Destino - Venta con Intermediario"="d2",
               "Destino - Venta directa"="d3",
               "Destino - Mcdo. Internacional"="d4",
               "Destino - Industria"="d5",
               # Mano de Obra
               "Mcdo. Laboral - % Mano obra familiar"="mo1",
               "Mcdo. Laboral - Jornales adicionales"="mo2"
             ),
             facet.label.pos="bottom",
             scale = TRUE,robust = list("HC0","HC0","HC0"),
             model.names = c("Pequeños \n (<=5ha)", 
                             "Medianos \n (>5ha - <=20)", "Grandes \n (>20ha)"))
  
  p <- p  + theme_minimal()+
    theme(#panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.border=element_blank(),
      axis.line=element_line(),
      text=element_text(size=5),
      legend.title=element_blank(), 
      axis.text=element_text(size=7),
      axis.title=element_text(size=7),
      legend.text = element_text(size = 7),
      axis.text.y = element_text(color = "grey20", size = 7, angle = 0, hjust = 1, vjust = 0, face = "plain"))+guides(colour = guide_legend(override.aes = list(size=0.05)),size=1)+
    ylab("")
  
  
 return(p)
}



  i= unique(prod_pca1$grupo_cultivo)[1]
  tiff(file=paste0(out,"saving_plot",i,".tiff"),
      width=8, height=4, units="in", res=200)
  get_reg(i)
dev.off()


i= unique(prod_pca1$grupo_cultivo)[2]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca1$grupo_cultivo)[3]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca1$grupo_cultivo)[4]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca1$grupo_cultivo)[5]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca1$grupo_cultivo)[6]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca1$grupo_cultivo)[7]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca1$grupo_cultivo)[8]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca1$grupo_cultivo)[9]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca1$grupo_cultivo)[10]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca1$grupo_cultivo)[11]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()

i= unique(prod_pca1$grupo_cultivo)[12]
tiff(file=paste0(out,"saving_plot",i,".tiff"),
     width=8, height=4, units="in", res=200)
get_reg(i)
dev.off()




  










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

