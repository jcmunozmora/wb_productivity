#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 12 de julio, 2022
# Regresiones: Determinantes de la productividad agricola
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, lfe, ggstance, jtools, broom.mixed)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
graficas <- "03_Graficas"
options(scipen = 999)

# Tamanos graficas (ancho, alto, texto, resolucion)
w <- 8*1.6
h <- 5*3
text <- 14
d <- 200
s <- 1.2
a_line <- 0.6
a_dot <- 0.7

#-------------------------------------------------------#
# 0. Funciones ----
#-------------------------------------------------------#

# Regresiones por tipo de productividad agricola, tamano y grupo de cultivo
fun_reg <- function(data, reg_def, tag, sample = NA){
  
  # reg_def <- "Productividad agregada"
  # tag <- 'All'
  # data <- data_prod
 
  print(glue("Reg {reg_def} - {tag}"))
  
  # Definir formula de productividad (Cobb-Douglas o rendimiento)
  if(reg_def == "Productividad agregada"){
    formula <- prod_cobb
  } else {
    formula <- prod_ren
  }
  
  # Filtrar (o no) por cadena productiva
  if(tag != 'All'){
    data <- data[data$clasificacion_utilizada == tag,]
  }
  
  # Filtrar por tamano tierra
  micro <- lfe::felm(formula, data = data %>% dplyr::filter(tipo_upa == "Microfundio")) 
  small <- lfe::felm(formula, data = data %>% dplyr::filter(tipo_upa == "Pequeña"))
  med <- lfe::felm(formula, data = data %>% dplyr::filter(tipo_upa == "Mediana"))
  large <- lfe::felm(formula, data = data %>% dplyr::filter(tipo_upa == "Grande"))
  
  regs_list <- list(micro, small, med, large)
  
  # Identificar variables que tienen al menos 1 coeficiente significativo al 95%
  regs <- lapply(list(micro, small, med, large), function(x){
    data <- broom::tidy(x) %>% dplyr::filter(p.value < 0.05)
    return(data)
  }) %>% bind_rows() %>% distinct(term) %>% unlist() %>% as.character()
  
  coefs <- c(
    # Riego
    "Riego no mecanizado"="riego",
    "Riego - Lluvia"="f1",
    "Riego - Fuentes Naturales"="f2",
    "Riego - Fuentes Artificiales"="f3",
    "Riego - Acumulación"="f4",
    # Asistencia Técnica
    "Asist. Técnica"="a1",
    # Crédito
    "Necesidad de financiamiento"="c10",
    "Crédito - Solicitado y no aprobado (formal)"="no_aprob",
    
    "Crédito - B. Agrario"="agrario",
    "Crédito - Otros Bancos"="otros_ban",
    "Crédito - Cooperativas"="coo",
    "Crédito - ONG's"="ong",
    "Crédito - Gobierno"="gob",
    "Crédito - Fuente internacional"="int",
    "Acceso a crédito bancario" = 'cred1', 
    # "Crédito - B. Agrario"="c20",
    # "Crédito - Otros Bancos"="c21",
    # "Crédito - Bancario"="c2",
    # "Crédito - Cooperativas"="c3",
    # "Crédito - No bancario"="c4",
    # "Crédito - Otras Fuentes"="c5",
    # 'Crédito - Solicitado no Aprobado' = 'no_cred',
    # Manejo Suelos
    "Suelos"="Conserv. Suelos - ",
    "Suelos - Labranza"="s1",
    "Suelos - Siembra manual"="s2",
    "Suelos - Conservación"="s3",
    "Suelos - Rotación"="s4",
    # Fuentes de Energia
    "Energía - Solo Red Eléctrica" ="energia",
    "Energía - Red Eléctrica"="e1",
    "Energía - Alternativas"="e2",
    "Energía - Tradicionales"="e3",
    # Manejo de Cultivo
    "Cultivos - Fertilizante"="m1",
    "Cultivos - Enmienda"="m2",
    # Control de plagas
    "Plagas - Control orgánico"="p1",
    "Plagas - Control químico"="p2",
    "Plagas - Control biológico"="p3",
    "Plagas - Control orgánico o biológico" ="plagas",
    # Infraestructura
    "Infraestructura"="i1",
    # Mercados
    # "Destino - Autoconsumo"="d1",
    # "Destino - Venta con Intermediario"="d2",
    # "Destino - Venta directa"="d3",
    # "Destino - Mcdo. Internacional"="d4",
    # "Destino - Industria"="d5",
    # Mano de Obra
    "Mcdo. Laboral - % Mano obra familiar"="mo1",
    "Mcdo. Laboral - Jornales adicionales"="mo2",
    # Caracteristicas productor
    "Edad jefe hogar"= 'edad',
    "Años educación jefe hogar"= 'educ',
    'Sexo jefe de hogar (mujer)'= 'sexoMujer',
    # Entorno
    'Distancia a mercado mayorista'= 'dismdo',
    'Distancia a mercado alimentos'= 'distancia_mercado',
    # Agroecologicas
    'Temperatura'= 'temp',
    'Precipitaciones'= 'rain',
    'Elevación' = 'elevation',
    # Capacidad institucional
    'Transferencias SGP (per cápita)' = 'sgp_pc',
    'Índice desempeño municipal'= 'ind_desempeno_int',
    'Inversión municipal per cápita' = 'inv_mpio_pc',
    # Estructura economica
    'Indice diversidad económica' = 'ind_div_econ',
    'Valor agregado'= 'valor_agregado'
  )
  
  coefs <- coefs[coefs %in% regs]
  
  # Graficar
  tag2 <- gsub("\\(rendimiento.*", "", reg_def)
  tag2 <- gsub(" ", "_", tag2)
  tag2 <- str_to_lower(tag2)
  
  if(!is.na(sample)){
    tag3 <- "_full_sample"
  } else {
    tag3 <- ""
  }

  # jpeg(glue("{graficas}/fe_region_dpto/determinantes_{tag2}_{tag}{tag3}.jpeg"), width = w*1000, height = h*1000, units="in", res = d)
  # tiff(file = glue("{graficas}/fe_region_dpto/determinantes_{tag2}_{tag}{tag3}.tiff"), width = w, height = h, units="in", res = d)
  tiff(file = glue("{graficas}/fe_region_dpto/determinantes_{tag2}_{tag}{tag3}_sindestino.tiff"), width = w*1.6, height = h, units="in", res = d)
    p <- plot_summs(micro, small, med, large,
             ci_level = 0.95,
             point.size = 4,
             legend.title = glue("UPA - {reg_def}"),
             colors = c("#6FBDD1", "#007B87", "#9983B5", "#D36D59"),
             coefs = coefs,
             facet.label.pos="bottom",
             scale = TRUE, 
             robust = list("HC0","HC0","HC0", "HC0"),
             model.names = c("Microfundio (< 5ha)", "Pequeña (5-20ha)", "Mediana (20-100ha)", "Grande (>100ha)")) + 
    ggtitle(glue("UPA - {reg_def}"))+
    theme_minimal(base_size = text*1.5) +
    theme(legend.position="bottom",
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line(),
          # text=element_text(size=text*0.8),
          text=element_text(size=text*1.5),
          legend.title=element_blank(), 
          axis.text=element_text(size=text*1.5),
          axis.title=element_text(size=text*1.5),
          legend.text = element_text(size = text*1.6),
          axis.text.y = element_text(color = "grey20", size = text*1.8, angle = 0, hjust = 1, vjust = 0, face = "plain")) + 
    guides(colour = guide_legend(override.aes = list(size = 1.5), nrow = 1, byrow = TRUE), size = 1.5)+
    ylab("") +
    xlab("Coeficiente")
  
  plot(p)
  dev.off()
  
  return(regs_list)

}

#-------------------------------------------------------#
# 1. Regresiones ----
#-------------------------------------------------------#

# Formulas estimaciones
prod_cobb <- formula(ls2 ~
                       # Formas de Riego
                       # f1+f2+
                       #f1+
                       riego+
                       # f4+
                       # Asistencia Técnica
                       a1+
                       # Crédito
                        #c10 +
                       # c11+
                       c10 + cred1 +
                       # cred_aprob +
                       # agrario + otros_ban + coo +
                       #c2+
                       #c20+
                       #c20+
                       #c3+
                       #c5+
                       #no_cred +
                       #c21+
                       #c3+
                       #c4 + 
                       # Manejo Suelos
                       #suelos+
                       #s1+s2+s3+s4+
                       # Fuentes de Energia
                       energia+
                       #e1+e2+e3+
                       # Manejo de Cultivo
                       #suelos+
                       #m1+
                       #m2+
                       # Control de plagas
                       plagas+
                       #p1+p2+p3+
                       # Infraestructura
                       #i1+
                       # Mercados
                       d1+d2+d3+d5+
                       # Mano de Obra
                       #mo1+
                       # mo2 +
                       # Caracteristicas productor
                       #edad2 + educ +
                       # Entorno
                       #dismdo + 
                      #distancia_mercado +
                       # Agroecologicas
                       temp + rain + elevation +
                       # Capacidad institucional
                       sgp_pc + ind_desempeno_int + inv_mpio_pc +
                       # Estructura economica
                       #valor_agregado + 
                       ind_div_econ | cod_dpto + region | 0 | cod_mpio)

prod_ren <- formula(rend_cos ~
                      # Formas de Riego
                      f1+f2+f3+f4+
                      # Asistencia Técnica
                      a1+
                      # Crédito
                      # c11+c2+
                      c3+c4+
                      # Manejo Suelos
                      s1+s2+s3+s4+
                      # Fuentes de Energia
                      #e1+e2+e3+
                      e4_no+
                      # Manejo de Cultivo
                      m1+
                      #m2+
                      # Control de plagas
                      p1+p2+p3+
                      # Infraestructura
                      #i1+
                      # Mercados
                      d1+d2+d3+d5+
                      # Mano de Obra
                      mo1+
                      # mo2 +
                      # Caracteristicas productor
                      edad2 + educ +
                      # Entorno
                      #dismdo + 
                      #distancia_mercado +
                      # Agroecologicas
                      temp + rain + elevation +
                      # Capacidad institucional
                      sgp_pc + ind_desempeno_int + inv_mpio_pc +
                      # Estructura economica
                      ind_div_econ + valor_agregado | cod_dpto + region | 0 | cod_mpio)

#--------------------------#
# A. Muestra productividad agregada ----
# Basada en estimaciones de Hamman et al (2019)
#--------------------------#

# Abrimos base a nivel de UPA-jefe hogar con caracteristicas del productor, entorno, agroecologicas, etc 
data_prod <- readRDS(glue("{datos}/CNA/prod_caracteristicas_upa2.rds")) %>%
  # SGP per capita en pesos, distancia a mercado mayorista en metros
  mutate(sgp_pc = sgp_pc*1000000, edad2 = edad*edad, dismdo = dismdo*1000,
         no_aprob = ifelse(c10 == 1 & c11 == 0, 1, 0),
         c11 = ifelse(c4 == 1, 0, c11),
         c11 = ifelse((agrario == 1 | otros_ban == 1 | coo == 1 | ong == 1 | gob == 1 | int == 1) & (c4 == 0) & (c11 == 1), 1, 0),
         all_cred = agrario + otros_ban + coo,
         agrario = ifelse(agrario == 1 & all_cred == 1, 1, 0),
         otros_ban = ifelse(otros_ban == 1 & all_cred == 1, 1, 0),
         coo = ifelse(coo == 1 & all_cred == 1, 1, 0),
         ong = ifelse(ong == 1 & all_cred == 1, 1, 0),
         gob = ifelse(gob == 1 & all_cred == 1, 1, 0),
         int = ifelse(int == 1 & all_cred == 1, 1, 0),
         cred1 = ifelse(all_cred >= 1 & c4 == 0, 1, 0),
         cred2 = ifelse(all_cred >= 2 & c4 == 0, 1, 0),
         no_cred=ifelse(c4==0,c20+c21,0),
         c3=ifelse(c4==0 & no_cred==0,c3,0),
         c20=ifelse(c20==1 & c21==0 & c3==0 & c4==0 & c5==0,1,0),
         c2=ifelse(c20==1|c21==1,1,0),
         riego=ifelse(f2==1,1,0),
         c_fina=ifelse(f2==1,1,0),
         suelos=ifelse(s1+s2+s4>=1,1,0),
         energia=ifelse(e4_no==0 & e1==1 & e2==0 & e3==0,1,0),
         plagas=ifelse((p1==0 & p3==0 & p4==0) & p2==1,0,1)
         #c4=ifelse(c11==1 & c20==0 & c21==0 & c3==0,c4+c5,0),
         #c4=ifelse(c4==0 & c5==1,1,c4),
         #no_cred = ifelse(c10 == 1 & c11==0, 1, 0),
         #no_cred = ifelse(c11 == 1 & c2==0  & c3==0, 1, 0),
         # c11 = ifelse((c4 == 1) & (agrario == 1 | otros_ban == 1 | coo == 1 | ong == 1 | gob == 1 | int == 1), 0, c11)
         # c11=ifelse((c5 == 1 | c4 == 1) & (c20==0 & c21==0 & c3==0), 0, c10),
         # c11=ifelse((c5==1) & (c20==0 & c21==0 & c3==0 & c4==0),0,1),
         )

# Exportar datos
dir <- getwd()
dir <- gsub("BM_productividad", "BM_Escenarios", dir)

data_prod_clean <- data_prod %>%
  dplyr::select(cod_dpto,cod_vereda, region, tipo_upa, ls2, 
                area_upa,rend_cos,area_sembrada,cant_cosecha,
                f1:mo2,
                no_cred:plagas,
                df_desemp_fisc, sgp_pc, riego,
                a1, c10, cred1, energia, suelos, plagas, d1, d2, d3, d5, temp, rain, elevation, ind_desempeno_int, inv_mpio_pc,
                ind_div_econ, f1:mo2)

names(data_prod_clean)

saveRDS(data_prod_clean, glue("{dir}/DS_Escenarios_productividad.rds"))

##### -------- 
# Fix productiv

# Agregado
all <- fun_reg(data = data_prod, reg_def = "Productividad agregada", tag = 'All')

# Maiz
maiz <- fun_reg(data = data_prod, reg_def = "Productividad agregada", tag = 'Maíz (blanco o amarillo)')

# Arroz
arroz <- fun_reg(data = data_prod, reg_def = "Productividad agregada", tag = 'Arroz')

# Productividad como rendimiento de la tierra
# fun_reg(data = data_prod, reg_def = "Rendimiento de la tierra (rendimiento/ha)", tag = 'All')
# fun_reg(data = data_prod, reg_def = "Rendimiento de la tierra (rendimiento/ha)", tag = 'Arroz')
# fun_reg(data = data_prod, reg_def = "Rendimiento de la tierra (rendimiento/ha)", tag = 'Maíz (blanco o amarillo)')

rm(data_prod, prod1, prod2)

#--------------------------#
# B. Tablas ----
#--------------------------#

# Todos
# labels_dependientes = c("$Treatment*Period$", "$Treatment$")

coefs <- c(
  # Riego
  "riego" = "Riego no mecanizado",
  # Asistencia Técnica
  "a1" = "Asist. Técnica",
  # Crédito
  "c10" = "Necesidad de financiamiento",
  'cred1' = "Acceso a crédito bancario",
  # Fuentes de Energia
  "energia" = "Energía - Solo Red Eléctrica",
  # Control de plagas
  "plagas" = "Plagas - Control orgánico o biológico",
  # Mercados
  "d1" = "Destino - Autoconsumo",
  "d2" = "Destino - Venta con Intermediario",
  "d3" = "Destino - Venta directa",
  "d5" = "Destino - Industria",
  # Agroecologicas
  'temp' = 'Temperatura',
  'rain' = 'Precipitaciones',
  'elevation' = 'Elevación',
  # Capacidad institucional
  'sgp_pc' = 'Transferencias SGP (per cápita)',
  'ind_desempeno_int' = 'Índice desempeño municipal',
  'inv_mpio_pc' = 'Inversión municipal per cápita',
  # Estructura economica
  'ind_div_econ' = 'Indice diversidad económica'
)

names(attributes(all[[1]]$coefficients))
names(all[[1]]$coefficientsp[1])

stargazer::stargazer(all[[1]], all[[2]], all[[3]], all[[4]],   
                     header=FALSE, type= 'html',
                     digits = 2,
                     dep.var.labels.include = TRUE,
                     model.numbers = TRUE,
                     # keep = c("riego", "f1", "f2", "f3", "f4", "a1", "c10", "no_aprob", "agrario", "otros_ban"),
                     # omit.stat = c("ser", "adj.rsq", "rsq"),
                     dep.var.labels = "Log(Productividad agrícola)",
                     title = "Determinantes de la productividad agrícola agregada",
                     column.labels = c("Microfundios", "Pequeños", "Medianos", "Grandes"),
                     column.separate = c(1, 1, 1, 1),
                     covariate.labels = coefs,
                     table.placement = "H", 
                     df = FALSE,
                     notes = "",
                     notes.append = FALSE,
                     out = "04_Tablas/Estimaciones/productividad_agregada.doc")


# Arroz
stargazer::stargazer(arroz[[1]], arroz[[2]], arroz[[3]], arroz[[4]],   
                     header=FALSE, type= 'html',
                     digits = 2,
                     dep.var.labels.include = TRUE,
                     model.numbers = TRUE,
                     # omit.stat = c("ser", "adj.rsq", "rsq"),
                     dep.var.labels = "Log(Productividad agrícola)",
                     title = "Determinantes de la productividad agrícola: cadena de arroz",
                     column.labels = c("Microfundios", "Pequeños", "Medianos", "Grandes"),
                     column.separate = c(1, 1, 1, 1),
                     covariate.labels = coefs,
                     table.placement = "H", 
                     df = FALSE,
                     notes = "",
                     notes.append = FALSE,
                     out = "04_Tablas/Estimaciones/productividad_arroz.doc")

# Maiz
stargazer::stargazer(maiz[[1]], maiz[[2]], maiz[[3]], maiz[[4]],   
                     header=FALSE, type= 'html',
                     digits = 2,
                     dep.var.labels.include = TRUE,
                     model.numbers = TRUE,
                     # omit.stat = c("ser", "adj.rsq", "rsq"),
                     dep.var.labels = "Log(Productividad agrícola)",
                     title = "Determinantes de la productividad agrícola: cadena de maíz blanco o amarillo",
                     column.labels = c("Microfundios", "Pequeños", "Medianos", "Grandes"),
                     column.separate = c(1, 1, 1, 1),
                     covariate.labels = coefs,
                     table.placement = "H", 
                     df = FALSE,
                     notes = "",
                     notes.append = FALSE,
                     out = "04_Tablas/Estimaciones/productividad_maiz.doc")

#--------------------------#
# C. Muestra completa ----
# Solo con filtros de zona de exclusion, resguardos, entre otros
#--------------------------#

# # Abrimos base a nivel de UPA-jefe hogar con caracteristicas del productor, entorno, agroecologicas, etc 
# data_prod <- readRDS(glue("{datos}/CNA/prod_caracteristicas_upa_all.rds")) %>%
#   # SGP per capita en pesos
#   mutate(sgp_pc = sgp_pc*1000000, edad2 = edad*edad)
# 
# # Agregado
# fun_reg(data = data_prod, reg_def = "Rendimiento de la tierra (rendimiento/ha)", tag = 'All', sample = 'yes')
# 
# # Arroz
# fun_reg(data = data_prod, reg_def = "Rendimiento de la tierra (rendimiento/ha)", tag = 'Maíz (blanco o amarillo)', sample = 'yes')
# 
# # Maiz
# fun_reg(data = data_prod, reg_def = "Rendimiento de la tierra (rendimiento/ha)", tag = 'Arroz', sample = 'yes')
