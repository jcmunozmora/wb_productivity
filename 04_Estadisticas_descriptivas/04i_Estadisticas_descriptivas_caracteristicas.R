#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 21 feb, 2022
# Estadisticas descriptivas por region, tamano de la tierra y cadena productiva
# UPA: Caracteristicas productor, agroecologicas; Municipal: entorno, institucional, estructura economica
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readr, readxl, janitor,tidyr)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
graficas <- "03_Graficas"
tablas <- "04_Tablas"
options(scipen = 999)

#-------------------------------------------------------#
# 0. Funciones ----
#-------------------------------------------------------#

# funcion para agregar a nivel de region y tierra
tablareg_tierra <- function(base,varname,variable,tipo) {
  base <- cbind(base,variable)
  if(tipo=="arroz"){
    agregado_clas <- base %>% dplyr::filter(clasificacion_utilizada == "Arroz")
  } else if (tipo=="maiz"){
    agregado_clas <- base %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)")
  } else {
    agregado_clas <- base  
  }
  agregado_clas <- agregado_clas %>% 
    filter(region!="NA") %>% 
    group_by(region,tipo_upa) %>%
    summarise(var = varname,
              mean=round(mean(variable,na.rm = TRUE),1),
              sd=round(sd(variable,na.rm = TRUE),1),
              min = round(min(variable,na.rm = TRUE),1),
              max = round(max(variable,na.rm = TRUE),1),
              p10 = round(quantile(variable, 0.1, na.rm = T), 1),
              p90 = round(quantile(variable, 0.9, na.rm = T), 1),
              n = round(length(variable),1)) %>% 
    ungroup() %>%
    relocate(var,region,tipo_upa,mean,sd,min,max,n)
  return(agregado_clas)
}

# funcion para agregar a nivel de region
tablaregion <- function(base,varname,variable,tipo) {
  base <- cbind(base,variable)
  if(tipo=="arroz"){
    agregado_clas <- base %>% dplyr::filter(clasificacion_utilizada == "Arroz")
  }else if (tipo=="maiz"){
    agregado_clas <- base %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)")
  } else {
    agregado_clas <- base  
  }
  agregado_clas <- agregado_clas %>% filter(region!="NA") %>% group_by(region) %>%
    summarise(var = varname,
              mean=round(mean(variable,na.rm = TRUE),1),
              sd=round(sd(variable,na.rm = TRUE),1),
              min=round(min(variable,na.rm = TRUE),1),
              max=round(max(variable,na.rm = TRUE),1)) %>% ungroup() %>%
    relocate(var,region,mean,sd,min,max)
  return(agregado_clas)
}

# funcion para agregar por tamano de la tierra
tablatierra <- function(base,varname,variable,tipo) {
  base <- cbind(base,variable)
  if(tipo=="arroz"){
    agregado_clas <- base %>% dplyr::filter(clasificacion_utilizada == "Arroz")
  }else if (tipo=="maiz"){
    agregado_clas <- base %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)")
  } else {
    agregado_clas <- base  
  }
  agregado_clas <- agregado_clas %>% filter(region!="NA") %>% group_by(tipo_upa) %>%
    summarise(var = varname,mean=round(mean(variable,na.rm = TRUE),1),sd=round(sd(variable,na.rm = TRUE),1),
              min=round(min(variable,na.rm = TRUE),1),
              max=round(max(variable,na.rm = TRUE),1)) %>% ungroup() %>%
    relocate(var,tipo_upa,mean,sd,min,max)
  return(agregado_clas)
}

#-------------------------------------------------------#
# 1. Agregado ----
#-------------------------------------------------------#

# Abrir base CNA con controles
# Creamos identificador de acceso a credito, asistencia tecnica, control de plagas, infraestructura
# Excluimos solicitudes de credito
cna_ag <-  readRDS(glue("{datos}/CNA/prod_caracteristicas_upa.rds")) %>% 
  mutate(acceso_cred = ifelse(c11 + c20 + c21 + c2 + c3 + c4 + c5 > 0, 1, NA),
         asistencia = ifelse(a1 > 0, 1, NA), infraestructura = ifelse(i1 > 0, 1, NA),
         control_plagas = ifelse(p1 + p2 + p3 > 0, 1, NA))

#dataframe por region y tipo de tierra
agregado <- cna_ag %>%
  dplyr::select(region,tipo_upa,ls2, rend_cos,edad,educ,temp, rain, dismdo,distancia_mercado,
                sgp_pc, ind_desempeno_int,valor_agregado, ind_div_econ, elevation, inv_mpio_pc,
                acceso_cred, asistencia, control_plagas, infraestructura) %>% 
  as.data.frame() %>%
  relocate(region,tipo_upa,ls2, rend_cos,edad,educ,temp, rain, dismdo,distancia_mercado,
           sgp_pc, ind_desempeno_int,valor_agregado, ind_div_econ, elevation, inv_mpio_pc)

# Descriptivas agregadas (sin distinguir por cadena)

# Etiquetas en espanol
labels <- c("Productividad (Cobb-Douglas)", "Rendimiento por hectarea sembrada",
            "Edad", "Anios educacion", "Temperatura (centigrados)", "Lluvias (mm)", "Distancia mercado mayorista",
            "Distancia mercado alimentos", "Transferencias SGP per-capita", "Indice desempenio institucional", 
            "Valor agregado", "Indice diversidad economica", "Elevación (km)", "Inversión municipal per cápita (millones)",
            "Acceso a crédito", "Asistencia técnica", "Realiza control de plagas", "Tiene infraestructura")

# Tabla descriptivas
stargazer::stargazer(agregado %>% dplyr::select(-c(region, tipo_upa)), 
                     summary.stat = c("mean", "sd", "min", "max", "n"),
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = labels,
                     title = "Estadisticas descriptivas generales",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_general_descriptive.doc"))

#-------------------------------------------------------#
# 2. Region y tamanio de la tierra  ----
#-------------------------------------------------------#

#--------------------------#
# A. Agregado x region y tierra ----
#--------------------------#

# Paquete tecnologico
tec <- agregado %>%
  mutate(upas = 1) %>%
  group_by(region, tipo_upa) %>%
  summarise(upas = sum(upas),
            across(.cols = c('acceso_cred', 'asistencia', 'control_plagas', 'infraestructura'), ~sum(.x, na.rm = T))) %>%
  ungroup() %>% 
  pivot_longer(cols = c('acceso_cred', 'asistencia', 'control_plagas', 'infraestructura'), 
               names_to = 'var', values_to = 'n') %>% 
  mutate(part = 100*(n/upas))

var1 <- tablareg_tierra(base=agregado,varname="ls2",variable=agregado$ls2, tipo="agregado")
var2 <- tablareg_tierra(base=agregado,varname="rend_cos",variable=agregado$rend_cos, tipo="agregado")
var3 <- tablareg_tierra(base=agregado,varname="edad",variable=agregado$edad, tipo="agregado")
var4 <- tablareg_tierra(base=agregado,varname="educ",variable=agregado$educ, tipo="agregado")
var5 <- tablareg_tierra(base=agregado,varname="temp",variable=agregado$temp, tipo="agregado")
var6 <- tablareg_tierra(base=agregado,varname="rain",variable=agregado$rain, tipo="agregado")
var7 <- tablareg_tierra(base=agregado,varname="dismdo",variable=agregado$dismdo, tipo="agregado")
var8 <- tablareg_tierra(base=agregado,varname="distancia_mercado",variable=agregado$distancia_mercado, tipo="agregado")
var9 <- tablareg_tierra(base=agregado,varname="spg_pc",variable=agregado$sgp_pc, tipo="agregado")
var10 <- tablareg_tierra(base=agregado,varname="ind_desempeno_int",variable=agregado$ind_desempeno_int, tipo="agregado")
var11 <- tablareg_tierra(base=agregado,varname="valor_agregado",variable=agregado$valor_agregado, tipo="agregado")
var12 <- tablareg_tierra(base=agregado,varname="ind_div_econ",variable=agregado$ind_div_econ, tipo="agregado")
var13 <- tablareg_tierra(base=agregado,varname="elevation",variable=agregado$elevation, tipo="agregado")
var14 <- tablareg_tierra(base=agregado,varname="inv_mpio_pc",variable=agregado$inv_mpio_pc, tipo="agregado")

dfagregado <- bind_rows(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12, var13, var14, tec) %>% 
  arrange(var, region, tipo_upa)

rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12, var13)

names(dfagregado) <- c("Variable","region","tipo_upa","mean","sd","min","max","n", "p10", "p90", "upas", "part")

dfagregado <- dfagregado  %>%
  pivot_longer(cols = c("mean","sd","min","max","n", "p10", "p90", "upas", "part"),
               names_to = "estadistica", values_to = "valor") 

saveRDS(dfagregado, glue("{datos}/CNA/bases_long/df_region_tierra.rds"))

#--------------------------#
# B. Region x Tierra x Cultivo principal ----
#--------------------------#

stats <- c('mean', 'sd', 'min', 'max', 'p10', 'p90', '_n')

# Guardamos una base por clasificacion de cultivo principal de las UPA
data_cul <- cna_ag %>% 
  dplyr::select(region, tipo_upa, clasificacion_utilizada, ls2, rend_cos, edad, educ, temp, rain, 
                dismdo, distancia_mercado, sgp_pc, ind_desempeno_int, valor_agregado, ind_div_econ, 
                elevation, inv_mpio_pc, acceso_cred, asistencia, control_plagas, infraestructura)

# Paquete tecnologico
tec <- data_cul %>%
  mutate(upas = 1) %>%
  group_by(region, tipo_upa, clasificacion_utilizada) %>%
  summarise(upas = sum(upas),
            across(.cols = c('acceso_cred', 'asistencia', 'control_plagas', 'infraestructura'), ~sum(.x, na.rm = T))) %>%
  ungroup() %>% 
  pivot_longer(cols = c('acceso_cred', 'asistencia', 'control_plagas', 'infraestructura'), 
               names_to = 'var', values_to = 'n') %>% 
  mutate(part = 100*(n/upas))

# Cambiamos la estructura de nombres de las variables para facilitar distinguir la variable de las estadisticas
names(data_cul) <- gsub("_", ".", names(data_cul))

# Calculamos estadisticas por region, tamano de la upa, y cultivo principal
data_cul <- data_cul %>%
  group_by(region, tipo.upa, clasificacion.utilizada) %>%
  summarise(across(everything(), 
                   list(mean = ~mean(., na.rm = T), 
                        sd = ~sd(., na.rm = T),
                        min = ~min(., na.rm = T),
                        max = ~max(., na.rm = T),
                        p10 = ~quantile(., 0.1, na.rm = T),
                        p90 = ~quantile(., 0.9, na.rm = T),
                        n = ~n()))) %>%
  ungroup() %>%
  pivot_longer(cols = ends_with(stats), names_to = c('var','estadistica'), values_to = 'valor', names_sep = "_") %>%
  mutate(var = gsub("\\.", "_", var))

names(data_cul) <- gsub("\\.", "_", names(data_cul))

data_cul <- data_cul %>%
  bind_rows(., tec) %>%
  mutate(valor = round(valor, 1)) %>% 
  arrange(clasificacion_utilizada, var, region, tipo_upa) %>%
  relocate(clasificacion_utilizada, .before = 'region')

# Guardamos una base de datos por cultivo
lapply(unique(data_cul$clasificacion_utilizada), function(x){
  
  # Filtramos base para tener cultivo de interes
  df <- data_cul %>% dplyr::filter(clasificacion_utilizada == x)
  cul <- df %>% distinct(clasificacion_utilizada) %>% unique() %>% unlist()
  cul <- str_to_lower(cul)
  cul <- gsub(" ", "_", cul)
  cul <- gsub(",", "", cul)
  cul <- gsub("\\(", "", cul)
  cul <- gsub("\\)", "", cul)
  cul <- stringi::stri_trans_general(cul, "latin-ascii")
  print(glue("Guardando cultivo: {cul}"))
  
  # Guardamos base de datos
  saveRDS(df, glue("{datos}/CNA/bases_long/df_region_tierra_{cul}.rds"))
})

#--------------------------#
# C. Agregado x region ----
#--------------------------#

var1 <- tablaregion(base=agregado,varname="Productividad (Cobb-Douglas)",variable=agregado$ls2,tipo="agregado")
var2 <- tablaregion(base=agregado,varname="Rendimiento por hectarea sembrada",variable=agregado$rend_cos,tipo="agregado")
var3 <- tablaregion(base=agregado,varname="Edad",variable=agregado$edad,tipo="agregado")
var4 <- tablaregion(base=agregado,varname="Anios educacion",variable=agregado$educ,tipo="agregado")
var5 <- tablaregion(base=agregado,varname="Temperatura (centigrados)",variable=agregado$temp,tipo="agregado")
var6 <- tablaregion(base=agregado,varname="Lluvias (mm)",variable=agregado$rain,tipo="agregado")
var7 <- tablaregion(base=agregado,varname="Distancia mercado mayorista",variable=agregado$dismdo,tipo="agregado")
var8 <- tablaregion(base=agregado,varname="Distancia mercado alimentos",variable=agregado$distancia_mercado,tipo="agregado")
var9 <- tablaregion(base=agregado,varname="Transferencias SGP per-capita",variable=agregado$sgp_pc,tipo="agregado")
var10 <- tablaregion(base=agregado,varname="Indice desempenio institucional",variable=agregado$ind_desempeno_int,tipo="agregado")
var11 <- tablaregion(base=agregado,varname="Valor agregado",variable=agregado$valor_agregado,tipo="agregado")
var12 <- tablaregion(base=agregado,varname="Indice diversidad economica",variable=agregado$ind_div_econ,tipo="agregado")
var13 <- tablaregion(base=agregado,varname="Elevación",variable=agregado$elevation,tipo="agregado")

agregado_region <- rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12, var13)

rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)

label1 <- c("Variable","region","mean","sd","min","max")

stargazer::stargazer(agregado_region, summary = FALSE,
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = label1,
                     title = "Estadisticas descriptivas por region",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", rownames = FALSE, 
                     out = glue("{tablas}/Descriptivas/cna_gen_desc_region.doc"))

#--------------------------#
# D. Arroz - region ----
#--------------------------#

var1 <- tablaregion(base=cna_ag,varname="Productividad",variable=cna_ag$ls2,tipo="arroz")
var2 <- tablaregion(base=cna_ag,varname="Rendimiento por hectarea sembrada",variable=cna_ag$rend_cos,tipo="arroz")
var3 <- tablaregion(base=cna_ag,varname="Edad",variable=cna_ag$edad,tipo="arroz")
var4 <- tablaregion(base=cna_ag,varname="Anios educacion",variable=cna_ag$educ,tipo="arroz")
var5 <- tablaregion(base=cna_ag,varname="Temperatura (centigrados)",variable=cna_ag$temp,tipo="arroz")
var6 <- tablaregion(base=cna_ag,varname="Lluvias (mm)",variable=cna_ag$rain,tipo="arroz")
var7 <- tablaregion(base=cna_ag,varname="Distancia mercado mayorista",variable=cna_ag$dismdo,tipo="arroz")
var8 <- tablaregion(base=cna_ag,varname="Distancia mercado alimentos",variable=cna_ag$distancia_mercado,tipo="arroz")
var9 <- tablaregion(base=cna_ag,varname="Transferencias SGP per-capita",variable=cna_ag$sgp_pc,tipo="arroz")
var10 <- tablaregion(base=cna_ag,varname="Indice desempenio institucional",variable=cna_ag$ind_desempeno_int,tipo="arroz")
var11 <- tablaregion(base=cna_ag,varname="Valor Agregado",variable=cna_ag$valor_agregado,tipo="arroz")
var12 <- tablaregion(base=cna_ag,varname="Indice diversidad economica",variable=cna_ag$ind_div_econ,tipo="arroz")
var13 <- tablaregion(base=cna_ag,varname="Elevación",variable=cna_ag$elevation,tipo="arroz")

arroz_reg <- rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12, var13)

rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)

stargazer::stargazer(arroz_reg, summary = FALSE, rownames = FALSE,
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = label1,
                     title = "Estadisticas descriptivas por region - Arroz",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_arroz_var_region.doc"))

#--------------------------#
# E. Maiz- region ----
#--------------------------#

var1 <- tablaregion(base=cna_ag,varname="Productividad (Cobb-Douglas)",variable=cna_ag$ls2,tipo="maiz")
var2 <- tablaregion(base=cna_ag,varname="Rendimiento por hectarea sembrada",variable=cna_ag$rend_cos,tipo="maiz")
var3 <- tablaregion(base=cna_ag,varname="Edad",variable=cna_ag$edad,tipo="maiz")
var4 <- tablaregion(base=cna_ag,varname="Anios educacion",variable=cna_ag$educ,tipo="maiz")
var5 <- tablaregion(base=cna_ag,varname="Temperatura (centigrados)",variable=cna_ag$temp,tipo="maiz")
var6 <- tablaregion(base=cna_ag,varname="Lluvias (mm)",variable=cna_ag$rain,tipo="maiz")
var7 <- tablaregion(base=cna_ag,varname="Distancia mercado mayorista",variable=cna_ag$dismdo,tipo="maiz")
var8 <- tablaregion(base=cna_ag,varname="Distancia mercado alimentos",variable=cna_ag$distancia_mercado,tipo="maiz")
var9 <- tablaregion(base=cna_ag,varname="Transferencias SGP per-capita",variable=cna_ag$sgp_pc,tipo="maiz")
var10 <- tablaregion(base=cna_ag,varname="Indice desempenio institucional",variable=cna_ag$ind_desempeno_int,tipo="maiz")
var11 <- tablaregion(base=cna_ag,varname="Valor Agregado",variable=cna_ag$valor_agregado,tipo="maiz")
var12 <- tablaregion(base=cna_ag,varname="Indice diversidad economica",variable=cna_ag$ind_div_econ,tipo="maiz")
var13 <- tablaregion(base=cna_ag,varname="Elevación",variable=cna_ag$elevation,tipo="maiz")

maiz_reg <- rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12, var13)

rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)

stargazer::stargazer(maiz_reg, summary = FALSE, rownames = FALSE,
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = label1,
                     title = "Estadisticas descriptivas por region - Maiz",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_maiz_var_region.doc"))


#---------------------#
# F. Agregado - tierra ----
#---------------------#

var1 <- tablatierra(base=agregado,varname="Productividad (Cobb-Douglas)",variable=agregado$ls2, tipo="agregado")
var2 <- tablatierra(base=agregado,varname="Rendimiento por hectarea sembrada",variable=agregado$rend_cos, tipo="agregado")
var3 <- tablatierra(base=agregado,varname="Edad",variable=agregado$edad, tipo="agregado")
var4 <- tablatierra(base=agregado,varname="Anios educacion",variable=agregado$educ, tipo="agregado")
var5 <- tablatierra(base=agregado,varname="Temperatura (centigrados)",variable=agregado$temp, tipo="agregado")
var6 <- tablatierra(base=agregado,varname="Lluvias (mm)",variable=agregado$rain, tipo="agregado")
var7 <- tablatierra(base=agregado,varname="Distancia mercado mayorista",variable=agregado$dismdo, tipo="agregado")
var8 <- tablatierra(base=agregado,varname="Distancia mercado alimentos",variable=agregado$distancia_mercado, tipo="agregado")
var9 <- tablatierra(base=agregado,varname="Transferencias SGP per-capita",variable=agregado$sgp_pc, tipo="agregado")
var10 <- tablatierra(base=agregado,varname="Indice desempenio institucional",variable=agregado$ind_desempeno_int, tipo="agregado")
var11 <- tablatierra(base=agregado,varname="Valor agregado",variable=agregado$valor_agregado, tipo="agregado")
var12 <- tablatierra(base=agregado,varname="Indice diversidad economica",variable=agregado$ind_div_econ, tipo="agregado")
var13 <- tablatierra(base=agregado,varname="Elevación",variable=agregado$elevation, tipo="agregado")

agregado_tierra <- rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12, var13)

rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)

label2 <- c("Variable","tamanio tierra","mean","sd","min","max")

stargazer::stargazer(agregado_tierra, summary = FALSE,
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = label2,
                     title = "Estadisticas descriptivas por tamanio de la tierra",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", rownames = FALSE, 
                     out = glue("{tablas}/Descriptivas/cna_gen_desc_tierra.doc"))

#---------------------#
# G. Arroz - tierra ----
#---------------------#

var1 <- tablatierra(base=cna_ag,varname="Productividad",variable=cna_ag$ls2,tipo="arroz")
var2 <- tablatierra(base=cna_ag,varname="Rendimiento por hectarea sembrada",variable=cna_ag$rend_cos,tipo="arroz")
var3 <- tablatierra(base=cna_ag,varname="Edad",variable=cna_ag$edad,tipo="arroz")
var4 <- tablatierra(base=cna_ag,varname="Anios educacion",variable=cna_ag$educ,tipo="arroz")
var5 <- tablatierra(base=cna_ag,varname="Temperatura (centigrados)",variable=cna_ag$temp,tipo="arroz")
var6 <- tablatierra(base=cna_ag,varname="Lluvias (mm)",variable=cna_ag$rain,tipo="arroz")
var7 <- tablatierra(base=cna_ag,varname="Distancia mercado mayorista",variable=cna_ag$dismdo,tipo="arroz")
var8 <- tablatierra(base=cna_ag,varname="Distancia mercado alimentos",variable=cna_ag$distancia_mercado,tipo="arroz")
var9 <- tablatierra(base=cna_ag,varname="Transferencias SGP per-capita",variable=cna_ag$sgp_pc,tipo="arroz")
var10 <- tablatierra(base=cna_ag,varname="Indice desempenio institucional",variable=cna_ag$ind_desempeno_int,tipo="arroz")
var11 <- tablatierra(base=cna_ag,varname="Valor Agregado",variable=cna_ag$valor_agregado,tipo="arroz")
var12 <- tablatierra(base=cna_ag,varname="Indice diversidad economica",variable=cna_ag$ind_div_econ,tipo="arroz")
var13 <- tablatierra(base=cna_ag,varname="Elevación",variable=cna_ag$elevation,tipo="arroz")

arroz_upa <- rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12, var13)

rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)

stargazer::stargazer(arroz_upa, summary = FALSE, rownames = FALSE,
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = label2,
                     title = "Estadisticas descriptivas por tamanio de tierra - Arroz",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_arroz_var_tierra.doc"))

#---------------------#
# H. Maiz - tierra ----
#---------------------#

var1 <- tablatierra(base=cna_ag,varname="Productividad (Cobb-Douglas)",variable=cna_ag$ls2,tipo="maiz")
var2 <- tablatierra(base=cna_ag,varname="Rendimiento por hectarea sembrada",variable=cna_ag$rend_cos,tipo="maiz")
var3 <- tablatierra(base=cna_ag,varname="Edad",variable=cna_ag$edad,tipo="maiz")
var4 <- tablatierra(base=cna_ag,varname="Anios educacion",variable=cna_ag$educ,tipo="maiz")
var5 <- tablatierra(base=cna_ag,varname="Temperatura (centigrados)",variable=cna_ag$temp,tipo="maiz")
var6 <- tablatierra(base=cna_ag,varname="Lluvias (mm)",variable=cna_ag$rain,tipo="maiz")
var7 <- tablatierra(base=cna_ag,varname="Distancia mercado mayorista",variable=cna_ag$dismdo,tipo="maiz")
var8 <- tablatierra(base=cna_ag,varname="Distancia mercado alimentos",variable=cna_ag$distancia_mercado,tipo="maiz")
var9 <- tablatierra(base=cna_ag,varname="Transferencias SGP per-capita",variable=cna_ag$sgp_pc,tipo="maiz")
var10 <- tablatierra(base=cna_ag,varname="Indice desempenio institucional",variable=cna_ag$ind_desempeno_int,tipo="maiz")
var11 <- tablatierra(base=cna_ag,varname="Valor Agregado",variable=cna_ag$valor_agregado,tipo="maiz")
var12 <- tablatierra(base=cna_ag,varname="Indice diversidad economica",variable=cna_ag$ind_div_econ,tipo="maiz")
var13 <- tablatierra(base=cna_ag,varname="Elevación",variable=cna_ag$elevation,tipo="maiz")

maiz_tie <- rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12, var13)

rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)

stargazer::stargazer(maiz_tie, summary = FALSE, rownames = FALSE,
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = label2,
                     title = "Estadisticas descriptivas por tamanio de tierra - Maiz",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_maiz_var_tierra.doc"))

