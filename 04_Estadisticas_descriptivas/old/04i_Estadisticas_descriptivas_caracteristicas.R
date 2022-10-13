#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 15 feb, 2022
# Estadisticas descriptivas por region, tamano de la tierra y cadena productiva
# UPA: Caracteristicas productor, agroecologicas; Municipal: entorno, institucional, estructura economica
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readr, readxl, janitor)
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
# 1. Organizar datos ----
# Unir productividad + caracteristicas upa + cultivo principal + variables de control
#-------------------------------------------------------#

# Variables de control
data_control <- readRDS(glue("{datos}/CNA/caracteristicas_upa.rds")) %>% 
  mutate(educ = ifelse(nivel_educ==1,1,ifelse(nivel_educ==9,0,
                ifelse(anios_educ==99 | anios_educ==97,NA,anios_educ-2)))) #opcion para educ

data_control <- data_control %>%
  dplyr::select(-c(encuesta, parentesco_jefe, sexo, edad, nivel_educ, anios_educ, nivel_educ_cat)) %>%
  distinct(cod_mpio, cod_vereda, .keep_all = T) %>%
  mutate(cod_vereda = as.numeric(cod_vereda))


prod <- readRDS(glue("{datos}/CNA/caracteristicas_productor_upa.rds")) %>% 
  dplyr::select(cod_vereda, encuesta, parentesco_jefe, sexo, edad, nivel_educ, anios_educ, nivel_educ_cat) %>%
  drop_na(parentesco_jefe) %>%
  dplyr::filter(parentesco_jefe == 1) %>%
  distinct() %>%
  mutate(cod_vereda = as.numeric(cod_vereda))

# Base de productividad y caracteristicas nivel de UPA-cultivo
cna_prod <- readRDS(glue("{datos}/CNA/base_cna_productividad.rds")) %>% 
  drop_na(ls2) %>% 
  dplyr::select(cod_vereda, encuesta, tipo_cul_lote, area_upa, area_sembrada, cant_cosecha, ls2) %>%
  distinct()

# Agregamos caracteristicas a nivel de UPA
cna_ag <- cna_prod %>%
  group_by(cod_vereda, encuesta, ls2) %>%
  summarise(area_upa = mean(area_upa, na.rm = T), area_sembrada = mean(area_sembrada, na.rm = T), 
            cant_cosecha = mean(cant_cosecha, na.rm = T)) %>%
  ungroup() %>%
  distinct() %>%
  mutate(rend_cos = cant_cosecha/area_sembrada)

colSums(is.na(data_control))
colSums(is.na(cna_ag))

# Cultivo principal de cada UPA
cultivo <- readRDS(glue("{datos}/CNA/limpieza_censo/lista_cultivo_principal_upa.rds"))

# Base de productividad, variables de control y cultivo principal
# Esta base tiene duplicates en casos en los que jefatura del hogar sea compartida
# La edad se puede promediar, pero el nivel educativo SOLO cuando se hayan calculado los anios de educacion
cna_ag <- cna_ag %>% 
  left_join(cultivo, by = c("cod_vereda", "encuesta")) %>%
  left_join(data_control, by = c("cod_vereda")) %>%
  left_join(prod, by = c('cod_vereda', 'encuesta')) %>% 
  dplyr::select(region, cod_dpto, cod_mpio, cod_vereda, encuesta, everything())

colSums(is.na(cna_ag2))

test <- cna_ag[duplicated(cna_ag$encuesta),]

# Se crea variable cantidad de tierra 
cna_ag <- cna_ag %>% mutate(cat_tierra = NA)

# Crear la variable de categorias de tamano de tierra 
cna_ag$cat_tierra[cna_ag$area_upa < 5] <- "0-5"
cna_ag$cat_tierra[cna_ag$area_upa >=5 & cna_ag$area_upa < 20] <- "5-20"
cna_ag$cat_tierra[cna_ag$area_upa >=20 & cna_ag$area_upa < 100] <- "20-100"
cna_ag$cat_tierra[cna_ag$area_upa >= 100] <- "+100"

# Tamano de la tierra (0-5 microfundio; 5-20 pequena; 20-100 mediana; +100 grande)
cna_ag <- cna_ag %>%
  mutate(tipo_upa = ifelse(cat_tierra == "0-5", "Microfundio",
                           ifelse(cat_tierra == "5-20", "Pequeña", 
                                  ifelse(cat_tierra == "20-100", "Mediana", "Grande"))))

table(cna_ag$cat_tierra, cna_ag$tipo_upa)

saveRDS(cna_ag, glue("{datos}/CNA/prod_caracteristicas_upa.rds"))
#cna_ag <-  readRDS(glue("{datos}/CNA/prod_caracteristicas_upa.rds"))


# Analizar jefaturas compartidas por aparte (tablas aparte)
# Stand-by: sexo productor (% mujeres)

#-------------------------------------------------------#
# 2. Agregado ----
#-------------------------------------------------------#

# Descriptivas agregadas (sin distinguir por cadena)
agregado <- cna_ag %>% dplyr::select(region,tipo_upa,ls2, rend_cos,edad,anios_educ,temp, rain, dismdo,distancia_mercado,
                              sgp_pc,ind_desempeno_int,valor_agregado, ind_div_econ) %>% as.data.frame() %>%
  relocate(region,tipo_upa,ls2, rend_cos,edad,anios_educ,temp, rain, dismdo,distancia_mercado,
           sgp_pc,ind_desempeno_int,valor_agregado, ind_div_econ)
# Etiquetas en espanol
labels <- c("Productividad (Cobb-Douglas)", "Rendimiento por hectarea sembrada",
"Edad", "Anios educacion", "Temperatura (centigrados)", "Lluvias (mm)", "Distancia mercado mayorista",
"Distancia mercado alimentos", "Transferencias SGP per-capita",
"Indice desempenio institucional", "Valor agregado", "Indice diversidad economica")

# Tabla descriptivas
stargazer::stargazer(agregado[,3:14], summary.stat = c("mean", "sd", "min", "max", "n"),
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = labels,
                     title = "Estadisticas descriptivas generales",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_general_descriptive.doc"))
#---------------------------------#
# Por region y tamanio de la tierra  ----
#---------------------------------#

#----------#
# REGION ---
#----------#
tabla2 <- function(base,varname,variable) {
  base <- cbind(base,variable)
agregado_clas <- base %>% filter(region!="NA") %>% group_by(region) %>%
  summarise(var = varname,mean=round(mean(variable,na.rm = TRUE),1),sd=round(sd(variable,na.rm = TRUE),1),
            min=round(min(variable,na.rm = TRUE),1),
            max=round(max(variable,na.rm = TRUE),1)) %>% ungroup() %>%
  relocate(var,region,mean,sd,min,max)
return(agregado_clas)
}

var1 <- tabla2(base=agregado,varname="Productividad (Cobb-Douglas)",variable=agregado$ls2)
var2 <- tabla2(base=agregado,varname="Rendimiento por hectarea sembrada",variable=agregado$rend_cos)
var3 <- tabla2(base=agregado,varname="Edad",variable=agregado$edad)
var4 <- tabla2(base=agregado,varname="Anios educacion",variable=agregado$anios_educ)
var5 <- tabla2(base=agregado,varname="Temperatura (centigrados)",variable=agregado$temp)
var6 <- tabla2(base=agregado,varname="Lluvias (mm)",variable=agregado$rain)
var7 <- tabla2(base=agregado,varname="Distancia mercado mayorista",variable=agregado$dismdo)
var8 <- tabla2(base=agregado,varname="Distancia mercado alimentos",variable=agregado$distancia_mercado)
var9 <- tabla2(base=agregado,varname="Transferencias SGP per-capita",variable=agregado$sgp_pc)
var10 <- tabla2(base=agregado,varname="Indice desempenio institucional",variable=agregado$ind_desempeno_int)
var11 <- tabla2(base=agregado,varname="Valor agregado",variable=agregado$valor_agregado)
var12 <- tabla2(base=agregado,varname="Indice diversidad economica",variable=agregado$ind_div_econ)
agregado_region <- rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)
rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)
label1 <- c("Variable","region","mean","sd","min","max")
stargazer::stargazer(agregado_region, summary = FALSE,
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = label1,
                     title = "Estadisticas descriptivas por region",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", rownames = FALSE, 
                     out = glue("{tablas}/Descriptivas/cna_gen_desc_region.doc"))
#----------#
# TIERRA ---
#----------#
tabla3 <- function(base,varname,variable) {
  base <- cbind(base,variable)
  agregado_clas <- base %>% filter(region!="NA") %>% group_by(tipo_upa) %>%
    summarise(var = varname,mean=round(mean(variable,na.rm = TRUE),1),sd=round(sd(variable,na.rm = TRUE),1),
              min=round(min(variable,na.rm = TRUE),1),
              max=round(max(variable,na.rm = TRUE),1)) %>% ungroup() %>%
    relocate(var,tipo_upa,mean,sd,min,max)
  return(agregado_clas)
}

var1 <- tabla3(base=agregado,varname="Productividad (Cobb-Douglas)",variable=agregado$ls2)
var2 <- tabla3(base=agregado,varname="Rendimiento por hectarea sembrada",variable=agregado$rend_cos)
var3 <- tabla3(base=agregado,varname="Edad",variable=agregado$edad)
var4 <- tabla3(base=agregado,varname="Anios educacion",variable=agregado$anios_educ)
var5 <- tabla3(base=agregado,varname="Temperatura (centigrados)",variable=agregado$temp)
var6 <- tabla3(base=agregado,varname="Lluvias (mm)",variable=agregado$rain)
var7 <- tabla3(base=agregado,varname="Distancia mercado mayorista",variable=agregado$dismdo)
var8 <- tabla3(base=agregado,varname="Distancia mercado alimentos",variable=agregado$distancia_mercado)
var9 <- tabla3(base=agregado,varname="Transferencias SGP per-capita",variable=agregado$sgp_pc)
var10 <- tabla3(base=agregado,varname="Indice desempenio institucional",variable=agregado$ind_desempeno_int)
var11 <- tabla3(base=agregado,varname="Valor agregado",variable=agregado$valor_agregado)
var12 <- tabla3(base=agregado,varname="Indice diversidad economica",variable=agregado$ind_div_econ)
agregado_tierra <- rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)
rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)
label2 <- c("Variable","tamanio tierra","mean","sd","min","max")
stargazer::stargazer(agregado_tierra, summary = FALSE,
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = label2,
                     title = "Estadisticas descriptivas por tamanio de la tierra",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", rownames = FALSE, 
                     out = glue("{tablas}/Descriptivas/cna_gen_desc_tierra.doc"))

#-------------------------------------------------------#
# 3. Arroz ----
#-------------------------------------------------------#
# Cadena arroz

#--------------#
#REGION -------#
#--------------#
tabla4 <- function(base,varname,variable) {
  base <- cbind(base,variable)
  agregado_clas <- base %>% dplyr::filter(clasificacion_utilizada == "Arroz") %>%
    dplyr::filter(region!="NA") %>% group_by(region) %>%
    summarise(var = varname,mean=round(mean(variable,na.rm = TRUE),1),sd=round(sd(variable,na.rm = TRUE),1),
              min=round(min(variable,na.rm = TRUE),1),
              max=round(max(variable,na.rm = TRUE),1)) %>% ungroup() %>%
    relocate(var,region,mean,sd,min,max)
  return(agregado_clas)
}
var1 <- tabla4(base=cna_ag,varname="Productividad",variable=cna_ag$ls2)
var2 <- tabla4(base=cna_ag,varname="Rendimiento por hectarea sembrada",variable=cna_ag$rend_cos)
var3 <- tabla4(base=cna_ag,varname="Edad",variable=cna_ag$edad)
var4 <- tabla4(base=cna_ag,varname="Anios educacion",variable=cna_ag$anios_educ)
var5 <- tabla4(base=cna_ag,varname="Temperatura (centigrados)",variable=cna_ag$temp)
var6 <- tabla4(base=cna_ag,varname="Lluvias (mm)",variable=cna_ag$rain)
var7 <- tabla4(base=cna_ag,varname="Distancia mercado mayorista",variable=cna_ag$dismdo)
var8 <- tabla4(base=cna_ag,varname="Distancia mercado alimentos",variable=cna_ag$distancia_mercado)
var9 <- tabla4(base=cna_ag,varname="Transferencias SGP per-capita",variable=cna_ag$sgp_pc)
var10 <- tabla4(base=cna_ag,varname="Indice desempenio institucional",variable=cna_ag$ind_desempeno_int)
var11 <- tabla4(base=cna_ag,varname="Valor Agregado",variable=cna_ag$valor_agregado)
var12 <- tabla4(base=cna_ag,varname="Indice diversidad economica",variable=cna_ag$ind_div_econ)
arroz_reg <- rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)
rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)

stargazer::stargazer(arroz_reg, summary = FALSE, rownames = FALSE,
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = label1,
                     title = "Estadisticas descriptivas por region - Arroz",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_arroz_var_region.doc"))
#--------------#
#TIERRA -------#
#--------------#
tabla5 <- function(base,varname,variable) {
  base <- cbind(base,variable)
  agregado_clas <- base %>% dplyr::filter(clasificacion_utilizada == "Arroz") %>%
    dplyr::filter(region!="NA") %>% group_by(tipo_upa) %>%
    summarise(var = varname,mean=round(mean(variable,na.rm = TRUE),1),sd=round(sd(variable,na.rm = TRUE),1),
              min=round(min(variable,na.rm = TRUE),1),
              max=round(max(variable,na.rm = TRUE),1)) %>% ungroup() %>%
    relocate(var,tipo_upa,mean,sd,min,max)
  return(agregado_clas)
}
var1 <- tabla5(base=cna_ag,varname="Productividad",variable=cna_ag$ls2)
var2 <- tabla5(base=cna_ag,varname="Rendimiento por hectarea sembrada",variable=cna_ag$rend_cos)
var3 <- tabla5(base=cna_ag,varname="Edad",variable=cna_ag$edad)
var4 <- tabla5(base=cna_ag,varname="Anios educacion",variable=cna_ag$anios_educ)
var5 <- tabla5(base=cna_ag,varname="Temperatura (centigrados)",variable=cna_ag$temp)
var6 <- tabla5(base=cna_ag,varname="Lluvias (mm)",variable=cna_ag$rain)
var7 <- tabla5(base=cna_ag,varname="Distancia mercado mayorista",variable=cna_ag$dismdo)
var8 <- tabla5(base=cna_ag,varname="Distancia mercado alimentos",variable=cna_ag$distancia_mercado)
var9 <- tabla5(base=cna_ag,varname="Transferencias SGP per-capita",variable=cna_ag$sgp_pc)
var10 <- tabla5(base=cna_ag,varname="Indice desempenio institucional",variable=cna_ag$ind_desempeno_int)
var11 <- tabla5(base=cna_ag,varname="Valor Agregado",variable=cna_ag$valor_agregado)
var12 <- tabla5(base=cna_ag,varname="Indice diversidad economica",variable=cna_ag$ind_div_econ)
arroz_upa <- rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)
rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)

stargazer::stargazer(arroz_upa, summary = FALSE, rownames = FALSE,
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = label2,
                     title = "Estadisticas descriptivas por tamanio de tierra - Arroz",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_arroz_var_tierra.doc"))

#-------------------------------------------------------#
# 4. Maiz ----
#-------------------------------------------------------#

#--------------#
#REGION -------#
#--------------#
tabla6 <- function(base,varname,variable) {
  base <- cbind(base,variable)
  agregado_clas <- base %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)") %>%
    dplyr::filter(region!="NA") %>% group_by(region) %>%
    summarise(var = varname,mean=round(mean(variable,na.rm = TRUE),1),sd=round(sd(variable,na.rm = TRUE),1),
              min=round(min(variable,na.rm = TRUE),1),
              max=round(max(variable,na.rm = TRUE),1)) %>% ungroup() %>%
    relocate(var,region,mean,sd,min,max)
  return(agregado_clas)
}
var1 <- tabla6(base=cna_ag,varname="Productividad (Cobb-Douglas)",variable=cna_ag$ls2)
var2 <- tabla6(base=cna_ag,varname="Rendimiento por hectarea sembrada",variable=cna_ag$rend_cos)
var3 <- tabla6(base=cna_ag,varname="Edad",variable=cna_ag$edad)
var4 <- tabla6(base=cna_ag,varname="Anios educacion",variable=cna_ag$anios_educ)
var5 <- tabla6(base=cna_ag,varname="Temperatura (centigrados)",variable=cna_ag$temp)
var6 <- tabla6(base=cna_ag,varname="Lluvias (mm)",variable=cna_ag$rain)
var7 <- tabla6(base=cna_ag,varname="Distancia mercado mayorista",variable=cna_ag$dismdo)
var8 <- tabla6(base=cna_ag,varname="Distancia mercado alimentos",variable=cna_ag$distancia_mercado)
var9 <- tabla6(base=cna_ag,varname="Transferencias SGP per-capita",variable=cna_ag$sgp_pc)
var10 <- tabla6(base=cna_ag,varname="Indice desempenio institucional",variable=cna_ag$ind_desempeno_int)
var11 <- tabla6(base=cna_ag,varname="Valor Agregado",variable=cna_ag$valor_agregado)
var12 <- tabla6(base=cna_ag,varname="Indice diversidad economica",variable=cna_ag$ind_div_econ)
maiz_reg <- rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)
rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)
stargazer::stargazer(maiz_reg, summary = FALSE, rownames = FALSE,
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = label1,
                     title = "Estadisticas descriptivas por region - Maiz",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_maiz_var_region.doc"))

#--------------#
#TIERRA -------#
#--------------#
tabla7 <- function(base,varname,variable) {
  base <- cbind(base,variable)
  agregado_clas <- base %>% dplyr::filter(clasificacion_utilizada == "Maíz (blanco o amarillo)") %>%
    dplyr::filter(region!="NA") %>% group_by(tipo_upa) %>%
    summarise(var = varname,mean=round(mean(variable,na.rm = TRUE),1),sd=round(sd(variable,na.rm = TRUE),1),
              min=round(min(variable,na.rm = TRUE),1),
              max=round(max(variable,na.rm = TRUE),1)) %>% ungroup() %>%
    relocate(var,tipo_upa,mean,sd,min,max)
  return(agregado_clas)
}
var1 <- tabla7(base=cna_ag,varname="Productividad (Cobb-Douglas)",variable=cna_ag$ls2)
var2 <- tabla7(base=cna_ag,varname="Rendimiento por hectarea sembrada",variable=cna_ag$rend_cos)
var3 <- tabla7(base=cna_ag,varname="Edad",variable=cna_ag$edad)
var4 <- tabla7(base=cna_ag,varname="Anios educacion",variable=cna_ag$anios_educ)
var5 <- tabla7(base=cna_ag,varname="Temperatura (centigrados)",variable=cna_ag$temp)
var6 <- tabla7(base=cna_ag,varname="Lluvias (mm)",variable=cna_ag$rain)
var7 <- tabla7(base=cna_ag,varname="Distancia mercado mayorista",variable=cna_ag$dismdo)
var8 <- tabla7(base=cna_ag,varname="Distancia mercado alimentos",variable=cna_ag$distancia_mercado)
var9 <- tabla7(base=cna_ag,varname="Transferencias SGP per-capita",variable=cna_ag$sgp_pc)
var10 <- tabla7(base=cna_ag,varname="Indice desempenio institucional",variable=cna_ag$ind_desempeno_int)
var11 <- tabla7(base=cna_ag,varname="Valor Agregado",variable=cna_ag$valor_agregado)
var12 <- tabla7(base=cna_ag,varname="Indice diversidad economica",variable=cna_ag$ind_div_econ)
maiz_tie <- rbind(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)
rm(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)
stargazer::stargazer(maiz_tie, summary = FALSE, rownames = FALSE,
                     digits = 1, align = FALSE, no.space = TRUE, covariate.labels = label2,
                     title = "Estadisticas descriptivas por tamanio de tierra - Maiz",
                     type = "html",
                     table.placement = "H",
                     label = "stats_table", 
                     out = glue("{tablas}/Descriptivas/cna_maiz_var_tierra.doc"))

