#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 24 nov, 2021
# Analisis PCA a nivel de UPA 
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
packageList<-c("tidyverse", "glue")
lapply(packageList,require,character.only=TRUE)

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales/CNA/Total_nacional(csv)"
datos <- "02_Datos/CNA"
graficas <- "03_Graficas"
options(scipen = 999)

#-------------------------------------------------------#
# 0. Funciones ----
#-------------------------------------------------------#

get_pca <- function(db,name_pca) {
  
  pca <- prcomp(round(db[, !(colnames(db) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA"))],6), retx=TRUE, center=TRUE)
  
  ### Get the first
  pred <- predict(pca, newdata=db[, !(colnames(db) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA"))])
  out <- cbind(db[, colnames(db) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA")],pred[,1])
  colnames(out) <- c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA",name_pca)
  return(out)
  
}

#-------------------------------------------------------#
# 1. Organizar datos CNA ----
#-------------------------------------------------------#

# Abrimos base limpia Censo e identificamos encuestas de interes
print("abriendo CNA")
cna <- readRDS(glue("{datos}/base_cna_all_maq.rds")) %>%
  distinct(cod_dpto, cod_mpio, cod_vereda, encuesta)

# No tenemos estas preguntas en base limpia, abrimos CNA original y solo tomamos UPAs en muestra final
und_produc <- read_csv(glue("{datos_ori}/S01_15(Unidad_productora).csv")) %>% 
  dplyr::filter(ENCUESTA %in% cna$encuesta)

# #-------------------------------------------------------#
# # 1. Cultivos y maquinaria ----
# #-------------------------------------------------------#
# 
# #--------------------------#
# # A. Cultivos ----
# #--------------------------#
# 
# # Abrimos base de cultivos
# # No tenemos estas preguntas en base limpia, abrimos CNA original y solo tomamos UPAs en muestra final
# cultivos <- read_csv(glue("{datos_ori}/S06A(Cultivos).csv")) %>%
#   dplyr::filter(ENCUESTA %in% cna$encuesta) 
# 
# und_cultivos <- und_produc %>% 
#   dplyr::select(P_DEPTO, P_MUNIC, UC_UO, ENCUESTA, COD_VEREDA, P_S6P61_SP1:P_S6P61_SP11,
#                 P_S6P52_SP99,P_S6P52_SPA02,P_S6P52_SPA04,P_S6P52_SPC20,P_S6P52_SPC21,P_S6P52_SPC22)
# 
# print("procesando cultivos")
# cultivos <- cultivos %>%
#   left_join(und_cultivos, by = c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA")) %>%
#   ### Solo cultivos PRESENTES
#   filter(P_S6P45A==1) %>%
#   ### Borraos los de Autoconsumo
#   mutate(n_usos = select(., P_S6P61_SP1:P_S6P61_SP11) %>% apply(1, sum, na.rm=TRUE)) %>%
#   ### Borrar solo los que son Autoconsumo
#   filter(!(n_usos==1 & P_S6P61_SP1==1)) %>%
#   ### Borrar solo trueque
#   filter(!(n_usos==1 & P_S6P61_SP2==1)) %>%
#   ### Borrar solo trueque o autoconsumo
#   filter(!(n_usos==2 & P_S6P61_SP1==1 & P_S6P61_SP2==1)) %>%
#   ### Seleccionar solo algunos VARIABLES
#   dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,P_S6P46,P_S6P52_SP99,P_S6P52_SPA02,
#                 P_S6P52_SPA04,P_S6P52_SPC20,P_S6P52_SPC21,P_S6P52_SPC22,P_S6P59_UNIF,
#                 AREA_SEMBRADA,AREA_COSECHADA) %>%  
#   mutate_all(funs(ifelse(is.na(.),0,.)))
# 
# # PCA
# print("pca")
# pca_cult <- prcomp(cultivos[, !(colnames(cultivos) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA","P_S6P46","P_S6P59_UNIF","AREA_SEMBRADA","AREA_COS","AREA_SEMBRADA","AREA_COSECHADA"))], retx=TRUE, center=TRUE)
# pred_cult <- predict(pca_cult, newdata=cultivos[, !(colnames(cultivos) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA","P_S6P46","P_S6P59_UNIF","AREA_SEMBRADA","AREA_COS","AREA_SEMBRADA","AREA_COSECHADA"))])
# cultivos <- cbind(cultivos[, colnames(cultivos) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA","P_S6P46","P_S6P59_UNIF","AREA_SEMBRADA","AREA_COS","AREA_SEMBRADA","AREA_COSECHADA")],pred_cult[,1])
# colnames(cultivos) <- c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA","P_S6P46","P_S6P59_UNIF","AREA_SEMBRADA","AREA_COSECHADA","f00_irrig")
# 
# # Datos a nivel de cultivo
# print("base cultivos pca")
# db_cultivos <- cultivos %>%  
#   dplyr::rename(crop_type = P_S6P46) %>%
#   # dplyr::select(-P_S6P46) %>%
#   ### Add at crop type level
#   group_by(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,crop_type) %>%
#   ### Get All Values
#   mutate(n_crops=1)  %>%
#   summarise_all(sum, na.rm = TRUE) %>%
#   ### Get Agregados veredales
#   ungroup() %>% group_by(COD_VEREDA,crop_type) %>%
#   mutate(AREA_SEMBRADA_T=sum(AREA_SEMBRADA,na.rm=TRUE),AREA_COS_T=sum(AREA_COSECHADA,na.rm=TRUE)) %>%
#   ### Go back to the grouping level
#   ungroup() %>% group_by(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,crop_type) %>%
#   ## Create a variable of crops to get the percentage
#   mutate(n_crops_temp=n_crops) %>%
#   ### Gather variables
#   gather(variable, value, -(P_DEPTO:COD_VEREDA),-crop_type,-n_crops_temp) %>%
#   ### Create the value % of all Crops
#   mutate(value=ifelse(value=="n_crops","n_crops",value/n_crops_temp))  %>%
#   dplyr::select(-n_crops_temp) %>%
#   ### Crear el nombre de las variables
#   unite(temp, crop_type, variable)  %>%
#   ## Spread data set
#   spread(temp, value,fill=0)
# 
# saveRDS(db_cultivos, glue("{datos}/PCA/db_cultivos.rds"))

#-------------------------------------------------------#
# 3. Base final ----
#-------------------------------------------------------#

# Abrimos datos y creamos base final
print("leyendo bases de PCA")
db_cultivos <- readRDS(glue("{datos}/PCA/db_cultivos.rds"))
db_maqu <- readRDS(glue("{datos}/PCA/db_maq.rds"))
db_water <- readRDS(glue("{datos}/PCA/db_water.rds"))
db_soil <- readRDS(glue("{datos}/PCA/db_soil.rds"))
db_energy <- readRDS(glue("{datos}/PCA/db_energy.rds"))
db_assit <- readRDS(glue("{datos}/PCA/db_assit.rds"))
db_credit <- readRDS(glue("{datos}/PCA/db_credit.rds"))
db_workers <- readRDS(glue("{datos}/PCA/db_workers.rds"))
db_mag_crops <- readRDS(glue("{datos}/PCA/db_mag_crops.rds"))
db_other <- readRDS(glue("{datos}/PCA/db_other.rds"))
db_plaque <- readRDS(glue("{datos}/PCA/db_plaque.rds"))
db_animals <- readRDS(glue("{datos}/PCA/db_animals.rds"))

print("procesando bases PCA")
ds_final <- und_produc %>%
  #### Select VARIABLES
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA) %>%
  ### CULTIVOS
  left_join(db_cultivos) %>%
  ### MAQUINARIA
  left_join(db_maqu) %>%
  ### Source, water protection practices, water use problems
  left_join(db_water) %>%
  ### Soil protection practices
  left_join(db_soil) %>%
  ###   Type of energy source
  left_join(db_energy) %>%
  ### Type of technical assistance
  left_join(db_assit) %>%
  ### Credit
  left_join(db_credit) %>%
  ### Workers
  left_join(db_workers) %>%
  ### Crop management
  left_join(db_mag_crops) %>%
  ### Other
  left_join(db_other) %>%
  ###  Plaque control
  left_join(db_plaque) %>%
  ### Animals
  left_join(db_animals) %>%
  ### No agricultural production
  # left_join(db_nonagri) %>%
  ##### ---- CLEAN DATA ---- #####
  ##### Clean data set
  mutate_all(funs(ifelse(is.na(.),0,.))) %>%
    ##### ---- ----- ---- #####
  #### Collapsing
  ##### ---- ----- ---- #####
  dplyr::select(-P_DEPTO,-(UC_UO:ENCUESTA)) %>%
  ### Create important VARIABLES
  mutate(n_upas=1) %>%
  group_by(P_MUNIC,COD_VEREDA) %>%
  mutate(n_upas=sum(n_upas,na.rm=TRUE)) %>%
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup()

write.csv(ds_final, glue("02_Datos/Productividad/base_PCA_censo.csv"))
saveRDS(ds_final, glue("02_Datos/Productividad/base_PCA_censo.rds"))
saveRDS(ds_final, glue("{datos}/PCA/base_PCA_censo.rds"))
