#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 24 nov, 2021
# Analisis PCA a nivel de UPA 
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
cna <- readRDS(glue("{datos}/base_cna_all_maq.rds")) %>%
  distinct(cod_dpto, cod_mpio, cod_vereda, encuesta)

# No tenemos estas preguntas en base limpia, abrimos CNA original y solo tomamos UPAs en muestra final
und_produc <- read_csv(glue("{datos_ori}/S01_15(Unidad_productora).csv")) %>% 
  dplyr::filter(ENCUESTA %in% cna$encuesta)

#-------------------------------------------------------#
# 1. Cultivos y maquinaria ----
#-------------------------------------------------------#

#--------------------------#
# A. Cultivos ----
#--------------------------#

# Abrimos base de cultivos
# No tenemos estas preguntas en base limpia, abrimos CNA original y solo tomamos UPAs en muestra final
cultivos <- read_csv(glue("{datos_ori}/S06A(Cultivos).csv")) %>%
  dplyr::filter(ENCUESTA %in% cna$encuesta) 

und_cultivos <- und_produc %>% 
  dplyr::select(P_DEPTO, P_MUNIC, UC_UO, ENCUESTA, COD_VEREDA, P_S6P61_SP1:P_S6P61_SP11,
                P_S6P52_SP99,P_S6P52_SPA02,P_S6P52_SPA04,P_S6P52_SPC20,P_S6P52_SPC21,P_S6P52_SPC22)

cultivos <- cultivos %>%
  left_join(und_cultivos, by = c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA")) %>%
  ### Solo cultivos PRESENTES
  filter(P_S6P45A==1) %>%
  ### Borraos los de Autoconsumo
  mutate(n_usos = select(., P_S6P61_SP1:P_S6P61_SP11) %>% apply(1, sum, na.rm=TRUE)) %>%
  ### Borrar solo los que son Autoconsumo
  filter(!(n_usos==1 & P_S6P61_SP1==1)) %>%
  ### Borrar solo trueque
  filter(!(n_usos==1 & P_S6P61_SP2==1)) %>%
  ### Borrar solo trueque o autoconsumo
  filter(!(n_usos==2 & P_S6P61_SP1==1 & P_S6P61_SP2==1)) %>%
  ### Seleccionar solo algunos VARIABLES
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,P_S6P46,P_S6P52_SP99,P_S6P52_SPA02,
                P_S6P52_SPA04,P_S6P52_SPC20,P_S6P52_SPC21,P_S6P52_SPC22,P_S6P59_UNIF,
                AREA_SEMBRADA,AREA_COSECHADA) %>%  
  mutate_all(funs(ifelse(is.na(.),0,.)))

# PCA
pca_cult <- prcomp(cultivos[, !(colnames(cultivos) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA","P_S6P46","P_S6P59_UNIF","AREA_SEMBRADA","AREA_COS","AREA_SEMBRADA","AREA_COSECHADA"))], retx=TRUE, center=TRUE)
pred_cult <- predict(pca_cult, newdata=cultivos[, !(colnames(cultivos) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA","P_S6P46","P_S6P59_UNIF","AREA_SEMBRADA","AREA_COS","AREA_SEMBRADA","AREA_COSECHADA"))])
cultivos <- cbind(cultivos[, colnames(cultivos) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA","P_S6P46","P_S6P59_UNIF","AREA_SEMBRADA","AREA_COS","AREA_SEMBRADA","AREA_COSECHADA")],pred_cult[,1])
colnames(cultivos) <- c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA","P_S6P46","P_S6P59_UNIF","AREA_SEMBRADA","AREA_COSECHADA","f00_irrig")

# Datos a nivel de cultivo
db_cultivos <- cultivos %>%  
  dplyr::rename(crop_type = P_S6P46) %>%
  # dplyr::select(-P_S6P46) %>%
  ### Add at crop type level
  group_by(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,crop_type) %>%
  ### Get All Values
  mutate(n_crops=1)  %>%
  summarise_all(sum, na.rm = TRUE) %>%
  ### Get Agregados veredales
  ungroup() %>% group_by(COD_VEREDA,crop_type) %>%
  mutate(AREA_SEMBRADA_T=sum(AREA_SEMBRADA,na.rm=TRUE),AREA_COS_T=sum(AREA_COSECHADA,na.rm=TRUE)) %>%
  ### Go back to the grouping level
  ungroup() %>% group_by(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,crop_type) %>%
  ## Create a variable of crops to get the percentage
  mutate(n_crops_temp=n_crops) %>%
  ### Gather variables
  gather(variable, value, -(P_DEPTO:COD_VEREDA),-crop_type,-n_crops_temp) %>%
  ### Create the value % of all Crops
  mutate(value=ifelse(value=="n_crops","n_crops",value/n_crops_temp))  %>%
  dplyr::select(-n_crops_temp) %>%
  ### Crear el nombre de las variables
  unite(temp, crop_type, variable)  %>%
  ## Spread data set
  spread(temp, value,fill=0)

saveRDS(db_cultivos, glue("{datos}/PCA/db_cultivos.rds"))

#--------------------------#
# B. Maquinaria ----
#--------------------------#

# No tenemos estas preguntas en base limpia, abrimos CNA original y solo tomamos UPAs en muestra final
db_maqu <- read_csv(glue("{datos_ori}/S09(Maquinaria_uso_agropecuario).csv")) %>%
  dplyr::filter(ENCUESTA %in% cna$encuesta) %>%
  ## Other Filter    
  dplyr::filter(!is.na(P_S9P118)) %>%
  mutate(equi_new=P_S9P119,equi_old=P_S9P120)  %>%
  ## group_by
  group_by(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA)  %>%
  summarise_at(vars(equi_new,equi_old),sum,na.rm=TRUE) %>%
  mutate(equi_n=equi_new+equi_old)

saveRDS(db_maqu, glue("{datos}/PCA/db_maq.rds"))

#-------------------------------------------------------#
# 2. Unidad productora ----
#-------------------------------------------------------#

#--------------------------#
# A. Proteccion recursos ----
#--------------------------#

# Unidad productora: agua
db_water <- und_produc %>% 
  dplyr::filter(TIPO_UC == 1) %>%
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##FUENTE DE AGUA
                "P_S11P124_SP1","P_S11P124_SP2","P_S11P124_SP3","P_S11P124_SP4","P_S11P124_SP5","P_S11P124_SP6","P_S11P124_SP7","P_S11P124_SP8","P_S11P124_SP9","P_S11P124_SP10","P_S11P124_SP11",
                ### PROTECCION FUENTES DE AGUA
                "P_S11P125_SP1","P_S11P125_SP2","P_S11P125_SP3","P_S11P125_SP4","P_S11P125_SP5","P_S11P125_SP6","P_S11P125_SP7","P_S11P125_SP8","P_S11P125_SP9","P_S11P125_SP10","P_S11P125_SP11","P_S11P125_SP12",
                ## DIFICULTAD USO DEL AGUA
                "P_S11P126_SP1","P_S11P126_SP2","P_S11P126_SP3","P_S11P126_SP4","P_S11P126_SP5","P_S11P126_SP6","P_S11P126_SP7","P_S11P126_SP8","P_S11P126_SP9") %>%
  ### Clean Data Set
  mutate_all(funs(ifelse(is.na(.),0,.))) %>% 
  get_pca(.,"f01_water")

saveRDS(db_water, glue("{datos}/PCA/db_water.rds"))

# Unidad productora: suelos
db_soil <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
         ##SOIL PRACTICES
         "P_S11P127_SP1","P_S11P127_SP2","P_S11P127_SP3","P_S11P127_SP4","P_S11P127_SP5","P_S11P127_SP6","P_S11P127_SP7","P_S11P127_SP8","P_S11P127_SP9","P_S11P127_SP10","P_S11P127_SP11") %>%
  ### Clean Data Set
  mutate_all(funs(ifelse(is.na(.),0,.))) %>% 
  get_pca(.,"f02_soil")

saveRDS(db_soil, glue("{datos}/PCA/db_soil.rds"))

# Unidad productora: fuente energia
db_energy <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
         ##Energy
         "P_S11P133_SP1","P_S11P133_SP2","P_S11P133_SP3","P_S11P133_SP4","P_S11P133_SP5","P_S11P133_SP6","P_S11P133_SP7","P_S11P133_SP8","P_S11P133_SP9","P_S11P133_SP10") %>%
  ### Clean Data Set
  mutate_all(funs(ifelse(is.na(.),0,.))) %>% 
  get_pca(.,"f03_energy")

saveRDS(db_energy, glue("{datos}/PCA/db_energy.rds"))

#--------------------------#
# B. Asistencia y Credito ----
#--------------------------#

# Unidad productora: asistencia tecnica
db_assit <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##Energy
                "P_S11P135A_SP1","P_S11P135A_SP2","P_S11P135A_SP3","P_S11P135A_SP4","P_S11P135A_SP5","P_S11P135A_SP6","P_S11P135A_SP7","P_S11P135A_SP8","P_S11P135A_SP9","P_S11P135A_SP10","P_S11P135A_SP11") %>%
  ### Clean Data Set
  mutate_all(funs(ifelse(is.na(.),0,.))) %>% 
  get_pca(.,"f04_assit")

saveRDS(db_assit, glue("{datos}/PCA/db_assit.rds"))

# Unidad productora: credito
db_credit <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##QUE CREDITO SOLICITO (FUENTE)
                "P_S11P136B_SP1","P_S11P136B_SP2","P_S11P136B_SP3","P_S11P136B_SP4","P_S11P136B_SP5","P_S11P136B_SP6","P_S11P136B_SP7","P_S11P136B_SP8",
                ##SOLICITO CREDITO
                "P_S11P136",
                ##APROBACION DEL CREDITO
                "P_S11P136A" ) %>%
  ### Clean Data Set
  mutate_all(funs(ifelse(is.na(.),0,.))) %>% 
  get_pca(.,"f05_credit")

saveRDS(db_credit, glue("{datos}/PCA/db_credit.rds"))

#--------------------------#
# C. Trabajo ----
#--------------------------#

db_workers <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##NUMERO DE TRABAJADORES
                "P_S11P138","P_S11P138A","P_S11P138B","P_S11P139","P_S11P139A","P_S11P139B","P_S11P140","P_S11P141" ) %>%
  ### Clean Data Set
  mutate_all(funs(ifelse(is.na(.),0,.))) %>% 
  get_pca(.,"f06_workers")

saveRDS(db_workers, glue("{datos}/PCA/db_workers.rds"))

#--------------------------#
# D. Manejo cultivos ----
#--------------------------#

db_mag_crops <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##Use of fertilizers etc.
                "P_S6P76_SP1","P_S6P76_SP2","P_S6P76_SP3","P_S6P76_SP4","P_S6P76_SP5","P_S6P76_SP6","P_S6P76_SP7","P_S6P76_SP8" ) %>%
  ### Clean Data Set
  mutate_all(funs(ifelse(is.na(.),0,.))) %>% 
  get_pca(.,"f07_mag_crops")

saveRDS(db_mag_crops, glue("{datos}/PCA/db_mag_crops.rds"))

#--------------------------#
# E. Otros ----
#--------------------------#

# Gestion de residuos y recursos
db_other <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##GESTION DE RESIDUOS (ANIMALES Y VEGETALES)
                "P_S11P131_SP1","P_S11P131_SP2","P_S11P131_SP3","P_S11P131_SP4","P_S11P131_SP5","P_S11P131_SP6","P_S11P131_SP7","P_S11P131_SP8","P_S11P131_SP9","P_S11P131_SP10","P_S11P131_SP11","P_S11P131_SP12","P_S11P131_SP13","P_S11P131_SP14","P_S11P131_SP15",
                ## GESTION DE RESIDUOS (PASTICO, VIDRIO, PVC)
                "P_S11P132_SP1","P_S11P132_SP2","P_S11P132_SP3","P_S11P132_SP4","P_S11P132_SP5",
                ##RECURSOS NATURALES
                "P_S11P129_SP1","P_S11P129_SP2","P_S11P129_SP3","P_S11P129_SP4","P_S11P129_SP5","P_S11P129_SP6","P_S11P129_SP7","P_S11P129_SP8","P_S11P129_SP9","P_S11P129_SP10") %>%
  ### Clean Data Set
  mutate_all(funs(ifelse(is.na(.),0,.))) %>% 
  get_pca(.,"f08_others")

saveRDS(db_other, glue("{datos}/PCA/db_other.rds"))

# Control plagas
db_plaque <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##Type of plaque controls practices
                "P_S6P77_SP1","P_S6P77_SP2","P_S6P77_SP3","P_S6P77_SP4","P_S6P77_SP5","P_S6P77_SP6","P_S6P77_SP7","P_S6P77_SP8","P_S6P77_SP9","P_S6P77_SP10","P_S6P77_SP11") %>%
  ### Clean Data Set
  mutate_all(funs(ifelse(is.na(.),0,.))) %>% 
  get_pca(.,"f09_plaques")

saveRDS(db_plaque, glue("{datos}/PCA/db_plaque.rds"))

#-------------------------------------------------------#
# 2. Animales ----
#-------------------------------------------------------#

db_animals <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##Cattle, pig, poultry etc.
                P_S7P83A,P_S7P84A,"P_S7P102A","P_S7P102B","P_S7P102C","P_S7P102D","P_S7P102E","P_S7P102F","P_S7P102G","P_S7P102H","P_S7P102I","P_S7P102J","P_S7P102K","P_S7P102L","P_S7P106A","P_S7P106B","P_S7P106C","P_S7P106D","P_S7P106E","P_S7P106F","P_S7P106G","P_S7P106H","P_S7P106I","P_S7P106J","P_S7P106K","P_S7P106L","P_S7P106M","P_S7P106N","P_S7P106O","P_S7P106P","P_S7P106Q","P_S7P106R","P_S7P106S","P_S7P106T") %>%
  ### Clean Data
  mutate_all(funs(ifelse(is.na(.),0,.))) %>%
  ## GEN TLU
  mutate(TLU=
           ## ¿Cuántos machos de ganado bovino tiene en total?
           (P_S7P83A+P_S7P84A)*0.7+
           ## Caballos (horses=0.65)
           (P_S7P102C+P_S7P102D)*0.65+
           ## Mulos,mulas (buffalos y buffalas), (Mules=0.6)
           (P_S7P102E+P_S7P102F+P_S7P102A+P_S7P102B)*0.6+
           ##  burros y burras (Asses=0.5)
           (P_S7P102G+P_S7P102H)*0.5+
           ## Cerdos o marranos de traspatio (pigs = 0.25)
           P_S7P106A*0.25+
           ## Gallos, pollos y gallinas de traspatio (chicken = 0.01)
           P_S7P106B*0.01+
           ## ovejas y ovejos (sheep and goats= 0.1)
           (P_S7P102I+P_S7P102J+P_S7P102K+P_S7P102L)*0.1) %>%
  ## Select data
  dplyr::select(-starts_with("P_"))

saveRDS(db_animals, glue("{datos}/PCA/db_animals.rds"))

#-------------------------------------------------------#
# 3. Base final ----
#-------------------------------------------------------#

# Abrimos datos y creamos base final
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

write.csv(ds_final, glue("{datos}/Productividad/base_PCA_censo.csv"))
saveRDS(ds_final, glue("{datos}/CNA/PCA/base_PCA_censo.rds"))

data <- readRDS(glue("{datos}/PCA/base_PCA_censo.rds")) %>% 
  dplyr::select(P_MUNIC, COD_VEREDA, n_upas, starts_with("f"), TLU, equi_new, equi_old)

saveRDS(data, glue("02_Datos/Productividad/base_PCA_censo.rds"))
