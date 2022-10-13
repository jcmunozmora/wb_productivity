#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 24 nov, 2021
# Analisis PCA a nivel de UPA 
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue,factoextra,FactoMineR,gridExtra)
# .rs.restartR()


#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales/CNA/Total_nacional(csv)"
datos <- "02_Datos/CNA"
graficas <- "03_Graficas"
options(scipen = 999)

#--------------------------#
# Labels ----
#--------------------------#

lb <- readxl::read_xlsx("02_Datos/CNA/CNA_Diccionario_Datos.xlsx",sheet="Variables") %>%
      transmute(var=VARIABLE,lbl=`DESCRIPCIÓN`)

#-------------------------------------------------------#
# 0. Funciones ----
#-------------------------------------------------------#


clean_dt <- function(db) {
  
  db0 <- db[, which(colMeans(!is.na(db)) > 0.05)]
  db1 <- db0[, which(colMeans(!is.na(db0)) > 0.1)]
  db2 <- db1[, which(colMeans(!is.na(db1)) > 0.2)]
  
  if (length(db2)>=7) {
    db <- db2
  } else if (length(db1) >= 7) {
    db <- db1
  } else if (length(db0) >= 7) {
    db <- db0
  } else {
    db <- db
  }

  db <- db %>% mutate_all(funs(ifelse(is.na(.),0,.)))
  
  return(db)
}

### Extra Labels
extract_label <- function(db) {
  
  f <- data.frame(c(colnames(db)))
  colnames(f) <- c("var")
  
  f <- merge(f,lb)
  
}

### Get PCA
get_pca <- function(db,name_pca) {
  
  ds_end <- db[, !(colnames(db) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA"))]
  
  ### Get pca
  pca <- prcomp(ds_end, retx=TRUE, center=TRUE,scale = TRUE)

  #### Figure -- Export
  p1 <- fviz_eig(pca, addlabels = TRUE, ylim = c(0, 90))
  p2 <-fviz_contrib(pca, choice = "var", axes = 1, top = 10)
  p <- grid.arrange(p1,p2,nrow = 2)
  ggsave(paste0("02_Datos/CNA/PCA/figures/",name_pca,".png"),p,width = 10,height = 5,dpi=300)
  
  #### Label -- Export
  writexl::write_xlsx(extract_label(ds_end),paste0("02_Datos/CNA/PCA/figures/",name_pca,".xlsx")) 
  
  ### Get the first
  pred <- predict(pca, newdata=ds_end)
  ds_o <- db[, colnames(db) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA")]
  out <- cbind(ds_o,pred[,1])
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
# 2. Unidad productora ----
#-------------------------------------------------------#

#--------------------------#
# A. Proteccion recursos ----
#--------------------------#

# Unidad productora: agua (tiene acceso )
db_water <- und_produc %>% 
  dplyr::filter(TIPO_UC == 1) %>%
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##FUENTE DE AGUA  (Solo con fuentes)... 
                "P_S11P124_SP1","P_S11P124_SP2","P_S11P124_SP3","P_S11P124_SP4","P_S11P124_SP5","P_S11P124_SP6","P_S11P124_SP7","P_S11P124_SP8","P_S11P124_SP9","P_S11P124_SP10","P_S11P124_SP11",
                
                ### PROTECCION FUENTES DE AGUA
                "P_S11P125_SP1","P_S11P125_SP2","P_S11P125_SP3","P_S11P125_SP4","P_S11P125_SP5","P_S11P125_SP6","P_S11P125_SP7","P_S11P125_SP8","P_S11P125_SP9","P_S11P125_SP10","P_S11P125_SP11","P_S11P125_SP12",
                ## DIFICULTAD USO DEL AGUA
                "P_S11P126_SP1","P_S11P126_SP2","P_S11P126_SP3","P_S11P126_SP4","P_S11P126_SP5","P_S11P126_SP6","P_S11P126_SP7","P_S11P126_SP8","P_S11P126_SP9") 

db_water <- get_pca(clean_dt(db_water),"f01_water")

saveRDS(db_water, glue("{datos}/PCA/db_water.rds"))

#--------------------------#
# B. Manejo de Suelos ----
#--------------------------#

# Unidad productora: suelos
db_soil <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##SOIL PRACTICES
                "P_S11P127_SP1","P_S11P127_SP2","P_S11P127_SP3","P_S11P127_SP4","P_S11P127_SP5","P_S11P127_SP6","P_S11P127_SP7","P_S11P127_SP8","P_S11P127_SP9","P_S11P127_SP10","P_S11P127_SP11") 

db_soil <- get_pca(clean_dt(db_soil),"f02_soil")

saveRDS(db_soil, glue("{datos}/PCA/db_soil.rds"))

#--------------------------#
# C. Fuentes de Energia
#--------------------------#

# Unidad productora: fuente energia
db_energy <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##Energy
                "P_S11P133_SP1","P_S11P133_SP2","P_S11P133_SP3","P_S11P133_SP4","P_S11P133_SP5","P_S11P133_SP6","P_S11P133_SP7","P_S11P133_SP8","P_S11P133_SP9","P_S11P133_SP10") 


db_energy <- get_pca(clean_dt(db_energy),"f03_energy")

saveRDS(db_energy, glue("{datos}/PCA/db_energy.rds"))

#--------------------------#
# D. Asistencia Técnica
#--------------------------#

# Unidad productora: asistencia tecnica
db_assit <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##Energy
                "P_S11P135A_SP1","P_S11P135A_SP2","P_S11P135A_SP3","P_S11P135A_SP4","P_S11P135A_SP5","P_S11P135A_SP6","P_S11P135A_SP7","P_S11P135A_SP8","P_S11P135A_SP9","P_S11P135A_SP10","P_S11P135A_SP11") 


db_assit <- get_pca(clean_dt(db_assit),"f04_assit")

saveRDS(db_assit, glue("{datos}/PCA/db_assit.rds"))

#--------------------------#
# D. Acceso a Crédito
#--------------------------#

# Unidad productora: credito
db_credit <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##QUE CREDITO SOLICITO (FUENTE)
                "P_S11P136B_SP1","P_S11P136B_SP2","P_S11P136B_SP3","P_S11P136B_SP4","P_S11P136B_SP5","P_S11P136B_SP6","P_S11P136B_SP7","P_S11P136B_SP8",
                ##SOLICITO CREDITO
                "P_S11P136",
                ##APROBACION DEL CREDITO
                "P_S11P136A" ) 

db_credit <- get_pca(clean_dt(db_credit),"f05_credit")

saveRDS(db_credit, glue("{datos}/PCA/db_credit.rds"))

#--------------------------#
# C. Trabajo ----
#--------------------------#

db_workers <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##NUMERO DE TRABAJADORES
                "P_S11P138","P_S11P138A","P_S11P138B","P_S11P139","P_S11P139A","P_S11P139B","P_S11P140","P_S11P141" ) 

db_workers <- get_pca(clean_dt(db_workers),"f06_workers")

saveRDS(db_workers, glue("{datos}/PCA/db_workers.rds"))

#--------------------------#
# D. Manejo cultivos ----
#--------------------------#

db_mag_crops <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##Use of fertilizers etc.
                "P_S6P76_SP1","P_S6P76_SP2","P_S6P76_SP3","P_S6P76_SP4","P_S6P76_SP5","P_S6P76_SP6","P_S6P76_SP7","P_S6P76_SP8" ) 

db_mag_crops <- get_pca(clean_dt(db_mag_crops),"f07_mag_crops")

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
                "P_S11P129_SP1","P_S11P129_SP2","P_S11P129_SP3","P_S11P129_SP4","P_S11P129_SP5","P_S11P129_SP6","P_S11P129_SP7","P_S11P129_SP8","P_S11P129_SP9","P_S11P129_SP10") 

db_other <- get_pca(clean_dt(db_other),"f08_others")

saveRDS(db_other, glue("{datos}/PCA/db_other.rds"))

#--------------------------#
# 09. Control de Plagas
#--------------------------#

db_plaque <- und_produc %>% 
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                ##Type of plaque controls practices
                "P_S6P77_SP1","P_S6P77_SP2","P_S6P77_SP3","P_S6P77_SP4","P_S6P77_SP5","P_S6P77_SP6","P_S6P77_SP7","P_S6P77_SP8","P_S6P77_SP9","P_S6P77_SP10","P_S6P77_SP11")

db_plaque <- get_pca(clean_dt(db_plaque),"f09_plaques")

saveRDS(db_plaque, glue("{datos}/PCA/db_plaque.rds"))

#-------------------------------------------------------#
# 1' Animales ----
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
# BASE DATOS
#-------------------------------------------------------#

# Abrimos datos y creamos base final
#db_water <- readRDS(glue("{datos}/PCA/db_water.rds"))
#db_soil <- readRDS(glue("{datos}/PCA/db_soil.rds"))
#db_energy <- readRDS(glue("{datos}/PCA/db_energy.rds"))
#db_assit <- readRDS(glue("{datos}/PCA/db_assit.rds"))
#db_credit <- readRDS(glue("{datos}/PCA/db_credit.rds"))
#db_workers <- readRDS(glue("{datos}/PCA/db_workers.rds"))
#db_mag_crops <- readRDS(glue("{datos}/PCA/db_mag_crops.rds"))
#db_other <- readRDS(glue("{datos}/PCA/db_other.rds"))
#db_plaque <- readRDS(glue("{datos}/PCA/db_plaque.rds"))
#db_animals <- readRDS(glue("{datos}/PCA/db_animals.rds"))

ds_final <- und_produc %>%
  #### Select VARIABLES
  dplyr::select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA) %>%
  ### CULTIVOS
  #left_join(db_cultivos) %>%
  ### MAQUINARIA
  #left_join(db_maqu) %>%
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
  left_join(db_animals) 


saveRDS(ds_final, glue("{datos}/PCA/base_PCA_censo.rds"))




################## CHECK BELOW ############


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


data <- readRDS(glue("{datos}/PCA/base_PCA_censo.rds")) %>% 
  dplyr::select(P_MUNIC, COD_VEREDA, n_upas, starts_with("f"), TLU, equi_new, equi_old)

saveRDS(data, glue("02_Datos/Productividad/base_PCA_censo.rds"))
