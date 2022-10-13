#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
#mpio <- ifelse(nchar(args[1])==4,paste0("0",args[1]),args[1])
dpto <- args[1]


##### ------------------------------- ######
##### ------------------------------- ######
#       Project: Climate Change and Agriculture
#       Created by: Vileidy Gonzales y Juan Carlos Muñoz
#       Creation Date: Marzo 5
#       Description:
#       It reads the CNA
##### ------------------------------- ######
##### ------------------------------- ######

  ##### Packages
  library(haven)
  library(tidyverse)
  library(stats)



#dpto="99"
  ### Functions
get_pca <- function(db,name_pca) {

        pca <- prcomp(round(db[, !(colnames(db) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA"))],6), retx=TRUE, center=TRUE)

        ### Get the first
        pred <- predict(pca, newdata=db[, !(colnames(db) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA"))])

        out <- cbind(db[, colnames(db) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA")],pred[,1])

        colnames(out) <- c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA",name_pca)

  return(out)

}


  ###### Working directory


      ### Data Path
    main_path <- "/Users/juan-carlosm/Universidad EAFIT/Vileydy Adriana Gonzalez Mejia - Climate_Change_Tesis/06_Rawdata/06_Final_CNA"
    #main_path <- "/gpfs/scratch/upf06/upf06662/10_Climate_Change"

### Data Path VILEYDY
#main_path <- "/Users/usuario/OneDrive - Universidad EAFIT/Climate_Change_Tesis/06_Rawdata/06_Final_CNA"

      ### Out Path
  path_data <- paste0(main_path,"/00_Rawdata/02_CNA_Dataset/")
  path_codes <-  paste0(main_path,"/aux_codes/")
  path_out <- paste0(main_path,"/02_Outputs_CNA/")
  

###################################################
############ ---- 00 - Cultivos ----- ############
###################################################

  ############ ---- Variables Seleccionadas ----- ############
  #P_S6P46= ¿Cuál cultivo o plantación forestal tiene en el lote.?
  #P_S6P52_SP99= Cuáles de los siguientes sistemas de riego utiliza? No Utiliza
  #P_S6P52_SPA02= ¿Cuáles de los siguientes sistemas de riego utiliza? Goteo
  #P_S6P52_SPA04= ¿Cuáles de los siguientes sistemas de riego utiliza? Aspersion
  #P_S6P52_SPC20= ¿Cuáles de los siguientes sistemas de riego utiliza? Gravedad
  #P_S6P52_SPC21= ¿Cuáles de los siguientes sistemas de riego utiliza? Manual o por mateo
  #P_S6P52_SPC22= ¿Cuáles de los siguientes sistemas de riego utiliza? Bombeo
  # P_S6P59_UNIF= Rendimiento (Ton/ Ha)

  ############ ---- Read DataBase ----- ############
  cultivos <- read_dta(paste0(path_data,"S06A(Cultivos)_",dpto,".dta")) %>%
                      ### Select only Mpio
                      #filter(P_MUNIC==mpio) %>%
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
                      select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,P_S6P46,P_S6P52_SP99,P_S6P52_SPA02,P_S6P52_SPA04,P_S6P52_SPC20,P_S6P52_SPC21,P_S6P52_SPC22,P_S6P59_UNIF,AREA_SEMBRADA,AREA_COS) %>%  mutate_all(funs(ifelse(is.na(.),0,.)))
                      ## Include Crop classifcation
                      ############# --- Include Crop classification --- ####

    pca_cult <- prcomp(cultivos[, !(colnames(cultivos) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA","P_S6P46","P_S6P59_UNIF","AREA_SEMBRADA","AREA_COS","AREA_SEMBRADA","AREA_COS"))], retx=TRUE, center=TRUE)

    ### Get the first
    pred_cult <- predict(pca_cult, newdata=cultivos[, !(colnames(cultivos) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA","P_S6P46","P_S6P59_UNIF","AREA_SEMBRADA","AREA_COS","AREA_SEMBRADA","AREA_COS"))])

    cultivos <- cbind(cultivos[, colnames(cultivos) %in% c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA","P_S6P46","P_S6P59_UNIF","AREA_SEMBRADA","AREA_COS","AREA_SEMBRADA","AREA_COS")],pred_cult[,1])

      colnames(cultivos) <- c("P_DEPTO","P_MUNIC","UC_UO","ENCUESTA","COD_VEREDA","P_S6P46","P_S6P59_UNIF","AREA_SEMBRADA","AREA_COS","f00_irrig")

    source(paste0(path_codes,"aux_cultivos.R"))

    ############ ---- Create Data at Crop level ----- ############
    db_cultivos <- cultivos %>%  select(-P_S6P46) %>%
                      ### Add at crop type level
                      group_by(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,crop_type) %>%
                      ### Get All Values
                      mutate(n_crops=1)  %>%
                      summarise_all(sum, na.rm = TRUE) %>%
                      ### Get Agregados veredales
                      ungroup() %>% group_by(COD_VEREDA,crop_type) %>%
                      mutate(AREA_SEMBRADA_T=sum(AREA_SEMBRADA,na.rm=TRUE),AREA_COS_T=sum(AREA_COS,na.rm=TRUE)) %>%
                      ### Go back to the grouping level
                      ungroup() %>% group_by(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,crop_type) %>%
                      ## Create a variable of crops to get the percentage
                      mutate(n_crops_temp=n_crops) %>%
                      ### Gather variables
                      gather(variable, value, -(P_DEPTO:COD_VEREDA),-crop_type,-n_crops_temp) %>%
                      ### Create the value % of all Crops
                      mutate(value=ifelse(value=="n_crops","n_crops",value/n_crops_temp))  %>%
                      select(-n_crops_temp) %>%
                      ### Crear el nombre de las variables
                      unite(temp, crop_type, variable)  %>%
                      ## Spread data set
                      spread(temp, value,fill=0)

###################################################
############ ---- 00 - Maquinaria ----- ############
###################################################
  db_maqu <- read_dta(paste0(path_data,"S09(Maquinaria_uso_agropecuario)_",dpto,".dta")) %>%
    ### Select only Mpio
    #filter(P_MUNIC==mpio) %>% 
    ## Other Filter    
    filter(!is.na(P_S9P118)) %>%
    mutate(equi_new=P_S9P119,equi_old=P_S9P120)  %>%
    ## group_by
    group_by(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA)  %>%
    summarise_at(vars(equi_new,equi_old),sum,na.rm=TRUE) %>%
    mutate(equi_n=equi_new+equi_old)



###################################################
############ ---- 01- Unidad Productora ----- ############
###################################################

und_produc <- read_dta(paste0(path_data,"S01_15(Unidad_productora)_",dpto,".dta")) %>%
          ### Select only Mpio
          #filter(P_MUNIC==mpio) %>%
          ### Solo UPAS (  #1:UPA #2:UPNA)
          filter(TIPO_UC==1) %>%
          ##solo necesito las Upas que tengan una extencion maxima de 20 hectareas, pero la pregunta esta en metros entonces 20*10000
          filter(P_S5PAUTOS<=200000)
          


          ###################################################
          ############ ---- 01 - Source, water protection practices, water use problems ----- ############
          ##################################################

            db_water <- und_produc %>% select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                  ##FUENTE DE AGUA
                  "P_S11P124_SP1","P_S11P124_SP2","P_S11P124_SP3","P_S11P124_SP4","P_S11P124_SP5","P_S11P124_SP6","P_S11P124_SP7","P_S11P124_SP8","P_S11P124_SP9","P_S11P124_SP10","P_S11P124_SP11",
                  ### PROTECCION FUENTES DE AGUA
                  "P_S11P125_SP1","P_S11P125_SP2","P_S11P125_SP3","P_S11P125_SP4","P_S11P125_SP5","P_S11P125_SP6","P_S11P125_SP7","P_S11P125_SP8","P_S11P125_SP9","P_S11P125_SP10","P_S11P125_SP11","P_S11P125_SP12",
                  ## DIFICULTAD USO DEL AGUA
                  "P_S11P126_SP1","P_S11P126_SP2","P_S11P126_SP3","P_S11P126_SP4","P_S11P126_SP5","P_S11P126_SP6","P_S11P126_SP7","P_S11P126_SP8","P_S11P126_SP9") %>%
                  ### Clean Data Set
                  mutate_all(funs(ifelse(is.na(.),0,.)))  %>% get_pca(.,"f01_water")


            ###################################################
            ############ ---- Soil protection practices ----- ############
            ##################################################

            db_soil <- und_produc %>% select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                                              ##SOIL PRACTICES
                                              "P_S11P127_SP1","P_S11P127_SP2","P_S11P127_SP3","P_S11P127_SP4","P_S11P127_SP5","P_S11P127_SP6","P_S11P127_SP7","P_S11P127_SP8","P_S11P127_SP9","P_S11P127_SP10","P_S11P127_SP11") %>%
              ### Clean Data Set
              mutate_all(funs(ifelse(is.na(.),0,.))) %>% get_pca(.,"f02_soil")

            ###################################################
            ############ ---- Type of energy source ----- ############
            ##################################################

            db_energy <- und_produc %>% select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                                             ##Energy
                                             "P_S11P133_SP1","P_S11P133_SP2","P_S11P133_SP3","P_S11P133_SP4","P_S11P133_SP5","P_S11P133_SP6","P_S11P133_SP7","P_S11P133_SP8","P_S11P133_SP9","P_S11P133_SP10") %>%
              ### Clean Data Set
              mutate_all(funs(ifelse(is.na(.),0,.))) %>% get_pca(.,"f03_energy")

            ###################################################
            ############ ---- Type of technical assistance ----- ############
            ##################################################

            db_assit <- und_produc %>% select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                                               ##Energy
                                              "P_S11P135A_SP1","P_S11P135A_SP2","P_S11P135A_SP3","P_S11P135A_SP4","P_S11P135A_SP5","P_S11P135A_SP6","P_S11P135A_SP7","P_S11P135A_SP8","P_S11P135A_SP9","P_S11P135A_SP10","P_S11P135A_SP11") %>%
              ### Clean Data Set
              mutate_all(funs(ifelse(is.na(.),0,.))) %>% get_pca(.,"f04_assit")

            ###################################################
            ############ ---- Credit ----- ############
            ##################################################

            db_credit <- und_produc %>% select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                                              ##QUE CREDITO SOLICITO (FUENTE)
                                              "P_S11P136B_SP1","P_S11P136B_SP2","P_S11P136B_SP3","P_S11P136B_SP4","P_S11P136B_SP5","P_S11P136B_SP6","P_S11P136B_SP7","P_S11P136B_SP8",
                                              ##SOLICITO CREDITO
                                              "P_S11P136",
                                              ##APROBACION DEL CREDITO
                                              "P_S11P136A" ) %>%
              ### Clean Data Set
              mutate_all(funs(ifelse(is.na(.),0,.))) %>% get_pca(.,"f05_credit")

            ###################################################
            ############ ---- Workers ----- ############
            ##################################################

            db_workers <- und_produc %>% select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                                               ##NUMERO DE TRABAJADORES
                                             "P_S11P138","P_S11P138A","P_S11P138B","P_S11P139","P_S11P139A","P_S11P139B","P_S11P140","P_S11P141" ) %>%
              ### Clean Data Set
              mutate_all(funs(ifelse(is.na(.),0,.))) %>% get_pca(.,"f06_workers")

            ###################################################
            ############ ---- Crop management ----- ############
            ##################################################

            db_mag_crops <- und_produc %>% select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                                                ##Use of fertilizers etc.
                                                "P_S6P76_SP1","P_S6P76_SP2","P_S6P76_SP3","P_S6P76_SP4","P_S6P76_SP5","P_S6P76_SP6","P_S6P76_SP7","P_S6P76_SP8" ) %>%
              ### Clean Data Set
              mutate_all(funs(ifelse(is.na(.),0,.))) %>% get_pca(.,"f07_mag_crops")

            ###################################################
            ############ ---- Other ----- ############
            ##################################################

            db_other <- und_produc %>% select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                                                  ##GESTION DE RESIDUOS (ANIMALES Y VEGETALES)
                                                  "P_S11P131_SP1","P_S11P131_SP2","P_S11P131_SP3","P_S11P131_SP4","P_S11P131_SP5","P_S11P131_SP6","P_S11P131_SP7","P_S11P131_SP8","P_S11P131_SP9","P_S11P131_SP10","P_S11P131_SP11","P_S11P131_SP12","P_S11P131_SP13","P_S11P131_SP14","P_S11P131_SP15",
                                                  ## GESTION DE RESIDUOS (PASTICO, VIDRIO, PVC)
                                                  "P_S11P132_SP1","P_S11P132_SP2","P_S11P132_SP3","P_S11P132_SP4","P_S11P132_SP5",
                                                  ##RECURSOS NATURALES
                                                  "P_S11P129_SP1","P_S11P129_SP2","P_S11P129_SP3","P_S11P129_SP4","P_S11P129_SP5","P_S11P129_SP6","P_S11P129_SP7","P_S11P129_SP8","P_S11P129_SP9","P_S11P129_SP10") %>%
              ### Clean Data Set
              mutate_all(funs(ifelse(is.na(.),0,.))) %>% get_pca(.,"f08_others")

            ###################################################
            ############ ---- Plaque control ----- ############
            ##################################################

            db_plaque <- und_produc %>% select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
                                                  ##Type of plaque controls practices
                                                  "P_S6P77_SP1","P_S6P77_SP2","P_S6P77_SP3","P_S6P77_SP4","P_S6P77_SP5","P_S6P77_SP6","P_S6P77_SP7","P_S6P77_SP8","P_S6P77_SP9","P_S6P77_SP10","P_S6P77_SP11") %>%
              ### Clean Data Set
              mutate_all(funs(ifelse(is.na(.),0,.))) %>% get_pca(.,"f09_plaques")
      
      
      
         
  ###################################################
  ############ ---- Count of animals----- ############
  ##################################################
  ##################################################

        ## TLU
        ## Tropical Livestock Units are livestock numbers converted to a common unit (in 2005). Conversion factors are: cattle = 0.7 (x), sheep = 0.1, goats = 0.1, pigs = 0.25, chicken = 0.01,Asses=0.5,Horses=0.65,Mules=0.6. Factors taken mostly from Chilonda, P. and J Otte. Indicators to Monitor Trends in Livestock Production at National, Regional and International Levels. Livestock Research for Rural Development, v.18, no.8, 2006. (http://www.lrrd.org/lrrd18/8/chil18117.htm)

  db_animals <- und_produc %>% select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
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
              select(-starts_with("P_"))
             ##


  ###################################################
  ############ ---- Non-agri production (Y/N)----- ############
  ##################################################
  ##################################################

    ### Clean Data Set
    db_nonagri <- read_dta(paste0(path_data,"S14(Actividad_no_agropecuaria)_",dpto,".dta")) %>%
  ### Select only Mpio
  #filter(P_MUNIC==mpio) %>%
    ### Solo UPAS (  #1:UPA #2:UPNA)
    select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA,
           ## E.g. Deforestation, Milk processing, sugar refinery
           "P_S14P157_SP1","P_S14P157_SP2","P_S14P157_SP3","P_S14P157_SP4","P_S14P157_SP5","P_S14P157_SP6","P_S14P157_SP7","P_S14P157_SP8","P_S14P157_SP9","P_S14P157_SP10","P_S14P157_SP11","P_S14P157_SP12","P_S14P157_SP13","P_S14P157_SP14","P_S14P157_SP15","P_S14P157_SP16","P_S14P157_SP17","P_S14P157_SP18","P_S14P157_SP19","P_S14P157_SP20","P_S14P157_SP21","P_S14P157_SP22","P_S14P157_SP23",
           #E.g. Education, health, religious services, mining
           "P_S14P157_SP24","P_S14P157_SP25","P_S14P157_SP26","P_S14P157_SP27","P_S14P157_SP28","P_S14P157_SP29","P_S14P157_SP30","P_S14P157_SP31","P_S14P157_SP32","P_S14P157_SP33","P_S14P157_SP34",
           # Count of workers in each activity
           "P_S14P157A","P_S14P157B","P_S14P157C","P_S14P157D","P_S14P157E","P_S14P157F","P_S14P157G","P_S14P157H","P_S14P157I","P_S14P157J","P_S14P157K","P_S14P157L","P_S14P157M","P_S14P157N","P_S14P157O","P_S14P157P","P_S14P157Q","P_S14P157R","P_S14P157S","P_S14P157T","P_S14P157U","P_S14P157V","P_S14P157W",
           # Count of workers in each activity
           "P_S14P157X","P_S14P157Y","P_S14P157Z","P_S14P157AA","P_S14P157AB","P_S14P157AC","P_S14P157AD","P_S14P157AE","P_S14P157AF","P_S14P157AG","P_S14P157AH") %>%
          ## Fix Variable NA -> 0
    mutate_all(funs(ifelse(is.na(.),0,.))) %>% get_pca(.,"f10_nonagri")


      ###################################################
      ############ ---- FINAL DATA ET ----- ############
      ###################################################
      
      
      ############ ---- Read Data base ----- ############
      ds_final <- und_produc %>%
        #### Select VARIABLES
        select(P_DEPTO,P_MUNIC,UC_UO,ENCUESTA,COD_VEREDA)  %>%
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
        left_join(db_nonagri) %>%
        ##### ---- CLEAN DATA ---- #####
      ##### Clean data set
      mutate_all(funs(ifelse(is.na(.),0,.))) %>%
        ##### ---- ----- ---- #####
      #### Collapsing
      ##### ---- ----- ---- #####
      select(-P_DEPTO,-(UC_UO:ENCUESTA)) %>%
        ### Create important VARIABLES
        mutate(n_upas=1) %>%
        group_by(P_MUNIC,COD_VEREDA) %>%
        mutate(n_upas=sum(n_upas,na.rm=TRUE)) %>%
        summarise_all(mean, na.rm = TRUE)

###################################################
############ ---- 02 - Unidades productoras ----- ############
###################################################

############ ---- Save Data Set ----- ############
  write.csv(ds_final, file=paste0(path_out,"cna_",dpto,".csv"))
