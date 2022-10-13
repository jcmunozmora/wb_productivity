#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 21 feb, 2022
# Procesamiento de variables de control
# UPA: Caracteristicas productor, agroecologicas; Municipal: entorno, institucional, estructura economica
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readr, readxl, janitor, sf)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
graficas <- "03_Graficas"
options(scipen = 999)

#-------------------------------------------------------#
# 0. Funciones ----
#-------------------------------------------------------#

# Organizar dicotomicas del CNA
fv <- function(x) { ifelse(ifelse(is.na(x),0,x)==1,1,0) }
fv1 <- function(x) { ifelse(x>=1,1,0) }
mo_obra <- function(x,y) { (ifelse(is.na(x),0,x)/ifelse(is.na(y),0,y))}
maq <- function(x,y) { ifelse(is.na(x),0,x)/(ifelse(is.na(x),0,x)+ifelse(is.na(y),0,y)) }

#-------------------------------------------------------#
# 1. Municipales ----
# Incluye tambien departamental y por vereda
#-------------------------------------------------------#

# Inversion municipal en millones de pesos corrientes
inversion <- c('5.   GASTOS DE CAPITAL (INVERSION)')
inv <- read_excel(glue("{datos_ori}/Ejecuciones/ejecuciones-presupuestales-00-18.xls"), sheet = 'Base OEC Municipios') %>%
  clean_names() %>% 
  dplyr::filter(cuenta %in% inversion) %>%
  dplyr::select(cdigo_dane, x2013) %>% 
  rename(cod_mpio = cdigo_dane, inv_mpio = x2013) %>%
  mutate(cod_mpio = as.numeric(cod_mpio))

# Panel CEDE: distancia a mercado mas cercano, distancia a puerto mas cercano, % vias terciarias, 
# dismdo: Distancia lineal al principal mercado mayorista de alimentos
# distancia_mercado: Distancia lineal al municipio en el que se encuentra un mercado de alimento cercano.
general <- read_excel(glue("{datos_ori}/CEDE/PANEL_CARACTERISTICAS_GENERALES(2019).xlsx"), sheet = "Hoja1" , col_names = T) %>%
  dplyr::select(coddepto, codmpio, ano, pobl_rur, pobl_urb, pobl_tot, dismdo, distancia_mercado, starts_with("g")) %>% 
  filter(ano == 2013) %>%
  mutate(region = ifelse(gcaribe == 1, "Caribe", 
                         ifelse(gandina == 1, "Andina",
                                ifelse(gpacifica == 1, "Pacifica", 
                                       ifelse(gorinoquia == 1, "Orinoquia", "Amazonia")))))

# Inversion total solo llega hasta 2010 (inv_total), sustituimos por transferencias del Sistema General de Participaciones
# DF_desemp_fisc: Indicador de desempeño fiscal
gobierno <- haven::read_dta(glue("{datos_ori}/CEDE/PANEL_BUEN_GOBIERNO(2019).dta")) %>%
  haven::zap_labels() %>% haven::zap_formats() %>% haven::zap_label() %>%
  clean_names() %>%
  filter(ano == 2013) %>%
  dplyr::select(codmpio, ano, sgp_total, df_desemp_fisc)

# SGP per capita en millones de pesos (corrientes)
cede <- left_join(general, gobierno, by = c("codmpio", "ano")) %>% 
  dplyr::select(-starts_with("g")) %>%
  mutate(sgp_pc = sgp_total/pobl_tot, sgp_pc = sgp_pc/1000000) %>%
  dplyr::rename(year = ano, cod_mpio = codmpio, cod_dpto = coddepto)

# Estructura economica: Medicion de desempeno municipal (DNP)
dnp <- read_excel(glue("{datos_ori}/DNP/Desempeno integral 2013 (Pagina Web).xlsx"), sheet = "IDI 2013") %>%
  drop_na(`...2`) %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  dplyr::select(codigo, indicador_desempeno_integral_2013) %>%
  dplyr::rename(cod_mpio = codigo, ind_desempeno_int = indicador_desempeno_integral_2013) %>%
  mutate(ind_desempeno_int = as.numeric(ind_desempeno_int), year = 2013, cod_mpio = as.numeric(cod_mpio)) 

# Capacidad institucional (Valor agregado, parece millones de pesos)
va <- read_excel(glue("{datos_ori}/DANE/VALOR AGREGADO MUNICIPAL .xlsx"), sheet = "Base_VA") %>%
  clean_names() %>%
  dplyr::select(codigo_dane, ano, valor_agregado) %>%
  dplyr::filter(ano == 2013 & nchar(codigo_dane) > 2) %>%
  dplyr::rename(cod_mpio = codigo_dane, year = ano)

# Diversidad economica (departamental)
div <- readr::read_csv(glue("{datos_ori}/DANE/base_ind_div_econ_dpto_2005-2020.csv")) %>%
  dplyr::select(nivel_value, time, value) %>%
  dplyr::rename(cod_dpto = nivel_value, year = time, ind_div_econ = value) %>%
  dplyr::filter(year == 2013)

# Agroecologicas: temperatura y precipitaciones (01c_procesar_variables_clima)
# Estan a nivel de vereda
data_clima <- readRDS(glue("{datos}/base_clima_veredas_2013.rds")) %>% 
  # dplyr::select(-cod_vereda_dane) %>%
  drop_na(cod_vereda) %>%
  mutate(cod_mpio = ifelse(nchar(cod_vereda) == 7, substr(cod_vereda, 1, 4), 
                           ifelse(nchar(cod_vereda) == 6, substr(cod_vereda, 1, 3), substr(cod_vereda, 1, 5))),
         cod_mpio = as.numeric(cod_mpio),
         cod_vereda = ifelse(nchar(cod_vereda) == 6, glue("00{cod_vereda}"),
                             ifelse(nchar(cod_vereda) == 7, glue("0{cod_vereda}"), cod_vereda)),
         temp = ifelse(is.nan(temp), NA, temp), rain = ifelse(is.nan(rain), NA, rain))

# Unimos informacion municipal, departamental (borramos San Andres)
data_control <- cede %>%
  left_join(dnp, by = c('cod_mpio', 'year')) %>%
  left_join(va, by = c('cod_mpio', 'year')) %>%
  left_join(div, by = c('cod_dpto', 'year')) %>%  
  left_join(inv, by = 'cod_mpio') %>%
  dplyr::filter(cod_dpto != 88) %>%
  dplyr::select(-year) %>%
  mutate(inv_mpio_pc = inv_mpio/pobl_tot)

# Guardamos bases de variables de control
saveRDS(data_clima, glue("{datos}/data_clima_veredas_2013.rds"))
saveRDS(data_control, glue("{datos}/data_controles_2013.rds"))

#-------------------------------------------------------#
# 2. UPA ----
#-------------------------------------------------------#

# Elevacion y pendiente de la UPA (centroide UPA)
upas <- st_read(glue("{datos}/CNA/mapas/centroides_upa_cna.shp"))
upas <- as(upas, "Spatial")
elev <- data.frame(elevatr::get_elev_point(upas, prj = "EPSG:4326", src = "aws"))

# Elevation from meters to km
elev <- elev %>% dplyr::select(cod_vereda, encuesta, elevation) %>% mutate(elevation = elevation/1000)

# Copiar codigo de encuesta en caracter (util para proximos procesos)
codes <- readRDS(glue("{datos}/CNA/limpieza_censo/lista_codigos_upa.rds"))

elev <- elev %>% 
  left_join(codes, by = "encuesta") %>% dplyr::select(-encuesta) %>% 
  dplyr::rename(encuesta = encuesta_s)

# Procesar datos caracteristicas productor (se crearon en script 01d)
cna_per <- readRDS(glue("{datos}/CNA/base_personas.rds")) %>%
  dplyr::rename(sexo = p_s15p168, parentesco_jefe = p_s15p167, edad = p_s15p169,
                nivel_educ = p_s15p175a, anios_educ = p_s15p175c,
                cod_dpto = p_depto, cod_mpio = p_munic) %>%
  # Categorias educacion
  mutate(cod_dpto = as.numeric(cod_dpto), cod_mpio = as.numeric(cod_mpio),
         productor_residente = ifelse(is.na(id_prod), 0, id_prod),
         sexo = ifelse(sexo == 1, "Hombre", "Mujer"),
         nivel_educ_cat = ifelse(nivel_educ == 1, "Preescolar",
                                 ifelse(nivel_educ == 2, "Basica primaria",
                                        ifelse(nivel_educ == 3, "Basica secundaria",
                                               ifelse(nivel_educ == 4, "Media",
                                                      ifelse(nivel_educ == 5, "Tecnico",
                                                             ifelse(nivel_educ == 6, 'Tecnologico',
                                                                    ifelse(nivel_educ == 7, 'Universitario',
                                                                           ifelse(nivel_educ == 8, 'Posgrado',
                                                                                  ifelse(nivel_educ == 9, "Ninguno", "Sin informacion")))))))))) %>%
  # Anios de educacion
  mutate(cod_vereda = as.numeric(cod_vereda),
         aprob = ifelse(anios_educ==17 | anios_educ==20 | anios_educ==24 | anios_educ==30,1,
                        ifelse(anios_educ==18 | anios_educ==21 | anios_educ==25 | anios_educ==31,2,
                               ifelse(anios_educ==19 | anios_educ==22 | anios_educ==26 | anios_educ==32,3,
                                      ifelse(anios_educ==23 | anios_educ==27 | anios_educ==33,4,
                                             ifelse(anios_educ==34,0,ifelse(anios_educ==28,5,ifelse(anios_educ==29,6,NA))))))),
         educ= ifelse(nivel_educ == 1, 1,
                      ifelse(nivel_educ == 9 | nivel_educ==0, 0,
                             ifelse(anios_educ == 99 | anios_educ == 97, NA,
                                    ifelse(anios_educ==4 | anios_educ==5 | anios_educ==6 |
                                             anios_educ==7 | anios_educ==8 | anios_educ==9 |
                                             anios_educ==10 | anios_educ==11 | anios_educ==12 |
                                             anios_educ==13 | anios_educ==14 | anios_educ==15 |
                                             anios_educ==16, anios_educ-3,ifelse(nivel_educ==2, anios_educ-3,11+aprob)))))) %>%
  dplyr::select(-c(anios_educ, aprob)) %>%
  distinct()

# Guardamos bases de variables de control
saveRDS(elev, glue("{datos}/CNA/elevacion_upa.rds"))
saveRDS(cna_per, glue("{datos}/CNA/caracteristicas_productor_upa.rds"))

#-------------------------------------------------------#
# 3. Organizar bases ----
# Unir productividad + caracteristicas upa + cultivo principal + variables de control
#-------------------------------------------------------#

# Variables de control
data_control <- readRDS(glue("{datos}/data_controles_2013.rds")) %>% 
  distinct(cod_dpto, cod_mpio, .keep_all = T)

data_clima <- readRDS(glue("{datos}/data_clima_veredas_2013.rds")) %>% 
  mutate(cod_vereda = as.numeric(cod_vereda)) %>%
  dplyr::select(-c(cod_mpio, cod_vereda_dane))

# Caracteristicas productor: Solo nos interesan los jefes de hogar
# Promediamos edad y educacion por casos de jefatura compartida
prod <- readRDS(glue("{datos}/CNA/caracteristicas_productor_upa.rds")) %>%
  dplyr::select(-c(cod_dpto, cod_mpio)) %>%
  drop_na(parentesco_jefe) %>%
  dplyr::filter(parentesco_jefe == 1) %>%
  group_by(cod_vereda, encuesta) %>%
  summarise(edad = mean(edad, na.rm = T), educ = mean(educ, na.rm = T)) %>%
  ungroup()

# Elevacion de las UPA
elev <- readRDS(glue("{datos}/CNA/elevacion_upa.rds"))

# Preguntas adicionales censo (aspectos productivos)
und_produc_raw <- read_csv(glue("{datos_ori}/CNA/Total_nacional(csv)/S01_15(Unidad_productora).csv")) %>%
  mutate(cod_vereda = as.numeric(COD_VEREDA)) %>% dplyr::rename(encuesta = ENCUESTA)

# Cultivo principal de cada UPA
cultivo <- readRDS(glue("{datos}/CNA/limpieza_censo/lista_cultivo_principal_upa.rds"))

#--------------------------#
# A. Base muestra productividad ----
# Usamos la muestra de 654k UPAs para las que se calculo productividad agregada basada en Hamann et al (2019)
#--------------------------#

# Base de productividad y caracteristicas nivel de UPA-cultivo
cna_prod <- readRDS(glue("{datos}/CNA/base_cna_productividad.rds")) %>% 
  drop_na(ls2) %>% 
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, 
                tipo_cul_lote, area_upa, area_sembrada, cant_cosecha, ls2) %>%
  distinct()

# Agregamos caracteristicas a nivel de UPA
cna_prod <- cna_prod %>%
  group_by(cod_dpto, cod_vereda, cod_mpio, encuesta, ls2) %>%
  summarise(area_upa = mean(area_upa, na.rm = T), area_sembrada = sum(area_sembrada, na.rm = T), 
            cant_cosecha = sum(cant_cosecha, na.rm = T)) %>%
  ungroup() %>%
  distinct() %>%
  mutate(rend_cos = cant_cosecha/area_sembrada)

# Base de productividad, variables de control y cultivo principal
cna_ag <- cna_prod %>% 
  left_join(cultivo, by = c("cod_vereda", "encuesta")) %>%
  left_join(elev, by = c('cod_vereda', 'encuesta')) %>%
  left_join(data_control, by = c("cod_dpto", "cod_mpio")) %>%
  left_join(data_clima, by = c("cod_vereda")) %>%
  left_join(prod, by = c('cod_vereda', 'encuesta')) %>%
  dplyr::select(-year) %>%
  dplyr::select(region, cod_dpto, cod_mpio, cod_vereda, encuesta, everything()) 

rm(cultivo, elev, data_control, data_clima, prod)
# .rs.restartR()

# Se crea variable cantidad de tierra 
cna_ag <- cna_ag %>% mutate(cat_tierra = NA)

summary(cna_ag)

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

# Unimos con base de controles 
und_produc <- merge(cna_ag[ ,c("cod_vereda","encuesta")], und_produc_raw, by = c("cod_vereda","encuesta"))

# Organizar variables productivas CNA
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
            # cred_aprob = ifelse(P_S11P136A == 1 & c4 == 0, 1, P_S11P136A),
            cred_aprob = ifelse(c4 == 1 & P_S11P136A == 1, 0, P_S11P136A),
            agrario = fv(P_S11P136B_SP1),
            otros_ban = fv(P_S11P136B_SP2),
            coo = fv(P_S11P136B_SP3),
            ong = fv(P_S11P136B_SP5),
            gob = fv(P_S11P136B_SP6),
            int = fv(P_S11P136B_SP7),
            ## Manejo Suelos
            s1=fv1(fv(P_S11P127_SP1)+fv(P_S11P127_SP10)),
            s2=fv(P_S11P127_SP2),
            s3=fv1(fv(P_S11P127_SP3)+fv(P_S11P127_SP4)+fv(P_S11P127_SP5)),
            s4=fv(P_S11P127_SP9),
            ## Fuentes de Energia
            e1=fv(P_S11P133_SP1),
            e2=fv1(fv(P_S11P133_SP3)+fv(P_S11P133_SP4)),
            e3=fv1(fv(P_S11P133_SP2)+fv(P_S11P133_SP6)),
            e4_no=fv(P_S11P133_SP10),
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

data_prod <- merge(cna_ag, ds_cna, by = c("cod_vereda","encuesta"))
colSums(is.na(data_prod))
colSums(is.na(und_produc))

# Exportar
# saveRDS(data_prod, glue("{datos}/CNA/prod_caracteristicas_upa.rds"))
saveRDS(data_prod, glue("{datos}/CNA/prod_caracteristicas_upa2.rds"))

rm(und_produc_raw)

#--------------------------#
# B. Base muestra completa ----
# Usamos la muestra de 1'950k UPAs filtradas por zonas de exclusion, resguardos, etc
#--------------------------#

# Muestra completa: borramos UPA sin registro de cultivos
cna_all <- readRDS(glue("{datos}/CNA/base_cna_all_maq.rds")) %>% 
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, 
                tipo_cul_lote, area_upa, area_sembrada, cant_cosecha) %>%
  dplyr::filter(!is.na(tipo_cul_lote)) %>%
  drop_na(area_sembrada, cant_cosecha) %>%
  distinct()

# Agregamos caracteristicas a nivel de UPA
cna_all <- cna_all %>%
  group_by(cod_dpto, cod_vereda, cod_mpio, encuesta) %>%
  summarise(area_upa = mean(area_upa, na.rm = T), area_sembrada = sum(area_sembrada, na.rm = T), 
            cant_cosecha = sum(cant_cosecha, na.rm = T)) %>%
  ungroup() %>%
  distinct() %>%
  mutate(rend_cos = cant_cosecha/area_sembrada)

# Base de productividad, variables de control y cultivo principal
cna_ag <- cna_all %>% 
  left_join(cultivo, by = c("cod_vereda", "encuesta")) %>%
  left_join(elev, by = c('cod_vereda', 'encuesta')) %>%
  left_join(data_control, by = c("cod_dpto", "cod_mpio")) %>%
  left_join(data_clima, by = c("cod_vereda")) %>%
  left_join(prod, by = c('cod_vereda', 'encuesta')) %>%
  dplyr::select(-year) %>%
  dplyr::select(region, cod_dpto, cod_mpio, cod_vereda, encuesta, everything()) %>%
  # Pasamos de metros a hectareas
  mutate(area_upa = area_upa/10000,
          area_sembrada = area_sembrada/10000, rend_cos = cant_cosecha/area_sembrada)

colSums(is.na(cna_ag))
rm(cultivo, elev, data_control, data_clima, prod)
#.rs.restartR()

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

table(cna_ag$tipo_upa)

# Unimos con base de controles 
und_produc <- merge(cna_ag[ ,c("cod_vereda","encuesta")], und_produc_raw, by = c("cod_vereda","encuesta"))
rm(und_produc_raw)
#.rs.restartR()

# Organizar variables productivas CNA
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
            e4_no=fv(P_S11P133_SP10),
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

data_prod <- merge(cna_ag, ds_cna, by = c("cod_vereda","encuesta"))

# Eliminamos UPA en San Andres
data_prod <- data_prod %>% dplyr::filter(cod_dpto != 88)
colSums(is.na(data_prod))

# Exportar
saveRDS(data_prod, glue("{datos}/CNA/prod_caracteristicas_upa_all.rds"))
