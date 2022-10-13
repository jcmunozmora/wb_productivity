#-------------------------------------------------------#
# Medicion de productividad agricola BM ----
# Ultima fecha de modificacion: 14 sept, 2021
# Procesamiento de datos Canasta exportadora
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readr, readxl)
# .rs.restartR()

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales"
datos <- "02_Datos"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Procesar datos originales ----
#-------------------------------------------------------#

years <- c(1995, 2000, 2005, 2010, 2015, 2019)

data <- lapply(years, function(x){
  exp <- read_csv(glue("{datos_ori}/Export_basket/What did Colombia export in {x}_.csv")) %>%
    janitor::clean_names() %>% mutate(year = x)
  return(exp)
}) %>% bind_rows()

table(data$year)

# Top 5 por anio
top_prod <- data %>% group_by(year) %>% arrange(desc(share)) %>% slice(1:5)

# Exportar
writexl::write_xlsx(data, glue("{datos}/Export_basket/canasta_exportadora_col_1995-2019.xlsx"))
writexl::write_xlsx(top_prod, glue("{datos}/Export_basket/top_exportaciones_col_1995-2019.xlsx"))






