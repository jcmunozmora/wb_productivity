#-------------------------------------------------------#
# Estadisticas de exportaciones e importaciones agricolas ----
# Ultima fecha de modificacion: 22 octubre, 2021
# Procesamiento de datos BM
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

datos_ori <- "01_Datos_originales/FAOSTAT"
datos <- "02_Datos"
options(scipen = 999)

data_import <- read_excel(glue("{datos_ori}/imports_value_agricultural_products_1961-2019.xlsx")) %>%
  clean_names()
names(data_import)

# Calculamos valor importaciones anuales
annual_import <- data_import %>%
  group_by(ano) %>%
  summarise(total = sum(valor, na.rm = T)) %>%
  ungroup()

# Elegimos el top 10 de productos con mayor valor por año
data_import <- data_import %>% arrange(-valor) %>% arrange(ano)

top_data <- data_import %>% 
  group_by(ano) %>%
  top_n(valor, n = 25) %>%
  dplyr::select(producto, ano, valor)

# Agrupamos datos por decada y calculamos participacion promedio
top_data <- top_data %>%
  mutate(period = ifelse(ano >= 1961 & ano <= 1970, "1961-1970",
                         ifelse(ano >= 1971 & ano <= 1980, "1971-1980",
                                ifelse(ano >= 1981 & ano <= 1990, "1981-1990",
                                       ifelse(ano >= 1991 & ano <= 2000, "1991-2000",
                                              ifelse(ano >= 2001 & ano <= 2010, "2001-2010",
                                                     ifelse(ano >= 2011, "2011-2019", NA))))))) %>%
  left_join(annual_import, by = "ano") %>%
  mutate(share = 100*(valor/total)) %>%
  group_by(ano) %>%
  mutate(annual_share = sum(share)) %>%
  ungroup()

mean_share <- top_data %>%
  group_by(period, producto) %>%
  summarise(share = mean(share), annual_share = mean(annual_share)) %>%
  ungroup() %>%
  arrange(-share) %>%
  arrange(period) %>%
  mutate(id = row_number()) %>%
  group_by(period) %>%
  top_n(share, n = 10) %>%
  dplyr::select(id, producto, period, share, annual_share)

table(top_data$period)
writexl::write_xlsx(mean_share, glue("{datos}/importaciones_1961-2019.xlsx"))
