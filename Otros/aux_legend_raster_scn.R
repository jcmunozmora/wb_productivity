############
############    00 - OPEN FILEs
############

#### Precipitations

prcp_1976_2015 <- paste0(main_path,"06_Rawdata/01_Climate_Change_Scenarios/Precipitation/V1_TerceraComunicacion/00_LineaBase_1976-2005/ECC_Prcp_1976_2005_100K_2015.tif")
prcp_2011_2040 <- paste0(main_path,"06_Rawdata/01_Climate_Change_Scenarios/Precipitation/V1_TerceraComunicacion/01_Proyeccion_Cambio_2011-2040_lb76-05/ECC_CmPrcp_2011_2040_100K_2015.tif")
prcp_2041_2050 <- paste0(main_path,"06_Rawdata/01_Climate_Change_Scenarios/Precipitation/V1_TerceraComunicacion/01_Proyeccion_Cambio_2011-2040_lb76-05/ECC_CmPrcp_2011_2040_100K_2015.tif")

#### Temperature

tmpr_1976_2015 <-paste0(main_path,"06_Rawdata/01_Climate_Change_Scenarios/Temperature/v1_TerceraComunicacion/tmp/ECC_Tmpr_1976_2005_100K_2015.tif")
tmpr_2011_2040 <-paste0(main_path,"06_Rawdata/01_Climate_Change_Scenarios/Temperature/v1_TerceraComunicacion/01_Proyeccion_Cambio_2011-2040_lb76-05/ECC_DfTmpr_2011_2040_100K_2015.tif")
tmpr_2041_2050 <-paste0(main_path,"06_Rawdata/01_Climate_Change_Scenarios/Temperature/v1_TerceraComunicacion/02_Proyeccion_Cambio_2041-2070_lb76-05/ECC_DfTmpr_2041_2070_100K_2015.tif")

##### Open Rasters
prcp_1976_2015_r <- raster(prcp_1976_2015)
prcp_2011_2040_r <- raster(prcp_2011_2040)
prcp_2041_2050_r <- raster(prcp_2041_2050)
tmpr_1976_2015_r <- raster(tmpr_1976_2015)
tmpr_2011_2040_r <- raster(tmpr_2011_2040)
tmpr_2041_2050_r <- raster(tmpr_2041_2050)

############
############    01 - LEGENDS
############

# Temperature - Baseline
 
# 1976_2015 - Baseline
#tmpr_baseline_lgn <- c("Menor a 3C","3,1C  a 4C","4,1C a 5C","5,1C a 6C","6,1C a 7C","7,1C a 8C","8,1C a 9C","9,1C a 10C","10,1C a 11C","11,1C a 12C","12,1C a 13C","13,1C a 14C","14,1C a 15C","15,1C a 16C","16,1C a 17C","17,1C a 18C","18,1C a 19C","19,1C a 20C","20,1C a 21C","21,1C a 22C","22,1C a 23C","23,1C a 24C","24,1C a 25C","25,1C a 26C","26,1C a 27C","27,1C a 28C","28,1C a 29C","29,1C a 30C","30,1C a 31C","31,1C a 32C","32,1C a 33C","33,1C a 34C")
tmpr_baseline_lgn <- c("Menor a 3C","3,1C  a 4C","4,1C a 5C","5,1C a 6C","6,1C a 7C","7,1C a 8C","8,1C a 9C","9,1C a 10C","10,1C a 11C","11,1C a 12C","12,1C a 13C","13,1C a 14C","14,1C a 15C","15,1C a 16C","16,1C a 17C","17,1C a 18C","18,1C a 19C","19,1C a 20C","20,1C a 21C","21,1C a 22C","22,1C a 23C","23,1C a 24C","24,1C a 25C","25,1C a 26C","26,1C a 27C","27,1C a 28C","28,1C a 29C","29,1C a 30C","30,1C a 31C","31,1C a 32C","32,1C a 33C","33,1C a 34C")
tmpr_baseline_pal <- c("#4c0073","#8400a8","#a80084","#ff00c5","#df73ff","#0000ff","#2432ff","#3870ff","#3b90ff","#38afff","#2ed2ff","#2edcff","#14f7ff","#13ffea","#beffe8","#d1ff73","#aaff00","#b4ff00","#98e600","#4ce600","#38a800","#267300","#4c7300","#ffff00","#e6e600","#ffaa00","#e69800","#a87000","#ff5500","#e64c00","#a83800","#732600")

#2011-2040
tmpr_endline_lgn<- c( ### No change 
                      "0.0C - 0,5C" ,"0,51C - 0,8C",
                      ## +1 
                      "0,81C - 1,0C","1,01C - 1,2C","1,21C - 1,6C","1,61C- 1,8C" ,
                      # + 2
                      "1,81C - 2,0C","2,01C -2,1C" ,"2,11C - 2,2C","2,21C -2,3 C","2,31C - 2,4C","2,41C -2,5C" ,"2,51C - 2,6C","2,61C - 2,7C",
                      # + 3
                      "2,71C - 3,0C")
tmpr_endline_pal<- c("#006100","#2b7500","#38a800","#679e00","#98e600","#b0cf00","#d6e600","#ffff00","#ffe500","#ffc800","#ffa600","#ff8c00","#ff6f00","#ff4d00","#ff2600")


# Precipitation - Baseline

# 1976_2015
#  Baseline
prcp_baseline_lgn <- c("0 mm -500mm","501 mm -1000 mm","1001 mm -1500 mm","1501 mm -2000 mm","2001 mm - 2500 mm","2501 mm - 3000 mm","3001 mm - 4000 mm","4001 mm - 5000 mm","5001 mm - 6000 mm","6001 mm - 7000 mm","7001 mm - 7500 mm","7501 mm - 8000 mm","8001 mm - 8500 mm")
prcp_baseline_pal <- c("#ff0000","#ff5500","#ffaa00","#ffff00","#d1ff73","#aaff00","#4ce600","#14f7ff","#2ed2ff","#38afff","#3355ff","#0000ff","#df73ff")


#endline
prcp_endline_lgn <- c(
                      # Same -2
                      "Menor a -40",
                      # Same -1
                      "-39 a -30","-29 a -20","-19 a -10",
                      # Same 
                      "-9 a 10","11 a 20",
                      # Same +1
                      "21 a 30","31 a 40",
                      # Same +2
                      "Mayor a 40")
prcp_endline_pal <- c("#ff0000","#ff5500","#ffaa00","#ffff00","#aaff00","#beffe8","#14f7ff","#3870ff","#0000ff")

