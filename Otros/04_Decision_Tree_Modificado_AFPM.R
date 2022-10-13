##### ------------------------------- ######
##### ------------------------------- ######
#       Project: Climate Change and Agriculture
#       Description:Decision Tree
##### ------------------------------- ######
##### ------------------------------- ######
##### 
##### 

library(tidyverse)
library(raster)
library(rasterVis)
library(stargazer) ## Make Tabla
library(viridis) ## Make nice graphs
library(ggspatial)
library(ggthemes) ### Extra themes
library(mapdata) ### Get Maps
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(RWeka)
library(gridExtra)

### Working paths
main_path <- "D:/Climate change/"
out_graph <- paste0(main_path,"9_Presentaciones/00_Bucaramanga_2019/figures/")
out_tablas <- paste0(main_path,"9_Presentaciones/00_Bucaramanga_2019/tables/")

source(paste0(main_path,"3_Syntaxis/aux_legend_raster_scn.R"))
source(paste0(main_path,"3_Syntaxis/aux_functions.R"))

load(paste0(main_path,"9_ds_final/ds_final_March18.Rdata"))



#### Cross Validation (separación de la DB en un conjunto de entrenamiento y otro de validación) 

cross_cash <- tree_cross("cash")
cross_food <- tree_cross("food")
cross_other <- tree_cross("other")

ggsave(cross_cash$graph,file=paste0(out_graph,"tree_cash_crops.png"))
ggsave(cross_food$graph,file=paste0(out_graph,"tree_food_crops.png"))
ggsave(cross_other$graph,file=paste0(out_graph,"tree_other_crops.png"))


#### Prediction
best_model_cash <- dec_tree(data_cna_data,"cash",cross_cash$best_th)$model
best_model_food <- dec_tree(data_cna_data,"food",cross_food$best_th)$model
best_model_other <- dec_tree(data_cna_data,"other",cross_other$best_th)$model

ds_out <- data_cna_data %>% 
    mutate(cash_crops_out=dec_tree_out(data_cna_data,"cash",best_model_cash),
           food_crops_out=dec_tree_out(data_cna_data,"food",best_model_food),
           other_crops_out=dec_tree_out(data_cna_data,"other",best_model_other))

ds_out

### Spatial Distribution
ggsave(make_map_cna_out(ds_out,"cash_crops_out","Cash \n Crops"),file=paste0(out_graph,"rdto_cash_crops_out_tree.png"))
ggsave(make_map_cna_out(ds_out,"food_crops_out","Food \n Crops"),file=paste0(out_graph,"rdto_food_crops_out_tree.png"))
ggsave(make_map_cna_out(ds_out,"other_crops_out","Other \n Crops"),file=paste0(out_graph,"rdto_other_crops_out_tree.png"))

### Data
### Cash Crops
png(filename=paste0(out_graph,"heat_cash_crops_out.png"), 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=300)
make_heat_graph_out(ds_out,"cash_crops_out","Cash \n Crops")
dev.off()

png(filename=paste0(out_graph,"heat_food_crops_out.png"), 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=300)
make_heat_graph_out(ds_out,"food_crops_out","Cash \n Crops")
dev.off()

png(filename=paste0(out_graph,"heat_other_crops_out.png"), 
    units="in", 
    width=5, 
    height=4, 
    pointsize=12, 
    res=300)
make_heat_graph_out(ds_out,"other_crops_out","Cash \n Crops")
dev.off()


### Analysis Change
ggsave(get_ranking(ds_out,"cash","Cash \n Crops"),file=paste0(out_graph,"ranking_cash_tree.png"))
ggsave(get_ranking(ds_out,"food","Food \n Crops"),file=paste0(out_graph,"ranking_food_tree.png"))
ggsave(get_ranking(ds_out,"other","Other \n Crops"),file=paste0(out_graph,"ranking_other_tree.png"))


