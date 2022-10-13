colombia <- map_data("world", region = c("Colombia"))  
library(RColorBrewer)

#### #### #### #### #### #### #### 
#### 
#### Function 1: Make map
####
#### #### #### #### #### #### ####  

  make_map_cna_out <- function(ds_out,var,title) {
    
    ## Prepare Data
    dat_g <- ds_out %>% dplyr::select(LON,LAT,var)
    colnames(dat_g) <- c("LON","LAT","var_d")
    
    ## Graph
    ggplot(dat_g, aes(x=LON, y=LAT, z = var_d))+
      ### Bin
      stat_summary_2d(bins = 80,fun=mean) +
      theme_void() +
      scale_fill_gradient(low="#d6f5d6", high="#003300")+
      theme(legend.position =c(0.2, 0.16)) +ylim(-4,12.5)+
      labs(fill=paste0(title,"\n (ton/ha)"))
    
  }
  
#### #### #### #### #### #### #### 
#### 
#### Function 2: Make heat map
####
#### #### #### #### #### #### ####  
  
  make_heat_graph_out <- function(ds_out,var) {
    
    ## Colors
    cc = colorRampPalette( c("#d6f5d6","#003300"))
    
    ## Prepare Data
    dat_g <- ds_out %>% dplyr::select(prcp_2011_2040,tmpr_2011_2040,var)
    colnames(dat_g) <- c("prcp_2011_2040","tmpr_2011_2040","var_d")
    
    ### Make Data Set
    ds <- dat_g %>% dplyr::filter(!is.na(prcp_2011_2040)) %>% 
      ### Group by
      group_by(prcp_2011_2040,tmpr_2011_2040) %>% 
      ## Summarize data
      summarise_all(mean, na.rm = TRUE) %>%
      spread(prcp_2011_2040,var_d,fill=0)
    
    ### Convert in Matrix
    mat_ds <- data.matrix(ds %>% dplyr::select(-tmpr_2011_2040))
    colnames(mat_ds) <- prcp_baseline_lgn
    rownames(mat_ds) <- tmpr_baseline_lgn[1:27]
    
    ### Make Map
    levelplot(mat_ds,
              ## Xlab
              xlab="Precipitation \n (endline 2011-2040)",
              ##Y lab
              ylab="Temperature \n (endline 2011-2040)",
              main="",colorkey = TRUE, region = TRUE,
              ###Fonts
              scales=list(x=list(cex=.6,rot = 90),y=list(cex=.6)),
              ###
              col.regions =cc
    )
  }
  

#### #### #### #### #### #### #### 
#### 
#### Function 3: Decision Tree
####
#### #### #### #### #### #### ####

dec_tree <- function(ds_final,var,treshold) {
  
  ## Get Data Set
  ds_input <- ds_final[c("n_upas",paste0(var,"_crops_P_S6P59_UNIF"),paste0(var,"_crops_f00_irrig"),"f01_water","f02_soil","f03_energy",
                                     "f04_assit","f05_credit","f06_workers","f07_mag_crops","f08_others","f09_plaques","f10_nonagri","equi_new","equi_old",
                                     "TLU","prcp_1976_2005","tmpr_1976_2005")]
  
  colnames(ds_input) <- c("n_upas","var_dep",paste0(var,"_crops_f00_irrig"),"f01_water","f02_soil","f03_energy","f04_assit","f05_credit","f06_workers","f07_mag_crops","f08_others","f09_plaques","f10_nonagri","equi_new","equi_old",
                          "TLU","prcp","tmpr")
  
  # Legth of data (take any variable)
  l_data <- length(ds_input$n_upas)
  
  #Sort DataSet - Randomly order
  set.seed(12345)
  ds_input_rand <- ds_input[order(runif(l_data)), ]
  
  # Splitng Data
  end_training <- round(l_data*treshold,0)
  st_training <-end_training+1
  ds_train <- ds_input_rand[1:end_training, ]
  ds_test <- ds_input_rand[st_training:l_data, ]
  
  # Get the model
  m.rpart <- rpart(var_dep ~ ., data = ds_train)
  
  #Prediction
  p.rpart <- predict(m.rpart,ds_test)

  #MAE
  MAE <- function(actual, predicted) {
    mean(abs(actual - predicted))
  }
  
  ### Prediction
  mae_pred <- MAE(p.rpart,ds_test$var_dep)
  mae_mean <- MAE(mean(ds_test$var_dep),ds_test$var_dep)
  

  ## If the model fails
    if (mae_pred>mae_mean) {
      ## Using another improved performance model
      m.rpart <- M5P(var_dep ~ ., data = ds_train)
      
      ## Predict
      p.rpart <- predict(m.rpart, ds_test)
      
      ### Prediction
      mae_pred <- MAE(p.rpart,ds_test$var_dep)
      mae_mean <- MAE(mean(ds_test$var_dep),ds_test$var_dep)
      
  }
  
  ## Output
  output <- list('model'=m.rpart,
                 ## Correlation
                 'pred_corr'=cor(p.rpart,ds_test$var_dep),
                 ## MAE - Overall
                 'pred_mae_tree'=mae_pred,
                 ## MAE a mean
                 'pred_mae_mean'=mae_mean)
  return(output)
  
}

#### #### #### #### #### #### #### 
#### 
#### Function 4: Cross-Validation
####
#### #### #### #### #### #### ####

tree_cross <- function(var) {
  
  dat <- c()
  for (i in seq(0.1,0.9,0.1)) {
    dat <- rbind(dat,c(i,dec_tree(data_cna_data,var,i)$pred_mae_tree))
  }
  
  dat <- as.data.frame(dat)
  colnames(dat) <- c("treshold","mae")
  
  best_training <- seq(0.1,0.9,0.1)[which(dat$mae==min(dat$mae))]
  
  ## Graph 
  graph <- ggplot(dat, aes(treshold,mae))+
    geom_point() + 
    labs( y="Mean Absolute Error", 
          x="Treshold")
  
  ## output
  output <- list('best_th'=best_training,
                 ## Correlation
                 'graph'=graph)
}

#### #### #### #### #### #### #### 
#### 
#### Function 5: Out Sample
####
#### #### #### #### #### #### ####


dec_tree_out <- function(ds_final,var,model) {
  
  ## Get Data Set
  ds_input <- ds_final[c("n_upas",paste0(var,"_crops_P_S6P59_UNIF"),paste0(var,"_crops_f00_irrig"),"f01_water","f02_soil","f03_energy",
                         "f04_assit","f05_credit","f06_workers","f07_mag_crops","f08_others","f09_plaques","f10_nonagri","equi_new","equi_old",
                         "TLU","prcp_2011_2040","tmpr_2011_2040")]
  
  colnames(ds_input) <- c("n_upas","var_dep",paste0(var,"_crops_f00_irrig"),"f01_water","f02_soil","f03_energy","f04_assit","f05_credit","f06_workers","f07_mag_crops","f08_others","f09_plaques","f10_nonagri","equi_new","equi_old",
                          "TLU","prcp","tmpr")
  
  p.rpart <- predict(model, ds_input)
  
  return(p.rpart)
  
}
   
#### #### #### #### #### #### #### 
#### 
#### Function 6: Random Forest
####
#### #### #### #### #### #### #### 
    
  rand_forest <- function(ds_final,var,treshold) {
    
    ## Get Data Set
    ds_input <- ds_final[c("n_upas",paste0(var,"_crops_P_S6P59_UNIF"),paste0(var,"_crops_f00_irrig"),"f01_water","f02_soil","f03_energy",
                           "f04_assit","f05_credit","f06_workers","f07_mag_crops","f08_others","f09_plaques","f10_nonagri","equi_new","equi_old",
                           "TLU","prcp_1976_2005","tmpr_1976_2005")]
    
    colnames(ds_input) <- c("n_upas","var_dep",paste0(var,"_crops_f00_irrig"),"f01_water","f02_soil","f03_energy","f04_assit","f05_credit","f06_workers","f07_mag_crops","f08_others","f09_plaques","f10_nonagri","equi_new","equi_old",
                            "TLU","prcp","tmpr")
    
    ### Train Data
    # Legth of data (take any variable)
    l_data <- length(ds_input$n_upas)
    #Sort DataSet - Randomly order
    set.seed(12345)
    ds_input_rand <- ds_input[order(runif(l_data)), ]
    
    # Splitng Data
    end_training <- round(l_data*treshold,0)
    st_training <-end_training+1
    TrainSet <- ds_input_rand[1:end_training, ]
    ValidSet <- ds_input_rand[st_training:l_data, ]
    
    ### MAE
    #MAE
    MAE <- function(actual, predicted) {
      mean(abs(actual - predicted))
    }
    
    ### Tunning Model
    i=5
    a=c()
    for (i in 3:18) {
      model3 <- randomForest(var_dep ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
      predValid <- predict(model3, ValidSet, type = "class")
      a[i-2] = MAE(predValid,ValidSet$var_dep)
    }
    
    best_mtry=which(a==min(a))+3
    ### Minimun value
    png(filename=paste0(out_graph,"forest_tunning_",var,".png"), 
        units="in", 
        width=5, 
        height=4, 
        pointsize=12, 
        res=300)
      plot(3:18,a,
         xlab = "Number of variables randomly sampled at each stage (mtry)",
         ylab = "Mean Absolute Error")
      dev.off()
    
    ## Best Model
    best_model <- randomForest(var_dep ~ ., data = TrainSet, ntree = 500, mtry = best_mtry, importance = TRUE)
    return(best_model)
  }

#### #### #### #### #### #### #### 
#### 
#### Function 7: Winners
####
#### #### #### #### #### #### ####   

    get_ranking <- function(ds_out,var,title) {
      
      ## Prepare Data
      dat_g <- ds_out %>% dplyr::select(P_MUNIC,LON,LAT,paste0(var,"_crops_P_S6P59_UNIF"),paste0(var,"_crops_out"))
      colnames(dat_g) <- c("P_MUNIC","LON","LAT","prod_base","prod_end")
      
      ## Fix data
      dat_g <- dat_g %>% 
        ## Fix some negative numbers
        mutate(prod_base=ifelse(prod_base<0,0,prod_base),
                                prod_end=ifelse(prod_base<0,0,prod_end)) %>%
        ## Get the difference
        mutate(dif=(prod_end-prod_base)) %>% 
        ## Get Cat Variable
        mutate(cat=ifelse(dif<0,1,ifelse(dif==0,2,3)))
          
      ## Make map
      g_1 <- ggplot(dat_g, aes(x=LON, y=LAT, z = dif))+
        ### Bin
        stat_summary_2d(bins = 100,fun=mean) +
        theme_void() +
        scale_fill_gradient(low="#ff3300",high="#00cc00")+
        theme(legend.position =c(0.2, 0.16)) +ylim(-4,12.5)+
        labs(fill=paste0(title,"\n (ton/ha)"))
      
      ### Top Deptp
      dat_g_depto <- dat_g %>% mutate(dpto=substr(P_MUNIC,1,2)) %>% group_by(dpto) %>%
        summarise(dif_mea=mean(dif,na.rm=TRUE)) %>% mutate(dpto_type=ifelse(dif_mea>0,"winner","looser")) %>% arrange(dif_mea)
        
      dat_g_depto$dpto <- factor(dat_g_depto$dpto,levels=c("05","08","11","13","15","17","18","19","20","23","25","27","41","44","47","50","52","54","63","66","68","70","73","76","81","85","86","88","91","94","95","97","99"),labels=c("Antioquia","Atlantico","Bogota, D.C.","Bolivar","Boyaca","Caldas","Caqueta","Cauca","Cesar","Cordoba","Cundinamarca","Choco","Huila","La Guajira","Magdalena","Meta","Narino","N. Santander","Quindio","Risaralda","Santander","Sucre","Tolima","Valle","Arauca","Casanare","Putumayo","San Andres","Amazonas","Guainia","Guaviare","Vaupes","Vichada"))
      
    
      g_2 <- dat_g_depto %>%
        mutate(dpto = fct_reorder(dpto, dif_mea, fun=median)) %>%
        ggplot( aes(x=dif_mea , y=dpto, label=round(dif_mea,3))) +
        geom_point(stat='identity', aes(col=as.factor(dpto_type)), size=5)  +
        scale_color_manual(name="", 
                           labels = c("Looser", "Winners"), 
                           values = c("winner"="#00ba38", "looser"="#f8766d")) + 
        geom_text(color="white", size=2) +
        xlab("Average Difference (tons/ha) in vereda") + ylab("")
        
        
      grid.arrange(g_1, g_2, nrow = 1)
      
      
    }
  
