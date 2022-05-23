
# to do -------------------------
# merge resisduals with cell_id to compare cell values
# merge residuals with spp data and then go for it


# library -------------------------------------------------------------
  library(tidyverse)
  library(raster)

# data ----------------------------------------------------------------   
# model data
  load("Data files/rdata/models.RData")

# raster
  aus <- raster("Data files/Australia/Australia 1104.grd")

# variables for Australia
  spp_pv <- read.csv("Results/csv/spp predictor variables 1104.csv")
  head(spp_pv)

# cell ID
  cell_id <- pv %>% dplyr::select(cell_id)


# compare native-exotic model residuals ---------------------------------------------
# C3
  nc3_res <- data.frame(nc3_res = model_list[[1]]$residuals)
  nnc3_res <- data.frame(nnc3_res = model_list[[4]]$residuals)
  
  nat_col <- "native_C3"
  nonnat_col <- "nonnative_C3"
  
  nat_res <- spp_pv %>%
             dplyr::select(cell_id, nat_col) %>%
             drop_na() %>%
             bind_cols(., nc3_res) 
    
  nonnat_res <- spp_pv %>%
                dplyr::select(cell_id, nonnat_col) %>%
                drop_na() %>%
                bind_cols(., nnc3_res) 
  
  res <- left_join(cell_id, nat_res, by = "cell_id") %>%
         left_join(., nonnat_res, by = "cell_id")
    
  head(res)  
 
  
  plot(nnc3_res ~ nc3_res, data = res)
  
  cor(res$nnc3_res, res$nc3_res, use = "complete.obs", method = "pearson")
  
# C4
  nc4_res <- data.frame(nc4_res = model_list[[2]]$residuals)
  nnc4_res <- data.frame(nnc4_res = model_list[[5]]$residuals)
  
  nat_col <- "native_C4"
  nonnat_col <- "nonnative_C4"
  
  nat_res <- spp_pv %>%
    dplyr::select(cell_id, nat_col) %>%
    drop_na() %>%
    bind_cols(., nc4_res) 
  
  nonnat_res <- spp_pv %>%
    dplyr::select(cell_id, nonnat_col) %>%
    drop_na() %>%
    bind_cols(., nnc4_res) 
  
  res <- left_join(cell_id, nat_res, by = "cell_id") %>%
    left_join(., nonnat_res, by = "cell_id")
  
  head(res)  
  
  
  plot(nnc4_res ~ nc4_res, data = res)
  
  cor(res$nnc4_res, res$nc4_res, use = "complete.obs", method = "pearson")

# -------------------------------------------------------------    