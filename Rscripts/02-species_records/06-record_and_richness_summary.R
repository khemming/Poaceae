

# library --------------------------------------------------------------------
  library(tidyverse)
  library(data.table)
  library(janitor)
  library(raster)

  rm(list = ls())


# data ------------------------------------------------------------------------
  ala <- readRDS("Data files/ALA/master data/master grass records.rds")
  dat <- ala %>%
         dplyr::select(species, status, pp)
  
  aus <- raster("Data files/Australia/Australia 1104.grd")
  
# records, raw species richness --------------------------------------------
  spp_no <- dat %>% 
            group_by(status, pp) %>%
            distinct(species) %>%
            summarise(sr = n()) %>%
            pivot_wider(names_from = status, values_from = sr) %>%
            rename(nat_spp = native,
                   nn_spp = nonnative) %>%
            mutate(spp_tot = nat_spp + nn_spp)
  
  rec_no <- dat %>% 
            group_by(status, pp) %>%
            summarise(recs = n()) %>%
            pivot_wider(names_from = status, values_from = recs) %>%
            rename(nat_rec = native,
                   nn_rec = nonnative) %>%
            mutate(rec_tot = nat_rec + nn_rec)
          
# merge
  table <- left_join(spp_no, rec_no, by = "pp") %>%
            adorn_totals()
  table
  
# number of cells occupied -------------------------------------------------  
# 1 or more record
  occ_pp_1 <- function(p_p, stat){
    
    dat <- ala2 %>% filter(pp == p_p, status == stat)
    xy <- cbind(dat$longitude, dat$latitude)
    occ <- rasterize(xy, aus, fun = function(x,...) {length(unique(na.omit(x))) })
    occ2 <- data.frame(getValues(occ))
    occ3 <- nrow(occ2 %>% filter(!is.na(getValues.occ.)))
    return(occ3)
  }
  occ_tot_1 <- function(stat){
    
    dat <- ala2 %>% filter(status == stat)
    xy <- cbind(dat$longitude, dat$latitude)
    occ <- rasterize(xy, aus, fun = function(x,...) {length(unique(na.omit(x))) })
    occ2 <- data.frame(getValues(occ))
    occ3 <- nrow(occ2 %>% filter(!is.na(getValues.occ.)))
    return(occ3)
  }
  
# 15 or more records  
  occ_pp_15 <- function(p_p, stat){
    
    dat <- ala2 %>% filter(pp == p_p, status == stat)
    xy <- cbind(dat$longitude, dat$latitude)
    occ <- rasterize(xy, aus, fun = function(x,...) {length(unique(na.omit(x))) })
    occ2 <- data.frame(getValues(occ))
    occ3 <- nrow(occ2 %>% filter(!is.na(getValues.occ.),
                                 getValues.occ. >= 15))
    return(occ3)
  }
  occ_tot_15 <- function(stat){
    
    dat <- ala2 %>% filter(status == stat)
    xy <- cbind(dat$longitude, dat$latitude)
    occ <- rasterize(xy, aus, fun = function(x,...) {length(unique(na.omit(x))) })
    occ2 <- data.frame(getValues(occ))
    occ3 <- nrow(occ2 %>% filter(!is.na(getValues.occ.),
                                 getValues.occ. >= 15))
    return(occ3)
  }
  
  occ_df <- data.frame(status = c("native",    "native",    "native",
                                "nonnative", "nonnative", "nonnative"),
                       pp =     c("C3", "C4", "Total",
                                  "C3", "C4", "Total"),
                       cell_occ_1 = NA,
                       cell_occ_15 = NA)  
  
  occ_df$cell_occ_1[1] <- occ_pp_1("C3", "native")
  occ_df$cell_occ_1[2] <- occ_pp_1("C4", "native")
  occ_df$cell_occ_1[3] <- occ_tot_1("native")
  
  occ_df$cell_occ_1[4] <- occ_pp_1("C3", "nonnative")
  occ_df$cell_occ_1[5] <- occ_pp_1("C4", "nonnative")
  occ_df$cell_occ_1[6] <- occ_tot_1("nonnative")
  
  occ_df$cell_occ_15[1] <- occ_pp_15("C3", "native")
  occ_df$cell_occ_15[2] <- occ_pp_15("C4", "native")
  occ_df$cell_occ_15[3] <- occ_tot_15("native")
  
  occ_df$cell_occ_15[4] <- occ_pp_15("C3", "nonnative")
  occ_df$cell_occ_15[5] <- occ_pp_15("C4", "nonnative")
  occ_df$cell_occ_15[6] <- occ_tot_15("nonnative")
  
  occ_df
  
# save -----------------------------------------------------------------------
  write.csv(table, "Results/csv/records and species by pp and status.csv", row.names = F)
  write.csv(occ_df, "Results/csv/cell occupancy by pp and status.csv", row.names = F)
  
# ----------------------------------------------------------------------------
  
  