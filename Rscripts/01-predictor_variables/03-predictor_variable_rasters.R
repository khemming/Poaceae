

# library -----------------------------------------------------------------------------
  library(raster)
  
  rm(list = ls())
  
# data ---------------------------------------------------------------------------------  
  files <- list.files(path = "Data files/predictor variables/cropped",
                      pattern = ".grd", full.names = T)
  names <- gsub(pattern = "Data files/predictor variables/cropped/|.grd", "", files)
  names
  stack <- stack(files) # ignore error message
  list2env(setNames(unstack(stack), names), .GlobalEnv)
  
# scale resolution from 1 km to 100 km -------------------------------------
# note: aggregate resisted my loop charms
  
# amt, ap, arid, elev, iso  
  amt_ag <- aggregate(amt, fact = 100, fun = mean)
  writeRaster(amt_ag, "Results/rasters/predictor variables/amt", overwrite = T)
  ap_ag <- aggregate(ap, fact = 100, fun = mean)
  writeRaster(ap_ag, "Results/rasters/predictor variables/ap", overwrite = T)
  arid_ag <- aggregate(arid, fact = 100, fun = mean)
  writeRaster(arid_ag, "Results/rasters/predictor variables/arid", overwrite = T)
  elev_ag <- aggregate(elev, fact = 100, fun = mean)
  writeRaster(elev_ag, "Results/rasters/predictor variables/elev", overwrite = T)
  iso_ag <- aggregate(iso, fact = 100, fun = mean)
  writeRaster(iso_ag, "Results/rasters/predictor variables/iso", overwrite = T)
  
# mdr, pawc, pcoldq, pdrym, pdryq
  mdr_ag <- aggregate(mdr, fact = 100, fun = mean)
  writeRaster(mdr_ag, "Results/rasters/predictor variables/mdr", overwrite = T)
  pawc_ag <- aggregate(pawc, fact = 100, fun = mean)
  writeRaster(pawc_ag, "Results/rasters/predictor variables/pawc", overwrite = T)
  pcoldq_ag <- aggregate(pcoldq, fact = 100, fun = mean)
  writeRaster(pcoldq_ag, "Results/rasters/predictor variables/pcoldq", overwrite = T)
  pdrym_ag <- aggregate(pdrym, fact = 100, fun = mean)
  writeRaster(pdrym_ag, "Results/rasters/predictor variables/pdrym", overwrite = T)
  pdryq_ag <- aggregate(pdryq, fact = 100, fun = mean)
  writeRaster(pdryq_ag, "Results/rasters/predictor variables/pdryq", overwrite = T)
  
# pewc, ps. pwarmq, pwetm, pwetq
  pewc_ag <- aggregate(pewc, fact = 100, fun = mean)
  writeRaster(pewc_ag, "Results/rasters/predictor variables/pewc", overwrite = T)
  ps_ag <- aggregate(ps, fact = 100, fun = mean)
  writeRaster(ps_ag, "Results/rasters/predictor variables/ps", overwrite = T)
  pwarmq_ag <- aggregate(pwarmq, fact = 100, fun = mean)
  writeRaster(pwarmq_ag, "Results/rasters/predictor variables/pwarmq", overwrite = T)
  pwetm_ag <- aggregate(pwetm, fact = 100, fun = mean)
  writeRaster(pwetm_ag, "Results/rasters/predictor variables/pwetm", overwrite = T)
  pwetq_ag <- aggregate(pwetq, fact = 100, fun = mean)
  writeRaster(pwetq_ag, "Results/rasters/predictor variables/pwetq", overwrite = T)
  
# rz, sp, st, tar, tcoldm
  rz_ag <- aggregate(rz, fact = 100, fun = mean)
  writeRaster(rz_ag, "Results/rasters/predictor variables/rz", overwrite = T)
  sp_ag <- aggregate(sp, fact = 100, fun = mean)
  writeRaster(sp_ag, "Results/rasters/predictor variables/sp", overwrite = T)
  st_ag <- aggregate(st, fact = 100, fun = mean)
  writeRaster(st_ag, "Results/rasters/predictor variables/st", overwrite = T)
  tar_ag <- aggregate(tar, fact = 100, fun = mean)
  writeRaster(tar_ag, "Results/rasters/predictor variables/tar", overwrite = T)
  tcoldm_ag <- aggregate(tcoldm, fact = 100, fun = mean)
  writeRaster(tcoldm_ag, "Results/rasters/predictor variables/tcoldm", overwrite = T)
  
# tcoldq, tdryq, ts, twarmm, twarmq
  tcoldq_ag <- aggregate(tcoldq, fact = 100, fun = mean)
  writeRaster(tcoldq_ag, "Results/rasters/predictor variables/tcoldq", overwrite = T)
  tdryq_ag <- aggregate(tdryq, fact = 100, fun = mean)
  writeRaster(tdryq_ag, "Results/rasters/predictor variables/tdryq", overwrite = T)
  ts_ag <- aggregate(ts, fact = 100, fun = mean)
  writeRaster(ts_ag, "Results/rasters/predictor variables/ts", overwrite = T)
  twarmm_ag <- aggregate(twarmm, fact = 100, fun = mean)
  writeRaster(twarmm_ag, "Results/rasters/predictor variables/twarmm", overwrite = T)
  twarmq_ag <- aggregate(twarmq, fact = 100, fun = mean)
  writeRaster(twarmq_ag, "Results/rasters/predictor variables/twarmq", overwrite = T)
  
# twetq, clay, hii, ghm
  twetq_ag <- aggregate(twetq, fact = 100, fun = mean)
  writeRaster(twetq_ag, "Results/rasters/predictor variables/twetq", overwrite = T)
  clay_ag <- aggregate(clay, fact = 100, fun = mean)
  writeRaster(clay_ag, "Results/rasters/predictor variables/clay", overwrite = T)
  hii_ag <- aggregate(hii, fact = 100, fun = mean)
  writeRaster(hii_ag, "Results/rasters/predictor variables/hii", overwrite = T)
  ghm_ag <- aggregate(ghm, fact = 100, fun = mean)
  writeRaster(ghm_ag, "Results/rasters/predictor variables/ghm", overwrite = T)
  
  
# topographic heterogeneity
  th <- aggregate(elev, fact = 100, fun = sd)
  writeRaster(th, "Results/rasters/predictor variables/th", overwrite = T)
  
# ------------------------------------------------------------------------------