# go from fully compiled stations to QAQC'd station data
# still keeping ALL time points, but replacing
# data with suspect/rejected flags with NAs
# according to the flags and codes we've discsused and agreed to keep
# which are defined in the helper_files/definitions.R script

library(doParallel)
library(foreach)
library(dplyr)


path <- here::here("Data", "compiled_by_stn")
outpath <- here::here("Data", "QAQCd_by_stn")

stns_wq <- stringr::str_sub(dir(path, pattern = "wq.RData$"), end = -9)
stns_nut <- stringr::str_sub(dir(path, pattern = "nut.RData$"), end = -10)
stns_met <- stringr::str_sub(dir(path, pattern = "met.RData$"), end = -10)

# setup parallel backend
cl<-makeCluster(10)  
registerDoParallel(cl)
strt<-Sys.time()

# process all stations
# wq ----
foreach(stat = stns_wq, .packages = c('dplyr', 'stringr')) %dopar% {

  source(here::here("helper_files", "definitions.R"))
  source(here::here("helper_files", "functions.R"))
  
  statqc <- paste0(stat, "wq_qc")
  
  file_in <- here::here(path, paste0(stat, "wq.RData"))
  dat <- get(load(file_in))
  parms_to_keep <- names(dat)[which(names(dat) %in% c("temp", "spcond", "sal", "ph", "turb",
                                     "do_pct", "do_mgl", "depth", "cdepth",
                                     "level", "clevel"))]
  datqc <- qaqc_df(dat, parms_to_keep, type = "wq")
  assign(statqc, datqc)
  save(list = statqc, file = here::here("Data", "QAQCd_by_stn", 
                                paste0(stat, "wq_qc.RData")))
  rm(list = statqc)
  rm('datqc')
  
}


# nut ----
# note - also need to keep a column for censored/uncensored
foreach(stat = stns_nut, .packages = c('dplyr', 'stringr')) %dopar% {
  
  source(here::here("helper_files", "definitions.R"))
  source(here::here("helper_files", "functions.R"))
  
  statqc <- paste0(stat, "nut_qc")
  
  file_in <- here::here(path, paste0(stat, "nut.RData"))
  dat <- get(load(file_in))
  parms_to_keep <- names(dat)[which(names(dat) %in% c("nh4f", "no23f", 
                                                      "no2f", "no3f",
                                                      "po4f", "chla_n"))]
  datqc <- qaqc_df(dat, parms_to_keep, type = "nut")
  assign(statqc, datqc)
  save(list = statqc, file = here::here("Data", "QAQCd_by_stn",
                                paste0(stat, "nut_qc.RData")))
  rm(list = statqc)
  rm('datqc')
  
}

# met ----
foreach(stat = stns_met, .packages = c('dplyr', 'stringr')) %dopar% {
  
  source(here::here("helper_files", "definitions.R"))
  source(here::here("helper_files", "functions.R"))
  
  statqc <- paste0(stat, "met_qc")
  
  file_in <- here::here(path, paste0(stat, "met.RData"))
  dat <- get(load(file_in))
  parms_to_keep <- names(dat)[which(names(dat) %in% c("atemp", "rh", "bp", 
                                                      "wspd", "maxwspd", 
                                                      "wdir", "sdwdir",
                                                      "totpar", "totprcp"))]
  datqc <- qaqc_df(dat, parms_to_keep, type = "wq") # same qaqc as wq
  assign(statqc, datqc)
  save(list = statqc, file = here::here("Data", "QAQCd_by_stn", 
                                paste0(stat, "met_qc.RData")))
  rm(list = statqc)
  rm('datqc')
  
}

Sys.time() - strt
stopCluster(cl)
beepr::beep(8)