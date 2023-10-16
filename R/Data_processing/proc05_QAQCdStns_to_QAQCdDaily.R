# Aggregate to daily stats:
# date; for each param:  # valid data points; min, median, mean, max, sd, IQR
# EXCEPT: for precip and PAR, want daily sum
# PAR units are total (millimoles/m2) within each 15-minute interval;
# makes sense to sum for the day, daily total PAR. for monthly, can find median/mean whatever.
# may also need to do something different with wdir

library(doParallel)
library(foreach)
library(dplyr)
library(lubridate)


path <- here::here("Data", "QAQCd_by_stn")
outpath <- here::here("Data", "QAQCd_daily")

stns_wq <- stringr::str_sub(dir(path, pattern = "wq_qc.RData$"), end = -10)
stns_nut <- stringr::str_sub(dir(path, pattern = "nut_qc.RData$"), end = -10)
stns_met <- stringr::str_sub(dir(path, pattern = "met_qc.RData$"), end = -10)
stns_wqANDmet <- c(stns_wq, stns_met)

# setup parallel backend
ncores <- detectCores()
ncores <- max(1, ncores - 2)
cl<-makeCluster(ncores) 
registerDoParallel(cl)
strt<-Sys.time()

# process all stations
foreach(stat = stns_wqANDmet, .packages = c('dplyr', 'stringr', 'lubridate')) %dopar% {
  
  source(here::here("helper_files", "definitions.R"))
  source(here::here("helper_files", "functions.R"))
  
  file_in <- here::here(path, paste0(stat, "_qc.RData"))
  dat <- get(load(file_in))
  
  if(str_ends(stat, "wq")){
  dat <- dat %>% 
    mutate(doLessThan2 = do_mgl < 2,
           doLessThan5 = do_mgl < 5)
  }
  
  parms_gen <- names(dat)[which(names(dat) %in% c("temp", "spcond", "sal", "ph", "turb",
                                                  "do_pct", "do_mgl", "depth", "cdepth",
                                                  "level", "clevel",
                                                  "atemp", "rh", "bp", "wspd", "maxwspd",
                                                  "wdir"))]
  
  parms_sums <- names(dat)[which(names(dat) %in% c("totprcp", "totpar",
                                                   "doLessThan2", "doLessThan5"))]
  
  dat_daily <- dat %>% 
    mutate(date = lubridate::as_date(datetimestamp)) %>% 
    summarize(.by = date,
              across(any_of(parms_gen),
                     summary_stats),
              across(any_of(parms_sums),
                     summary_sums))
  
  # assign daily df to object, save, clear memory
  flnm <- paste0(stat, "_daily")
  assign(flnm, dat_daily)
  save(list = flnm, file = here::here(outpath, 
                                      paste0(flnm, ".RData")))
  rm(list = flnm)
  rm('dat_daily')
  
}

Sys.time() - strt
stopCluster(cl)
beepr::beep(8)