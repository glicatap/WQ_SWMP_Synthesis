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

# setup parallel backend
cl<-makeCluster(10)  
registerDoParallel(cl)
strt<-Sys.time()

# process all stations
# wq ----
foreach(stat = stns_wq, .packages = c('dplyr', 'stringr', 'lubridate')) %dopar% {
  
  source(here::here("helper_files", "definitions.R"))
  source(here::here("helper_files", "functions.R"))
  
  file_in <- here::here(path, paste0(stat, "_qc.RData"))
  dat <- get(load(file_in))
  
  parms_gen <- names(dat)[which(names(dat) %in% c("temp", "spcond", "sal", "ph", "turb",
                                                  "do_pct", "do_mgl", "depth", "cdepth",
                                                  "level", "clevel",
                                                  "atemp", "rh", "bp", "wspd", "maxwspd",
                                                  "wdir"))]
  
  parms_sums <- names(dat)[which(names(dat) %in% c("totprcp", "totpar"))]
  
  dat_daily <- dat %>% 
    mutate(date = lubridate::as_date(datetimestamp)) %>% 
    summarize(.by = date,
              across(c(parms_gen),
                     daily_stats))
  
  # get the Inf, -Inf, and NaNs out
  to_rmv_nas <- names(dat_daily)[-which(names(dat_daily) == "date")]
  dat_daily <- dat_daily %>% 
    mutate(across(all_of(to_rmv_nas),
                  ~ case_when(. %in% c(Inf, -Inf, NaN) ~ NA,
                              .default = .)))

  # assign daily df to object, save, clear memory
  flnm <- paste0(stat, "_daily")
  assign(flnm, dat_daily)
  save(list = flnm, file = here::here(outpath, 
                                      paste0(flnm, ".RData")))
  rm(list = flnm)
  rm('dat_daily')
  
}


# met ----
foreach(stat = stns_met, .packages = c('dplyr', 'stringr', 'lubridate')) %dopar% {
  
  source(here::here("helper_files", "definitions.R"))
  source(here::here("helper_files", "functions.R"))
  
  file_in <- here::here(path, paste0(stat, "_qc.RData"))
  dat <- get(load(file_in))
  parms_gen <- names(dat)[which(names(dat) %in% c("temp", "spcond", "sal", "ph", "turb",
                                                  "do_pct", "do_mgl", "depth", "cdepth",
                                                  "level", "clevel",
                                                  "atemp", "rh", "bp", "wspd", "maxwspd",
                                                  "wdir"))]
  
  parms_sums <- names(dat)[which(names(dat) %in% c("totprcp", "totpar"))]
  
  dat2 <- dat %>% 
    mutate(date = lubridate::as_date(datetimestamp)) %>% 
    summarize(.by = date,
              across(c(parms_gen),
              daily_stats))
  
  # met only: get sums, then join dfs
  dat3 <- dat %>% 
    mutate(date = lubridate::as_date(datetimestamp)) %>% 
    summarize(.by = date,
              across(c(parms_sums),
              daily_sums))
  
  dat_daily <- full_join(dat2, dat3, by = "date")
  
  # get the Inf, -Inf, and NaNs out
  to_rmv_nas <- names(dat_daily)[-which(names(dat_daily) == "date")]
  dat_daily <- dat_daily %>% 
    mutate(across(all_of(to_rmv_nas),
                  ~ case_when(. %in% c(Inf, -Inf, NaN) ~ NA,
                              .default = .)))
  
  # assign tmp to object, save, clear memory
  flnm <- paste0(stat, "_daily")
  assign(flnm, dat_daily)
  save(list = flnm, file = here::here(outpath, 
                                paste0(flnm, ".RData")))
  rm(list = flnm)
  rm(dat_daily, dat2, dat3)
  
}

Sys.time() - strt
beepr::beep(8)

stopCluster(cl)