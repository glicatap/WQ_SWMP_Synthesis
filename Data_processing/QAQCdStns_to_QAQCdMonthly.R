# Aggregate to monthly stats, from 15-minute data:
# Year-Month; for each param:  # valid data points; min, median, mean, max, sd, IQR
# EXCEPT: for precip and PAR, want daily sum
# PAR units are total (millimoles/m2) within each 15-minute interval;
# makes sense to sum for the day, daily total PAR. for monthly, can find median/mean whatever.
# may also need to do something different with wdir

library(doParallel)
library(foreach)
library(dplyr)
library(lubridate)


path <- here::here("Data", "QAQCd_by_stn")
outpath <- here::here("Data", "QAQCd_monthly")

stns_wq <- stringr::str_sub(dir(path, pattern = "wq_qc.RData$"), end = -10)
stns_nut <- stringr::str_sub(dir(path, pattern = "nut_qc.RData$"), end = -10)
stns_met <- stringr::str_sub(dir(path, pattern = "met_qc.RData$"), end = -10)
stns_wqANDmet <- c(stns_wq, stns_met)

# setup parallel backend
cl<-makeCluster(10)  
registerDoParallel(cl)
strt<-Sys.time()

# wq and met----
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
  
  parms_sums <- names(dat)[which(names(dat) %in% c("totprcp", 
                                                   "doLessThan2", "doLessThan5"))]
  
  # calculate monthly stats on everything but PAR
  # (will work with daily total PAR below)
  dat_monthly <- dat %>% 
    mutate(date = lubridate::as_date(datetimestamp),
           year = lubridate::year(date),
           month = lubridate::month(date),
           YearMonth = paste0(year, "-", month, "-15")) %>% 
    summarize(.by = c(year, month),
              across(any_of(parms_gen),
                     summary_stats),
              across(any_of(parms_sums),
                     summary_sums))
  
  # calculate daily PAR and do min, median, etc. on that value
  # at a monthly level. then join to dat_monthly
  if(str_ends(stat, "met")){
    par_daily <- dat %>% 
      mutate(date = lubridate::as_date(datetimestamp)) %>% 
      summarize(.by = date,
                across(totpar, summary_sums))
    par_monthly <- par_daily %>% 
      mutate(year = lubridate::year(date),
             month = lubridate::month(date)) %>%
      rename(dailyPAR = totpar_total) %>% 
      summarize(.by = c(year, month),
                across(dailyPAR, summary_stats))
    dat_monthly <- left_join(dat_monthly, par_monthly, by = c("year", "month"))
  }
  
  
  # assign df to object, save, clear memory
  flnm <- paste0(stat, "_monthly")
  assign(flnm, dat_monthly)
  save(list = flnm, file = here::here(outpath, 
                                      paste0(flnm, ".RData")))
  rm(list = flnm)
  rm('dat_monthly')
  
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

Sys.time() - strt
stopCluster(cl)
beepr::beep(8)