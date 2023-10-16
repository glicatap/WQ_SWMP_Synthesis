# Aggregate to monthly stats, from 15-minute data:
# Year-Month; for each param:  # valid data points; min, median, mean, max, sd, IQR
# EXCEPT: for precip and PAR, want daily sum
# PAR units are total (millimoles/m2) within each 15-minute interval;
# makes sense to sum for the day, daily total PAR. for monthly, can find median/mean whatever.
# may also need to do something different with wdir

library(doParallel)
library(foreach)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)


path <- here::here("Data", "QAQCd_by_stn")
outpath <- here::here("Data", "QAQCd_monthly")

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

# wq and met----
foreach(stat = stns_wqANDmet, .packages = c('dplyr', 'stringr', 'lubridate', 'tidyr')) %dopar% {
  
  source(here::here("helper_files", "definitions.R"))
  source(here::here("helper_files", "functions.R"))
  
  file_in <- here::here(path, paste0(stat, "_qc.RData"))
  dat <- get(load(file_in))
  
  if(str_ends(stat, "wq")){
  dat <- dat %>% 
    mutate(doLessThan2 = do_mgl < 2,
           doLessThan5 = do_mgl < 5) %>% 
    rename("do-pct" = do_pct,
           "do-mgl" = do_mgl)  # for easier pivoting of DO params
  }
  
  # general parameters to calculate summaries of
  parms_gen <- names(dat)[which(names(dat) %in% c("temp", "spcond", "sal", "ph", "turb",
                                                  "do-pct", "do-mgl", "depth", "cdepth",
                                                  "level", "clevel",
                                                  "atemp", "rh", "bp", "wspd", "maxwspd",
                                                  "wdir"))]
  # parameters that need to be summed
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
  
  # make sure there is at least one week's worth of data points
  # 672 points when 15-minute data; 336 when 30-minute data
  # if not, remove the summary statistics

  # only need to worry about 30-minute sampling for WQ
  # MET has always been 15-minute only  
  if(str_ends(stat, "wq")){
    summary_calc_params <- dat %>% 
      mutate(year = year(datetimestamp),
             month = month(datetimestamp),
             minute = minute(datetimestamp)) %>% 
      summarize(.by = c(year, month),
                fifmins = ifelse(15L %in% unique(minute), TRUE, FALSE),
                across(temp:doLessThan5, function(x) sum(!is.na(x)))) %>% 
      pivot_longer(temp:doLessThan5,
                   names_to = "param", 
                   values_to = "nValid") %>% 
      mutate(nValid_min = ifelse(fifmins, 672, 336),
             enoughToCalc = nValid >= nValid_min)
  }
  
  
  if(str_ends(stat, "met")){
    summary_calc_params <- dat %>% 
      mutate(year = year(datetimestamp),
             month = month(datetimestamp)) %>% 
      summarize(.by = c(year, month),
                across(atemp:totprcp, function(x) sum(!is.na(x)))) %>% 
      pivot_longer(atemp:totprcp,
                   names_to = "param", 
                   values_to = "nValid") %>% 
      mutate(param = case_when(param == "totpar" ~ "dailyPAR",
                               .default = param),
             nValid_min = 672,
             enoughToCalc = nValid >= nValid_min)
  }
  
  
  # join the tables and if there were values calculated on 
  # fewer than the number of data points we need, replace them with NA
  dat_monthly_longer <- dat_monthly %>% 
    pivot_longer(-c(year, month),
                 names_to = c("param", "stat"),
                 names_sep = "_",
                 values_to = "value") %>% 
    left_join(summary_calc_params) %>% 
    mutate(value = case_when(stat == "nValid" ~ value,
                             enoughToCalc == FALSE ~ NA_real_,
                             .default = value))
  
  # pivot back to one row per month format
  # also change DO names back to the _ format, if present 
  dat_monthly_final <- dat_monthly_longer %>% 
    select(year, month, param, stat, value) %>% 
    mutate(param = case_when(param == "do-mgl" ~ "do_mgl",
                             param == "do-pct" ~ "do_pct",
                             .default = param)) %>% 
    pivot_wider(names_from = c("param", "stat"),
                names_sep = "_",
                values_from = value)
  
  
  # assign df to object, save, clear memory
  flnm <- paste0(stat, "_monthly")
  assign(flnm, dat_monthly_final)
  save(list = flnm, file = here::here(outpath, 
                                      paste0(flnm, ".RData")))
  rm(list = flnm)
  rm('dat_monthly')
  
}

# nut ----
# note - also need to keep a column for censored/uncensored
foreach(stat = stns_nut, .packages = c('dplyr', 'stringr', 'lubridate')) %dopar% {
  
  source(here::here("helper_files", "definitions.R"))
  source(here::here("helper_files", "functions.R"))
  
  file_in <- here::here(path, paste0(stat, "_qc.RData"))
  dat <- get(load(file_in))
  
  parms <- names(dat)[which(names(dat) %in% c("nh4f", "no23f", 
                                              "no2f", "no3f",
                                              "po4f", "chla_n"))]
  
  cens_parms <- names(dat)[which(names(dat) %in% paste0(parms, "_cens"))]
  
  dat_monthly <- dat %>% 
    mutate(date = lubridate::as_date(datetimestamp),
           year = lubridate::year(date),
           month = lubridate::month(date)) %>% 
    summarize(.by = c(year, month),
              across(any_of(parms),
                     ~modFunn(.x, mean)),
              across(any_of(cens_parms),
                     cens_fun))
  
  
  
  flnm <- paste0(stat, "_monthly")
  assign(flnm, dat_monthly)
  save(list = flnm, file = here::here(outpath, 
                                      paste0(flnm, ".RData")))
  rm(list = flnm)
  rm('dat_monthly')
  
}

Sys.time() - strt
stopCluster(cl)
beepr::beep(8)