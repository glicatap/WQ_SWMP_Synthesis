# Take aggregated daily files and generate csvs

library(doParallel)
library(foreach)
library(dplyr)
library(lubridate)


path <- here::here("Data", "QAQCd_daily")
outpath <- here::here("Data", "QAQCd_daily_csvs")

stns_wq <- stringr::str_sub(dir(path, pattern = "wq_daily.RData$"), end = -13)
stns_met <- stringr::str_sub(dir(path, pattern = "met_daily.RData$"), end = -13)
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
  
  file_in <- here::here(path, paste0(stat, "_daily.RData"))
  dat <- get(load(file_in)) %>% 
    mutate(station = stat) %>% 
    relocate(station)
  
    # assign daily df to object, save, clear memory
  flnm <- paste0(stat, "_daily")
  write.csv(dat, file = here::here(outpath, 
                                           paste0(flnm, ".csv")),
            row.names = FALSE)
  rm(list = flnm)
  rm('dat')
  
}

Sys.time() - strt
stopCluster(cl)
beepr::beep(7)