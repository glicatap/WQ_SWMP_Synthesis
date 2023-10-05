#' This script is the control board for generating files for each reserve
#' that include time series for each parameter at each station
library(foreach)
library(doParallel)

outpath <- here::here("Station_level_information", "time_series_graphs")
reserves <- c("ace", "apa", "cbm", "cbv", "del", "elk", "gnd", "grb", "gtm", 
              "hee", "hud", "jac", "job", "kac", "lks", "mar", "nar", "niw", 
              "noc", "owc", "pdb", "rkb", "sap", "sfb", "sos", "tjr", "wel", 
              "wkb", "wqb")
# reserves <- c("gnd", "lks", "niw")
# what didn't work the first time:
reserves <- c("ace", "del", "gnd", "gtm", "job", "wkb")
# what didnt' work the second time:
reserves <- c("gnd", "wkb") # ran them manually
res = "lks"
res = "niw"
res = "gnd"

# setup parallel backend
cl<-makeCluster(10)  
registerDoParallel(cl)
strt<-Sys.time()

# process all stations
foreach(res = reserves, .packages = c('xfun', 'rmarkdown')) %dopar% {
  outname <- paste0(toupper(res), "_TimeSeries-4.html")

  try(
    xfun::Rscript_call(
      rmarkdown::render,
      list(input = here::here("Station_level_information", "Station_time_series.Rmd"), 
           params = list("reserve" = res),
           output_file = here::here(outpath, outname))
    )
  )
}

Sys.time() - strt
stopCluster(cl)
beepr::beep(8)
