#' This script is the control board for generating files for each reserve
#' that include time series for each parameter at each station
library(foreach)
library(doParallel)

reserves <- c("ace", "apa", "cbm", "cbv", "del", "elk", "gnd", "grb", "gtm", 
              "hee", "hud", "jac", "job", "kac", "lks", "mar", "nar", "niw", 
              "noc", "owc", "pdb", "rkb", "sap", "sfb", "sos", "tjr", "wel", 
              "wkb", "wqb")

# setup parallel backend
# cl<-makeCluster(6)  
# registerDoParallel(cl)
strt<-Sys.time()

# process all stations
foreach(res = reserves) %do% {
  outpath <- here::here("Station_level_information", "time_series_graphs")
  outname <- paste0(toupper(res), "_TimeSeries.html")

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
# stopCluster(cl)
beepr::beep(8)
