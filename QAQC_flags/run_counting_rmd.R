library(foreach)
library(doParallel)

dataTypes <- c("nut", "wq")

cl<-makeCluster(9)  
registerDoParallel(cl)
strt<-Sys.time()

# process all stations
foreach(dt = dataTypes) %dopar% {
  outname <- paste0("Flagged_data_", dt, ".html")

xfun::Rscript_call(
  rmarkdown::render,
  list(input = here::here("QAQC_flags", "Counting_Data.Rmd"), 
       params = list("dataType" = dt),
       output_file = here::here("QAQC_flags", outname))
)
}

Sys.time() - strt

stopCluster(cl)
