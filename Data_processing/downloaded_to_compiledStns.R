library(doParallel)
library(foreach)
library(SWMPr)

path <- here::here("Data_processing", "downloaded")
stats <- c("gndblwq", "gndbhwq", "gndbcwq", "gndpcwq")
# really will just go through all stations


# processing code from Marcus
# https://github.com/fawda123/swmp_rats/blob/master/R/dat_proc.R
# modified to remove qaqc step, keep add'l qaqc columns, and keep only grab sample data


# setup parallel backend
cl<-makeCluster(6)
registerDoParallel(cl)
strt<-Sys.time()

# process all stations
foreach(stat = stats, .packages = 'SWMPr') %dopar% {
  
  sink('log.txt')
  cat(stat, which(stat == stats), 'of', length(stats), '\n')
  print(Sys.time()-strt)
  sink()
  
  # import raw
  tmp <- import_local(path, stat,
                      collMethd = 1,
                      keep_qaqcstatus = TRUE)
  
  # remove empty columns
  # find them, modifying some code from janitor::remove_empty(): 
  mask_keep <- colSums(!is.na(tmp)) > 0
  # also find f_ cols for those parameters, then keep only ones that are in the data frame
  # (some f_ cols are empty so 'to_boot' ends up with f_f_param - get rid of those)
  to_boot <- c(names(tmp)[!mask_keep], paste0("f_", names(tmp)[!mask_keep]))
  to_boot <- to_boot[which(to_boot %in% names(tmp))]
  # and only keep the non-to_bot cols
  tmp <- tmp[!(names(tmp) %in% to_boot)]

  # assign tmp to object, save, clear memory
  assign(stat, tmp)
  save(list = stat, file = paste0('Data_processing/compiled_by_stn/', stat, '.RData'))
  rm(list = stat)
  rm('tmp')
  
  
}

Sys.time() - strt
# 4 GND WQ stations took 1.14 mins sequentially
# and 22 seconds using 6 cores

stopCluster(cl)