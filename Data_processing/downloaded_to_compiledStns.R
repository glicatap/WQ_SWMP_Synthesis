library(doParallel)
library(foreach)
library(SWMPr)

path <- here::here("Data", "downloaded")

# get all the stations with data files (files ending in yyyy.csv)
data_files <- grep("\\d{4}.csv", dir(path), value = TRUE)
stats <- unique(stringr::str_sub(data_files, end = -9))

# processing code from Marcus
# https://github.com/fawda123/swmp_rats/blob/master/R/dat_proc.R
# modified to remove qaqc step, keep add'l qaqc columns, and keep only grab sample data


# setup parallel backend
cl<-makeCluster(10)  
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

  # save attributes to reattach after subsetting  
  attrs <- attributes(tmp)

  # remove empty columns
  # find them, modifying some code from janitor::remove_empty(): 
  mask_keep <- colSums(!is.na(tmp)) > 0
  # also find f_ cols for those parameters, then keep only ones that are in the data frame
  # (some f_ cols are empty so 'to_boot' ends up with f_f_param - get rid of those)
  to_boot <- c(names(tmp)[!mask_keep], paste0("f_", names(tmp)[!mask_keep]))
  boot <- to_boot[which(to_boot %in% names(tmp))]

  # and only keep the non-to_boot cols
  tmp <- tmp[!(names(tmp) %in% boot)]
  
  # reattach SWMPr attributes. Some names and params are gone now so remove them.
  attrs$names <- names(tmp)
  attrs$parameters <- attrs$parameters[which(attrs$parameters %in% names(tmp))]
  attributes(tmp) <- attrs

  # assign tmp to object, save, clear memory
  assign(stat, tmp)
  save(list = stat, file = paste0('Data/compiled_by_stn/', stat, '.RData'))
  rm(list = stat)
  rm('tmp')
  
  
}

Sys.time() - strt
beepr::beep(8)

stopCluster(cl)
