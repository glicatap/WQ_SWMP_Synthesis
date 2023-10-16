#' go from individual QA/QC'd files, at monthly aggregation level
#' to a single file for each of WQ, MET, and NUT
#' 

library(dplyr)
library(purrr)

path <- here::here("Data", "QAQCd_monthly")
outpath <- here::here("Data", "QAQCd_monthly_byType")

#' identify files of each type  
fls_wq <- here::here(path, dir(path, pattern = "wq_monthly.RData$"))
fls_met <- here::here(path, dir(path, pattern = "met_monthly.RData$"))
fls_nut <- here::here(path, dir(path, pattern = "nut_monthly.RData$"))

#' give the files station names
names(fls_wq) <- stringr::str_sub(fls_wq, start = -21L, end = -15L)
names(fls_met) <- stringr::str_sub(fls_met, start = -22L, end = -15L)
names(fls_nut) <- stringr::str_sub(fls_nut, start = -22L, end = -15L)

#' read in, assign station, rbind
wq <- purrr::map2(fls_wq, names(fls_wq), ~get(load(.))) %>% 
  list_rbind(names_to = "station")

met <- purrr::map2(fls_met, names(fls_met), ~get(load(.))) %>% 
  list_rbind(names_to = "station")

nut <- purrr::map2(fls_nut, names(fls_nut), ~get(load(.))) %>% 
  list_rbind(names_to = "station")

#' write to files
#' .RData
save(wq, file = paste0(outpath, "/SWMP_monthlyWQ.RData"))
save(met, file = paste0(outpath, "/SWMP_monthlyMET.RData"))
save(nut, file = paste0(outpath, "/SWMP_monthlyNUT.RData"))
#' .csv
write.csv(wq, file = paste0(outpath, "/SWMP_monthlyWQ.csv"),
          row.names = FALSE, na = "")
write.csv(met, file = paste0(outpath, "/SWMP_monthlyMET.csv"),
          row.names = FALSE, na = "")
write.csv(nut, file = paste0(outpath, "/SWMP_monthlyNUT.csv"),
          row.names = FALSE, na = "")
