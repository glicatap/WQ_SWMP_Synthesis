library(dplyr)
library(purrr)


path <- here::here("Data_processing", "compiled_by_stn")

stns_wq <- stringr::str_sub(dir(path, pattern = "wq.RData$"), end = -9)
stns_nut <- stringr::str_sub(dir(path, pattern = "nut.RData$"), end = -10)
# find stations that have BOTH wq and nut data  
stns_wq_nut <- intersect(stns_wq, stns_nut)

# load all those stations and bind them
# first wq
to_load <- paste0(path, "/", stns_wq_nut, "wq.RData") 
names(to_load) <- paste0(stns_wq_nut, "wq")

WQ <- purrr::map2(to_load, names(to_load), ~get(load(.))) %>% 
  list_rbind(names_to = "station")

# write to file and remove
save(WQ, file = here::here("Data_processing", "compiled_by_type", "WQ.RData"))
rm(WQ)

# nut
to_load <- paste0(path, "/", stns_wq_nut, "nut.RData") 
names(to_load) <- paste0(stns_wq_nut, "nut")

NUT <- purrr::map2(to_load, names(to_load), ~get(load(.))) %>% 
  list_rbind(names_to = "station")

save(NUT, file = here::here("Data_processing", "compiled_by_type", "NUT.RData"))
rm(NUT)

