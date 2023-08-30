# go from fully compiled stations to QAQC'd station data
# still keeping ALL time points, but replacing
# data with suspect/rejected flags with NAs
# according to the flags and codes we've discsused and agreed to keep
# which are defined in the helper_files/definitions.R script


library(dplyr)
source(here::here("helper_files", "definitions.R"))
source(here::here("helper_files", "functions.R"))


path <- here::here("Data", "compiled_by_stn")
outpath <- here::here("Data", "QAQCd_by_stn")

stns_wq <- stringr::str_sub(dir(path, pattern = "wq.RData$"), end = -9)
stns_nut <- stringr::str_sub(dir(path, pattern = "nut.RData$"), end = -10)

# find stations that have BOTH wq and nut data  
stns_wq_nut <- intersect(stns_wq, stns_nut)

# wq ----
for(i in seq_along(stns_wq_nut)){
  file_in <- here::here(path, paste0(stns_wq_nut[i], "wq.RData"))
  dat <- get(load(file_in))
  parms_to_keep <- names(dat)[which(names(dat) %in% c("temp", "spcond", "sal", "ph", "turb",
                                     "do_pct", "do_mgl", "depth", "cdepth",
                                     "level", "clevel"))]
  datqc <- qaqc_df(dat, parms_to_keep, type = "wq")
  save(datqc, file = here::here("Data_processing", "QAQCd_by_stn", 
                                paste0(stns_wq_nut[i], "wq_qc.RData")))
  
}


# nut ----
# note - also need to keep a column for censored/uncensored
for(i in seq_along(stns_wq_nut)){
  file_in <- here::here(path, paste0(stns_wq_nut[i], "nut.RData"))
  dat <- get(load(file_in))
  parms_to_keep <- names(dat)[which(names(dat) %in% c("nh4f", "no23f", 
                                                      "no2f", "no3f",
                                                      "po4f", "chla_n"))]
  datqc <- qaqc_df(dat, parms_to_keep, type = "nut")
  save(datqc, file = here::here("Data_processing", "QAQCd_by_stn",
                                paste0(stns_wq_nut[i], "nut_qc.RData")))
  
}
