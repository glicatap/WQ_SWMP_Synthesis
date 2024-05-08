#' 5-8-2024 kac
#' 
#' Problems late in the process with trend calculations caused us to 
#' find some issues in the data files that need to be dealt with before
#' compiling to monthly.
#' 
#' 1 - several 0s in data files - should be the MDL for that parameter,
#' and flagged censored. details: https://docs.google.com/spreadsheets/d/1c4GHyGLdza0Peb1sHZrMtghcx0HTPyY0A_tYybjGvrU/edit#gid=0
#' 
#' 2 - NO23 was a calculated parameter at one reserve (OWC). By CDMO protocols,
#' when one component was censored, the NO23 value was left blank.
#' Here, it will be recalculated as NO2 + NO3, and the value will
#' be retained even if censored. When applicable, the value will be flagged as censored.

library(stringr)
library(dplyr)

# OWC NO23 ----
#' 
#' calculated from Dec 2017 through June 2019
#' measured directly through Nov 2016 and again starting July 2019
#' 
#' only do these calculations for Dec 2017 - June 2019 and earlier

all_stns <- dir(here::here("Data", "QAQCd_by_stn"))
owc_fls <- all_stns[which(str_detect(all_stns, "owc[a-z]{2}nut_qc.RData"))]

for(i in seq_along(owc_fls)){
  
  stn_nm <- str_remove(owc_fls[i], "_qc.RData")
  
  # load the data frame 
  dat <- get(load(here::here("Data", "QAQCd_by_stn", owc_fls[i])))

  
  # save a copy of the data frame; appending "uncorrected"
  # the next script looks for files that end in "qc.RData", so 
  # put "uncorrected" before the . and all should still work fine after
  flnm <- paste0(stn_nm, "_qcUncorrected")
  assign(flnm, dat)
  save(list = flnm, file = here::here("Data", "QAQCd_by_stn", 
                                      paste0(flnm, ".RData")))
  rm(list = flnm)
  rm(list = paste0(stn_nm, "_qc"))
  
  
  # update the data frame
  # first split into time periods where we need to calculate, and where we don't.
  # do the calculation where necessary.
  dat_toCalc <- dat |> 
    filter(datetimestamp >= lubridate::ymd_hms("2017-12-01 00:00:00"),
           datetimestamp <= lubridate::ymd_hms("2019-07-01 00:00:00")) |> 
    mutate(no23f = no2f + no3f,
           no23f_cens = case_when(is.na(no2f_cens + no3f_cens) ~ NA_real_,
                                  no2f_cens + no3f_cens > 0 ~ 1,
                                  .default = 0))
  
  dat_nonCalc <- dat |> 
    filter(!(datetimestamp %in% dat_toCalc$datetimestamp))
  
  
  # then join back together and put in order.
  dat_corrected <- bind_rows(dat_toCalc, dat_nonCalc) |> 
    arrange(datetimestamp)

  
  
  # re-save the data frame
  flnm <- paste0(stn_nm, "_qc")
  assign(flnm, dat_corrected)
  save(list = flnm, file = here::here("Data", "QAQCd_by_stn", 
                                      paste0(flnm, ".RData")))
  rm(list = flnm)
  rm(dat, dat_toCalc, dat_nonCalc, dat_corrected, flnm, stn_nm)
}
