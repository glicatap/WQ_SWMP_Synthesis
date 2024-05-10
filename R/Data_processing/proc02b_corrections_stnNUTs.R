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
all_stns <- dir(here::here("Data", "QAQCd_by_stn"))

# OWC NO23 ----
#' 
#' calculated from Dec 2017 through June 2019
#' measured directly through Nov 2016 and again starting July 2019
#' 
#' only do these calculations for Dec 2017 - June 2019 and earlier


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


# Other 0s ----
#' several station and year combinations to deal with. Have looked in metadata
#' for MDLs and will use the highest MDL for a given year to replace any 0s
#' also need to look out for other values below and MDL but unflagged.


# NAR ----
#' 0s in NH4, NO23, PO4. All in 2022. Not flagged as censored.
#' sort of..... setting any non-detects to the highest detection limit 
#' by doing this, which I'm not really crazy about
#' but I don't want to have to monkey with every individual MDL time period
#' 
res_fls <- all_stns[which(str_detect(all_stns, "nar[a-z]{2}nut_qc.RData"))]
mdls <- list(nh4 = 0.0062,
             no23 = 0.0055,
             po4 = 0.0029)

for(i in seq_along(res_fls)){
  
  stn_nm <- str_remove(res_fls[i], "_qc.RData")
  
  # load the data frame 
  dat <- get(load(here::here("Data", "QAQCd_by_stn", res_fls[i])))
  
  
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
  # for MDL corrections, deal with censoring column first
  # because then both case_whens can be based on the MDL value
  
  dat_toCalc <- dat |> 
    filter(lubridate::year(datetimestamp) == 2022) |> 
    mutate(no23f_cens = case_when(no23f < mdls$no23 ~ 1,
                                  .default = no23f_cens),
           no23f = case_when(no23f < mdls$no23 ~ mdls$no23,
                             .default = no23f),
           nh4f_cens = case_when(nh4f < mdls$nh4 ~ 1,
                                  .default = nh4f_cens),
           nh4f = case_when(nh4f < mdls$nh4 ~ mdls$nh4,
                             .default = nh4f),
           po4f_cens = case_when(po4f < mdls$po4 ~ 1,
                                 .default = po4f_cens),
           po4f = case_when(po4f < mdls$po4 ~ mdls$po4,
                            .default = po4f))
  
  dat_nonCalc <- dat |> 
    filter(lubridate::year(datetimestamp) != 2022)
  
  
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

# NOC ----
#' similar to NAR situation - 0s in NH4, NO23, PO4; only in 2022.
#' MDLs are different so change that table.  

res_fls <- all_stns[which(str_detect(all_stns, "noc[a-z]{2}nut_qc.RData"))]
mdls <- list(nh4 = 0.0015,
             no23 = 0.0008,
             po4 = 0.0008)

for(i in seq_along(res_fls)){
  
  stn_nm <- str_remove(res_fls[i], "_qc.RData")
  
  # load the data frame 
  dat <- get(load(here::here("Data", "QAQCd_by_stn", res_fls[i])))
  
  
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
  # for MDL corrections, deal with censoring column first
  # because then both case_whens can be based on the MDL value
  
  dat_toCalc <- dat |> 
    filter(lubridate::year(datetimestamp) == 2022) |> 
    mutate(no23f_cens = case_when(no23f < mdls$no23 ~ 1,
                                  .default = no23f_cens),
           no23f = case_when(no23f < mdls$no23 ~ mdls$no23,
                             .default = no23f),
           nh4f_cens = case_when(nh4f < mdls$nh4 ~ 1,
                                 .default = nh4f_cens),
           nh4f = case_when(nh4f < mdls$nh4 ~ mdls$nh4,
                            .default = nh4f),
           po4f_cens = case_when(po4f < mdls$po4 ~ 1,
                                 .default = po4f_cens),
           po4f = case_when(po4f < mdls$po4 ~ mdls$po4,
                            .default = po4f))
  
  dat_nonCalc <- dat |> 
    filter(lubridate::year(datetimestamp) != 2022)
  
  
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


# OWC chl ----
#' just one value in 2002 - have already written out updated files for NO23
#' so this is just an addition to one file

load(here::here("Data", "QAQCd_by_stn", "owcbrnut_qc.RData"))
# save the version with NO23 corrections but not chl correction as "partially corrected"
save(owcbrnut_qc, file = here::here("Data", "QAQCd_by_stn",
                             "owcbrnut_qcPartiallyCorrected.RData"))

# and correct it
owcbrnut_qc <- owcbrnut_qc |> 
  mutate(chla_n_cens = case_when(chla_n == 0 ~ 1,
                                 .default = chla_n_cens),
         chla_n = case_when(chla_n == 0 ~ 0.3,
                            .default = chla_n))

# save back out
save(owcbrnut_qc, file = here::here("Data", "QAQCd_by_stn", "owcbrnut_qc.RData"))
rm(owcbrnut_qc)


# WQB ----
#' WQB has different combinations of years/parameters that are affected
#' 2005 has all 3 nutrients, so let's start there
#' 2019 and 2022 are NO23
#' 2002 and 2003 are chla

res_fls <- all_stns[which(str_detect(all_stns, "wqb[a-z]{2}nut_qc.RData"))]
mdls <- list(nh4.2005 = 0.003,
             no23.2005 = 0.001,
             po4.2005 = 0.004,
             no23.2019 = 0.0005,
             no23.2022 = 0.0001,
             chl = 0.5)


for(i in seq_along(res_fls)){
  
  stn_nm <- str_remove(res_fls[i], "_qc.RData")
  
  # load the data frame 
  dat <- get(load(here::here("Data", "QAQCd_by_stn", res_fls[i])))
  
  
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
  # for MDL corrections, deal with censoring column first
  # because then both case_whens can be based on the MDL value
  
  # 0203 - chl only
  dat_0203 <- dat |> 
    filter(lubridate::year(datetimestamp) %in% c(2002, 2003)) |> 
    mutate(chla_n_cens = case_when(chla_n < mdls$chl ~ 1,
                                  .default = chla_n_cens),
           chla_n = case_when(chla_n < mdls$chl ~ mdls$chl,
                              .default = chla_n))
  
  # 2005 - all 3 nuts
  dat_05 <- dat |> 
    filter(lubridate::year(datetimestamp) == 2005) |> 
    mutate(no23f_cens = case_when(no23f < mdls$no23.2005 ~ 1,
                                  .default = no23f_cens),
           no23f = case_when(no23f < mdls$no23.2005 ~ mdls$no23.2005,
                             .default = no23f),
           nh4f_cens = case_when(nh4f < mdls$nh4.2005 ~ 1,
                                 .default = nh4f_cens),
           nh4f = case_when(nh4f < mdls$nh4.2005 ~ mdls$nh4.2005,
                            .default = nh4f),
           po4f_cens = case_when(po4f < mdls$po4.2005 ~ 1,
                                 .default = po4f_cens),
           po4f = case_when(po4f < mdls$po4.2005 ~ mdls$po4.2005,
                            .default = po4f))
  
  # 2019 - no23 only
  dat_19 <- dat |> 
    filter(lubridate::year(datetimestamp) == 2019) |> 
    mutate(no23f_cens = case_when(no23f < mdls$no23.2019 ~ 1,
                                  .default = no23f_cens),
           no23f = case_when(no23f < mdls$no23.2019 ~ mdls$no23.2019,
                             .default = no23f))
  
  # 2022 - no23 only  
  dat_22 <- dat |> 
    filter(lubridate::year(datetimestamp) == 2022) |> 
    mutate(no23f_cens = case_when(no23f < mdls$no23.2022 ~ 1,
                                  .default = no23f_cens),
           no23f = case_when(no23f < mdls$no23.2022 ~ mdls$no23.2022,
                             .default = no23f))
  
  # the rest
  dat_nonCalc <- dat |> 
    filter(!(lubridate::year(datetimestamp) %in% c(2002, 2003, 2005, 2019, 2022)))
  
  
  # then join back together and put in order.
  nrow(dat_0203) + nrow(dat_05) + nrow(dat_19) + nrow(dat_22) + nrow(dat_nonCalc) == nrow(dat)
  dat_corrected <- bind_rows(dat_0203, dat_05, dat_19, dat_22, dat_nonCalc) |> 
    arrange(datetimestamp)
  
  
  
  # re-save the data frame
  flnm <- paste0(stn_nm, "_qc")
  assign(flnm, dat_corrected)
  save(list = flnm, file = here::here("Data", "QAQCd_by_stn", 
                                      paste0(flnm, ".RData")))
  rm(list = flnm)
  # rm(dat, dat_toCalc, dat_nonCalc, dat_corrected, flnm, stn_nm)
}

