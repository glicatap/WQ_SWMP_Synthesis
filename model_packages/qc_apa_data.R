# this is the script Kim used to generate apacpwq_qc.RData and apacpnut_qc.RData
library(dplyr)
library(tidyr)
source(here::here("QAQC_flags", "functions.R"))
load("model_packages/apacpwq.RData")
load("model_packages/apacpnut.RData")


# definitions of parameters etc. ----
# flags that are 'useable'
flags_keep <- c("<0>", "<4>", "<-4>", "<5>", "<-5>", "<2>", "<3>")
flagCodes_keep <- c("<1> \\[GIT\\]")  # Use \\ for parentheses and square brackets.   
flagCodes_keepvec <- paste(flagCodes_keep, collapse = "|")

# wq params
wq_parms <- c("temp", "spcond", "sal",
              "do_pct", "do_mgl")
wq_keep <- c(wq_parms, paste0("f_", wq_parms))
names(wq_parms) <- paste0("val_", wq_parms)  # for easier pivoting later

# nut params
nut_parms <- c("chla_n", "po4f", 
               "nh4f", "no23f")
nut_keep <- c(nut_parms, paste0("f_", nut_parms))
names(nut_parms) <- paste0("val_", nut_parms)  # for easier pivoting later



# subset nut data ----
# rename some things
apacpnutqc <- apacpnut %>% 
  select(datetimestamp, any_of(nut_keep)) %>% 
  rename(any_of(nut_parms)) %>% 
  rename(val_chlaN = val_chla_n,
         f_chlaN = f_chla_n)

# set up to pivot
specnut <- apacpnutqc %>% 
  build_longer_spec(
    cols = (starts_with("val_")[1]):ncol(apacpnutqc),
    names_to = c(".value", "param"),
    names_sep = "_")

# pivot, replace discarded data with NAs, and pivot back to wider
apacpnutqc <- apacpnutqc %>% 
  pivot_longer_spec(spec = specnut) %>% 
  mutate(Year = lubridate::year(datetimestamp),
         Month = lubridate::month(datetimestamp),
         Day = lubridate::mday(datetimestamp),
         flag = extract_flag(f),
         keepStatus = case_when(flag %in% flags_keep ~ "keep",
                                grepl(flagCodes_keepvec, f) ~ "keep",
                                .default = "discard"),
         val = case_when(keepStatus == "discard" ~ NA_real_,
                         .default = val),
         val = round(val, 4)) %>% 
  select(datetimestamp, Year, Month, Day, param, val) %>% 
  pivot_wider(names_from = param,
              values_from = val)


# subset wq data ----
# rename some things
apacpwqqc <- apacpwq %>% 
  select(datetimestamp, any_of(wq_keep)) %>% 
  rename(any_of(wq_parms)) %>% 
  rename(val_domgl = val_do_mgl,
         val_dopct = val_do_pct,
         f_domgl = f_do_mgl,
         f_dopct = f_do_pct)

# set up to pivot
specwq <- apacpwqqc %>% 
  build_longer_spec(
    cols = (starts_with("val_")[1]):ncol(apacpwqqc),
    names_to = c(".value", "param"),
    names_sep = "_")

# pivot, replace discarded data with NAs, and pivot back to wider
apacpwqqc <- apacpwqqc %>% 
  pivot_longer_spec(spec = specwq) %>% 
  mutate(Year = lubridate::year(datetimestamp),
         Month = lubridate::month(datetimestamp),
         Day = lubridate::mday(datetimestamp),
         flag = extract_flag(f),
         keepStatus = case_when(flag %in% flags_keep ~ "keep",
                                grepl(flagCodes_keepvec, f) ~ "keep",
                                .default = "discard"),
         val = case_when(keepStatus == "discard" ~ NA_real_,
                         .default = val),
         val = round(val, 4)) %>% 
  select(datetimestamp, Year, Month, Day, param, val) %>% 
  pivot_wider(names_from = param,
              values_from = val)


# write the data frames back out ----
save(apacpwqqc, apacpnutqc, file = "model_packages/apaqc.RData")
