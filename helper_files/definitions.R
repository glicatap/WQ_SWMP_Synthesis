#' this script is where we can define which parameters to keep
#' as well as which QA/QC flags and codes to keep
#' 
#' 
library(stringr)
library(dplyr)

# Stations ----
#' only want stations with both wq and nut?
#' for long-term trends, only want active stations with at least 10 years of data?
#' for seasonal investigation, only keep active stations?


# active stations, match wq and nut
mstns <- readr::read_csv(here::here("helper_files", "sampling_stations.csv")) %>%  #read.csv threw an error for some reason
  janitor::clean_names() %>%   
  filter(status == "Active")
stns_wq_d <- str_sub(grep("wq$", mstns$station_code, value = TRUE),
                   end = -3)
stns_nut_d <- str_sub(grep("nut$", mstns$station_code, value = TRUE),
                    end = -4)
stns_wq_nut_d <- intersect(stns_wq_d, stns_nut_d)
stns_in_regions <- mstns[grepl(paste(stns_wq_nut_d, collapse = "|"), mstns$station_code), c("station_code", "region")]
# cleanup  
rm(stns_nut_d, stns_wq_d)


#' active stations with >= 10 years of data
mstns10 <- mstns |>    
  tidyr::separate(active_dates, into = c("start_date", "first_ end_date"),
                  sep = "-",
                  fill = "right",
                  extra = "drop") %>%  # only want the first date if there are multiples
  filter(lubridate::decimal_date(lubridate::my(start_date)) <= 2013.0,
         status == "Active")

stns_wq_d10 <- str_sub(grep("wq$", mstns10$station_code, value = TRUE),
                     end = -3)
stns_nut_d10 <- str_sub(grep("nut$", mstns10$station_code, value = TRUE),
                      end = -4)
stns_wq_nut_d10 <- intersect(stns_wq_d10, stns_nut_d10)
stns_met_d10 <- str_sub(grep("met$", mstns10$station_code, value = TRUE),
                        end = -4)

# cleanup
rm(mstns, mstns10, stns_nut_d10, stns_wq_d10)


# Parameters ----
params_wq <- c("temp", "spcond", "sal", "do_mgl", "do_pct", "turb")
params_nut <- c("nh4f", "no23f", "po4f", "chla_n")
f_wq <- paste0("f_", params_wq)
f_nut <- paste0("f_", params_nut)

# QA/QC flags and codes ----

#' general flags to keep for wq
qaqcKeep_wq_startsWith <- c("<0>", "<2>", "<3>", "<4>", "<5>")
#' to use the above: 
#' str_starts(VECTOR, paste(qaqcKeep_wq_startsWith, collapse = "|"))


#' specific suspect flags to keep
qaqcKeep_wq_complete <- c("^<1> \\[GIT\\]$",          # exactly match <1> [GIT]
                          "(^<1>)(.*GIT)(.*CSM)"     # start with <1>, also contain GIT and CSM
                          )
#' the above vector is so we don't keep 1 GIT with other codes 
#' that we might not want,like CBF
#' to use the above: 
#' str_detect(VECTOR, paste(qaqcKeep_wq_complete, collapse = "|"))


#' nutrients - read in spreadsheet with definitions
#' because there were just so many specific details
#' f is the column that should match up with the full qa/qc col
#' KeepOrDiscard is the column defining what we should do with it
nut_flagscodes <- read.csv(here::here("helper_files",
                                      "QAQC_FlagsCodes_NUT.csv"))[c("f", "KeepOrDiscard", "below_detection")]
qaqcKeep_nut_complete <- nut_flagscodes[nut_flagscodes$KeepOrDiscard == "keep", "f"]

cens_flagscodes <- nut_flagscodes %>% 
  filter(KeepOrDiscard == "keep",
         below_detection == 1) %>% 
  pull(f)
