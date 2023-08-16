library(dplyr)
library(tidyr)
library(ggplot2)
source(here::here("QAQC_flags", "functions.R"))

# flags and codes to keep ----
flags_keep <- c("<0>", "<4>", "<-4>", "<5>", "<-5>", "<2>", "<3>")
# combinations of flag and code
flagCodes_keep <- c("<1> \\[GIT\\]")  # Use \\ for parentheses and square brackets.   
flagCodes_keepvec <- paste(flagCodes_keep, collapse = "|")


# NUT ----

# load data
load(here::here("Data_processing",
                "compiled_by_type",
                "NUT.RData"))

# pivot data
nut <- NUT %>% 
  select(station, datetimestamp,
         val_po4f = po4f, 
         f_po4f,
         val_nh4f = nh4f, 
         f_nh4f,
         val_no23f = no23f, 
         f_no23f,
         val_chla = chla_n, 
         f_chla = f_chla_n)

specnut <- nut %>% 
  build_longer_spec(
    cols = (starts_with("val_")[1]):ncol(nut),
    names_to = c(".value", "param"),
    names_sep = "_")

nut <- nut %>% 
  pivot_longer_spec(spec = specnut) %>% 
  mutate(Year = lubridate::year(datetimestamp),
         Month = lubridate::month(datetimestamp),
         Day = lubridate::mday(datetimestamp),
         flag = extract_flag(f),
         keepStatus = case_when(flag %in% flags_keep ~ "keep",
                                grepl(flagCodes_keepvec, f) ~ "keep",
                                .default = "discard")) %>% 
  mutate(val = case_when(keepStatus == "discard" ~ NA_real_,
                         .default = val))

nut_monthly <- nut %>% 
  group_by(station, param, Year, Month) %>% 
  summarize(val = mean(val, na.rm = TRUE))

nut_monthly %>% 
  filter(param == "chla",
         !is.na(val)) %>% 
  ggplot(aes(x = Month, y = val, group = Month)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~station, scales = "free_y") +
  theme_bw()



flagCodes_params <- nut %>% 
  group_by(param, keepStatus, f) %>% 
  summarize(n = n()) %>% 
  arrange(param, keepStatus, f)

flagCodes_combined <- nut %>% 
  group_by(keepStatus, f) %>% 
  summarize(n = n(),
            percent_of_data = round(n/nrow(nut)*100, 2)) %>% 
  ungroup() %>% 
  arrange(keepStatus, f)

flagCodes_combined %>% 
  group_by(keepStatus) %>% 
  slice_max(percent_of_data, n = 40) %>% 
  arrange(keepStatus,
          f)  %>% 
  View()


# WQ ----

specwq <- wq %>% 
  build_longer_spec(
    cols = (starts_with("val_")[1]):ncol(wq),
    names_to = c(".value", "param"),
    names_sep = "_")
wq_long <- wq %>% 
  pivot_longer_spec(spec = specwq)
  
  
  
  