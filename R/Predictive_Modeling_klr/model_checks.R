library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(MuMIn)
library(doParallel)
library(glmmTMB)
library(tictoc)
library(vegan)
library(ggfortify)

trends <- read.csv("long-term-trends.csv")

ggplot(data = trends, aes(x = Slope, fill = sig_trend)) +
  geom_histogram(position = "identity", color = "black", bins = 20) +
  facet_wrap(~ parameter, scales = "free_x") +
  theme_minimal()


ggplot(data = trends, aes(x = Slope, y = std.error, color = sig_trend)) +
  geom_point() +
  facet_wrap(~ parameter, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Adding vertical line at zero
  theme_minimal()

chla_trends <- trends %>%  filter(parameter == "chla_n")

sig_chla_trends<-chla_trends %>%  filter(sig_trend == "yes")

sig_chla_trends <- sig_chla_trends %>%
  mutate(station_short = substr(station, 1, 5))

sig_chla_station<-sig_chla_trends %>%  select("station_short")

sig_chla_station <- sig_chla_station %>%
  rename(station = station_short)

ggplot(data=chla_trends,aes(x=Slope)) +
  geom_histogram(aes(fill = sig_trend), color = "black") +
  facet_wrap(~sig_trend) +
  theme_minimal()


trend_counts <- chla_trends %>% 
  group_by(sig_trend) %>% 
  summarise(count = n())


#######################
#load in Kim's code

source(here::here("R", "Analyses_for_paper",
                  "05_predictive_modeling",
                  "050_setup.R"))

# pick the specific predictors we've agreed on
# center and scale all but response 
dat_chl <- dat_all |> 
  select(reserve, station,
         chla_trend,
         # lat/temp/par PCA
         tpl_PC1,
         # wq medians
         spcond_median, turb_median.log,
         # wq trends
         temp_trend, spcond_trend, turb_trend,
         # nut medians
         chla_median.log, nh4_median.log, no23_median.log, po4_median.log,
         # nut trends
         nh4f_trend, no23f_trend, po4f_trend,
         # met
         precp_median, precp_trend, dailyPAR_trend) |> 
  mutate(across(tpl_PC1:last_col(),
                function(x) as.vector(scale(x))))

formula_chl <- paste0("chla_trend ~ ", paste(names(dat_chl[4:ncol(dat_chl)]), collapse = " + "), " + (1|reserve)")

mod_chl <- glmmTMB(as.formula(formula_chl),
                   data = dat_chl)
performance::check_singularity(mod_chl)
r.squaredGLMM(mod_chl)
summary(mod_chl)


##################
#only keep sig chla trends

matched_rows <- dat_chl %>% 
  semi_join(sig_chla_station, by = "station")

formula_chl2 <- paste0("chla_trend ~ ", paste(names(matched_rows[4:ncol(matched_rows)]), collapse = " + "), " + (1|reserve)")


mod_chl2 <- glmmTMB(as.formula(formula_chl2),
                   data = matched_rows)
performance::check_singularity(mod_chl2)
r.squaredGLMM(mod_chl2)
summary(mod_chl2)

################

# Load necessary libraries
# drop random factor due to singular fit and extremely low variance in random effect


formula_chl3 <- paste0("chla_trend ~ ", paste(names(matched_rows[4:ncol(matched_rows)]), collapse = " + "))


mod_chl2 <- lm(as.formula(formula_chl3),
                data = matched_rows)
performance::check_singularity(mod_chl2)

r2(mod_chl2)
check_collinearity(mod_chl2)
summary(mod_chl2)
