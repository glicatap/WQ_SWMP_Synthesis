library(tidyverse)
library(vegan)
library(lme4)
library(glmmTMB)
library(MuMIn)

load(here::here("Outputs",
                "06_model_selection",
                "R_objects",
                "chla_out_lme4.RData"))
# set up data frames
dat_all <- read.csv(here::here("Outputs",
                               "04_compiled_predictors",
                               "compiled_predictors.csv"))
source(here::here("R", "Analyses_for_paper",
                  "05_predictive_modeling",
                  "050_setup.R"))

mod_subsets <- chla_subsets

# means and sds used to scale ----

# start with dat_all; only keep what's in dat_chl and numeric
dat_means <- dat_all |> 
  select(any_of(names(dat_chl)),
         -reserve) |> 
  summarize(across(everything(), mean))
dat_sds <- dat_all |> 
  select(any_of(names(dat_chl)),
         -reserve) |> 
  summarize(across(everything(), sd))


# get top models ----

# include all, and do the nested models thing
# NOTE the se generated below is 'adjusted se' from output -
# verify what this means

top_mods <- mod_subsets[which(mod_subsets$delta < 4),]
top_mods_unnested <- subset(top_mods, !nested(.))
sw(top_mods)
sw(top_mods_unnested)
model.avg(top_mods)$coefficients
model.avg(top_mods_unnested)$coefficients

# average models ----
modavg_all <- model.avg(top_mods)

swdf <- data.frame(sw_all = sw(top_mods)) |> 
  rownames_to_column("predictor")
swdf2 <- data.frame(sw_nonnested = sw(top_mods_unnested)) |> 
  rownames_to_column("predictor")

swdf <- full_join(swdf, swdf2, by = "predictor") |> 
  arrange(desc(sw_all)) |> 
  mutate(predictor = str_remove(predictor, "cond\\("),
         predictor = str_remove(predictor, "\\)"),
         predictor = fct_inorder(predictor))

# plot variable importances  ----
ggplot(swdf, aes(x = predictor)) +
  geom_point(aes(y = sw_all,
                 col = "all top models"),
             size = 3) +
  geom_point(aes(y = sw_nonnested,
                 col = "nesting removed"),
             size = 3) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 40,
                                    hjust = 1,
                                    vjust = 1),
        legend.position = "bottom") +
  labs(title = "Relative variable importance, chla",
       subtitle = "models with delta < 4",
       x = "Predictor",
       y = "Sum of Akaike weights")


# plot standardized coefficients ----


# put in order by variable importance rather than coefficient
coeffs3 <- data.frame(summary(modavg_all)$coefmat.full) |> 
  rownames_to_column("term") |> 
  mutate(ci_low = Estimate - 1.96*Adjusted.SE,
         ci_high = Estimate + 1.96*Adjusted.SE,
         term = str_remove(term, "cond\\("),
         term = str_remove(term, "\\)")) |> 
  left_join(swdf, by = c("term" = "predictor")) |> 
  filter(!str_starts(term, "\\(Int")) |> 
  arrange(sw_all) |> 
  mutate(term = fct_inorder(term))

ggplot(coeffs3) +
  geom_pointrange(aes(y = term,
                      x = Estimate,
                      xmin = ci_low,
                      xmax = ci_high,
                      col = sw_all)) +
  khroma::scale_color_batlow(reverse = TRUE) +
  geom_vline(xintercept = 0,
             col = "gray40") +
  labs(title = "Standardized coefficients in averaged model for chl a trend",
       x = "Coefficient",
       y = "Term",
       col = "variable importance")



# make predictions ----

# lme4 doesn't let us use se for predictions, so back to glmmTMB output
# (glmmTMB wasn't generating deltas or weights)

# how many do we need to subset from glmmTMB for delta < 4?
n_d2 <- sum(chla_subsets$delta < 2)
n_d4 <- sum(chla_subsets$delta < 4)
n_d6 <- sum(chla_subsets$delta < 6)

load(here::here("Outputs",
                "06_model_selection",
                "R_objects",
                "chla_out_glmmTMB.RData"))

top_mods_glmmTMB <- chla_subsets[1:n_d4, ]
modavg_all <- model.avg(top_mods_glmmTMB, fit = TRUE)
