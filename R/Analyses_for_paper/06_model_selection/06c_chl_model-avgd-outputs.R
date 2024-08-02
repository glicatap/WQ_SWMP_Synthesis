library(tidyverse)
library(vegan)
# library(lme4)
# library(glmmTMB)
library(nlme)
library(MuMIn)

load(here::here("Outputs",
                "06_model_selection",
                "R_objects",
                "chla_out_nlme.RData"))
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

# delta < 4
top_modsd4 <- mod_subsets[which(mod_subsets$delta < 4),]
top_modsd4_unnested <- subset(top_modsd4, !nested(.))
sw(top_modsd4)
sw(top_modsd4_unnested)
model.avg(top_modsd4)$coefficients
model.avg(top_modsd4_unnested)$coefficients

# deltas < 2 and 6, for supplementary info
top_modsd2 <- mod_subsets[which(mod_subsets$delta < 2),]
top_modsd2_unnested <- subset(top_modsd2, !nested(.))

top_modsd6 <- mod_subsets[which(mod_subsets$delta < 6),]
top_modsd6_unnested <- subset(top_modsd6, !nested(.))


# average models ----
# for supplementary, get coeffs etc. for deltas 2 and 6
mod_avgd2 <- model.avg(top_modsd2)
mod_avgd6 <- model.avg(top_modsd6)

# for main, use delta < 4
mod_avgd4 <- model.avg(top_modsd4, fit = TRUE)

swdf <- data.frame(sw_all = sw(top_modsd4)) |> 
  rownames_to_column("predictor")
swdf2 <- data.frame(sw_nonnested = sw(top_modsd4_unnested)) |> 
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
coeffs_stnd <- data.frame(summary(mod_avgd4)$coefmat.full) |> 
  rownames_to_column("term") |> 
  mutate(ci_low = Estimate - 1.96*Adjusted.SE,
         ci_high = Estimate + 1.96*Adjusted.SE,
         term = str_remove(term, "cond\\("),
         term = str_remove(term, "\\)")) |> 
  left_join(swdf, by = c("term" = "predictor")) |> 
  filter(!str_starts(term, "\\(Int")) |> 
  arrange(sw_all) |> 
  mutate(term = fct_inorder(term))

ggplot(coeffs_stnd) +
  geom_pointrange(aes(y = term,
                      x = Estimate,
                      xmin = ci_low,
                      xmax = ci_high,
                      col = sw_all)) +
  khroma::scale_color_batlow(reverse = TRUE) +
  geom_vline(xintercept = 0,
             col = "gray40") +
  labs(title = "Standardized coefficients in averaged model for chl a trend",
       subtitle = "models with delta < 4",
       x = "Coefficient",
       y = "Term",
       col = "variable importance")


# save important outputs for predictor workup
save(dat_chl,                              # data frame used for model
     mod_chl,                              # global model
     dat_means, dat_sds,                   # means and sds of predictors on original scales
     coeffs_stnd,                          # standardized coeffs for delta < 4; includes importance vals
     mod_avgd2, mod_avgd4, mod_avgd6,      # model avgd objects; avgd4 has full fits
     top_modsd2, top_modsd2_unnested,        # nested and non-nested model sets for different deltas
     top_modsd4, top_modsd4_unnested,
     top_modsd6, top_modsd6_unnested,
     file = here::here("Outputs",
                       "06_model_selection",
                       "R_objects",
                       "chla_post-averaging.RData"),
     compress = "xz")
