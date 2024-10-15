library(tidyverse)
library(vegan)
# library(lme4)
# library(glmmTMB)
library(nlme)
library(MuMIn)
#V3 version removing LKS

load(here::here("Outputs",
                #"06_model_selection",
                #"R_objects",
                "chla_out_nlme_v3_mdl.RData"))

source(here::here("R", "Analyses_for_paper",
                  "05_predictive_modeling",
                  "050_setup_MDL.R"))

rm(dat_all)

mod_subsets <- chla_subsets

# means and sds used to scale ----

# start with dat_all; only keep what's in dat_chl and numeric
dat_means <- dat_all3 |> 
  select(any_of(names(dat_chl)),
         -reserve) |> 
  summarize(across(everything(), mean))
dat_sds <- dat_all3 |> 
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
ggplot(coeffs_stnd) +
    geom_pointrange(aes(y = term,
                        x = Estimate,
                        xmin = ci_low,
                        xmax = ci_high,
                        col = sw_all,
                        size = sw_all), 
                    show.legend = c(size = FALSE)) +  # Remove size legend
    khroma::scale_color_batlow(reverse = TRUE) +
    scale_size_continuous(range = c(0.5, 3))+
    geom_vline(xintercept = 0, col = "gray40") +
    labs(title = "Standardized coefficients in averaged model for chl a trend with LKS removed",
         subtitle = "models with delta < 4",
         x = "Coefficient",
         y = "Term",
         col = "Variable Importance") +  # Adjust col title in legend
    theme(
        plot.title = element_text(size = 16, face = "bold"),  # Title size
        plot.subtitle = element_text(size = 14),  # Subtitle size
        axis.title.x = element_text(size = 12),  # X-axis label size
        axis.title.y = element_text(size = 12),  # Y-axis label size
        axis.text = element_text(size = 14),  # Axis text size
        legend.title = element_text(size = 12),  # Legend title size
        legend.text = element_text(size = 10)  # Legend text size
    )


ggplot(coeffs_stnd) +
    geom_pointrange(aes(y = term,
                        x = Estimate,
                        xmin = ci_low,
                        xmax = ci_high,
                        col = sw_all)) +
    khroma::scale_color_batlow(reverse = TRUE) +
    #scale_size_continuous(range = c(0.5, 3))+
    geom_vline(xintercept = 0,
               col = "gray40") +
    #labs(title = "Standardized coefficients in averaged model for chl a trend",
    labs(title = "Standardized coefficients in averaged model for chl a trend",
         subtitle = "models with delta < 4",
         x = "Coefficient",
         y = "Term",
         col = "variable importance")


ggplot(coeffs_stnd) +
    geom_pointrange(aes(y = term,
                        x = Estimate,
                        xmin = ci_low,
                        xmax = ci_high,
                        col = sw_all,
                        size = sw_all)) +
    khroma::scale_color_batlow(reverse = TRUE, guide = "legend") +  # Use single guide
    scale_size_continuous(range = c(0.5, 3), limits = c(0, 1), guide = "legend") +  # Adjust size range and limits
    geom_vline(xintercept = 0, col = "gray40") +
    labs(title = "Standardized coefficients in averaged model for chl a trend",
         subtitle = "models with delta < 4",
         x = "Coefficient",
         y = "Term",
         col = "Variable Importance",
         size = "Variable Importance") +
    guides(colour = guide_legend(reverse = TRUE, title = "Variable Importance"),
           size = guide_legend(reverse = TRUE, title = "Variable Importance")) +
    theme(
        plot.title = element_text(size = 16, face = "bold"),  # Title size
        plot.subtitle = element_text(size = 14),  # Subtitle size
        axis.title.x = element_text(size = 12),  # X-axis label size
        axis.title.y = element_text(size = 12),  # Y-axis label size
        axis.text = element_text(size = 14),  # Axis text size
        legend.title = element_text(size = 12),  # Legend title size
        legend.text = element_text(size = 10)  # Legend text size
    )







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
                      # "06_model_selection",
                      # "R_objects",
                       "chla_post-averaging_v3_mdl.RData"),
     compress = "xz")
