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

custom_labels <- c(
    "temp_trend" = expression("Temp. Trend"),
    "spcond_median" = expression("Med. SpCond"),
    "chla_trend" = expression("Chl-a Trend"),
    "chla_median.log" = expression("Med. Log(Chl-a)"),
    "nh4f_mdl_trend" = expression("NH"[4] ~ "Trend"),
    "tld_PC1" = expression("TLD PC"[1]),
    "turb_trend" = expression("Turb. Trend"),
    "spcond_trend" = expression("SpCond Trend"),
    "no23_median.log" = expression("Med. Log(NO"[23]~")"),
    "turb_median.log" = expression("Med. Log(Turbidity)"),
    "nh4_median.log" = expression("Med. Log(NH"[4]~")"),
    "no23f_mdl_trend" = expression("NO"[23] ~ "Trend"),
    "po4_median.log" = expression("Med. Log(PO"[4]~")"),
    "po4f_mdl_trend" = expression("PO"[4] ~ "Trend"),
    "precp_median" = expression("Med. Precip."),
    "precp_trend" = expression("Precip. Trend")
)




# Plot variable importance for Chl a
ggplot(swdf, aes(x = predictor)) +
    geom_point(aes(y = sw_all, col = "All Top Models"), size = 4, alpha = 0.8) +
    geom_point(aes(y = sw_nonnested, col = "Nesting Removed"), size = 4, shape = 17, alpha = 0.8) +
    scale_x_discrete(labels = custom_labels) +  # Apply custom labels
    scale_color_manual(values = c("All Top Models" = "#1f77b4", "Nesting Removed" = "#ff7f0e")) +
    theme_bw(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)
    ) +
    labs(
        title = "Relative Variable Importance: Chl a",
        subtitle = "Models with Δ < 4",
        x = "",
        y = "Sum of Akaike Weights"
    )


# Plot standardized coefficients for Chl a
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

# Define common color scale
color_scale <- khroma::scale_color_batlow(reverse = TRUE, name = "Predictor Importance",limits = c(0, 1))

p1 <- ggplot(coeffs_stnd) +
    geom_pointrange(
        aes(y = term, x = Estimate, xmin = ci_low, xmax = ci_high, col = sw_all),
        size = 1.5, fatten = 3, alpha = 0.8
    ) +
    color_scale +  # Use shared color scale
    geom_vline(xintercept = 0, col = "gray40", linetype = "dashed", size = 1) +
    scale_y_discrete(labels = custom_labels) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(size = 16, face = "bold", color = "black"),
        plot.subtitle = element_text(size = 12, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 10, color = "black"),
        #legend.position = "none"
    ) +
    labs(
        x = "Chl-a Trend Model Coefficient",
        y = ""
    )

p1
####################################################

library(tidyverse)
library(MuMIn)
library(vegan)
#V3 version removing LKS

# load things
load(here::here("Outputs",
                #   "06_model_selection",
                #  "R_objects",
                "domgl_out_v3_mdl.RData"))

# get dat_all set up properly, with PCA and log transformations etc.
source(here::here("R", "Analyses_for_paper",
                  "05_predictive_modeling",
                  "050_setup_MDL.R"))

rm(dat_all)

mod_subsets <-  domgl_subsets

dat_means <- dat_all3 |> 
    select(any_of(names(dat_domgl)),
           -reserve) |> 
    summarize(across(everything(), mean))
dat_sds <- dat_all3 |> 
    select(any_of(names(dat_domgl)),
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

# for main, use delta < 4 and fit them
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



###########################

# Plot variable importance for DO
ggplot(swdf, aes(x = predictor)) +
    geom_point(aes(y = sw_all, col = "All Top Models"), size = 4, alpha = 0.8) +
    geom_point(aes(y = sw_nonnested, col = "Nesting Removed"), size = 4, shape = 17, alpha = 0.8) +
    scale_x_discrete(labels = custom_labels) +  # Apply custom labels
    scale_color_manual(values = c("All Top Models" = "#1f77b4", "Nesting Removed" = "#ff7f0e")) +
    theme_bw(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)
    ) +
    labs(
        title = "Relative Variable Importance: DO",
        subtitle = "Models with Δ < 4",
        x = "",
        y = "Sum of Akaike Weights"
    )


# Plot standardized coefficients for Chl a
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

p2 <- ggplot(coeffs_stnd) +
    geom_pointrange(
        aes(y = term, x = Estimate, xmin = ci_low, xmax = ci_high, col = sw_all),
        size = 1.5, fatten = 3, alpha = 0.8
    ) +
    color_scale +  # Use the same shared color scale
    geom_vline(xintercept = 0, col = "gray40", linetype = "dashed", size = 1) +
    scale_y_discrete(labels = custom_labels) +
    theme_minimal(base_size = 12) +
    theme(
        plot.title = element_text(size = 16, face = "bold", color = "black"),
        plot.subtitle = element_text(size = 12, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 10, color = "black"),
        #legend.position = "bottom"
    ) +
    labs(
        x = "DO Trend Model Coefficient",
        y = ""
    )
p2



combined_trends <- (p1 + p2) + 
    plot_layout(guides = "collect") &
    theme(
        legend.position = "bottom",
        legend.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 10, color = "black")
    ) & 
    guides(
        color = guide_colorbar(  # Ensures a continuous color scale
            title.position = "top",  # Moves title to the top
            title.hjust = 0.5,  # Centers the title
            barwidth = unit(8, "cm"),  # Increases legend length
            barheight = unit(0.5, "cm"),  # Keeps the bar thin
            title.theme = element_text(margin = margin(b = 6))  # Adds space below title
        )
    )

combined_trends


ggsave("TrendCoef_plot.png", combined_trends, width = 9, height = 6, dpi = 600, bg = "white")


