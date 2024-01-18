library(dplyr)
library(tidyr)
library(tibble)
library(vegan)

# PCA

load(here::here("Outputs", "calculated_trends", "long-term-trends.RData"))
trends_all <- bind_rows(trends_df, trends_nut)  # not met trends, because stations don't match wq/nut

# data shaping ----

all_wide <- trends_all |> 
  mutate(station = substr(station, start = 1, stop = 5),
         sig_trend = case_when(sig_trend == "yes" ~ 1,
                               sig_trend == "no" ~ 0,
                               is.na(sig_trend) ~ NA_real_),
         sig_seasonality = case_when(sig_seasonality == "yes" ~ 1,
                                     sig_seasonality == "no" ~ 0,
                                     is.na(sig_seasonality) ~ NA_real_)) |> 
  select(station, parameter, Slope_overall = Slope,
         Spring_estimate, Fall_estimate,  # possible in ordination or explanatory
         Winter_estimate, Summer_estimate,
         overall_median, sig_trend, sig_seasonality) |>   # explanatory  
  pivot_wider(id_cols = station,
              names_from = parameter,
              values_from = Slope_overall:sig_seasonality,
              names_glue = "{parameter}_{.value}",
              names_vary = "slowest") |> 
  tibble::column_to_rownames("station")


# select only chl and DO params of interest, and only their slopes
# and widen out the data frame
params_of_interest <- c("chla_n", "do_mgl_median",
                        "do_proportion_below2", "do_proportion_below5")

# only overall slopes of chl and DO params
slopes_only <- all_wide |> 
  select(all_of(paste0(params_of_interest, "_Slope_overall"))) 
# we lose 30 stations to that na.omit - I suspect a lot of them are because DO proportion
# trends couldn't be calculated - that was problematic
# yep 24 rows with NA in proportion below 2 slope; actually 23 for chl

# going to substitute 0 for when a slope couldn't be calculated
# RUN THIS BY OTHERS.......
slopes_only[][is.na(slopes_only[])] <- 0


# also include seasonal slopes
slopes2 <- all_wide |> 
  select(all_of(paste0(params_of_interest, "_Slope_overall")),
         starts_with(params_of_interest) & ends_with("estimate")) 
# na.omit cuts out half the stations, due to seasonal slopes not being able to be estimated
# so substituting 0 here too
slopes2[][is.na(slopes2[])] <- 0


# scale manually
# I'm not sure scaling is working the same way all PCA functions.....
# default options of scale center (subtract the mean) and scale to z-scores (divide by standard deviation)
slopes_scaled <- as.data.frame(apply(slopes_only, MARGIN = 2, scale))
rownames(slopes_scaled) <- rownames(slopes_only)

apply(slopes_scaled, MARGIN = 2, mean)  # all numerically 0
apply(slopes_scaled, MARGIN = 2, sd)  # all are 1

slopes2_scaled <- as.data.frame(apply(slopes2, MARGIN = 2, scale))
rownames(slopes2_scaled) <- rownames(slopes2)

# pull out a few additional factors for exploration
# seasonal slopes, temp trend (air and water),
# overall medians for all params
env_factors <- all_wide |> 
  select(ends_with("overall_median"),
         temp_median_Slope_overall)
names(env_factors) <- stringr::str_remove(names(env_factors), "median_")
# and scale
env_scaled <- as.data.frame(apply(env_factors, MARGIN = 2, scale))
rownames(env_scaled) <- rownames(env_factors)

# prcomp ----
pca1 <- prcomp(slopes_only, scale. = TRUE)
biplot(pca1)
pca1
summary(pca1)
# axis 1 is just the DOs separating in different directions
# axis 2 is the chl trend


# plus seasonal slopes ----
pca2 <- prcomp(slopes2, scale. = TRUE)
biplot(pca2)
# similar in terms of DO going opposite directions along axis 1
# and chl being big for axis 2
# new info is that seasons for DO start to separate along axis 2
# (chl seasons don't seem to separate)
# that's my visual interpretation anyway
# let's look at numbers
summary(pca2)
screeplot(pca2, type = "lines")
# 3 axes get us to 67% of variation explained and that's what I'd pick from the screeplot

# examine those first 3 axes
pca2_3axes <- pca2$rotation[, 1:3] |> 
  as_tibble(rownames = "parameter") 

# arrange by PC1
pca2_3axes |> arrange(desc(abs(PC1)))

# arrange by PC2
pca2_3axes |> arrange(desc(abs(PC2)))

# arrange by PC3
pca2_3axes |> arrange(desc(abs(PC3)))


# using vegan package ----
veg_pca1 <- rda(slopes_only, scale = TRUE)
biplot(veg_pca1,
       display = c("sites",
                   "species"),
       type = c("text",
                "text"))

veg_pca2 <- rda(slopes2, scale = TRUE)
biplot(veg_pca2,
       display = c("sites",
                   "species"),
       type = c("text",
                "text"))

# scaling manually ----
# moved code above to shaping

scaled_pca1 <- prcomp(slopes_scaled)
veg_scaled_pca1 <- rda(slopes_scaled)

biplot(scaled_pca1)
# looks the same as the biplot for prcomp with scaled = TRUE

biplot(veg_scaled_pca1,
       display = c("sites",
                   "species"),
       type = c("text",
                "text"))

# looks like the prcomp biplot - so something in vegan is scaling differently
# maybe it doesn't center first? - can see in the attributes for veg_pca1$Ybar
# centering is important for our variables

# princomp for comparison ----
pca1b <- princomp(slopes_scaled)
biplot(pca1b)  # same as prcomp, just flipped
# princomp doesn't include very low values in the loadings though
# (that's different from prcomp)


# all the loadings ----
pca1b$loadings # princomp loadings
pca1$rotation  # prcomp loadings
veg_scaled_pca1$CA$v  # match prcomp loadings
scores(veg_scaled_pca1, choices = c(1:4), 
       display = "species",
       scaling = 0)  # scaling = 0 gives unscaled raw scores that match prcomp
scores_for_ggplotting <- scores(veg_scaled_pca1, choices = c(1:4), 
                                display = "species",
                                scaling = 0,
                                tidy = TRUE)

# from this tutorial: https://www.flutterbys.com.au/stats/tut/tut14.2.html
# it appears princomp is spectral decomposition-based,
# but prcomp and rda are single value decomposition, starting with
# the rectangular data matrix


# envfit ----

# playing with overall medians, plus adding in overall temperature slopes

# all data frames have same number of rows because
# if a trend couldn't be calculated, I substituted 0

# re-run and name my pca with scaled data
pca_out <- rda(slopes_scaled)
veg_env <- envfit(pca_out, env_scaled)

biplot(pca_out,
       display = c("sites",
                   "species"),
       type = c("text",
                "text"))
plot(veg_env, cex = 0.5)


biplot(pca_out, choices = c(2, 3),
       display = c("sites",
                   "species"),
       type = c("text",
                "text"))
plot(veg_env, cex = 0.5)

biplot(pca_out, choices = c(2, 3),
            display = c("sites",
                        "species"),
            type = c("text",
                     "text"))

