# import predictor data frames ----

dat_all <- read.csv(here::here("Outputs", 
                               "04_compiled_predictors", 
                               "compiled_predictors.csv"))

preds_doLT2 <- read.csv(here::here("Outputs", 
                                   "04_compiled_predictors",
                                   "doLT2_compiled_predictors.csv"))


# PCA on temp/par/latitude ----

tpl <- dat_all |> 
  select(temp_median,
         dailyPAR_median,
         latitude)

pca_tpl <- prcomp(tpl, scale. = TRUE)
# biplot(pca_tpl)
# pca_tpl
# summary(pca_tpl)
# autoplot(pca_tpl,
#          loadings = TRUE,
#          loadings.label = TRUE)

# commented out; will delete when final pca decision is made
# pca_tp <- prcomp(tpl[, 1:2], scale. = TRUE)
# biplot(pca_tp)
# pca_tp
# summary(pca_tp)
# autoplot(pca_tp,
#          loadings = TRUE,
#          loadings.label = TRUE)


# for DO<2, we have fewer stations - do we want to use the same
# PCA score, or generate a new one for only these stations?
# I'm assuming the latter and doing that here
tpl2 <- preds_doLT2 |> 
  select(temp_median,
         dailyPAR_median,
         latitude)

pca_tpl2 <- prcomp(tpl2, scale. = TRUE)

# subset dfs, add PC score ----
dat_all <- dat_all |> 
  mutate(across(c(chla_median,
                  nh4f_median,
                  no23f_median,
                  po4f_median,
                  turb_median),
                function(x) log(x))) |> 
  rename(turb_median.log = turb_median,
         chla_median.log = chla_median,
         nh4_median.log = nh4f_median,
         no23_median.log = no23f_median,
         po4_median.log = po4f_median)
# add PC1 score to dat_all
dat_all$tpl_PC1 <- scores(pca_tpl)[,1]


preds_doLT2 <- preds_doLT2 |> 
  mutate(across(c(chla_median,
                  nh4f_median,
                  no23f_median,
                  po4f_median,
                  turb_median),
                function(x) log(x))) |> 
  rename(turb_median.log = turb_median,
         chla_median.log = chla_median,
         nh4_median.log = nh4f_median,
         no23_median.log = no23f_median,
         po4_median.log = po4f_median)
# add PC1 score to preds_doLT2
preds_doLT2$tpl_PC1 <- scores(pca_tpl2)[,1]
