library(tidyverse)
library(doParallel)
library(tictoc)
library(vegan)
library(MuMIn)


# global model ----

# need to either re-run or load the global model (from 05a_chl)
# this script is for generating and saving all subsets of the model
# re-running because copying and pasting is easier....
source(here::here("R", "Analyses_for_paper",
                  "05_predictive_modeling",
                  "050_setup_MDL.R"))

# pick the specific predictors we've agreed on
# center and scale all but response 
dat_domgl <- dat_all3 |> 
  select(reserve,
         domgl_trend,
         # lat/temp/do PCA
         tld_PC1,
         # wq medians
         spcond_median, turb_median.log, 
         # wq trends
         temp_trend, spcond_trend, turb_trend,
         # nut medians
         chla_median.log, nh4_median.log, no23_median.log, po4_median.log,
         # nut trends
         chla_trend, nh4f_mdl_trend, no23f_mdl_trend, po4f_mdl_trend,
         # met
         precp_median, precp_trend) |> 
  mutate(across(!reserve,
                function(x) as.vector(scale(x))))

formula_fixed_domgl <- paste0("domgl_trend ~ ", paste(names(dat_domgl[3:ncol(dat_domgl)]), collapse = " + "))


mod_domgl <- lm(as.formula(formula_fixed_domgl),
                data = dat_domgl)

# run models ----
# establish cluster
cl <- makeCluster(10)  
registerDoParallel(cl)

options(na.action = "na.fail")

tic("run models")
domgl_subsets <- MuMIn::dredge(mod_domgl, eval = TRUE,
                               cluster = cl)
toc()
beepr::beep(8)


# turn off cluster
stopCluster(cl)


# save subsets ----
save(dat_domgl, mod_domgl, domgl_subsets, 
     file = here::here("Outputs",
                       #"06_model_selection",
                     #  "R_objects",
                       "domgl_out_v3_mdl.RData"),
     compress = "xz")

