library(tidyr)
library(dplyr)
#Remove LKS sites
# import predictor data frames ----

dat_all <- read.csv(here::here("Outputs", 
                               "04_compiled_predictors", 
                               "compiled_predictors.csv"))


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


# adding domgl_median to the pca ----
tpld <- dat_all |> 
  select(temp_median,
         dailyPAR_median,
         latitude,
         domgl_median)

pca_tpld <- prcomp(tpld, scale. = TRUE)
# biplot(pca_tpld)
# pca_tpld
# summary(pca_tpld)
# autoplot(pca_tpld,
#          shape = FALSE,
#          label = TRUE,
#          loadings = TRUE,
#          loadings.label = TRUE)


# removing PAR median from PCA
# because PAR trends were removed from model 8/6/24
# and we think medians are fine to leave in
# but want to demonstrate 
tld <- dat_all |> 
  select(temp_median,
         latitude,
         domgl_median)

pca_tld <- prcomp(tld, scale. = TRUE)
# biplot(pca_tld)
# pca_tld
# summary(pca_tld)
# autoplot(pca_tld,
#          shape = FALSE,
#          label = TRUE,
#          loadings = TRUE,
#          loadings.label = TRUE)



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
#Kait change to repdict from base R - scores was not working
dat_all$tpl_PC1 <- predict(pca_tpl)[,1]
dat_all$tpld_PC1 <- predict(pca_tpld)[,1]
dat_all$tld_PC1 <- predict(pca_tld)[,1]

# PC1 from versions with/without PAR are ideally 1:1
# let's check
# ggplot(dat_all,
#        aes(x = tpld_PC1,
#            y = tld_PC1)) +
#     geom_text(aes(label = station)) +
#     geom_abline(slope = 1, intercept = 0,
#                 linetype = "dashed")
# cor(dat_all$tpld_PC1, dat_all$tld_PC1, method = "spearman")

#####################


stations_to_remove <- c("lksbl", "lksol", "lksba", "lkspo")

#, "marsc", "marcw"

# Filter out the rows with the specified station names
dat_all3 <- dat_all %>%
  filter(!station %in% stations_to_remove)
