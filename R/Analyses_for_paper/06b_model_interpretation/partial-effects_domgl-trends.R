# most important predictors of do mg/L trend ----
# partial effect is the effect of this predictor when all other
# predictors are held at their mean
# because predictors were centered and standardized before modeling,
# back-transformations are performed in the file 060_predicting_functions.R

# sourcing 06b-c_domgl.R averages models with delta < 4


# source the averaged model and functions
source(here::here("R",
                  "Analyses_for_paper",
                  "06_model_selection",
                  "06b-c_domgl.R"))
source(here::here("R",
                  "Analyses_for_paper",
                  "06_model_selection",
                  "060_predicting_functions.R"))


# standardized coefficients plot ----
ggplot(coeffs3) +
  geom_pointrange(aes(y = term,
                      x = Estimate,
                      xmin = ci_low,
                      xmax = ci_high,
                      col = sw_all)) +
  khroma::scale_color_batlow(reverse = TRUE) +
  geom_vline(xintercept = 0,
             col = "gray40") +
  labs(title = "Standardized coefficients in averaged model for DO mgl trend",
       x = "Coefficient",
       y = "Term",
       col = "variable importance")



# use the functions on key predictors
# going from top to bottom of the standardized coefficients graph


# daily PAR trend ----

par_on_do <- make_predictions(data = dat_domgl,
                              predictor = "dailyPAR_trend",
                              avgd_mod = modavg_all,
                              means = dat_means,
                              sds = dat_sds,
                              response.is.log.trend = FALSE,
                              predictor.is.log.trend = FALSE)

graph_predictions(par_on_do,
                  response.is.log.trend = FALSE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of PAR trend on DO mg/L trend",
       x = "Daily PAR trend (mmol/m2/day per year)",
       y = "Expected DO mg/L trend (mg/L per year)")


# temp trend ----

temp_on_do <- make_predictions(data = dat_domgl,
                               predictor = "temp_trend",
                               avgd_mod = modavg_all,
                               means = dat_means,
                               sds = dat_sds,
                               response.is.log.trend = FALSE,
                               predictor.is.log.trend = FALSE)

graph_predictions(temp_on_do,
                  response.is.log.trend = FALSE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of water temp trend on DO mg/L trend",
       x = "Water temperature trend (degC per year)",
       y = "Expected DO mg/L trend (mg/L per year)")


# chla trend ----

chltrnd_on_do <- make_predictions(data = dat_domgl,
                                  predictor = "chla_trend",
                                  avgd_mod = modavg_all,
                                  means = dat_means,
                                  sds = dat_sds,
                                  response.is.log.trend = FALSE,
                                  predictor.is.log.trend = TRUE)

graph_predictions(chltrnd_on_do,
                  response.is.log.trend = FALSE,
                  predictor.is.log.trend = TRUE) +
  labs(title = "Partial effect of chla trend on DO mg/L trend",
       x = "Chla trend (% per year)",
       y = "Expected DO mg/L trend (mg/L per year)")


# spcond median ----

spcond_on_do <- make_predictions(data = dat_domgl,
                                 predictor = "spcond_median",
                                 avgd_mod = modavg_all,
                                 means = dat_means,
                                 sds = dat_sds,
                                 response.is.log.trend = FALSE,
                                 predictor.is.log.trend = FALSE)

graph_predictions(spcond_on_do,
                  response.is.log.trend = FALSE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of median SpCond on DO mg/L trend",
       x = "Specific Conductance long-term median (mS/cm)",
       y = "Expected DO mg/L trend (mg/L per year)")


# spcond trend ----

spctrend_on_do <- make_predictions(data = dat_domgl,
                                   predictor = "spcond_trend",
                                   avgd_mod = modavg_all,
                                   means = dat_means,
                                   sds = dat_sds,
                                   response.is.log.trend = FALSE,
                                   predictor.is.log.trend = FALSE)

graph_predictions(spctrend_on_do,
                  response.is.log.trend = FALSE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of SpCond trend on DO mg/L trend",
       x = "Specific Conductance trend (mS/cm per year)",
       y = "Expected DO mg/L trend (mg/L per year)")


# chla median ----

chlmedian_on_do <- make_predictions(data = dat_domgl,
                                  predictor = "chla_median.log",
                                  avgd_mod = modavg_all,
                                  means = dat_means,
                                  sds = dat_sds,
                                  response.is.log.trend = FALSE,
                                  predictor.is.log.trend = FALSE) 

graph_predictions(chlmedian_on_do,
                  response.is.log.trend = FALSE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of log(chla median) on DO mg/L trend",
       x = "log(Chla long-term median (ug/L))",
       y = "Expected DO mg/L trend (mg/L per year)")

# back-calculated to actual median
# shape is a little weird because median chl was log-transformed prior to modeling
chlmedian_on_do <- make_predictions(data = dat_domgl,
                                    predictor = "chla_median.log",
                                    avgd_mod = modavg_all,
                                    means = dat_means,
                                    sds = dat_sds,
                                    response.is.log.trend = FALSE,
                                    predictor.is.log.trend = FALSE) |> 
  mutate(predictor.natural = exp(predictor.natural))

graph_predictions(chlmedian_on_do,
                  response.is.log.trend = FALSE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of chla median on DO mg/L trend",
       x = "Chla long-term median (ug/L)",
       y = "Expected DO mg/L trend (mg/L per year)")


# nh4f trend ----

nh4trend_on_do <- make_predictions(data = dat_domgl,
                                   predictor = "nh4f_trend",
                                   avgd_mod = modavg_all,
                                   means = dat_means,
                                   sds = dat_sds,
                                   response.is.log.trend = FALSE,
                                   predictor.is.log.trend = TRUE)

graph_predictions(nh4trend_on_do,
                  response.is.log.trend = FALSE,
                  predictor.is.log.trend = TRUE) +
  labs(title = "Partial effect of NH4 trend on DO mg/L trend",
       x = "NH4 trend (% per year)",
       y = "Expected DO mg/L trend (mg/L per year)")
