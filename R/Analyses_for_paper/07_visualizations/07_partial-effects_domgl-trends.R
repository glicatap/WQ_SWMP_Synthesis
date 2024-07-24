# predictors on do mg/L trend ----

# need to run most of 06b-c_domgl.R
modavg_all <- model.avg(test, fit = TRUE)

# source the functions
source(here::here("R",
                  "Analyses_for_paper",
                  "06_model_selection",
                  "060_predicting_functions.R"))

# use the functions on key predictors


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
       x = "Specific Conductance (mS/cm)",
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

