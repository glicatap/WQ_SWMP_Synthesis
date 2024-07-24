# most important predictors of chla trend ----
# partial effect is the effect of this predictor when all other
# predictors are held at their mean
# because predictors were centered and standardized before modeling,
# back-transformations are performed in the file 060_predicting_functions.R

# sourcing 06c_chl_model-avgd-outputs.R averages models with delta < 4


# source the averaged model and functions
source(here::here("R",
                  "Analyses_for_paper",
                  "06_model_selection",
                  "06c_chl_model-avgd-outputs.R"))
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
  labs(title = "Standardized coefficients in averaged model for Chla trend",
       x = "Coefficient",
       y = "Term",
       col = "variable importance")



# use the functions on key predictors
# going from top to bottom of the standardized coefficients graph


# po4f trend ----
po4trend_on_chl <- make_predictions(data = dat_chl,
                                    predictor = "po4f_trend",
                                    avgd_mod = modavg_all,
                                    means = dat_means,
                                    sds = dat_sds,
                                    response.is.log.trend = TRUE,
                                    predictor.is.log.trend = TRUE)

graph_predictions(po4trend_on_chl,
                  response.is.log.trend = TRUE,
                  predictor.is.log.trend = TRUE) +
  labs(title = "Partial effect of PO4 trend on Chl a trend",
       x = "PO4 trend (% per year)",
       y = "Expected Chl a trend (% per year)")


# turb trend ----
# note, my functions by default go from -3 to +3 sds for the predictor
# turb trend was skewed though; goes from -5 to 2 sds in the data frame
# so this graph isn't the greatest - we really want that x-axis to extend more
# to the left - may be worth updating the function
# this also isn't showing us the full range of chl predictions.... hm
turbtrend_on_chl <- make_predictions(data = dat_chl,
                                    predictor = "turb_trend",
                                    avgd_mod = modavg_all,
                                    means = dat_means,
                                    sds = dat_sds,
                                    response.is.log.trend = TRUE,
                                    predictor.is.log.trend = FALSE)

graph_predictions(turbtrend_on_chl,
                  response.is.log.trend = TRUE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of Turbidity trend on Chl a trend",
       x = "Turbidity trend (NTU per year)",
       y = "Expected Chl a trend (% per year)")


# nh4f trend ----
nh4trend_on_chl <- make_predictions(data = dat_chl,
                                   predictor = "nh4f_trend",
                                   avgd_mod = modavg_all,
                                   means = dat_means,
                                   sds = dat_sds,
                                   response.is.log.trend = TRUE,
                                   predictor.is.log.trend = TRUE)

graph_predictions(nh4trend_on_chl,
                  response.is.log.trend = TRUE,
                  predictor.is.log.trend = TRUE) +
  labs(title = "Partial effect of NH4 trend on Chl a trend",
       x = "NH4 trend (% per year)",
       y = "Expected Chl a trend (% per year)")


# spcond trend ----
spctrend_on_chl <- make_predictions(data = dat_chl,
                                   predictor = "spcond_trend",
                                   avgd_mod = modavg_all,
                                   means = dat_means,
                                   sds = dat_sds,
                                   response.is.log.trend = TRUE,
                                   predictor.is.log.trend = FALSE)

graph_predictions(spctrend_on_chl,
                  response.is.log.trend = TRUE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of SpCond trend on Chl a trend",
       x = "Specific Conductance trend (mS/cm per year)",
       y = "Expected Chl a trend (% per year)")


# chla median ----

chlmedian_on_chl <- make_predictions(data = dat_chl,
                                    predictor = "chla_median.log",
                                    avgd_mod = modavg_all,
                                    means = dat_means,
                                    sds = dat_sds,
                                    response.is.log.trend = TRUE,
                                    predictor.is.log.trend = FALSE) 

graph_predictions(chlmedian_on_chl,
                  response.is.log.trend = TRUE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of log(chla median) on Chl a trend",
       x = "log(Chla long-term median (ug/L))",
       y = "Expected Chl a trend (% per year)")

# back-calculated to actual median
# shape is a little weird because median chl was log-transformed prior to modeling
chlmedian_on_chl <- make_predictions(data = dat_chl,
                                    predictor = "chla_median.log",
                                    avgd_mod = modavg_all,
                                    means = dat_means,
                                    sds = dat_sds,
                                    response.is.log.trend = TRUE,
                                    predictor.is.log.trend = FALSE) |> 
  mutate(predictor.natural = exp(predictor.natural))

graph_predictions(chlmedian_on_chl,
                  response.is.log.trend = TRUE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of log(chla median) on Chl a trend",
       x = "Chla long-term median (ug/L)",
       y = "Expected Chl a trend (% per year)")




# latitudinal PCA ----
# this one is going to take some extra work to pull out temp, DO, latitude


# precp_trend ----
# this one is weird because precip was square-root transformed
# for trend calculation. am squaring below but first,
# the square-root units
precp_on_chl <- make_predictions(data = dat_chl,
                                    predictor = "precp_trend",
                                    avgd_mod = modavg_all,
                                    means = dat_means,
                                    sds = dat_sds,
                                    response.is.log.trend = TRUE,
                                    predictor.is.log.trend = FALSE)

graph_predictions(precp_on_chl,
                  response.is.log.trend = TRUE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of Precipitation trend on Chl a trend",
       x = "Trend in square-root of precipitation (per year)",
       y = "Expected Chl a trend (% per year)")

# now square it to get 'real' units
precp_on_chl <- precp_on_chl |> 
  mutate(predictor.natural = case_when(predictor.natural < 0 ~ -1 * (predictor.natural^2),
                                       .default = predictor.natural ^ 2))  # because the predictor trend was sqrt(precp)

graph_predictions(precp_on_chl,
                  response.is.log.trend = TRUE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of Precipitation trend on Chl a trend",
       x = "Precipitation trend (mm per year)",
       y = "Expected Chl a trend (% per year)")





