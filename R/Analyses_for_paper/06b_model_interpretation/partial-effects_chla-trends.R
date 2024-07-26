# most important predictors of chla trend ----
# partial effect is the effect of this predictor when all other
# predictors are held at their mean
# because predictors were centered and standardized before modeling,
# back-transformations are performed in the file 060_predicting_functions.R

# the default range for predictions is -3 to +3 standard deviations of the predictor
# this is modified when the range in the data exceeded those boundaries

library(dplyr)
library(ggplot2)
library(nlme)
library(MuMIn)

# get needed data frames and source functions
load(here::here("Outputs",
                "06_model_selection",
                "R_objects",
                "chla_post-averaging.RData"))
source(here::here("R",
                  "Analyses_for_paper",
                  "06_model_selection",
                  "060_predicting_functions.R"))


# standardized coefficients plot ----
ggplot(coeffs_stnd) +
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
                                    avgd_mod = mod_avgd4,
                                    means = dat_means,
                                    sds = dat_sds,
                                    sd.range = c(-3.7, 3.4),
                                    response.is.log.trend = TRUE,
                                    predictor.is.log.trend = TRUE)

graph_predictions(po4trend_on_chl,
                  response.is.log.trend = TRUE,
                  predictor.is.log.trend = TRUE) +
  labs(title = "Partial effect of PO4 trend on Chl a trend",
       x = "PO4 trend (% per year)",
       y = "Expected Chl a trend (% per year)")


# turb trend ----
turbtrend_on_chl <- make_predictions(data = dat_chl,
                                    predictor = "turb_trend",
                                    avgd_mod = mod_avgd4,
                                    means = dat_means,
                                    sds = dat_sds,
                                    sd.range = c(-5, 2),
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
                                   avgd_mod = mod_avgd4,
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
                                   avgd_mod = mod_avgd4,
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
                                    avgd_mod = mod_avgd4,
                                    means = dat_means,
                                    sds = dat_sds,
                                    sd.range = c(-3.2, 2),
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
chlmedian_on_chl <- chlmedian_on_chl |> 
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
                                 avgd_mod = mod_avgd4,
                                 means = dat_means,
                                 sds = dat_sds,
                                 sd.range = c(-3.4, 2.5),
                                 response.is.log.trend = TRUE,
                                 predictor.is.log.trend = FALSE)

graph_predictions(precp_on_chl,
                  response.is.log.trend = TRUE,
                  predictor.is.log.trend = FALSE) +
  labs(title = "Partial effect of Precipitation trend on Chl a trend",
       x = "Trend in square-root of precipitation (per year)",
       y = "Expected Chl a trend (% per year)")






