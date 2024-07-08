library(tidyverse)
library(glmmTMB)
library(MuMIn)

load(here::here("Outputs",
                "06_model_selection",
                "R_objects",
                "chla_out_plusDelta5.compressed.RData"))
dat_all <- read.csv(here::here("Outputs",
                               "04_compiled_predictors",
                               "compiled_predictors.csv"))


# where is the null model ----
dredge_results <- mod_subsets
null_model_index <- which(rowSums(is.na(dredge_results[, 2:ncol(dredge_results)])) == max(rowSums(is.na(dredge_results[, 2:ncol(dredge_results)]))))
dredge_results[null_model_index, ]

# Null model has delta of 7.91. It's about 7,000 models down.


# means and sds used to scale ----

# start with dat_all; only keep what's in dat_chl and numeric
dat_means <- dat_all |> 
  select(any_of(names(dat_chl)),
         -reserve) |> 
  summarize(across(everything(), mean))
dat_sds <- dat_all |> 
  select(any_of(names(dat_chl)),
         -reserve) |> 
  summarize(across(everything(), sd))


# get top models ----

# include all, and do the nested models thing

test <- mod_subsets[which(mod_subsets$delta < 5),]
test2 <- subset(test, !nested(.))
sw(test)
sw(test2)
model.avg(test)$coefficients
model.avg(test2)$coefficients

# average models ----
modavg_all <- model.avg(test)

swdf <- data.frame(sw_all = sw(test)) |> 
  rownames_to_column("predictor")
swdf2 <- data.frame(sw_nonnested = sw(test2)) |> 
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
  labs(title = "Relative variable importance in models with delta < 5",
       x = "Predictor",
       y = "Sum of Akaike weights")


# plot standardized coefficients ----

coeffs2 <- data.frame(summary(modavg_all)$coefmat.full) |> 
  rownames_to_column("term") |> 
  mutate(ci_low = Estimate - 1.96*Adjusted.SE,
         ci_high = Estimate + 1.96*Adjusted.SE,
         term = str_remove(term, "cond\\("),
         term = str_remove(term, "\\)")) |> 
  left_join(swdf, by = c("term" = "predictor")) |> 
  filter(term != "(Int)") |> 
  arrange(Estimate) |> 
  mutate(term = fct_inorder(term))

ggplot(coeffs2) +
  geom_pointrange(aes(y = term,
                      x = Estimate,
                      xmin = ci_low,
                      xmax = ci_high,
                      col = sw_all)) +
  khroma::scale_color_batlow(reverse = TRUE) +
  geom_vline(xintercept = 0,
             col = "gray40") +
  labs(title = "Standardized slopes in averaged model for chl a trend",
       x = "Slope",
       y = "Term",
       col = "variable importance")


# make predictions ----

newdata.names <- names(dat_chl)[3:ncol(dat_chl)] 
# make a sequence for the predictor
newdata.sds <- seq(-3, 3, by = 0.1)
# make a data frame - start as a matrix with 0s
# a column for every variable; we'll replace what we want as a predictor
# with newdata.sds later
newdata.matrix <- matrix(data = 0,
                         nrow = length(newdata.sds),
                         ncol = 16)
newdata <- data.frame(newdata.matrix)
names(newdata) <- newdata.names


# have to fit the models inside model.avg in order to predict
modavg_all <- model.avg(test, fit = TRUE)



# po4 trend ----

predict_po4trend <- newdata |> 
  mutate(po4f_trend = newdata.sds)

predictions_po4f <- predict(modavg_all,
                            newdata = predict_po4trend,
                            se.fit = TRUE,
                            re.form = NA)

predictions_po4f_df <- data.frame(predictor.sd = predict_po4trend$po4f_trend,
                                  predictor.natural = (predict_po4trend$po4f_trend * dat_sds$po4f_trend) + dat_means$po4f_trend,
                                  predicted = predictions_po4f$fit,
                                  se = predictions_po4f$se) |> 
  mutate(ci_low = predicted - 1.96*se,
         ci_high = predicted + 1.96*se,
         pct_per_year = exp(predicted) * 100 - 100,
         ci_low = exp(ci_low) * 100 - 100,
         ci_high = exp(ci_high) * 100 - 100,
         predictor.pct_per_year = exp(predictor.natural) * 100 - 100)



ggplot(predictions_po4f_df) +
  geom_ribbon(aes(x = predictor.sd,
                  ymin = ci_low,
                  ymax = ci_high),
              fill = "gray",
              alpha = 0.6) +
  geom_line(aes(x = predictor.sd,
                y = pct_per_year),
            col = "blue") +
  theme_bw() +
  labs(title = "Partial effect of PO4 trend on chl trend",
       x = "Standardized PO4 trend (standard deviations different from mean)",
       y = "Change in chl a (%/year)")


ggplot(predictions_po4f_df) +
  geom_ribbon(aes(x = predictor.pct_per_year,
                  ymin = ci_low,
                  ymax = ci_high),
              fill = "gray",
              alpha = 0.6) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             col = "gray20") +
  geom_line(aes(x = predictor.pct_per_year,
                y = pct_per_year),
            linewidth = 1,
            col = "blue") +
  theme_bw() +
  labs(title = "Partial effect of PO4 trend on chl trend",
       x = "PO4 trend (%/yr)",
       y = "Expected Chl a trend (%/year)")

# spcond trend ----

# using this because it has a reasonably sized coefficient but is not log transformed
# so we can see the difference in what it looks like for different params

predict.df <- newdata |> 
  mutate(spcond_trend = newdata.sds)

predictions <- predict(modavg_all,
                            newdata = predict.df,
                            se.fit = TRUE,
                            re.form = NA)

predictions.df <- data.frame(predictor.sd = newdata.sds,
                                  predictor.natural = (newdata.sds * dat_sds$spcond_trend) + dat_means$spcond_trend,
                                  predicted = predictions$fit,
                                  se = predictions$se) |> 
  mutate(ci_low = predicted - 1.96*se,
         ci_high = predicted + 1.96*se,
         pct_per_year = exp(predicted) * 100 - 100,
         ci_low = exp(ci_low) * 100 - 100,
         ci_high = exp(ci_high) * 100 - 100)



ggplot(predictions.df) +
  geom_ribbon(aes(x = predictor.natural,
                  ymin = ci_low,
                  ymax = ci_high),
              fill = "gray",
              alpha = 0.6) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             col = "gray20") +
  geom_line(aes(x = predictor.natural,
                y = pct_per_year),
            linewidth = 1,
            col = "blue") +
  theme_bw() +
  labs(title = "Partial effect of SpCond trend on chl trend",
       x = "Specific Conductance trend (mS/cm/yr)",
       y = "Expected Chl a trend (%/year)")



# temp trend ----

# not a big coefficient, or important, but negative and could be interesting
# especially since most temp trends were so positive

predict.df <- newdata |> 
  mutate(temp_trend = newdata.sds)

predictions <- predict(modavg_all,
                       newdata = predict.df,
                       se.fit = TRUE,
                       re.form = NA)

predictions.df <- data.frame(predictor.sd = newdata.sds,
                             predictor.natural = (newdata.sds * dat_sds$temp_trend) + dat_means$temp_trend,
                             predicted = predictions$fit,
                             se = predictions$se) |> 
  mutate(ci_low = predicted - 1.96*se,
         ci_high = predicted + 1.96*se,
         pct_per_year = exp(predicted) * 100 - 100,
         ci_low = exp(ci_low) * 100 - 100,
         ci_high = exp(ci_high) * 100 - 100)



ggplot(predictions.df) +
  geom_ribbon(aes(x = predictor.natural,
                  ymin = ci_low,
                  ymax = ci_high),
              fill = "gray",
              alpha = 0.6) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             col = "gray20") +
  geom_line(aes(x = predictor.natural,
                y = pct_per_year),
            linewidth = 1,
            col = "blue") +
  theme_bw() +
  labs(title = "Partial effect of temperature trend on chl trend",
       x = "Water temperature trend (degC/yr)",
       y = "Expected Chl a trend (%/year)")


# precip trend ----

# moderate size and importance
# and likely to be non-linear because of square-root transformation for prcp trend
# so could be interesting to try to interpret

predict.df <- newdata |> 
  mutate(precp_trend = newdata.sds)

predictions <- predict(modavg_all,
                       newdata = predict.df,
                       se.fit = TRUE,
                       re.form = NA)

predictions.df <- data.frame(predictor.sd = newdata.sds,
                             predictor.natural = (newdata.sds * dat_sds$precp_trend) + dat_means$precp_trend,
                             predicted = predictions$fit,
                             se = predictions$se) |> 
  mutate(ci_low = predicted - 1.96*se,
         ci_high = predicted + 1.96*se,
         pct_per_year = exp(predicted) * 100 - 100,
         ci_low = exp(ci_low) * 100 - 100,
         ci_high = exp(ci_high) * 100 - 100,
         predictor.natural = case_when(predictor.natural < 0 ~ -1 * (predictor.natural^2),
                                       .default = predictor.natural ^ 2))  # because the predictor trend was sqrt(precp)



ggplot(predictions.df) +
  geom_ribbon(aes(x = predictor.natural,
                  ymin = ci_low,
                  ymax = ci_high),
              fill = "gray",
              alpha = 0.6) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             col = "gray20") +
  geom_line(aes(x = predictor.natural,
                y = pct_per_year),
            linewidth = 1,
            col = "blue") +
  theme_bw() +
  labs(title = "Partial effect of precip trend on chl trend",
       subtitle = "looks weird because trend was calculated on sqrt(prcp) and it has been back-transformed",
       x = "Precip trend (mm/yr)",
       y = "Expected Chl a trend (%/year)")
