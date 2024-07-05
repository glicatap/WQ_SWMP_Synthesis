library(tidyverse)
library(glmmTMB)
library(MuMIn)

load(here::here("Outputs",
                "06_model_selection",
                "R_objects",
                "chla_out_plusDelta5.RData"))
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


predict_po4trend <- newdata |> 
  mutate(po4f_trend = newdata.sds)

# have to fit the models inside model.avg in order to predict
modavg_all <- model.avg(test, fit = TRUE)

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
