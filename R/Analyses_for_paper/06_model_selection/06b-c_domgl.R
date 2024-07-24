library(tidyverse)
library(MuMIn)
library(vegan)

load(here::here("Outputs",
                "06_model_selection",
                "R_objects",
                "domgl_out.RData"))
dat_all <- read.csv(here::here("Outputs",
                               "04_compiled_predictors",
                               "compiled_predictors.csv"))

# get dat_all set up properly, with PCA and log transformations etc.
source(here::here("R", "Analyses_for_paper",
                  "05_predictive_modeling",
                  "050_setup.R"))

mod_subsets <-  domgl_subsets

dat_means <- dat_all |> 
  select(any_of(names(dat_domgl)),
         -reserve) |> 
  summarize(across(everything(), mean))
dat_sds <- dat_all |> 
  select(any_of(names(dat_domgl)),
         -reserve) |> 
  summarize(across(everything(), sd))

# reusable code
sum(mod_subsets$delta<2)
sum(mod_subsets$delta<3)
sum(mod_subsets$delta<4)
sum(mod_subsets$delta<6)

mod_subsets$cumuwt <- cumsum(mod_subsets$weight)

as.data.frame(mod_subsets) |> 
    mutate(rownumber = 1:nrow(mod_subsets)) |> 
    select(rownumber, delta, weight, cumuwt) |> 
    filter(cumuwt >= 0.95) |> 
    head()

# 95% confidence set has 21,256 models
# delta of 13.35

subs2 <- data.frame(mod_subsets)
plot(subs2$delta[1:21000],
     main = "Delta AICc curve, domgl",
     xlab = "model number",
     ylab = "Delta")
abline(h = 2, col = "red3", lty = 2)
abline(h = 4, col = "blue", lty = 2)
abline(h = 6, col = "orange", lty = 2)


plot(subs2$delta,
     main = "Delta AICc curve",
     xlab = "model number",
     ylab = "Delta")
abline(h = 2, col = "red3")
abline(h = 4, col = "orange")
abline(h = 6, col = "orange")
abline(h = 5, col = "orange")

sum(subs2$delta <=4)
sum(subs2$delta <=5)



test <- mod_subsets[which(mod_subsets$delta < 4),]
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
    labs(title = "Relative variable importance, DO mg/L",
         subtitle = "models with delta < 4",
         x = "Predictor",
         y = "Sum of Akaike weights")


# plot standardized coefficients ----

# put in order by variable importance rather than coefficient
coeffs3 <- data.frame(summary(modavg_all)$coefmat.full) |> 
    rownames_to_column("term") |> 
    mutate(ci_low = Estimate - 1.96*Adjusted.SE,
           ci_high = Estimate + 1.96*Adjusted.SE,
           term = str_remove(term, "cond\\("),
           term = str_remove(term, "\\)")) |> 
    left_join(swdf, by = c("term" = "predictor")) |> 
    filter(!str_starts(term, "\\(Int")) |> 
    arrange(sw_all) |> 
    mutate(term = fct_inorder(term))

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


# make predictions ----

newdata.names <- names(dat_doLT2)[3:ncol(dat_doLT2)] 
# make a sequence for the predictor
newdata.sds <- seq(-3, 3, by = 0.1)
# make a data frame - start as a matrix with 0s
# a column for every variable; we'll replace what we want as a predictor
# with newdata.sds later
newdata.matrix <- matrix(data = 0,
                         nrow = length(newdata.sds),
                         ncol = length(newdata.names))
newdata <- data.frame(newdata.matrix)
names(newdata) <- newdata.names


# have to fit the models inside model.avg in order to predict
modavg_all <- model.avg(test, fit = TRUE)


# po4 trend ----

predict_partrend <- newdata |> 
  mutate(dailyPAR_trend = newdata.sds)

predictions_par <- predict(modavg_all,
                           newdata = predict_partrend,
                           se.fit = TRUE,
                           re.form = NA)

predictions_par_df <- data.frame(predictor.sd = predict_partrend$dailyPAR_trend,
                                 predictor.natural = (predict_partrend$dailyPAR_trend * dat_sds$dailyPAR_trend) + dat_means$dailyPAR_trend,
                                 predicted = predictions_par$fit,
                                 se = predictions_par$se) |> 
  mutate(ci_low = predicted - 1.96*se,
         ci_high = predicted + 1.96*se
  )

ggplot(predictions_par_df) +
  geom_ribbon(aes(x = predictor.natural,
                  ymin = ci_low,
                  ymax = ci_high),
              fill = "gray",
              alpha = 0.6) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             col = "gray20") +
  geom_line(aes(x = predictor.natural,
                y = predicted),
            linewidth = 1,
            col = "blue") +
  theme_bw() +
  labs(title = "Partial effect of PAR trend on DO<2 trend",
       x = "PAR trend (change per year)",
       y = "Expected change in DO<2")
