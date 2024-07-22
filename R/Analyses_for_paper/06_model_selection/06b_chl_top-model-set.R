library(tidyverse)
library(lme4)
library(MuMIn)

load(here::here("Outputs",
                "06_model_selection",
                "R_objects",
                "chla_out_lme4.RData"))

test <- get.models(chla_subsets[1:10000], subset = TRUE)
test2 <- model.sel(test)


# so code below can be reused to investigate DO models, generalize and clean up:
mod_subsets <-  chla_subsets



# reusable code
sum(mod_subsets$delta<2, na.rm = TRUE)
sum(mod_subsets$delta<4, na.rm = TRUE)
sum(mod_subsets$delta<6, na.rm = TRUE)

mod_subsets$cumuwt <- cumsum(mod_subsets$weight)

as.data.frame(mod_subsets) |> 
  mutate(rownumber = 1:nrow(mod_subsets)) |> 
  select(rownumber, delta, weight, cumuwt) |> 
  filter(cumuwt >= 0.95) |> 
  head()

# 31,166 models to get to cumulative weight of 0.95

subs2 <- data.frame(mod_subsets)
plot(subs2$delta[1:31000],
     main = "Delta AICc curve, chla",
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
