#' stations without a calculated trend in DO<2:
#' lksblwq
#' lksolwq
#' marcwwq
#' pdbgswq
#' wqbmhwq
#' 

library(tidyverse)

load(here::here("Data", "QAQCd_monthly_byType", "SWMP_monthlyWQ.RData"))


dat <- wq |> 
  filter(station %in% c("lksblwq",
                        "lksolwq",
                        "marcwwq",
                        "pdbgswq",
                        "wqbmhwq")) |> 
  select(station, year, month,
         starts_with("do")) |> 
  mutate(propLT2 = doLessThan2_total / doLessThan2_nValid,
         validPropLT2 = case_when(!is.na(propLT2) ~ 1,
                                  .default = 0))

summary(dat$propLT2)

# these stations literally only have 0s
# so there is no trend, no CI, no seasonality ..... nothing
# I'm not sure what to do about that