#' this script was originally written in Dec. 2023 to determine which
#' transformation of precip would work best in the gams
#'
#' re-visiting in June 2024 because natural-log would yield more interpretable
#' results than square-root
#' so even though square-root worked best, i want to make sure it's
#' "better enough" than log to be worth the loss of interpretability
#' ugh, I think it is. I don't like the QQ plot or histogram of residuals
#' for log as much as for sqrt. response vs. fitted looks better for sqrt too.
#' 
#' looks like I had been working off of data frames from another script;
#' adding enough to make this one standalone


library(tidyverse)
library(mgcv)
library(gratia)

source(here::here("helper_files", "definitions.R"))
source(here::here("helper_files", "functions.R"))
load(here::here("Data", "QAQCd_monthly_byType", "SWMP_monthlyMET.RData"))

stns_met_d10
# sqrt worked best for all the ones I've checked:
# acebp, apaeb, cbmjb, gndcr, niwol

stns <- paste0(stns_met_d10, "met")
i = 16

dat <- subset_df("wq", met, stns[i], "totprcp_total")

dat2 <- dat
dat2$value <- log(dat$value + 1)

dat3 <- dat
dat3$value <- sqrt(dat$value)

hist(dat$value, breaks = 30)
hist(dat2$value, breaks = 30)
hist(dat3$value, breaks = 30)

bam_reg <- run_bam_wq(dat, k= -1)
bam_log <- run_bam_wq(dat2, k = -1)
bam_sqrt <- run_bam_wq(dat3, k = -1)

appraise(bam_reg$dat_bam)
appraise(bam_log$dat_bam)
appraise(bam_sqrt$dat_bam)



