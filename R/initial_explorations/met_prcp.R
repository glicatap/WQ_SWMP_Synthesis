stns_met_d10
# sqrt worked best for all the ones I've checked:
# acebp, apaeb, cbmjb, gndcr, niwol

stns <- paste0(stns_met_d10, "met")
i = 16

dat <- subset_df("wq", met, stns[i], "totprcp_total")

dat2 <- dat
dat2$value <- log10(dat$value + 1)

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



