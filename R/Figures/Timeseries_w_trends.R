library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(mgcv)
library(gratia)
library(gt)
library(gtsummary)


source(here::here("helper_files", "definitions.R"))
source(here::here("helper_files", "functions.R"))

load(here::here("Data", "QAQCd_monthly_byType", "SWMP_monthlyWQ.RData"))
load(here::here("Data", "QAQCd_monthly_byType", "SWMP_monthlyNUT.RData"))
load(here::here("Data", "QAQCd_monthly_byType", "SWMP_monthlyMET.RData"))


trends<- read.csv(here::here("Outputs", 
                             "04_compiled_predictors",
                             "trend_intrcpt.csv"))

trends_met<- read.csv(here::here("Outputs", 
                             "04_compiled_predictors",
                             "trend_intrcpt_met.csv"))


dat_wq <- wq |> 
    filter(station %in% paste0(stns_wq_nut_d10, "wq")) |> 
    mutate(do_proportion_below2 = round(doLessThan2_total / doLessThan2_nValid, 4),
           do_proportion_below5 = round(doLessThan5_total / doLessThan5_nValid, 4)) |> 
    select(station, year, month, 
           do_pct_median, do_mgl_median,
           temp_median, spcond_median, 
           sal_median, turb_median,
           do_proportion_below2,
           do_proportion_below5)

# may need to treat DO proportions differently from others - not gaussian, but beta

dat_met <- met |> 
    mutate(station = case_when(station == "sostcmet" ~ "soscmmet",
                               .default = station)) |> 
    filter(station %in% c(paste0(stns_met_d10, "met"),
                          "lkspomet",
                          "soscmmet")) |> 
    select(station, year, month, 
           atemp_median,
           totprcp_total,
           dailyPAR_median) |> 
    mutate(sqrt_precp_total = sqrt(totprcp_total))  # SQUARE-ROOT TRANSFORM


# need to deal with HUD NO23 situation before removing NO3 from data frame
nut <- nut |> 
    mutate(no23b = case_when(str_starts(station, "hud") ~ no3f,
                             .default = no23f),
           no23_cens_b = case_when(str_starts(station, "hud") ~ no3f_cens,
                                   .default = no23f_cens)) |> 
    select(-no23f, -no23f_cens) |> 
    rename(no23f = no23b,
           no23f_cens = no23_cens_b)

# now filter and select df
dat_nut <- nut |> 
    filter(station %in% paste0(stns_wq_nut_d10, "nut")) |> 
    select(station, year, month, 
           chla_n, po4f,
           nh4f, no23f,
           chla_n_cens, po4f_cens,
           nh4f_cens, no23f_cens) |> 
    mutate(DIN_to_DIP = (nh4f + no23f) / po4f,
           DIN_to_DIP_cens = case_when(nh4f_cens + no23f_cens + po4f_cens == 0 ~ 0,
                                       .default = 1),
           DIN_to_DIP_censNum = case_when(nh4f_cens + no23f_cens > 0 ~ 1,
                                          .default = 0),
           DIN_to_DIP_censDenom = case_when(po4f_cens == 1 ~ 1,
                                            .default = 0),
           DIN_to_DIP_censBoth = case_when(DIN_to_DIP_censNum + DIN_to_DIP_censDenom == 2 ~ 1,
                                           .default = 0))

# DIN_to_DIP may also need to be treated in a non-gaussian way


# Preprocess and merge data
dat_met <- dat_met %>%
    mutate(reserve = substr(station, 1, 3)) %>% 
    mutate(date = ym(paste(year, month, sep = "-")))

dat_nut <- dat_nut %>%
    select(station, chla_n, po4f, nh4f, no23f, DIN_to_DIP,year, month) %>%
    mutate(reserve = substr(station, 1, 3)) %>% 
    mutate(date = ym(paste(dat_nut$year, dat_nut$month, sep = "-")))

dat_wq <- dat_wq %>%
    mutate(reserve = substr(station, 1, 3))%>% 
    mutate(date = ym(paste(year, month, sep = "-")))

ggplot()+
    geom_point(data=dat_met,aes(x=date,y=atemp_median,group=station,color=reserve))+
    facet_wrap(~reserve,scales="free")


met_annual <- dat_met %>%
    group_by(station, year,reserve) %>%
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

wq_annual <- dat_wq %>%
    group_by(station, year,reserve) %>%
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

nut_annual <- dat_nut %>%
    group_by(station, year,reserve) %>%
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

##WQ plots

ggplot()+
    geom_point(data=wq_annual,aes(x=year,y=temp_median,group=station,color=reserve))+
    geom_line(data=wq_annual,aes(x=year,y=temp_median,group=station,color=reserve))+
    geom_abline(data = trends, aes(intercept = temp_intcpt, slope = temp_trend,
                                             group=station,color=reserve), linetype = "dashed",
                linewdith=2) +
    facet_wrap(~reserve,scales="free")+theme(legend.position = "none")+
    ggtitle("Water temp")

ggplot()+
    geom_point(data=wq_annual,aes(x=year,y=do_mgl_median,group=station,color=reserve))+
    geom_line(data=wq_annual,aes(x=year,y=do_mgl_median,group=station,color=reserve))+
    geom_abline(data = trends, aes(intercept = domgl_intcpt, slope = domgl_trend,
                                   group=station,color=reserve), linetype = "dashed",
                linewdith=2) +
    facet_wrap(~reserve,scales="free")+
    ggtitle("DO_mgl")

ggplot()+
    geom_point(data=wq_annual,aes(x=year,y=spcond_median,group=station,color=reserve))+
    geom_line(data=wq_annual,aes(x=year,y=spcond_median,group=station,color=reserve))+
    geom_abline(data = trends, aes(intercept = spcond_intcpt, slope = spcond_trend,
                                   group=station,color=reserve), linetype = "dashed",
                linewdith=2) +
    facet_wrap(~reserve,scales="free")+
    ggtitle("spcond")

ggplot()+
    geom_point(data=wq_annual,aes(x=year,y=turb_median,group=station,color=reserve))+
    geom_line(data=wq_annual,aes(x=year,y=turb_median,group=station,color=reserve))+
    geom_abline(data = trends, aes(intercept = turb_intcpt, slope = turb_trend,
                                   group=station,color=reserve), linetype = "dashed",
                linewdith=2) +
    facet_wrap(~reserve,scales="free")+
    ggtitle("turb")


##NUT plots

ggplot()+
    geom_point(data=nut_annual,aes(x=year,y=log(chla_n),group=station,color=reserve))+
    geom_line(data=nut_annual,aes(x=year,y=log(chla_n),group=station,color=reserve))+
    geom_abline(data = trends, aes(intercept = chla_intcpt, slope = chla_trend,
                                   group=station,color=reserve), linetype = "dashed",
                linewdith=2) +
    facet_wrap(~reserve,scales="free")+
    ggtitle("log(chla)")

ggplot()+
    geom_point(data=nut_annual,aes(x=year,y=log(po4f),group=station,color=reserve))+
    geom_line(data=nut_annual,aes(x=year,y=log(po4f),group=station,color=reserve))+
    geom_abline(data = trends, aes(intercept = po4f_intcpt, slope = po4f_trend,
                                   group=station,color=reserve), linetype = "dashed",
                linewdith=2) +
    facet_wrap(~reserve,scales="free")+
    ggtitle("log(po4f)")


ggplot()+
    geom_point(data=nut_annual,aes(x=year,y=log(nh4f),group=station,color=reserve))+
    geom_line(data=nut_annual,aes(x=year,y=log(nh4f),group=station,color=reserve))+
    geom_abline(data = trends, aes(intercept = nh4f_intcpt, slope = nh4f_trend,
                                   group=station,color=reserve), linetype = "dashed",
                linewdith=2) +
    facet_wrap(~reserve,scales="free")+
    ggtitle("log(nh4f)")

ggplot()+
    geom_point(data=nut_annual,aes(x=year,y=log(no23f),group=station,color=reserve))+
    geom_line(data=nut_annual,aes(x=year,y=log(no23f),group=station,color=reserve))+
    geom_abline(data = trends, aes(intercept = no23f_intcpt, slope = no23f_trend,
                                   group=station,color=reserve), linetype = "dashed",
                linewdith=2) +
    facet_wrap(~reserve,scales="free")+
    ggtitle("log(no23f)")

##MET plots


ggplot()+
    geom_point(data=met_annual,aes(x=year,y=sqrt_precp_total,group=station,color=reserve))+
    geom_line(data=met_annual,aes(x=year,y=sqrt_precp_total,group=station,color=reserve))+
    geom_abline(data = trends_met, aes(intercept = precp_intcpt, slope = precp_trend,
                                   group=station,color=reserve), linetype = "dashed",
                linewdith=2) +
    facet_wrap(~reserve,scales="free")+
    ggtitle("sqrtprcp")



ggplot()+
    geom_point(data=met_annual,aes(x=year,y=dailyPAR_median,group=station,color=reserve))+
    geom_line(data=met_annual,aes(x=year,y=dailyPAR_median,group=station,color=reserve))+
    geom_abline(data = trends_met, aes(intercept = dailyPAR_intcpt, slope = dailyPAR_trend,
                                       group=station,color=reserve), linetype = "dashed",
                linewdith=2) +
    facet_wrap(~reserve,scales="free")+
    ggtitle("PAR")


