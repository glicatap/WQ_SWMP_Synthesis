# QAQC ----

keep_onlyQAQC <- function(data){
    # data is SWMPr data frame
    
    # to test: 
    # make sure datetimestamp, historical, provisionalplus are the only columns that don't start with f_
    # make sure at least one f_ column appears
    
    # to improve:
    # make historical and provisionalplus go away if they're not in the original data frame
    # make the function not work if there aren't any qaqc columns in the data
    
    
    to_keep_qaqc <- c("datetimestamp", "historical", "provisionalplus",
                      grep("^f_", names(data), value = TRUE))
    data[ , names(data) %in% to_keep_qaqc]
}

# vector of wq data
qaqc_wq <- function(data, f_data){
    # note these need to be provided as, e.g., wq$do_mgl, wq$f_do_mgl
    # qaqcKeep_wq_complete and qaqcKeep_wq_startsWith are defined in 'definitions.R'
    tmp_df <- data.frame(data, f_data) %>% 
        mutate(data = case_when(str_detect(f_data, paste(qaqcKeep_wq_complete, collapse = "|")) ~ data,
                                str_starts(f_data, paste(qaqcKeep_wq_startsWith, collapse = "|")) ~ data,
                                .default = NA_real_))
    tmp_df$data
}


# vector of nut data
qaqc_nut <- function(data, f_data){
    # note these need to be provided as, e.g., nut$chla_n, nut$f_chla_n
    # qaqcKeep_nut_complete is defined in 'definitions.R'
    tmp_df <- data.frame(data, f_data) %>% 
        mutate(data = case_when(f_data %in% qaqcKeep_nut_complete ~ data,
                                .default = NA_real_),
               censored = case_when(is.na(data) ~ NA_integer_,
                                    f_data %in% cens_flagscodes ~ 1L,
                                    .default = 0L)) %>% 
        select(-f_data)
    tmp_df
}

# entire data frame
qaqc_df <- function(df, param_vec, type){
    out_df <- list()
    if(type == "wq"){
        for(i in seq_along(param_vec)){
            tmp2 <- data.frame(qaqc_wq(unlist(df[which(names(df) == param_vec[i])]),
                                       unlist(df[which(names(df) == paste0("f_", param_vec[i]))])))
            names(tmp2) <- param_vec[i]
            out_df[[i]] <- tmp2
        }
    } else if(type == "nut") {
        for(i in seq_along(param_vec)){
            tmp2 <- qaqc_nut(unlist(df[which(names(df) == param_vec[i])]),
                             unlist(df[which(names(df) == paste0("f_", param_vec[i]))]))
            names(tmp2) <- c(param_vec[i], paste0(param_vec[i], "_cens"))
            rownames(tmp2) <- NULL
            out_df[[i]] <- tmp2
        }
    }
    out_df <- dplyr::bind_cols(out_df)
    # names(out_df) <- param_vec
    orig_cols <- df[, which(!(names(df) %in% c(param_vec, paste0("f_", param_vec))))]
    cbind(orig_cols, out_df)
}


extract_flag <- function(col){
    # col is a vector of QAQC flags, e.g. a column in a data frame
    flags <- stringr::str_extract(col, "<-?\\d>")
    return(flags)
}


replace_missing_flags <- function(col){
    # col is a vector of QAQC flags, e.g. a column in a data frame
    col[which(col == "" | is.na(col))] <- "<-2>"
    return(col)
}


is_outsideMDL <- function(data, side = "below"){
    # data is a value or vector of QA/QC flags/codes from an F_ column
    # side is a string, can be either "below" or "above
    # returns true/false for whether a point (or each point in a vector) is below/above detection
    
    if(side == "below"){
        return((str_starts(data, "<4>") & str_detect(data, "B")) | str_starts(data, "<-4> \\[SBL\\]"))
    }
    
    if(side == "above"){
        return((str_starts(data, "<4>") & str_detect(data, "A")) | str_starts(data, "<-5>"))
    }
    
}


# Aggregation ----

# make lists of stats we want,
# and summarize(across(....)) to run it:
# across all the variables we want

# summ_df <- test_df %>% 
#   group_by(site, condition) %>% 
#   summarize(across(c(var1, var2), 
#                    summ_stats))


# for some reason, using na.rm = TRUE causes a sum of NAs to return 0
# this stackoverflow answer provides a way around that
# https://stackoverflow.com/a/67335144
# modSum <- function(x){
#   if(all(is.na(x))){return(NA)}
#   sum(x, na.rm = TRUE)
# }

# I've modified it to work with any function

modFunn <- function(x, FUN){
    if(all(is.na(x))){return(NA)}
    eval(FUN(x, na.rm = TRUE))
}

# summary stats ----

summary_stats <- list(
    nValid = ~sum(!is.na(.x)),
    min = ~modFunn(.x, min),
    median = ~modFunn(.x, median),
    max = ~modFunn(.x, max),
    mean = ~modFunn(.x, mean),
    sd = ~modFunn(.x, sd),
    iqr = ~modFunn(.x, IQR)
)

summary_sums <- list(
    nValid = ~sum(!is.na(.x)),
    total = ~modFunn(.x, sum)
)

# for nutrient data, need to know if it's censored
# in _qc files, there is a _cens column, with 0 for uncensored, 1 for censored, and NA if empty
# when averaging values, if any are censored, then the average should be considered censored

cens_fun <- function(x, ...){
    if(all(is.na(x))){return(NA)}
    sumx <- sum(x, na.rm = TRUE)
    y <- case_when(sumx == 0 ~ 0L,
                   sumx > 0 ~ 1L)
    y
}


# Plots ----

plot_mdl_time <- function(data, param, data_full = NULL){
    # data = data frame of MDL values
    # data_full = data of non-censored values; does not have to be provided
    # param should be the same in both data frames
    
    if(!is.null(data_full)){
        p <- ggplot() +
            geom_point(data = filter({{data_full}}, param == {{param}}),
                       aes(x = datetimestamp,
                           y = val),
                       col = "gray80",
                       alpha = 0.6,
                       na.rm = TRUE) +
            geom_point(data = filter({{data}}, param == {{param}}),
                       aes(x = datetimestamp,
                           y = val),
                       na.rm = TRUE) +
            labs(subtitle = "MDL points overlaid on non-censored data")
    } else {
        p <- ggplot() +
            geom_point(data = filter({{data}}, param == {{param}}),
                       aes(x = datetimestamp,
                           y = val),
                       na.rm = TRUE)
    }
    p +
        facet_wrap(~reserve, ncol = 5) +
        labs(title = as.character(param),
             y = "mg/L",
             x = "Date") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45,
                                         hjust = 1,
                                         vjust = 1,
                                         size = rel(0.9)))
}



plot_mdl_dens <- function(data, param, xlim = c(0, 0.5), ...){
    # this works well on 'nut_long'
    # data is an unquoted data frame; param is a character string
    ggplot(filter({{data}}, param == {{param}})) +
        geom_density(aes(x = val,
                         y = after_stat(scaled),
                         fill = belowMDL),
                     alpha = 0.5,
                     na.rm = TRUE) +
        coord_cartesian(xlim = xlim) +
        facet_wrap(~reserve, ncol = 5) +
        labs(title = as.character(param),
             y = "scaled density",
             x = "value") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45,
                                         hjust = 1,
                                         vjust = 1,
                                         size = rel(0.9)))
}

plot_mdl_boxes <- function(data, param, ylim = c(0, 1), ...){
    ggplot(data = filter({{data}}, param == {{param}})) +
        geom_boxplot(aes(x = belowMDL,
                         y = val,
                         fill = belowMDL),
                     alpha = 0.7,
                     na.rm = TRUE) +
        coord_cartesian(ylim = ylim) +
        facet_wrap(~reserve, ncol = 5) +
        labs(title = as.character(param),
             y = "value (mg/L)",
             x = "below detection?") +
        theme_bw() +
        theme(legend.position = "none")
}


plot_mdl_weave <- function(data, param, ...){
    # uses geom_weave from ggdist
    # won't let me give a title
    ggplot(filter({{data}}, param == {{param}})) +
        geom_weave(aes(y = val),
                   col = "navyblue") +
        facet_wrap(~reserve, ncol = 5) +
        labs(y = paste(param, "  MDL (mg/L)")) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45,
                                         hjust = 1,
                                         vjust = 1,
                                         size = rel(0.9)))
}


plot_mdl_dotinterval <- function(data, param, ylim = c(0, 1), ...){
    ggplot(data = filter({{data}}, param == {{param}})) +
        stat_dotsinterval(aes(x = belowMDL,
                              y = val,
                              col = belowMDL)) +
        coord_cartesian(ylim = ylim) +
        facet_wrap(~reserve, ncol = 5) +
        labs(title = as.character(param),
             y = "value (mg/L)",
             x = "below detection?") +
        theme_bw() +
        theme(legend.position = "none")
}

plot_nuts <- function(dat){
    # this is very specific to a current script
    # in which a single nutrient from a single station has been
    # pulled out of a data frame, using subset_df() function
    # 
    p1 <- ggplot(dat, aes(x = dec_date, y = value, col = factor(cens))) +
        geom_line(col = "gray") +
        geom_point() +
        theme_bw() +
        labs(title = plt_titles,
             x = "",
             y = "value",
             col = "below detection")
    
    p2 <- p1 + 
        scale_y_log10() +
        labs(title = paste0(plt_titles, ", log-scaled axis"),
             x = "Date")
    
    p1 / p2 +
        plot_layout(guides = "collect") &
        theme(legend.position='bottom')
}

plot_wq <- function(dat){
    # this is very specific to a current script
    # in which a single wq param from a single station has been
    # pulled out of a data frame, using subset_df() function
    # 
    p1 <- ggplot(dat, aes(x = dec_date, y = value, col = year)) +
        geom_line(col = "gray") +
        geom_point() +
        theme_bw() +
        labs(title = plt_titles,
             x = "Date",
             y = "value",
             col = "Year")
    print(p1)
}


# Analysis Helpers ----

# subset data frame to single station and parameter, and set up for GAM
subset_df <- function(type, main_df, station, parameter){
    # type, station, parameter should be passed as character strings
    # main_df should not
    # example: cbmip_po4f <- subset_df("nut", nut, "cbmip", "po4f")
    
    # wq/met - just grab stn, year, month, and parameter
    if(type %in% c("wq", "met")){
        sub_df <- main_df |> 
            filter(station == {{station}}) |> 
            select(station, year, month,
                   "value" = all_of({{parameter}}))
    }
    # nut - grab stn, year, month, parameter, and censored column
    # then log-transform nutrient, and create a censored column for lognut 
    # as of 5/29/2024 this is natural-log, not log10  
    # will work with mgcv's gam functions
    if(type == "nut"){sub_df <- main_df |> 
            filter(station == {{station}}) |> 
            select(station, year, month,
                   "value" = all_of({{parameter}})) |> 
            mutate(lognut = log(value))
        }
    
    # for all: make sure all year-month combinations are represented,
    # even if blank
    # and create a decimal year column
    # also add which parameter we're looking at
    all_yearmonths <- expand.grid(year = min(sub_df$year):max(sub_df$year), 
                                  month = 1:12) |> 
        arrange(year, month) |> 
        mutate(station = station)
    sub_df <- left_join(all_yearmonths, sub_df) |> 
        mutate(dec_date = decimal_date(ymd(paste(year, month, "15", sep = "-"))),
               ARrestart = case_when(is.na(lag(value)) & !is.na(value) ~ TRUE,
                                     .default = FALSE),
               focal_param = parameter) |> 
        select(station, year, month, dec_date, focal_param, everything())
    # remove first and last months if necessary
    sub_df <- sub_df[min(which(!is.na(sub_df$value))):nrow(sub_df), ]
    sub_df <- sub_df[1:max(which(!is.na(sub_df$value))), ]
    
    return(sub_df)
}

run_bam_nut <- function(data, k){
    # input is a data frame and number of knots
    # returns a list with the bam object and information about
    # whether the model needed to be re-fit for autocorrelation
    
    dat <- data
    
    # run bam with an almost-0 rho, using AR.start
    # (in case missing/unevenly spaced data messed up the true ACF)
    # then get the lag-1 acf estimate
    # to use in what will be the "real" bam
    dat_bam <- bam(lognut ~ dec_date + s(month, bs = "cc", k = k),
                   family = gaussian(),
                   discrete = TRUE,
                   AR.start = ARrestart,
                   rho = 0.0001,
                   data = dat,
                   method = "fREML")
    # summary(dat_bam)
    # acf(dat_bam$std.rsd, plot = FALSE)[1]
    
    rhos <- acf(dat_bam$std.rsd, plot = FALSE)
    use_this_rho <- round(rhos$acf[2], 4)  # 2nd position is lag 1
    rho_threshold <- qnorm((1 + 0.95)/2)/sqrt(rhos$n.used)
    model_refit <- FALSE
    
    if(abs(use_this_rho) > rho_threshold){
        model_refit <- TRUE
        dat_bam <- bam(lognut ~ dec_date + s(month, bs = "cc", k = k),
                       family = gaussian(),
                       discrete = TRUE,
                       AR.start = ARrestart,
                       rho = 0.0001,
                       data = dat,
                       method = "fREML")
    }
    
    final_AR <- acf(dat_bam$std.rsd, plot = FALSE)
    final_AR <- round(final_AR$acf[2], 4)  # 2nd position is lag 1
    
    rho_info <- data.frame("ar1_start" = use_this_rho,
                           "threshold" = rho_threshold,
                           "model_refit" = model_refit,
                           "ar1_end" = final_AR)
    
    bam_out <- list(dat_bam = dat_bam, 
                    rho_info = rho_info)
    return(bam_out)
    
}


run_bam_wq <- function(data, k){
    # input is a data frame and number of knots
    # returns a list with the bam object and information about
    # whether the model needed to be re-fit for autocorrelation
    
    dat <- data
    
    # run bam with an almost-0 rho, using AR.start
    # (in case missing/unevenly spaced data messed up the true ACF)
    # then get the lag-1 acf estimate
    # to use in what will be the "real" bam
    dat_bam <- bam(value ~ dec_date + s(month, bs = "cc", k = k),
                   family = gaussian(),
                   discrete = TRUE,
                   AR.start = ARrestart,
                   rho = 0.0001,
                   data = dat,
                   method = "fREML")
    # summary(dat_bam)
    # acf(dat_bam$std.rsd, plot = FALSE)[1]
    
    rhos <- acf(dat_bam$std.rsd, plot = FALSE)
    use_this_rho <- round(rhos$acf[2], 4)  # 2nd position is lag 1
    rho_threshold <- qnorm((1 + 0.95)/2)/sqrt(rhos$n.used)
    model_refit <- FALSE
    
    if(abs(use_this_rho) > rho_threshold){
        model_refit <- TRUE
        dat_bam <- bam(value ~ dec_date + s(month, bs = "cc", k = k),
                       family = gaussian(),
                       discrete = TRUE,
                       AR.start = ARrestart,
                       rho = use_this_rho,
                       data = dat,
                       method = "fREML")
    }
    
    final_AR <- acf(dat_bam$std.rsd, plot = FALSE)
    final_AR <- round(final_AR$acf[2], 4)  # 2nd position is lag 1
    
    rho_info <- data.frame("ar1_start" = use_this_rho,
                           "threshold" = rho_threshold,
                           "model_refit" = model_refit,
                           "ar1_end" = final_AR)
    
    bam_out <- list(dat_bam = dat_bam, 
                    rho_info = rho_info)
    return(bam_out)
    
}


run_bam_wqBeta <- function(data, k){
    # input is a data frame and number of knots
    # this function uses the beta distribution in the bam
    # returns a list with the bam object and information about
    # whether the model needed to be re-fit for autocorrelation
    
    # up to 2976 readings in a month so the most extreme proportions 
    # would be 1/2976. Set eps to 1/10th of that.
    # This is used to adjust exact 0s and 1s.
    
    dat <- data
    
    # run bam with an almost-0 rho, using AR.start
    # (in case missing/unevenly spaced data messed up the true ACF)
    # then get the lag-1 acf estimate
    # to use in what will be the "real" bam
    dat_bam <- bam(value ~ dec_date + s(month, bs = "cc", k = k),
                   family = betar(eps = 1/29760),
                   discrete = TRUE,
                   AR.start = ARrestart,
                   rho = 0.0001,
                   data = dat,
                   method = "fREML")
    # summary(dat_bam)
    # acf(dat_bam$std.rsd, plot = FALSE)[1]
    
    rhos <- acf(dat_bam$std.rsd, plot = FALSE)
    use_this_rho <- round(rhos$acf[2], 4)  # 2nd position is lag 1
    rho_threshold <- qnorm((1 + 0.95)/2)/sqrt(rhos$n.used)
    model_refit <- FALSE
    
    if(abs(use_this_rho) > rho_threshold){
        model_refit <- TRUE
        dat_bam <- bam(value ~ dec_date + s(month, bs = "cc", k = k),
                       family = betar(eps = 1/29760),
                       discrete = TRUE,
                       AR.start = ARrestart,
                       rho = use_this_rho,
                       data = dat,
                       method = "fREML")
    }
    
    final_AR <- acf(dat_bam$std.rsd, plot = FALSE)
    final_AR <- round(final_AR$acf[2], 4)  # 2nd position is lag 1
    
    rho_info <- data.frame("ar1_start" = use_this_rho,
                           "threshold" = rho_threshold,
                           "model_refit" = model_refit,
                           "ar1_end" = final_AR)
    
    bam_out <- list(dat_bam = dat_bam, 
                    rho_info = rho_info)
    return(bam_out)
    
}

# argh, this doesn't work right because of how I'm trying to use outputs
run_bam_x <- function(which_bam){
    # attempt to run the bam with 12 knots
    # if it doesn't work, pick a better number based on the data
    # if that still doesn't work, spit out the model error
    
    if(inherits(try(which_bam(tmp, k = 12), silent = TRUE), "try-error")){
        # pick our own k:
        knew <- tmp |> 
            summarize(.by = month,
                      nVals = sum(!is.na(value))) |> 
            filter(nVals >= 1) |> 
            nrow()
        
        if(inherits(try(which_bam(tmp, k = knew), silent = TRUE), "try-error")){
            # if that didn't work, make a blank row in the output:
            out <- data.frame(station = stn,
                              parameter = param,
                              model_error = TRUE) 
            
            return(out)
            # if it did work:
        } else {
            bam_out <- which_bam(tmp, k = knew)
            return(bam_out)
        }
        
    } else {
        # run model
        bam_out <- which_bam(tmp, k = 12)
        return(bam_out)
    }
    
}




tidy_bam_output <- function(bam_obj){
    # bam_obj is the output from the above functions,
    # a list with bam_output itself and information about the rhos
    
    bam_out <- bam_obj
    
    # split it up for further tidying
    rho_info <- bam_out$rho_info  
    bam_out <- bam_out$dat_bam
    
    bam_tidy <- gtsummary::tidy_gam(bam_out, conf.int = TRUE) |> 
        select(-parametric) |> 
        mutate(station = stn,
               parameter = param,
               term = case_match(term,
                                 "(Intercept)" ~ "Intercept",
                                 "dec_date" ~ "Trend (/yr)",
                                 "s(month)" ~ "Seasonality")) |> 
        select(station, parameter, everything())
    
    bam_trend <- bam_tidy |> 
        filter(term == "Trend (/yr)") |> 
        select(station,
               parameter,
               "Slope" = estimate,
               std.error,
               conf.low,
               conf.high,
               statistic,
               p.value)
    
    bam_intercept <- bam_tidy |> 
        filter(term == "Intercept") |> 
        select(station,
               parameter,
               "Intercept" = estimate)
    
    bam_seas <- bam_tidy |> 
        filter(term == "Seasonality") |> 
        select(station,
               parameter,
               "Seas_edf" = edf,
               "Seas_ref.df" = ref.df,
               "Seas_stat" = statistic,
               "Seas_p.val" = p.value)
    
    bam_r2 <- data.frame(
        station = stn,
        parameter = param,
        R2_adj = summary(bam_out)$r.sq,
        Dev_expl = summary(bam_out)$dev.expl
    )
    
    out <- dplyr::left_join(bam_trend, bam_intercept) |> 
        dplyr::left_join(bam_r2) |> 
        mutate(model_error = FALSE)
    
    out <- cbind(out, rho_info)
    
}




print_bam_table <- function(bam.obj){
    tidy_gam(bam.obj,
             conf.int = TRUE) |> 
        filter(term != "(Intercept)") |> 
        select(term, estimate, std.error, conf.low, conf.high,
               statistic, p.value, edf, ref.df) |> 
        gt() |> 
        fmt_number(c("estimate", "std.error", "p.value"),
                   decimals = 3) |> 
        fmt_number(c("conf.low", "conf.high"),
                   decimals = 3) |> 
        fmt_number(c("statistic", "edf"), 
                   decimals = 1) |> 
        sub_missing(missing_text = "--")
}


annualize <- function(data, param){
    # get annual min, max, mean, and months of min and max
    # list columns are included in output because there may be
    # more than one month in any given year where the monthly value
    # equals the min or max  
    
    # removes any years where there weren't at least 6 readings
    data |> 
        summarise(.by = c(station, year),
                  annual_mean = mean({{param}}, na.rm = TRUE),
                  annual_amplitude = max({{param}}, na.rm = TRUE) - min({{param}}, na.rm = TRUE),
                  an_normalized_amplitude = annual_amplitude / annual_mean,
                  annual_n = sum(!is.na({{param}})),
                  annual_max = max({{param}}, na.rm = TRUE),
                  peak_month = list(month[which({{param}} == max({{param}}, na.rm = TRUE))]),
                  annual_min = min({{param}}, na.rm = TRUE),
                  floor_month = list(month[which({{param}} == min({{param}}, na.rm = TRUE))])) |> 
        filter(!is.na(annual_mean),
               annual_n >= 6)
}


timing_calcs <- function(data){
    ofInterest <- data |> 
        unnest(col = key_month) |> 
        select(station, year, key_month)
    
    stn_medians <- ofInterest |> 
        group_by(station, year) |> 
        mutate(nKeyMonths = n(),
               weighted = key_month / nKeyMonths) |>
        ungroup() |> 
        summarize(.by = c(station, year),
                  mean_keyMonth_for_year = mean(weighted)) |> 
        summarize(.by = station,
                  median_month = median(mean_keyMonth_for_year))
    
    weighted_months <- data |> 
        unnest(cols = key_month) |> 
        summarize(.by = c(station, year),
                  year_factor = length(unique(key_month))) |> 
        right_join(data) |> 
        mutate(month_weight_within_year = 1/year_factor) |> 
        unnest(cols = key_month)
    
    n_yrs <- data |> 
        summarize(.by = station,
                  nyears = length(unique(year)))
    
    stn_distn <- weighted_months |> 
        summarize(.by = c(station, key_month),
                  n_yrs_with_this_key = n(),
                  sum_weights = sum(month_weight_within_year)) |> 
        left_join(n_yrs, by = "station") |> 
        mutate(prob_of_keyMonth = sum_weights/nyears) |> 
        arrange(station, desc(prob_of_keyMonth)) |> 
        relocate(prob_of_keyMonth, .after = key_month)
    
    most_likely_month <- stn_distn |> 
        summarize(.by = station,
                  most_likely_months = list(key_month[which(prob_of_keyMonth == max(prob_of_keyMonth))])) |> 
        unnest(cols = most_likely_months) |> 
        left_join(stn_medians, by = "station")
    
    return(list("ofInterest" = ofInterest, 
                "stn_medians" = stn_medians,
                "weighted_months" = weighted_months, 
                "stn_distn" = stn_distn, 
                "most_likely_month" = most_likely_month))
}


time_ampl_plots <- function(wrangled_object, annualized_object, param, type){
    # requires patchwork to be loaded; prints side-by-side plots
    p1 <- wrangled_object$most_likely_month |> 
        arrange(median_month) |> 
        ggplot() +
        geom_point(aes(x = median_month,
                       y = forcats::fct_inorder(station)),
                   alpha = 0) +
        geom_point(data = wrangled_object$ofInterest,
                   aes(x = key_month, y = station),
                   shape = 1,
                   col = "navy",
                   alpha = 0.6) +
        # geom_point(data = wrangled_object$most_likely_month,
        #            aes(x = most_likely_months, y = station),
        #            # shape = 1,
        #            col = "darkorange",
        #            alpha = 0.6) +
        geom_point(aes(x = median_month,
                       y = forcats::fct_inorder(station)),
                   col = "red3"
        ) +
        theme(axis.text.y = element_text(size = rel(0.6)),
              plot.caption = element_text(hjust = 0)) +
        labs(title = param,
             x = paste("Timing of", type),
             y = "station, ordered by median")
    
    p2 <- annualized_object |> 
        select(station, year, an_normalized_amplitude) |> 
        group_by(station) |> 
        mutate(median_amplitude = median(an_normalized_amplitude)) |> 
        arrange(median_amplitude) |> 
        ggplot() +
        geom_point(aes(x = an_normalized_amplitude, y = forcats::fct_inorder(station)),
                   col = "navy",
                   shape = 1,
                   alpha = 0.5) +
        geom_point(aes(x = median_amplitude, y = station),
                   col = "red3") +
        theme(axis.text.y = element_text(size = rel(0.6)),
              plot.caption = element_text(hjust = 0)) +
        labs(title = param,
             x = "Normalized amplitude",
             y = "station, ordered by median")
    
    print(p1 + p2)
}

# NOT WORKING
time_ampl_plots_Region <- function(wrangled_object, annualized_object, param, type){
    # requires patchwork to be loaded; prints side-by-side plots
    p1 <- wrangled_object$most_likely_month |> 
        arrange(median_month) |> 
        mutate(reserve = stringr::str_sub(station, start = 1, end = 3)) |> 
        left_join(region_dictionary, by = c("reserve" = "Reserve"))
    ggplot() +
        geom_point(aes(x = median_month,
                       y = forcats::fct_inorder(station)),
                   alpha = 0) +
        geom_point(data = wrangled_object$ofInterest,
                   aes(x = key_month, y = station),
                   shape = 1,
                   col = "navy",
                   alpha = 0.6) +
        # geom_point(data = wrangled_object$most_likely_month,
        #            aes(x = most_likely_months, y = station),
        #            # shape = 1,
        #            col = "darkorange",
        #            alpha = 0.6) +
        geom_point(aes(x = median_month,
                       y = forcats::fct_inorder(station),
                       col = Region)) +
        theme(axis.text.y = element_text(size = rel(0.6)),
              plot.caption = element_text(hjust = 0)) +
        labs(title = param,
             x = paste("Timing of", type),
             y = "station, ordered by median")
    
    p2 <- annualized_object |> 
        select(station, year, an_normalized_amplitude) |> 
        group_by(station) |> 
        mutate(median_amplitude = median(an_normalized_amplitude)) |> 
        arrange(median_amplitude) |> 
        ggplot() +
        geom_point(aes(x = an_normalized_amplitude, y = forcats::fct_inorder(station)),
                   col = "navy",
                   shape = 1,
                   alpha = 0.5) +
        geom_point(aes(x = median_amplitude, y = station, col = Region)) +
        theme(axis.text.y = element_text(size = rel(0.6)),
              plot.caption = element_text(hjust = 0)) +
        labs(title = param,
             x = "Normalized amplitude",
             y = "station, ordered by median")
    
    print(p1 + p2)
}