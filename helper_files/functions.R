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


summary_stats <- list(
  n_valid = ~sum(!is.na(.x)),
  min = ~modFunn(.x, min),
  median = ~modFunn(.x, median),
  max = ~modFunn(.x, max),
  mean = ~modFunn(.x, mean),
  sd = ~modFunn(.x, sd),
  iqr = ~modFunn(.x, IQR)
)

summary_sums <- list(
  n_valid = ~sum(!is.na(.x)),
  total = ~modFunn(.x, sum)
)


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