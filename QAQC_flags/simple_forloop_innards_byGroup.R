# Actions depend on params$dataType


# for plot labels; thank you to https://stackoverflow.com/a/52920047
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


# wq data ----
if(params$dataType == "wq"){
  
  # read and combine 
  wq <- purrr::map(stnwq, function(x) get(load(paste0(path, "/", x, ".RData")))) %>% 
    purrr::set_names(stn_vec) %>% 
    purrr::list_rbind(names_to = "stn") %>% 
    select(stn, datetimestamp, any_of(wq_keep)) %>% 
    rename(any_of(wq_parms)) %>% 
    rename(val_doPct = val_do_pct,
           val_domgl = val_do_mgl,
           f_doPct = f_do_pct,
           f_domgl = f_do_mgl)
  
  # pivot longer and extract flags
  specwq <- wq %>% 
    build_longer_spec(
      cols = (starts_with("val_")[1]):ncol(wq),
      names_to = c(".value", "param"),
      names_sep = "_")
  wq_long <- wq %>% 
    pivot_longer_spec(spec = specwq) %>% 
    mutate(Year = lubridate::year(datetimestamp),
           Month = lubridate::month(datetimestamp),
           Day = lubridate::mday(datetimestamp),
           flag = extract_flag(f),
           keepStatus = case_when(flag %in% flags_keep ~ "keep",
                                  grepl(flagCodes_keepvec, f) ~ "keep",
                                  .default = "discard"))
  
  # summarize flags
  dails <- wq_long %>% 
    group_by(stn, param, Year, Month, Day) %>% 
    summarize(keptDataPoints = sum(keepStatus == "keep"))
  
  monts <- dails %>% 
    group_by(stn, param, Year, Month) %>% 
    summarize(useableDays = sum(keptDataPoints > 0),
              badDays = sum(keptDataPoints == 0)) %>% 
    mutate(YearMonth = as.Date(paste(Year, Month, "01", sep = "-")),
           YearMonthText = str_sub(as.character(YearMonth), end = -4))
  
  mont_flags <- wq_long %>% 
    group_by(stn, param, Year, Month, keepStatus) %>% 
    summarize(flags = paste(sort(unique(f)), collapse = "; ")) %>% 
    pivot_wider(names_from = keepStatus,
                values_from = flags)
  
  monts$badFlags <- mont_flags$discard
  monts$goodFlags <- mont_flags$keep
  monts$MonthAbbrev <- factor(month.abb[monts$Month], levels = month.abb)
  
  monts_simpler <- monts %>%
    group_by(stn, param, MonthAbbrev) %>% 
    summarize(useableDays = sum(useableDays),
              badDays = sum(badDays)) %>% 
    pivot_longer(c(useableDays, badDays),
                 names_to = "dayType",
                 values_to = "count") %>% 
    mutate(dayType = case_match(dayType,
                                "useableDays" ~ "useable",
                                "badDays" ~ "discarded")) 
  
  
  yrs_simpler <- monts %>%
    group_by(stn, param, Year) %>% 
    summarize(useableDays = sum(useableDays),
              badDays = sum(badDays)) %>% 
    pivot_longer(c(useableDays, badDays),
                 names_to = "dayType",
                 values_to = "count") %>% 
    mutate(dayType = case_match(dayType,
                                "useableDays" ~ "useable",
                                "badDays" ~ "discarded"))
  
  
  # plot wq red/gray ----
  # by month
  pwq <-  
    ggplot(monts_simpler,
           aes(x = MonthAbbrev, y = count,
               fill = dayType,
               text = paste0(MonthAbbrev, 
                             ":\n", dayType, " n = ", count
               ))) +
    geom_col() +
    facet_grid(stn~param) +
    theme_bw() +
    labs(title = "Overall by Month",
         y = "# days, across all years combined",
         x = "",
         fill = "Data type: ") +
    scale_fill_manual(values = c("red3", "gray")) +
    scale_x_discrete(breaks = every_nth(n = 2)) +
    theme(legend.position = "top",
          legend.justification = "left",
          legend.title = element_text(size = rel(1)), 
          legend.text  = element_text(size = rel(1)),
          legend.key.size = unit(0.8, "lines"),
          axis.text.x = element_text(size = rel(0.9),
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.9)))
  
  ggplotly(pwq, tooltip = "text",
           width = 625, height = 675) %>%  
    layout(legend = list(orientation = "h",
                         y = 1.11, x = 0.45)) %>% 
    htmltools::tagList() %>%
    print()
  
  
  # wq by month, discarded ----
  pwq3 <- ggplot(monts, aes(x = MonthAbbrev, y = badDays,
                            fill = Year,
                            text = paste0(YearMonthText, 
                                          ":\n n = ", badDays,
                                          "\n flags/codes present: ", badFlags))) +
    geom_col() +
    facet_grid(stn~param) +
    theme_bw() +
    labs(y = "# days with no readings passing qa/qc",
         x = "") +
    scale_x_discrete(breaks = every_nth(n = 2)) +
    scale_fill_distiller(palette = "YlGnBu") +
    theme(legend.position = "top",
          legend.justification = "left",
          legend.title = element_text(size = rel(1)), 
          legend.text  = element_text(size = rel(0.8)),
          legend.key.size = unit(1.5, "lines"),
          axis.text.x = element_text(size = rel(0.9),
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.9)))
  
  ggplotly(pwq3, tooltip = "text",
           width = 625, height = 675) %>%  
    layout(legend = list(x=1.2)) %>% 
    htmltools::tagList() %>%
    print()
  
  # wq year red/gray ----
  pwqyrs <-  
    ggplot(yrs_simpler,
           aes(x = as.factor(Year), y = count,
               fill = dayType,
               text = paste0(Year, 
                             ":\n", dayType, " n = ", count
               ))) +
    geom_col() +
    facet_grid(stn~param) +
    theme_bw() +
    labs(y = "# days per year",
         x = "",
         fill = "Data type: ") +
    scale_fill_manual(values = c("red3", "gray")) +
    scale_x_discrete(breaks = every_nth(n = 4)) +
    theme(legend.position = "top",
          legend.justification = "left",
          legend.title = element_text(size = rel(1)), 
          legend.text  = element_text(size = rel(1)),
          legend.key.size = unit(0.8, "lines"),
          axis.text.x = element_text(size = rel(0.9),
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.9)))
  
  
  ggplotly(pwqyrs, tooltip = "text",
           width = 650, height = 650) %>%  
    layout(legend = list(orientation = "h",
                         y = 1.13)) %>% 
    htmltools::tagList() %>%
    print()
  
  
  # plot wq by year, fill by month ----
  pwq4 <- ggplot(monts, aes(x = as.factor(Year), y = badDays,
                            fill = MonthAbbrev,
                            text = paste0(YearMonthText, 
                                          ":\n n = ", badDays,
                                          "\n flags/codes present: ", badFlags))) +
    geom_col() +
    facet_grid(stn~param) +
    theme_bw() +
    labs(title = "Discarded data - tall bars are problem years",
         y = "# days with no readings passing qa/qc",
         x = "",
         fill = "Month") +
    scale_x_discrete(breaks = every_nth(n = 4)) +
    theme(legend.position = "top",
          legend.justification = "left",
          legend.title = element_text(size = rel(1)), 
          legend.text  = element_text(size = rel(0.8)),
          legend.key.size = unit(0.8, "lines"),
          axis.text.x = element_text(size = rel(0.9),
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.9)))
  
  
  ggplotly(pwq4, tooltip = "text",
           width = 650, height = 650) %>% 
    layout(legend = list(x = 1.05)) %>% 
    htmltools::tagList() %>%
    print()
  
  
  # clean up
  rm(wq, wq_long, dails, monts,
     pwq)
  
}


# nut data ----
if(params$dataType == "nut"){
  
  # read and combine 
  nut <- purrr::map(stnnut, function(x) get(load(paste0(path, "/", x, ".RData")))) %>% 
    purrr::set_names(stn_vec) %>% 
    purrr::list_rbind(names_to = "stn") %>% 
    select(stn, datetimestamp, any_of(nut_keep)) %>% 
    rename(any_of(nut_parms)) %>% 
    rename(val_chlaN = val_chla_n,
           f_chlaN = f_chla_n) 
  
  # pivot longer and extract flags
  specnut <- nut %>% 
    build_longer_spec(
      cols = (starts_with("val_")[1]):ncol(nut),
      names_to = c(".value", "param"),
      names_sep = "_")
  nut_long <- nut %>% 
    pivot_longer_spec(spec = specnut) %>% 
    mutate(Year = lubridate::year(datetimestamp),
           Month = lubridate::month(datetimestamp),
           Day = lubridate::mday(datetimestamp),
           flag = extract_flag(f),
           keepStatus = case_when(flag %in% flags_keep ~ "keep",
                                  grepl(flagCodes_keepvec, f) ~ "keep",
                                  .default = "discard"))

  # summarize flags
  monts_nut <- nut_long %>% 
    group_by(stn, param, Year, Month) %>% 
    summarize(useable = sum(keepStatus == "keep"),
              flags = paste(unique(f), collapse = "; ")) %>% 
    mutate(YearMonth = as.Date(paste(Year, Month, "01", sep = "-")),
           YearMonthText = str_sub(as.character(YearMonth), end = -4),
           useableMonth = case_when(useable > 0 ~ 1,
                                    .default = 0),
           problemMonth = case_when(useable > 0 ~ 0,
                                    .default = 1),
           MonthAbbrev = factor(month.abb[Month], levels = month.abb))
  
  nut_simpler <- monts_nut %>%
    group_by(stn, param, MonthAbbrev) %>% 
    summarize(useableMonths = sum(useableMonth),
              badMonths = sum(problemMonth)) %>% 
    pivot_longer(c(useableMonths, badMonths),
                 names_to = "monthType",
                 values_to = "count") %>% 
    mutate(monthType = case_match(monthType,
                                "useableMonths" ~ "useable",
                                "badMonths" ~ "discarded")) 
  
  
  nut_yrs <- monts_nut %>%
    group_by(stn, param, Year) %>% 
    summarize(useableMonths = sum(useableMonth),
              badMonths = sum(problemMonth)) %>% 
    pivot_longer(c(useableMonths, badMonths),
                 names_to = "monthType",
                 values_to = "count") %>% 
    mutate(monthType = case_match(monthType,
                                  "useableMonths" ~ "useable",
                                  "badMonths" ~ "discarded"))
  
  # plot nut red/gray ----
  pnut <- ggplot(nut_simpler, aes(x = MonthAbbrev, y = count,
                                fill = monthType,
                                text = paste0(MonthAbbrev, ":\n", monthType, 
                                              "n = ", count))) +
    geom_col() +
    facet_grid(stn~param) +
    theme_bw() +
    labs(title = "Overall by Month",
      x = "",
      y = "# yrs with this data status",
      fill = "Data Type: ") +
    scale_fill_manual(values = c("red3", "gray")) +
    scale_x_discrete(breaks = every_nth(n = 2)) +
    theme(legend.position = "top",
          legend.justification = "left",
          legend.title = element_text(size = rel(1)), 
          legend.text  = element_text(size = rel(1)),
          legend.key.size = unit(0.8, "lines"),
          axis.text.x = element_text(size = rel(0.9),
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.9))) 
  
  ggplotly(pnut, tooltip = "text",
           width = 690, height = 650) %>% 
    layout(legend = list(orientation = "h",
                         y = 1.11, x = 0.45)) %>% 
    htmltools::tagList() %>%
    print()
  
  # plot nut, discarded  ----
  pnut2 <- ggplot(monts_nut, aes(x = MonthAbbrev, y = problemMonth,
                                 fill = Year,
                                 text = paste0(YearMonthText, 
                                               "\nunique flags:\n", flags))) +
    geom_col() +
    facet_grid(stn~param) +
    theme_bw() +
    labs(title = "Discarded data - tall bars are problem months",
         y = "# of years where this month was a problem",
         x = "") +
    scale_x_discrete(breaks = every_nth(n = 2)) +
    scale_fill_distiller(palette = "YlGnBu") +
    theme(legend.position = "top",
          legend.justification = "left",
          legend.title = element_text(size = rel(1)), 
          legend.text  = element_text(size = rel(0.8)),
          legend.key.size = unit(1.5, "lines"),
          axis.text.x = element_text(size = rel(0.9),
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.9)))
  
  
  ggplotly(pnut2, tooltip = "text",
           width = 675, height = 650) %>%  
    htmltools::tagList() %>%
    print()
  
  
  # nut year, red/gray ----
  pnutyrs <- ggplot(nut_yrs, aes(x = as.factor(Year), y = count,
                                  fill = monthType,
                                  text = paste0(Year, ":\n", monthType, 
                                                "n = ", count))) +
    geom_col() +
    facet_grid(stn~param) +
    theme_bw() +
    labs(title = "Overall by Year",
         x = "",
         y = "# months in year with each status",
         fill = "Data Type: ") +
    scale_fill_manual(values = c("red3", "gray")) +
    scale_x_discrete(breaks = every_nth(n = 5)) +
    scale_y_continuous(breaks = scales::breaks_width(4)) +
    theme(legend.position = "top",
          legend.justification = "left",
          legend.title = element_text(size = rel(1)), 
          legend.text  = element_text(size = rel(1)),
          legend.key.size = unit(0.8, "lines"),
          axis.text.x = element_text(size = rel(0.8),
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.9))) 
  
  ggplotly(pnutyrs, tooltip = "text",
           width = 675, height = 650) %>% 
    layout(legend = list(orientation = "h",
                         y = 1.11, x = 0.45)) %>% 
    htmltools::tagList() %>%
    print()
  
  
  # nut year, by month ----
  pnut2b <- ggplot(monts_nut, aes(x = as.factor(Year), y = problemMonth,
                                 fill = MonthAbbrev,
                                 text = paste0(YearMonthText, 
                                               "\nunique flags:\n", flags))) +
    geom_col() +
    facet_grid(stn~param) +
    theme_bw() +
    labs(title = "Discarded data - tall bars are problem years",
         y = "# of months with no passing data",
         x = "",
         fill = "Month") +
    scale_x_discrete(breaks = every_nth(n = 5)) +
    scale_y_continuous(breaks = scales::breaks_width(4)) +
    theme(legend.position = "top",
          legend.justification = "left",
          legend.title = element_text(size = rel(1)), 
          legend.text  = element_text(size = rel(0.8)),
          legend.key.size = unit(1.5, "lines"),
          axis.text.x = element_text(size = rel(0.8),
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.9)))
  
  
  ggplotly(pnut2b, tooltip = "text",
           width = 700, height = 650) %>% 
    layout(legend = list(x = 1.05)) %>%
    htmltools::tagList() %>%
    print()
  
  
  
  # clean up
  rm(nut, nut_long, monts_nut,
     pnut, pnut2)
  
}


