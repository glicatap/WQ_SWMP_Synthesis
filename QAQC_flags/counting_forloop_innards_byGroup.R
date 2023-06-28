# Actions depend on params$dataType

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
  
  # cat("QAQC flags/codes defined as 'useable' were:\n", 
  #     paste(sort(unique(wq_long$f[which(wq_long$keepStatus == "keep")])), collapse = "  |  "),
  #     "\n\n")
  # cat("QAQC flags/codes defined as 'bad' were:\n", 
  #     paste(sort(unique(wq_long$f[which(wq_long$keepStatus == "discard")])), collapse = "  |  "),
  #     "\n\n")
  
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
  
  
  # plot
  pwq <- ggplot(monts, aes(x = YearMonth, y = badDays,
                           fill = as.factor(Year),
                           text = paste0(YearMonthText, 
                                         ":\n n = ", badDays,
                                         "\n flags/codes present: ", badFlags))) +
    geom_col() +
    facet_grid(stn~param) +
    theme_bw() +
    scale_x_date(
      NULL,
      breaks = scales::breaks_width("5 years"), 
      labels = scales::label_date("'%y")
    ) + 
    scale_y_continuous(breaks = scales::breaks_width(15)) +
    labs(# title = paste(stnwq),
      subtitle = "Days in month with no useable data points",
      y = "# days with no readings passing qa/qc",
      x = "") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = rel(0.9)),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.9)))
  
  
  
  
  ggplotly(pwq, tooltip = "text",
           width = 625, height = 625) %>%  
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

  # below not working:
    # htmltools::tags$details(
    #   htmltools::tags$summary("Click for information on flags/codes for this group"),
    #   "Just kidding! Trying to work something in. Glad your clicking finger got some exercise."
    # )


  # cat("QAQC flags/codes defined as useable were:", "\n", 
  #         paste(sort(unique(nut_long$f[which(nut_long$keepStatus == "keep")])), collapse = "  |  "),
  #         "\n\n\n\n", "QAQC flags/codes defined as bad were:", "\n", 
  #         paste(sort(unique(nut_long$f[which(nut_long$keepStatus == "discard")])), collapse = "  |  "),
  #         "\n\n")  

  
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
                                    .default = 1))
  
  
  # plot
  pnut <- ggplot(monts_nut, aes(x = YearMonth, y = useable,
                                fill = as.factor(Year),
                                text = paste0(YearMonthText, ":\n n = ", useable))) +
    geom_col() +
    facet_grid(stn~param) +
    theme_bw() +
    scale_x_date(
      NULL,
      breaks = scales::breaks_width("5 years"), 
      labels = scales::label_date("'%y")
    ) + 
    scale_y_continuous(breaks = scales::breaks_width(2)) +
    labs(
      subtitle = "Useable NUT data points per month",
      y = "number samples passing qa/qc",
      x = "") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = rel(0.9)),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.9))) 
  
  
  pnut2 <- ggplot(monts_nut, aes(x = YearMonth, y = problemMonth,
                                 fill = as.factor(Year),
                                 text = paste0(YearMonthText, 
                                               "\nunique flags:\n", flags))) +
    geom_col() +
    facet_grid(stn~param) +
    theme_bw() +
    scale_x_date(
      NULL,
      breaks = scales::breaks_width("5 years"), 
      labels = scales::label_date("'%y")
    ) + 
    scale_y_continuous(breaks = c(0, 1)) +
    labs(subtitle = "Months with no useable NUT data points",
         y = "vertical line: no sample passed qa/qc in month",
         x = "") +
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = rel(0.9)),
          axis.title.y = element_text(size = rel(0.9)))
  
  
  ggplotly(pnut, tooltip = "text",
           width = 650, height = 625) %>% 
    htmltools::tagList() %>%
    print()
  
  ggplotly(pnut2, tooltip = "text",
           width = 650, height = 625) %>%  
    htmltools::tagList() %>%
    print()
  
  # clean up
  rm(nut, nut_long, monts_nut,
     pnut, pnut2)
  
}


