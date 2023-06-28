# should have stn_vec as the acting thing to iterate on
stnwq <- paste0(stn_vec, "wq")
stnnut <- paste0(stn_vec, "nut")


# Actions depend on params$dataType ----

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
  
  
  # plot
  pwq <- ggplot(monts, aes(x = YearMonth, y = badDays,
                           fill = as.factor(Year),
                           tooltip = paste0(YearMonthText, ":\n n = ", badDays))) +
    geom_col_interactive() +
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
      y = "# days with no readings passing qa/qc") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = rel(0.9)),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.9)))
  
  
  ggiraph::girafe(ggobj = pwq) %>%  
                  # width_svg = 8, height_svg = 6) %>%
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
                                  .default = "discard"))
  
  # summarize flags
  monts_nut <- nut_long %>% 
    group_by(stn, param, Year, Month) %>% 
    summarize(useable = sum(keepStatus == "keep")) %>% 
    mutate(YearMonth = as.Date(paste(Year, Month, "01", sep = "-")),
           YearMonthText = str_sub(as.character(YearMonth), end = -4),
           useableMonth = case_when(useable > 0 ~ 1,
                                    .default = 0),
           problemMonth = case_when(useable > 0 ~ 0,
                                    .default = 1))
  
  
  # plot
  pnut <- ggplot(monts_nut, aes(x = YearMonth, y = useable,
                                fill = as.factor(Year),
                                tooltip = paste0(YearMonthText, ":\n n = ", useable))) +
    geom_col_interactive() +
    facet_grid(stn~param) +
    theme_bw() +
    scale_x_date(
      NULL,
      breaks = scales::breaks_width("5 years"), 
      labels = scales::label_date("'%y")
    ) + 
    scale_y_continuous(breaks = scales::breaks_width(2)) +
    labs(# title = paste(stnwq),
      subtitle = "Useable NUT data points per month",
      y = "number samples passing qa/qc") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = rel(0.9)),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title.y = element_text(size = rel(0.9))) 
          
  
  pnut2 <- ggplot(monts_nut, aes(x = YearMonth, y = problemMonth,
                                 fill = as.factor(Year),
                                 tooltip = paste0(YearMonthText))) +
    geom_col_interactive() +
    facet_grid(stn~param) +
    theme_bw() +
    scale_x_date(
      NULL,
      breaks = scales::breaks_width("5 years"), 
      labels = scales::label_date("'%y")
    ) + 
    scale_y_continuous(breaks = c(0, 1)) +
    labs(# title = paste(stnwq),
      subtitle = "Months with no useable NUT data points",
      y = "vertical line if no sample\npassed qa/qc that month") +
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = rel(0.9)),
          axis.title.y = element_text(size = rel(0.9)))
  
  
  ggiraph::girafe(ggobj = pnut) %>% 
    # , width_svg = 8, height_svg = 6) %>%
    htmltools::tagList() %>%
    print()
  
  ggiraph::girafe(ggobj = pnut2) %>%  
                  # width_svg = 8, height_svg = 6) %>%
    htmltools::tagList() %>%
    print()
  
  # clean up
  rm(nut, nut_long, monts_nut,
     pnut, pnut2)
  
}


