nut <- purrr::map(stnnut, function(x) get(load(paste0(path, "/", x, ".RData")))) %>% 
  purrr::set_names(stn_vec) %>% 
  purrr::list_rbind(names_to = "stn") %>% 
  select(stn, datetimestamp, any_of(nut_keep)) %>% 
  rename(any_of(nut_parms)) %>% 
  rename(val_chlaN = val_chla_n,
         f_chlaN = f_chla_n) 

# pivot longer, remove rejected data, and extract flags
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
         reserve = str_sub(stn, 1, 3),
         flag = extract_flag(f),
         val = case_when(flag == "<-3>" ~ NA_real_,
                         .default = val),
         belowMDL = is_outsideMDL(f, "below"))

nut_long_nonchl <- nut_long %>% 
  filter(param != "chlaN")

nut_censored_nonchl <- nut_long %>% 
  filter(belowMDL == TRUE,
         param != "chlaN")

po4plt <- ggplot(filter(nut_censored_nonchl, param == "po4f"),
                 aes(x = datetimestamp,
                     y = val)) +
  geom_point() +
  facet_wrap(~stn, ncol = 5) +
  theme_bw()
  

po4 <- filter(nut_censored_nonchl, param == "po4f")

ggplot(po4) +
  geom_weave(aes(y = val)) +
  facet_wrap(~stn, ncol = 5)

ggplot(filter(nut_long, param == "nh4f"), 
              aes(x = belowMDL,
                  y = val)) +
  coord_cartesian(ylim = c(0, 0.1)) +
  geom_boxplot() +
  facet_wrap(~reserve, ncol = 5)

ggplot(filter(nut_long, param == "no23f"), 
       aes(x = belowMDL,
           y = val)) +
  stat_halfeye() +
  coord_cartesian(ylim = c(0, 0.1)) +
  facet_wrap(~reserve)

ggplot(filter(nut_long, param == "no23f"), 
       aes(x = belowMDL,
           y = val)) +
  stat_dotsinterval() +
  coord_cartesian(ylim = c(0, 0.1)) +
  facet_wrap(~reserve)

ggplot(filter(nut_long, param == "no23f"), 
       aes(x = belowMDL,
           y = val)) +
  stat_boxplot() +
  coord_cartesian(ylim = c(0, 0.1)) +
  facet_wrap(~reserve)

ggplot(filter(nut_long, param == "no23f")) +
  geom_density(aes(x = val,
                   y = ..scaled..,
                   fill = belowMDL),
               alpha = 0.3) +
  coord_cartesian(xlim = c(0, 0.25)) +
  facet_wrap(~reserve)



p <- ggplot(data = nut_censored_nonchl,
            aes(text = paste0(param,
                             ", ",
                             Year,
                             "-",
                             Month,
                             ": ",
                             val))) +
  geom_point(data = nut_long_nonchl, aes(x = datetimestamp,
                                         y = val),
             col = "gray80",
             alpha = 0.5)  +
  geom_point(data = nut_censored_nonchl, aes(x = datetimestamp,
                                             y = val),
             size = 2) +
  geom_line(data = nut_censored_nonchl, aes(x = datetimestamp,
                                            y = val)) +
  facet_grid(reserve ~ param, scales = "free") +
  labs(title = "Points flagged below MDL, overlaid on all readings",
       y = "value (mg/L)",
       x = "Date") +
  theme_bw()

ggplotly(p, tooltip = "text",
         width = 625, height = 675) %>%  
  htmltools::tagList() %>%
  print()
