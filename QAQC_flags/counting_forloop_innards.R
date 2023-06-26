cat("\n", st, "\n \n")
stnwq <- paste0(st, "wq")
stnnut <- paste0(st, "nut")

wq <- get(load(paste0(path, "/", stnwq, ".RData"))) %>% 
  select(datetimestamp, all_of(wq_keep)) %>% 
  rename(any_of(wq_parms)) %>% 
  rename(val_doPct = val_do_pct,
         val_domgl = val_do_mgl,
         f_doPct = f_do_pct,
         f_domgl = f_do_mgl) 

nut <- get(load(paste0(path, "/", stnnut, ".RData"))) %>% 
  select(datetimestamp, any_of(nut_keep)) %>% 
  rename(any_of(nut_parms)) %>% 
  rename(val_chlaN = val_chla_n,
         f_chlaN = f_chla_n) 


rm(list = c(stnwq, stnnut))


# do the stuff

# pivot longer and extract flags

#wq
specwq <- wq %>% 
  build_longer_spec(
    cols = (starts_with("val_")):ncol(wq),
    names_to = c(".value", "param"),
    names_sep = "_"
  )
wq_long <- wq %>% 
  pivot_longer_spec(spec = specwq) %>% 
  mutate(Year = lubridate::year(datetimestamp),
         Month = lubridate::month(datetimestamp),
         Day = lubridate::mday(datetimestamp),
         flag = extract_flag(f),
         keepStatus = case_when(flag %in% flags_keep ~ "keep",
                                .default = "discard"))

# nut
specnut <- nut %>% 
  build_longer_spec(
    cols = (starts_with("val_")):ncol(nut),
    names_to = c(".value", "param"),
    names_sep = "_"
  )

nut_long <- nut %>% 
  pivot_longer_spec(spec = specnut) %>% 
  mutate(Year = lubridate::year(datetimestamp),
         Month = lubridate::month(datetimestamp),
         Day = lubridate::mday(datetimestamp),
         flag = extract_flag(f),
         keepStatus = case_when(flag %in% flags_keep ~ "keep",
                                .default = "discard"))


# compile
dails <- wq_long %>% 
  group_by(param, Year, Month, Day) %>% 
  summarize(keptDataPoints = sum(keepStatus == "keep"))

monts <- dails %>% 
  group_by(param, Year, Month) %>% 
  summarize(useableDays = sum(keptDataPoints > 0),
            badDays = sum(keptDataPoints == 0)) %>% 
  mutate(YearMonth = as.Date(paste(Year, Month, "01", sep = "-")),
         YearMonthText = str_sub(as.character(YearMonth), end = -4))


# plot
# number of bad days by month
p <- ggplot(monts, aes(x = YearMonth, y = badDays,
                       fill = as.factor(Year),
                       tooltip = paste0(YearMonthText, ":\n n = ", badDays))) +
  geom_col_interactive() +
  facet_wrap(~param, ncol = 2) +
  theme_bw() +
  labs(title = paste(stnwq),
       subtitle = "Days in month with no useable data points") +
  theme(legend.position = "none")

x1 <- ggiraph::girafe(ggobj = p)
x1 %>% 
  htmltools::tagList() %>% 
  print()


# number of good days by month
p <- ggplot(monts, aes(x = YearMonth, y = useableDays,
                       fill = as.factor(Year),
                       tooltip = paste0(YearMonthText, ":\n n = ", useableDays))) +
  geom_col_interactive() +
  facet_wrap(~param, ncol = 2) +
  theme_bw() +
  labs(title = paste(stnwq),
       subtitle = "Days in month with >= 1 useable data point",
       fill = "Year") +
  theme(legend.position = "none")

ggiraph::girafe(ggobj = p) %>% 
  htmltools::tagList() %>% 
  print()

# print(htmltools::tagList(x1, x2))
