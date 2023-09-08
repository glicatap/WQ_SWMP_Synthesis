library(tidyverse)

load(here::here("Data", "QAQCd_daily", "gndcrmet_daily.RData"))
load(here::here("Data", "QAQCd_daily", "wqbchmet_daily.RData"))
load(here::here("Data", "QAQCd_daily", "niwolmet_daily.RData"))
load(here::here("Data", "QAQCd_daily", "soscmmet_daily.RData"))

load(here::here("Data", "QAQCd_daily", "gndbhwq_daily.RData"))
load(here::here("Data", "QAQCd_daily", "wqbmpwq_daily.RData"))
load(here::here("Data", "QAQCd_daily", "niwolwq_daily.RData"))
load(here::here("Data", "QAQCd_daily", "sosvawq_daily.RData"))

met <- bind_rows(list(gndcrmet = gndcrmet_daily,
                 wqbchmet = wqbchmet_daily,
                 soscmmet = soscmmet_daily,
                 niwolmet = niwolmet_daily),
                 .id = "station")

wq <- bind_rows(list (gndbhwq = gndbhwq_daily,
                      wqbmpwq = wqbmpwq_daily,
                      sosvawq = sosvawq_daily,
                      niwolwq = niwolwq_daily),
                 .id = "station") %>% 
  mutate(doy = lubridate::yday(date),
         year = lubridate::year(date),
         month = lubridate::month(date))

wq2 <- wq %>% 
  summarize(.by = c(station, doy),
            n_years = sum(!is.na(temp_median)),
            across(temp_min:doLessThan5_total,
                   function(x) median(x, na.rm = TRUE))
            )

do3 <- wq %>% 
  summarize(.by = c(station, month),
            median_domgl = median(do_mgl_median, na.rm = TRUE)) %>% 
  group_by(station) %>% 
  mutate(centered_median = median_domgl - median(median_domgl, na.rm = TRUE))

# PAR plots ----  

ggplot(met, aes(x = date, y = totpar_total, col = station)) +
  geom_point(alpha = 0.2) +
  coord_cartesian(ylim = c(0, 75000)) +
  labs(title = "PAR",
       x = "Date",
       y = "total daily PAR (mmol/m^2 for entire day)")

ggplot() +
  geom_line(data = met, aes(x = date, y = totpar_total, col = station)) +
  coord_cartesian(ylim = c(0, 75000)) +
  facet_wrap(~station)

met2 <- met %>% 
  mutate(doy = lubridate::yday(date),
         year = lubridate::year(date),
         month = lubridate::month(date))

ggplot(met2, aes(x = doy, y = totpar_total, col = station)) +
  geom_point(alpha = 0.1) +
  coord_cartesian(ylim = c(0, 75000)) +
  geom_smooth(method = "loess")

ggplot(met2, aes(x = doy, y = totpar_total, col = station)) +
  geom_smooth(method = "loess") +
  labs(title = "PAR annual pattern",
       subtitle = "LOESS smooth",
       x = "Day of Year",
       y = "total PAR (mmol/m^2) for entire day") +
  theme_bw()


ggplot(met2, aes(x = doy, y = totpar_total, col = station)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", col = "black") +
  coord_cartesian(ylim = c(0, 75000)) +
  facet_wrap(~station) +
  labs(title = "PAR annual pattern",
       subtitle = "raw data",
       x = "Day of Year",
       y = "total PAR (mmol/m^2) for entire day")

# DO plots ----
# do mg/L
ggplot() +
  geom_point(data = wq, aes(x = doy, y = do_mgl_median, col = year), 
             alpha = 0.3, size = 0.7) +
  facet_wrap(~station) +
  geom_line(data = wq2, aes(x = doy, y = do_mgl_median), col = "orange", size = 1) +
  labs(title = "Daily median DO mg/L",
       subtitle = "orange line is overall median for that day",
       x = "Day of Year",
       y = "DO (mg/L)")

ggplot() +
  geom_line(data = wq, aes(x = doy, y = do_mgl_median, col = year), 
            alpha = 0.3, size = 0.7) +
  facet_wrap(~station) +
  labs(title = "Daily median DO mg/L",
       subtitle = "one line per year - see when there's less data at ice stations",
       x = "Day of Year",
       y = "DO (mg/L)")

ggplot() +
  geom_line(data = wq2, aes(x = doy, y = do_mgl_median, col = station), 
            size = 1, alpha = 0.7) +
  labs(title = "Overall median of daily median DO mg/L",
       subtitle = "how do stations compare to each other?",
       x = "Day of Year",
       y = "DO (mg/L)")

ggplot() +
  geom_point(data = wq2, aes(x = doy, y = do_mgl_median, 
                             col = station, size = n_years), 
            alpha = 0.7) +
  labs(title = "Overall median of daily median DO mg/L",
       subtitle = "what about ice stations - smaller points when less data",
       x = "Day of Year",
       y = "DO (mg/L)",
       size = "years with values \non this day")

ggplot() +
  geom_col(data = do3, aes(x = as.factor(month), y = centered_median, fill = station)) +
  facet_wrap(~station) +
  labs(title = "DO mg/L seasonality",
       x = "Month",
       y = "centered monthly median DO mg/L")

ggplot() +
  geom_line(data = do3, aes(x = as.factor(month),
                            y = centered_median,
                            col = station,
                            group = station),
            size = 1) +
  labs(title = "DO mg/L seasonality",
       x = "Month",
       y = "centered monthly median DO mg/L")

# do < 2

