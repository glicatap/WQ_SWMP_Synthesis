library(here)
library(dplyr)
library(reshape2) 
library(ggplot2)
library(lubridate)
library(data.table)
library(tidyr)
library(viridis)
library(tigris)
library(sf)
#Set yousfheaders#Set your working directory

# Read data
cluster <- read.csv("Clusters.csv")
nut_trends <- read.csv("NUT_trends_back-transformed.csv")
all_trends <- read.csv("long-term-trends.csv")

# Preprocess and merge data
subset_trends <- all_trends %>%
  select(station, parameter, Slope, std.error, conf.low, conf.high, p.value, sig_trend) %>%
  mutate(
    station = substr(station, 1, 5),
    reserve = substr(station, 1, 3))

subset_trends <- subset_trends %>%
  mutate(ratio_SE_Slope = abs(std.error) / abs(Slope))

subset_trends <- subset_trends %>%
  filter(!parameter %in% c("dailyPAR_median", "do_pct_median",
                           "do_proportion_below5","sal_median"))

subset_nut_trends <- nut_trends %>%
  mutate(station = substr(station, 1, 5))

merged_df <- subset_trends %>%
  inner_join(cluster, by = "station")

merged_nut_df <- subset_nut_trends %>%
  inner_join(cluster, by = "station")

###################

remove_stns <- c("pdbgd", "kachd", "kacsd", "sfbfm","lksbl", "lksol", "lksba", "lkspo")

# weed out the problem stations - too deep, or missing trend(s)  
subset_trends <- subset_trends |> 
    filter(!(station %in% remove_stns))

subset_nut_trends <- subset_nut_trends |> 
    filter(!(station %in% remove_stns))

merged_df <- merged_df |> 
    filter(!(station %in% remove_stns))

######################################

# Slopes by cluster
cluster_colors <- c("A" = viridis(4)[1], "B" = viridis(4)[2], "C" = viridis(4)[3], "D" = viridis(4)[4])

# Filter and merge data for chlorophyll-a trends
chla_trend_log <- merged_df %>%
  filter(parameter == "chla_n")

chla_sig <- chla_trend_log %>%
  select(station, sig_trend)

chla_trend_pct <- merged_nut_df %>%
  filter(param == "chla_n")

merged_chla <- chla_trend_pct %>%
  inner_join(chla_sig, by = "station")

# Filter and merge data for dissolved oxygen trends
do_trend <- merged_df %>%
  filter(parameter == "do_mgl_median")


temp_trend <- merged_df %>%
    filter(parameter == "temp_median")


########################
### MAPS

# Read coordinates data
coords <- read.csv("coords.csv", header = TRUE)
coords <- coords %>%
  mutate(station = tolower(station)) %>%
  mutate(station = substr(station, 1, 5))

# Merge chlorophyll-a trends with coordinates
map_chla <- merged_chla %>%
  left_join(coords, by = "station")

# Get US states shapefile and shift geometry
us_sf <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry()

# Convert chlorophyll-a trends data to sf object and shift geometry
df_sf <- st_as_sf(map_chla, coords = c("lon", "lat"), crs = 4326) %>%
  tigris::shift_geometry()

# Plot map of chlorophyll-a trends
ggplot() +
  geom_sf(data = us_sf) +
  labs(title = "Trends in Chl-a",
       subtitle = "Filled circles indicate p < 0.05") +
  theme(panel.background = element_blank()) +
  geom_sf(data = df_sf, aes(color = trend_pctPerYear, shape = as.factor(sig_trend)), size = 4) +
  scale_color_gradient(low = "blue1", high = "forestgreen", name = "Chla trend, percent/year") +
  scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none") +
  theme(legend.position = "bottom")

# Merge dissolved oxygen trends with coordinates
map_do <- do_trend %>%
  left_join(coords, by = "station")

# Convert dissolved oxygen trends data to sf object and shift geometry
df_sf <- st_as_sf(map_do, coords = c("lon", "lat"), crs = 4326) %>%
  tigris::shift_geometry()

# Plot map of dissolved oxygen trends
ggplot() +
  geom_sf(data = us_sf) +
  labs(title = "Trends in DO",
       subtitle = "Filled circles indicate p < 0.05") +
  theme(panel.background = element_blank()) +
  geom_sf(data = df_sf, aes(color = Slope, shape = as.factor(sig_trend)), size = 4) +
  scale_color_gradient(low = "orange", high = "blue", name = "DO trend, mg/L/yr") +
  scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none") +
  theme(legend.position = "bottom")


# Merge dissolved oxygen trends with coordinates
map_temp <- temp_trend %>%
    left_join(coords, by = "station")

# Convert dissolved oxygen trends data to sf object and shift geometry
df_sf <- st_as_sf(map_temp, coords = c("lon", "lat"), crs = 4326) %>%
    tigris::shift_geometry()

# Plot map of temp trends
ggplot() +
    geom_sf(data = us_sf) +
    labs(title = "Trends in Water Temp",
         subtitle = "Filled circles indicate p < 0.05") +
    theme(panel.background = element_blank()) +
    geom_sf(data = df_sf, aes(color = Slope, shape = as.factor(sig_trend)), size = 4) +
    scale_color_gradient(low = "red", high = "blue", name = "Temp trend, C/yr") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none") +
    theme(legend.position = "bottom")

