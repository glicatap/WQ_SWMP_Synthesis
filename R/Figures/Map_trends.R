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
nut_trends <- read.csv(here::here("Outputs",
                                  "02_calculated_long-term-trends",
                                  "NUT_trends_back-transformed_MDL.csv"))
all_trends <- read.csv(here::here("Outputs","02_calculated_long-term-trends",
                                  "bam_outputs_MDL",
                                  "long-term-trends.csv"))

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

# Filter and merge data for dissolved oxygen, turb and temp trends
do_trend <- merged_df %>%
  filter(parameter == "do_mgl_median")


temp_trend <- merged_df %>%
    filter(parameter == "temp_median")

turb_trend <- merged_df %>%
    filter(parameter == "turb_median")

# Filter and merge data for PO4 trends
po4_trend_log <- merged_df %>%
    filter(parameter == "po4f_mdl")

po4_sig <- po4_trend_log %>%
    select(station, sig_trend)

po4_trend_pct <- merged_nut_df %>%
    filter(param == "po4f_mdl")

merged_po4 <- po4_trend_pct %>%
    inner_join(po4_sig, by = "station")


# Filter and merge data for NH4 trends
nh4_trend_log <- merged_df %>%
    filter(parameter == "nh4f_mdl")

nh4_sig <- nh4_trend_log %>%
    select(station, sig_trend)

nh4_trend_pct <- merged_nut_df %>%
    filter(param == "nh4f_mdl")

merged_nh4 <- nh4_trend_pct %>%
    inner_join(nh4_sig, by = "station")

# Filter and merge data for no23 trends
no23_trend_log <- merged_df %>%
    filter(parameter == "no23f_mdl")

no23_sig <- no23_trend_log %>%
    select(station, sig_trend)

no23_trend_pct <- merged_nut_df %>%
    filter(param == "no23f_mdl")

merged_no23 <- no23_trend_pct %>%
    inner_join(no23_sig, by = "station")



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


# Merge temp trends with coordinates
map_temp <- temp_trend %>%
    left_join(coords, by = "station")

# Convert  temp trends data to sf object and shift geometry
df_sf <- st_as_sf(map_temp, coords = c("lon", "lat"), crs = 4326) %>%
    tigris::shift_geometry()

# Plot map of temp trends
ggplot() +
    geom_sf(data = us_sf) +
    labs(title = "Trends in Water Temp",
         subtitle = "Filled circles indicate p < 0.05") +
    theme(panel.background = element_blank()) +
    geom_sf(data = df_sf, aes(color = Slope, shape = as.factor(sig_trend)), size = 4) +
    scale_color_gradient(high = "red", low = "blue", name = "Temp trend, C/yr") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none") +
    theme(legend.position = "bottom")


# Merge  po4 trends with coordinates
map_po4 <- merged_po4 %>%
    left_join(coords, by = "station")

# Get US states shapefile and shift geometry
us_sf <- states(cb = TRUE, resolution = "20m") %>%
    shift_geometry()

# Convert po4 trends data to sf object and shift geometry
df_sf <- st_as_sf(map_po4, coords = c("lon", "lat"), crs = 4326) %>%
    tigris::shift_geometry()

# Plot map of po4 trends
ggplot() +
    geom_sf(data = us_sf) +
   # labs(title = "Trends in po4",
   #      subtitle = "Filled circles indicate p < 0.05") +
    theme(panel.background = element_blank()) +
    geom_sf(data = df_sf, aes(color = trend_pctPerYear, shape = as.factor(sig_trend)), size = 5) +
    scale_color_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = 0, name = "PO4 trend, percent/year") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none") +
    theme(legend.position = "bottom")


# Merge  nh4 trends with coordinates
map_nh4 <- merged_nh4 %>%
    left_join(coords, by = "station")

# Get US states shapefile and shift geometry
us_sf <- states(cb = TRUE, resolution = "20m") %>%
    shift_geometry()

# Convert nh4 trends data to sf object and shift geometry
df_sf <- st_as_sf(map_nh4, coords = c("lon", "lat"), crs = 4326) %>%
    tigris::shift_geometry()

# Plot map of nh4 trends
ggplot() +
    geom_sf(data = us_sf) +
   # labs(title = "Trends in nh4",
   #      subtitle = "Filled circles indicate p < 0.05") +
    theme(panel.background = element_blank()) +
    geom_sf(data = df_sf, aes(color = trend_pctPerYear, shape = as.factor(sig_trend)), size = 5) +
    scale_color_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = 0, name = "nh4 trend, percent/year") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none") +
    theme(legend.position = "bottom")


# Merge  no23 trends with coordinates
map_no23 <- merged_no23 %>%
    left_join(coords, by = "station")

# Get US states shapefile and shift geometry
us_sf <- states(cb = TRUE, resolution = "20m") %>%
    shift_geometry()

# Convert no23 trends data to sf object and shift geometry
df_sf <- st_as_sf(map_no23, coords = c("lon", "lat"), crs = 4326) %>%
    tigris::shift_geometry()

# Plot map of no23 trends
ggplot() +
    geom_sf(data = us_sf) +
   # labs(title = "Trends in no23",
    #     subtitle = "Filled circles indicate p < 0.05") +
    theme(panel.background = element_blank()) +
    geom_sf(data = df_sf, aes(color = trend_pctPerYear, shape = as.factor(sig_trend)), size = 5) +
    scale_color_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = 0, name = "no23 trend, percent/year") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none") +
    theme(legend.position = "bottom")



# Merge turb trends with coordinates
map_turb <- turb_trend %>%
    left_join(coords, by = "station")

# Convert turb trends data to sf object and shift geometry
df_sf <- st_as_sf(map_turb, coords = c("lon", "lat"), crs = 4326) %>%
    tigris::shift_geometry()

# Plot map of turb trends
ggplot() +
    geom_sf(data = us_sf) +
    labs(title = "Trends in Turbidity",
         subtitle = "Filled circles indicate p < 0.05") +
    theme(panel.background = element_blank()) +
    geom_sf(data = df_sf, aes(color = Slope, shape = as.factor(sig_trend)), size = 4) +
    scale_color_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = 0, name = "Turbidity, NTU/yr") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none") +
    theme(legend.position = "bottom")



##################################################################################################
# Apply jitter and create new geometries
set.seed(123)  # For reproducibility
jittered_coords <- st_coordinates(df_sf$geometry) + matrix(rnorm(nrow(df_sf) * 2, sd = 0.01), ncol = 2)

# Create jittered geometry
df_sf$jittered_geometry <- st_sfc(st_point(jittered_coords), crs = st_crs(df_sf))

# Convert to sf object
df_sf <- st_as_sf(df_sf)

ggplot() +
    geom_sf(data = us_sf) +
    # labs(title = "Trends in no23",
    #     subtitle = "Filled circles indicate p < 0.05") +
    theme(panel.background = element_blank()) +
    geom_point(data = df_sf, aes(x = jittered_long, y = jittered_lat, color = trend_pctPerYear, shape = as.factor(sig_trend)), size = 4) +
    #scale_color_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = 0, name = "no23 trend, percent/year") +
    scale_color_gradient(low = "darkblue", high = "red", name = "no23 trend, percent/year") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none") +
    theme(legend.position = "bottom")
