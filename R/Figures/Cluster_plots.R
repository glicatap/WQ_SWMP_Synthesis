library(ggplot2)
library(maps)
library(ggforce)
library(scatterpie)
library(tidyr)
library(sf)
library(tigris)
library(dplyr)
library(viridis)
library(cowplot)
library(ggspatial)
library(ggpubr)
library(ggridges)

cluster_colors <- c("A" = viridis(4)[1],   # Replace with actual colors or modify as needed
                    "B" = viridis(4)[2],
                    "C" = viridis(4)[3],
                    "D" = viridis(4)[4])


###########################

#Clusters using all stations


# Read and preprocess data
cluster <- read.csv("Clusters.csv")

unique(cluster$Reserve)


cluster_upper <- cluster %>%
  mutate(across(where(is.character), toupper))

coords <- read.csv("coords_final.csv")

d <- inner_join(coords, cluster_upper, by = c("station", "Reserve"))

# Calculate average lat and lon for each Reserve
avg_coords <- d %>%
  group_by(Reserve,cluster) %>%
  summarise(lat = mean(lat), lon = mean(lon)) 

avg_coords2 <-avg_coords %>%
  group_by(Reserve) %>%
  mutate(lat = first(lat), lon = first(lon))


# Manually adjust coordinates for Alaska, Hawaii, and Puerto Rico
adjusted_coords <- avg_coords2 %>%
  mutate(lat = case_when(
    Reserve == "KAC" ~ 25,
    Reserve == "HEE" ~ 27,
    Reserve == "JOB" ~ 25,
    TRUE ~ lat
  ),
  lon = case_when(
    Reserve == "KAC" ~ -117,
    Reserve == "HEE" ~ -105,
    Reserve == "JOB" ~ -90,
    TRUE ~ lon
  ))

# Proceed with pivot_wider
d_wide <- adjusted_coords %>%
  count(Reserve, lat, lon, cluster) %>%
  pivot_wider(
    id_cols = c(Reserve, lat, lon),
    names_from = cluster,
    values_from = n,
    values_fill = 0
  )

# Get map data for the US
us_sf <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry()

# Convert d_wide_sf to sf object
d_wide_sf <- st_as_sf(d_wide, coords = c("lon", "lat"), crs = 4326)

# Transform d_wide to match us_sf coordinate system
d_wide_sf <- st_as_sf(d_wide, coords = c("lon", "lat"), crs = 4326)

us_sf <- st_transform(us_sf, crs = st_crs("+proj=longlat +datum=WGS84"))  # Transform to WGS84 if needed

# Extract lon and lat from geometry column
d_wide_sf <- st_coordinates(d_wide_sf)
d_wide_sf<-data.frame(d_wide_sf)

d_merged<-cbind(d_wide,d_wide_sf)

################


# Define your base plot with shifted geometries
p <- ggplot() +
  geom_sf(data = us_sf, color = "black",fill="white") +
  theme(panel.background = element_blank())  # Adjust other theme settings as needed

# Add scatter pie charts to the map
p <- p + geom_scatterpie(
  data = d_merged,
  aes(x = X, y = Y, group = Reserve, r = 1),  # Adjust radius 'r' as necessary
  cols = c("A", "B", "C", "D"),
  color = "black",
  alpha = 0.8
) +
  scale_fill_manual(values = cluster_colors)+
  xlab("") +
  ylab("") +
  guides(fill = guide_legend(title = "Cluster"))+ theme(
    legend.position = c(0.95, 0.15),  # Adjust x and y coordinates as needed
    legend.justification = c(1, 0),  # Adjust justification relative to coordinates
    legend.box.just = "right"  # Ensure legend box is justified to the right
  )

p


########################################################################################################
#######################################################################################################

####

##Clusters using trends stations (103) instead of all stations


# Read and preprocess data
cluster <- read.csv("clusters_trends.csv")

unique(cluster$Reserve)

cluster_upper <- cluster %>%
  mutate(across(where(is.character), toupper))

coords <- read.csv("coords_final.csv")

d <- inner_join(coords, cluster_upper, by = c("station", "Reserve"))

# Calculate average lat and lon for each Reserve
avg_coords <- d %>%
  group_by(Reserve,cluster) %>%
  summarise(lat = mean(lat), lon = mean(lon)) 

avg_coords2 <-avg_coords %>%
  group_by(Reserve) %>%
  mutate(lat = first(lat), lon = first(lon))


# Manually adjust coordinates for Alaska, Hawaii, and Puerto Rico
adjusted_coords <- avg_coords2 %>%
  mutate(lat = case_when(
    Reserve == "KAC" ~ 25,
    Reserve == "JOB" ~ 25,
    TRUE ~ lat
  ),
  lon = case_when(
    Reserve == "KAC" ~ -117,
    Reserve == "JOB" ~ -90,
    TRUE ~ lon
  ))

# Proceed with pivot_wider
d_wide <- adjusted_coords %>%
  count(Reserve, lat, lon, cluster) %>%
  pivot_wider(
    id_cols = c(Reserve, lat, lon),
    names_from = cluster,
    values_from = n,
    values_fill = 0
  )

# Get map data for the US
us_sf <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry()

# Convert d_wide_sf to sf object
d_wide_sf <- st_as_sf(d_wide, coords = c("lon", "lat"), crs = 4326)

# Transform d_wide to match us_sf coordinate system
d_wide_sf <- st_as_sf(d_wide, coords = c("lon", "lat"), crs = 4326)

us_sf <- st_transform(us_sf, crs = st_crs("+proj=longlat +datum=WGS84"))  # Transform to WGS84 if needed

# Extract lon and lat from geometry column
d_wide_sf <- st_coordinates(d_wide_sf)
d_wide_sf<-data.frame(d_wide_sf)

d_merged<-cbind(d_wide,d_wide_sf)

################


# Define your base plot with shifted geometries
p_v2 <- ggplot() +
  geom_sf(data = us_sf, color = "black",fill="white") +
  theme(panel.background = element_blank())  # Adjust other theme settings as needed

# Add scatter pie charts to the map
p_v2 <- p_v2 + geom_scatterpie(
  data = d_merged,
  aes(x = X, y = Y, group = Reserve, r = 1),  # Adjust radius 'r' as necessary
  cols = c("A", "B", "C", "D"),
  color = "black",
  alpha = 0.8
) +
  scale_fill_manual(values = cluster_colors)+
  xlab("") +
  ylab("") +
  guides(fill = guide_legend(title = "Cluster"))+ theme(
    legend.position = c(0.95, 0.15),  # Adjust x and y coordinates as needed
    legend.justification = c(1, 0),  # Adjust justification relative to coordinates
    legend.box.just = "right"  # Ensure legend box is justified to the right
  )

p_v2



#######################################################################################


###PCA Plots

library(dplyr)
library(viridis)
library(ggfortify)
library(ggplot2)
library(tidyr)
library(viridis)
library(patchwork)

cluster_colors <- c("A" = viridis(4)[1],   # Replace with actual colors or modify as needed
                    "B" = viridis(4)[2],
                    "C" = viridis(4)[3],
                    "D" = viridis(4)[4])


cluster_data <- read.csv("swmp_clstr_med_pc.csv")


# Relevant variables for PCA
variables <- c("Sal", "Temp", "DO", "pH", "Turb", "Chla", "NH4", "NO23", "PO4")

# Perform PCA
pca_model <- prcomp(cluster_data[, variables], scale. = TRUE)

# Get loadings
loadings <- as.data.frame(pca_model$rotation)
loadings$variable <- rownames(loadings)

# Function to compute convex hull for each cluster
compute_hull <- function(df) {
    df[chull(df$PC1, df$PC2), ]
}

# Apply the function to each cluster
hulls <- cluster_data %>% group_by(cluster) %>% do(compute_hull(.))

# Plot PCA with convex hulls and loadings
pca_1 <- ggplot(cluster_data, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 3) +
    geom_polygon(data = hulls, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.2) +
    geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5), 
                 arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1) +
    geom_text(data = loadings, aes(x = PC1 * 5, y = PC2 * 5, label = variable), 
              hjust = 1.5, vjust = 1.5, color = "black", size = 5, fontface = "bold") +
    labs(x = "PC1 (32.4%)", y = "PC2 (25.9%)") +
    theme_minimal() +
    scale_color_manual(values = cluster_colors) +
    scale_fill_manual(values = cluster_colors) +
    guides(color = guide_legend(title = NULL), fill = guide_legend(title = NULL)) +
    theme(legend.position = "none")

pca_1

# Function to compute convex hull for each cluster
compute_hull2 <- function(df) {
    df[chull(df$PC1, df$PC3), ]
}

# Apply the function to each cluster
hulls2 <- cluster_data %>% group_by(cluster) %>% do(compute_hull2(.))

# Plot PCA with convex hulls and loadings
pca_2 <- ggplot(cluster_data, aes(x = PC1, y = PC3, color = cluster)) +
    geom_point(size = 3) +
    geom_polygon(data = hulls2, aes(x = PC1, y = PC3, fill = cluster), alpha = 0.2) +
    geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 5, yend = -PC3 * 5), 
                 arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1) +
    geom_text(data = loadings, aes(x = PC1 * 5, y = -PC3 * 5, label = variable), 
              hjust = 1.5, vjust = 1.5, color = "black", size = 5, fontface = "bold") +
    labs(x = "PC1 (32.4%)", y = "PC3 (15.9%)") +
    theme_minimal() +
    scale_color_manual(values = cluster_colors) +
    scale_fill_manual(values = cluster_colors) +
    guides(color = guide_legend(title = NULL), fill = guide_legend(title = NULL)) +
    theme(legend.position = "bottom")


######
#GGRIDGES of Medians
cluster_data<-read.csv("swmp_clstr_med_pc.csv")

cluster_data<-cluster_data[,1:12]

cluster_data2 <- cluster_data %>%
  mutate(across(last_col(4):ncol(.), ~ 10^ .))

cluster_data3 <- cluster_data2 %>%
  mutate(across(last_col(2):ncol(.), ~ .*1000))

long_data <- pivot_longer(
  cluster_data3, 
  cols = starts_with(c("Sal", "Temp", "DO", "pH", "Turb", "Chla", "NH4", "NO23", "PO4")),
  names_to = "Parameter",
  values_to = "Value"
)

long_data <- long_data %>%
    rename(station = code,
           reserve = Map.Code)


long_data_wq <- long_data %>%
    filter(!Parameter %in% c("Chla", "NH4", "NO23", "PO4","Turb"))

long_data_nutchla<-long_data %>%
    filter(Parameter %in% c("Chla", "NH4", "NO23", "PO4","Turb"))


long_data_wq$cluster <- factor(long_data_wq$cluster, levels = rev(levels(factor(long_data_wq$cluster))))

long_data_nutchla$cluster <- factor(long_data_nutchla$cluster, levels = rev(levels(factor(long_data_nutchla$cluster))))

p1 <- ggplot(long_data_wq, aes(x = Value, y = cluster, fill = cluster)) + 
    geom_density_ridges(scale = 2, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(~ Parameter, scales = "free", labeller = labeller(Parameter = c(
        Sal = "Sal ppt",
        Temp = "Temp deg C",
        DO = "DO mg/L",
        pH = "pH"
    ))) +
    ylab("") + xlab("Median values") +
    scale_fill_manual(values = cluster_colors) +
    theme_minimal() +
    theme(strip.background = element_rect(fill = "lightgrey", color = "black"))

p1

p2 <- ggplot(long_data_nutchla, aes(x = Value, y = cluster, fill = cluster)) + 
    geom_density_ridges(scale = 2, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(~ Parameter, scales = "free", labeller = labeller(Parameter = c(
        Chla = "Chl-a ug/L",
        NH4 = "NH4 ug/L",
        NO23 = "NO23 ug/L",
        PO4 = "PO4 ug/L",
        Turb = "Turb NTU"
    ))) +
    ylab("") + xlab("Median values (log scale)") +
    scale_fill_manual(values = cluster_colors) +
    scale_x_log10(labels = scales::scientific) +  # Control the number of x-axis labels
    theme_minimal() +
    theme(strip.background = element_rect(fill = "lightgrey", color = "black"))

p2

# Combine plots with the first plot being larger
combined_plot <- p1 + p2 +
    plot_layout(guides = "collect", widths = c(1, 2)) & 
    theme(legend.position = "bottom")

# Display the combined plot
combined_plot



#####################

combined_plot2  <- (pca_1 / pca_2) +
    plot_layout(guides = "collect") & theme(legend.position = 'bottom')

combined_plot2

combined_plot1 <- p1 / p2 +
    plot_layout(guides = "collect", widths = c(1, 2)) & 
    theme(legend.position = "bottom")

# Combine the two combined plots into a final layout with height adjustment
final_patchwork <- combined_plot1 | combined_plot2 + 
    plot_layout(heights = c(1, 1))  # Adjust heights to make combined_plot2 shorter

# Display the final combined plot
final_patchwork

