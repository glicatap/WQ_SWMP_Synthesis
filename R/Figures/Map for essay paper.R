# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(tigris)
library(viridis)
library(scatterpie)

# Define cluster colors using viridis
cluster_colors <- c("A" = viridis(4)[1],
                    "B" = viridis(4)[2],
                    "C" = viridis(4)[3],
                    "D" = viridis(4)[4])

# Read and preprocess data
cluster <- read.csv("Clusters.csv") %>% mutate(across(where(is.character), toupper))  # Convert character columns to uppercase
coords <- read.csv("coords_final.csv")
data_combined <- inner_join(coords, cluster, by = c("station", "Reserve"))  # Merge data by station and Reserve

# Calculate average coordinates for each station and reserve
avg_coords <- data_combined %>%
    group_by(Reserve, cluster, station) %>%
    summarise(lat = mean(lat), lon = mean(lon), .groups = "drop") %>%
    group_by(Reserve) %>%
    mutate(lat = first(lat), lon = first(lon))  # Align all stations within a reserve to the same location

# Adjust coordinates for specific reserves (Alaska, Hawaii, Puerto Rico)
adjusted_coords <- avg_coords %>%
    mutate(lat = case_when(
        Reserve == "KAC" ~ 24,
        Reserve == "HEE" ~ 27,
        Reserve == "JOB" ~ 25,
        TRUE ~ lat
    ),
    lon = case_when(
        Reserve == "KAC" ~ -114,
        Reserve == "HEE" ~ -105,
        Reserve == "JOB" ~ -90,
        TRUE ~ lon
    ))

# Create a wide-format dataset for plotting scatter pies
d_wide <- adjusted_coords %>%
    count(Reserve, lat, lon, cluster) %>%
    pivot_wider(
        names_from = cluster,
        values_from = n,
        values_fill = 0
    )

# Add jitter to coordinates for better visibility
set.seed(123)  # Ensure reproducible jitter
d_wide <- d_wide %>%
    mutate(
        lat_jittered = if_else(
            lon > -85,  # East Coast condition
            lat + runif(n(), -2, 2),  # More jitter for East Coast
            lat + runif(n(), -1.25, 1.25)  # Regular jitter for others
        ),
        lon_jittered = if_else(
            lon > -85,  # East Coast condition
            lon + runif(n(), -2, 2),  # More jitter for East Coast
            lon + runif(n(), -1.25, 1.25)  # Regular jitter for others
        )
    )

# Load US map shapefile and shift geometries
us_sf <- states(cb = TRUE, resolution = "20m") %>%
    shift_geometry() %>%
    st_transform(crs = 4326)  # Ensure WGS84 CRS for compatibility

# Plot the map with scatter pies and Reserve labels
p_v2 <- ggplot() +
    # Base map
    geom_sf(data = us_sf, fill = "lightgray", color = "white") +
    
    # Scatter pies for clusters
    geom_point(
        data = d_wide,
        aes(x = lon, y = lat, group = Reserve, r = 1),  # Adjust radius for visibility
        cols = c("A", "B", "C", "D"),
        color = "black",
        size =3
    ) +
    
    # Add Reserve labels
  #  geom_text(
   #     data = d_wide,
    #    aes(x = lon_jittered, y = lat_jittered, label = Reserve),
     #   size = 3,  # Label size
      #  nudge_y = 1.5  # Nudge labels upward to avoid overlap
  #  ) +
    
    # Define color scale for clusters
    scale_fill_manual(values = cluster_colors) +
    
    
    
    # Minimal theme for clean appearance
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),  # Remove grid lines
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "bottom",  # Move legend to bottom
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_blank()
    ) +
    
    # Set coordinate limits and aspect ratio
    coord_sf(
        xlim = c(-126, -65),
        ylim = c(17, 51),
        expand = FALSE
    ) +
    
    # Adjust legend settings
    guides(
        fill = guide_legend(
            title.position = "top",
            title.hjust = 0.5
        )
    )

# Render the plot
p_v2

ggsave("NERR_Map.png", p_v2, width = 8, height = 5, dpi = 600, bg = "white")
