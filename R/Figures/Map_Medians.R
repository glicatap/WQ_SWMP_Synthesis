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
library(patchwork)
#Set yousfheaders#Set your working directory

# Slopes by cluster
cluster_colors <- c("A" = viridis(4)[1], "B" = viridis(4)[2], "C" = viridis(4)[3], "D" = viridis(4)[4])


data<- read.csv(here::here("Outputs", "04_compiled_predictors",
                           "compiled_predictors_withExternalInfo_MDL.csv"))

nut_trends <- read.csv(here::here("Outputs",
                                  "02_calculated_long-term-trends",
                                  "NUT_trends_back-transformed_MDL.csv"))

nut_trends_wide <- nut_trends %>%
    pivot_wider(id_cols = station, names_from = param, values_from = trend_pctPerYear)

nut_trends_wide <- nut_trends_wide %>%
    mutate(station = substr(station, 1, 5))

combined_data <- data %>%
    left_join(nut_trends_wide, by = "station")



coords <- read.csv("coords_final.csv")

coords <- coords %>%
    mutate(station = tolower(station)) %>%
    mutate(Reserve = tolower(Reserve))


map_data <- inner_join(coords, combined_data, by = c("station"))  # Merge data by station and Reserve


#############

library(sf)
library(dplyr)
library(ggplot2)
library(tigris)

# Load US states shapefile and ensure correct CRS
us_sf <- states(cb = TRUE, resolution = "20m") %>%
    shift_geometry() %>%
    st_transform(crs = 4326) # Transform to WGS 84


plot_variable_map <- function(data, coords, lon_adjustments, lat_adjustments, variable_name, color_gradient, color_label) {
    library(ggplot2)
    library(dplyr)
    library(sf)
    library(cowplot)  # To arrange plots
    library(grid)  # For inset legend positioning
    

map_data <- map_data %>%
    mutate(
        lon = ifelse(Reserve == "kac", lon_adjustments["kac"], lon),
        lat = ifelse(Reserve == "kac", lat_adjustments["kac"], lat),
        lon = ifelse(Reserve == "job", lon_adjustments["job"], lon),
        lat = ifelse(Reserve == "job", lat_adjustments["job"], lat)
    )

# Add jitter to latitude and longitude
set.seed(42)
map_data_jittered <- map_data %>%
    mutate(
        lon_jittered = lon + runif(n(), min = -1, max = 1),
        lat_jittered = lat + runif(n(), min = -1, max = 1)
    )

# Convert to sf object
df_sf_jittered <- st_as_sf(
    map_data_jittered,
    coords = c("lon_jittered", "lat_jittered"),
    crs = 4326
)

# Find min and max values for color scale
min_value <- min(map_data[[variable_name]], na.rm = TRUE)
max_value <- max(map_data[[variable_name]], na.rm = TRUE)

# Create the main map
map_plot <- ggplot() +
    geom_sf(data = us_sf, fill = "gray93", color = "white") +
    geom_sf(
        data = df_sf_jittered,
        aes(color = !!sym(variable_name)),
        size = 3
    ) +
    labs(
        #title = title,
        #subtitle = subtitle,
        color = color_label
    ) +
    scale_color_gradient2(
        low = color_gradient$low,
        mid = color_gradient$mid,
        high = color_gradient$high,
        midpoint = color_gradient$midpoint,
        name = color_label,
        limits = c(min_value, max_value) # Set consistent color limits
    ) +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),       # <--- Removes axis numbers
        axis.ticks = element_blank(),      # <--- Optional: removes tick marks too
        axis.title = element_blank(),
        legend.position = "none"
    )+
    coord_sf(
        xlim = c(-130, -65),
        ylim = c(18, 51),
        expand = FALSE
    )

histogram_legend <- ggplot(map_data, aes(x = !!sym(variable_name))) +
    
    # Background: "No" (white fill with gradient outline)
    geom_histogram(
        data = subset(map_data),
        aes(color = after_stat(x)),  # Gradient outline
        fill = "lightgray",
        bins = 10,
        size = 0.8
    ) +
    
    # Foreground: "Yes" (gradient fill with transparency)
    geom_histogram(
        data = subset(map_data),
        aes(fill = after_stat(x)),  # Gradient fill
        bins = 10,
        color = "black",
        size = 0.3,
        alpha = 0.6  # Adjust transparency (0 = fully transparent, 1 = solid)
    ) +
    
    # Gradient scales
    scale_fill_gradient2(
        low = color_gradient$low,
        mid = color_gradient$mid,
        high = color_gradient$high,
        midpoint = color_gradient$midpoint,
        name = color_label,
        limits = c(min_value, max_value) # Set same limits as the map
    ) +
    scale_color_gradient2(  # Gradient outline for "No"
        low = color_gradient$low,
        mid = color_gradient$mid,
        high = color_gradient$high,
        midpoint = color_gradient$midpoint,
        guide = "none"  # Hide separate color legend
    ) +
    
    # Vertical dashed line at zero
    geom_vline(
        xintercept = 0, 
        linetype = "dashed", 
        color = "black", 
        size = 0.8
    ) +
    
    # X-axis formatting
    scale_x_continuous(
        limits = c(min_value, max_value),
        breaks = if (abs(min_value) < 0.011) {
            c(0, max_value)  # Only keep 0 and max_value if min_value is too close to 0
        } else {
            sort(unique(c(0, min_value, max_value)))  # Otherwise, keep all three
        },
        labels = if (abs(min_value) < 0.01) {
            c("0", round(max_value, 2))  # Only label 0 and max_value
        } else {
            sort(unique(c("0", round(min_value, 2), round(max_value, 2))))  # Label all if min_value is far enough
        }
    )+
    
    # Labels and theme
    labs(x = color_label, y = "") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),  # Remove all panel grids
        axis.line.y = element_line(color = "black", size = 0.5),  # Add y-axis line
        axis.line.x = element_line(color = "black", size = 0.5),  # Add y-axis line
        axis.text = element_text(size = 8, color = "black"),
        axis.text.x = element_text(margin = margin(t = 3, b = 5)),
        axis.title = element_text(size = 10, color = "black"),
        plot.margin = margin(1, 1, 1, 1),
        legend.position = "none"  # Hide the legend
    )



# Convert the histogram into a grob (graphical object) for insetting
hist_grob <- ggplotGrob(histogram_legend)

# Combine the map and the inset histogram
final_plot <- map_plot +
    annotation_custom(
        grob = hist_grob,
        xmin = -115, xmax = -88,   # Adjust to position in the map
        ymin = 32, ymax = 48       # Adjust to position in the map
    )

# Return the final combined plot
return(final_plot)
}

# Run the function with your data
p1 <- plot_variable_map(
    data = map_data,
    coords = coords,
    lon_adjustments = c("kac" = -114, "job" = -90),
    lat_adjustments = c("kac" = 24, "job" = 25),
    variable_name = "po4f_median",
    color_gradient = list(low = "orange", mid = "#7f8fff", high = "red", midpoint = 0),
    #title = "Trends in Dissolved Oxygen",
    #subtitle = "Filled circles indicate p < 0.05",
    color_label =  bquote("Median"~PO[4] ~ "(mg/L)")
)



p1


# Run the function with your data
p2 <- plot_variable_map(
    data = map_data,
    coords = coords,
    lon_adjustments = c("kac" = -114, "job" = -90),
    lat_adjustments = c("kac" = 24, "job" = 25),
    variable_name = "nh4f_median",
    color_gradient = list(low = "orange", mid = "#7f8fff", high = "red", midpoint = 0),
    #title = "Trends in Dissolved Oxygen",
    #subtitle = "Filled circles indicate p < 0.05",
    color_label =  bquote("Median"~NH[4] ~ "(mg/L)")
)



p2

# Run the function with your data
p3 <- plot_variable_map(
    data = map_data,
    coords = coords,
    lon_adjustments = c("kac" = -114, "job" = -90),
    lat_adjustments = c("kac" = 24, "job" = 25),
    variable_name = "no23f_median",
    color_gradient = list(low = "orange", mid = "#7f8fff", high = "red", midpoint = 0),
    #title = "Trends in Dissolved Oxygen",
    #subtitle = "Filled circles indicate p < 0.05",
    color_label <- bquote("Median" ~ NO[23] ~ "(mg/L)")
    
)



p3


# Run the function with your data
p4 <- plot_variable_map(
    data = map_data,
    coords = coords,
    lon_adjustments = c("kac" = -114, "job" = -90),
    lat_adjustments = c("kac" = 24, "job" = 25),
    variable_name = "chla_median",
    color_gradient = list(low = "orange", mid = "#7f8fff", high = "red", midpoint = 0),
    #title = "Trends in Dissolved Oxygen",
    #subtitle = "Filled circles indicate p < 0.05",
    color_label = "Median chl-a (µg/L)"
)



p4




four_panel_plot <- (p1 | p2) /
    (p3   | p4)

four_panel_plot 

ggsave("Median_Nutrients_plot.png", four_panel_plot, width = 6, height = 4, dpi = 600, bg = "white")
