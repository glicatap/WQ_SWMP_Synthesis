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

# Read data
cluster <- read.csv("Clusters.csv")

NP_stations <- read.csv("NP_stations.csv")

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

# Get US states shapefile and shift geometry
us_sf <- states(cb = TRUE, resolution = "20m") %>%
    shift_geometry()

# Read coordinates data
coords <- read.csv("coords.csv", header = TRUE)
coords <- coords %>%
  mutate(station = tolower(station)) %>%
  mutate(station = substr(station, 1, 5))

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
    
    # Merge trends with coordinates
    map_data <- data %>%
        left_join(coords, by = "station")
    
    # Apply manual coordinate adjustments
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
        geom_sf(data = us_sf, fill = "lightgray", color = "white") +
        geom_sf(
            data = df_sf_jittered,
            aes(color = !!sym(variable_name), shape = as.factor(sig_trend)),
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
        scale_shape_manual(
            values = c("yes" = 16, "no" = 21),
            guide = "none"
        ) +
        theme_minimal() +
        theme(
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            #plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5),
            #plot.subtitle = element_text(size = 12, face = "italic", color = "black", hjust = 0.5),
            legend.position = "none",  # Remove default legend
            axis.text = element_text(size = 10, color = "black"),
            axis.title = element_blank()
        ) +
        coord_sf(
            xlim = c(-130, -60),
            ylim = c(15, 51),
            expand = FALSE
        )
    
    histogram_legend <- ggplot(map_data, aes(x = !!sym(variable_name))) +
        
        # Background: "No" (white fill with gradient outline)
        geom_histogram(
            data = subset(map_data, as.factor(sig_trend) == "no"),
            aes(color = after_stat(x)),  # Gradient outline
            fill = "lightgray",
            bins = 10,
            size = 0.8
        ) +
        
        # Foreground: "Yes" (gradient fill with transparency)
        geom_histogram(
            data = subset(map_data, as.factor(sig_trend) == "yes"),
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
            breaks = sort(unique(c(0, seq(min_value, max_value, length.out = 2)))),  
            labels = sort(unique(c(0, round(seq(min_value, max_value, length.out = 2), 2))))
        ) +
        
        # Labels and theme
        labs(x = color_label, y = "") +
        theme_minimal() +
        theme(
            panel.grid = element_blank(),
            axis.text = element_text(size = 8, color = "black"),
            axis.title = element_text(size = 10, color = "black"),
            plot.margin = margin(1, 1, 1, 1),
            legend.position = "none"
        )
    
    
    # Convert the histogram into a grob (graphical object) for insetting
    hist_grob <- ggplotGrob(histogram_legend)
    
    # Combine the map and the inset histogram
    final_plot <- map_plot +
        annotation_custom(
            grob = hist_grob,
            xmin = -80, xmax = -61.5,   # Adjust to position in the map
            ymin = 16, ymax = 25       # Adjust to position in the map
        )
    
    # Return the final combined plot
    return(final_plot)
}

# Run the function with your data
p1 <- plot_variable_map(
    data = do_trend,
    coords = coords,
    lon_adjustments = c("kac" = -114, "job" = -90),
    lat_adjustments = c("kac" = 24, "job" = 25),
    variable_name = "Slope",
    color_gradient = list(low = "orange", mid = "#7f8fff", high = "blue", midpoint = 0),
    #title = "Trends in Dissolved Oxygen",
    #subtitle = "Filled circles indicate p < 0.05",
    color_label = "DO trend (mg/L/yr)"
)



p1

p2<-plot_variable_map(
    data = temp_trend,
    coords = coords,
    lon_adjustments = c("kac" = -114, "job" = -90),
    lat_adjustments = c("kac" = 24, "job" = 25),
    variable_name = "Slope",
    color_gradient = list(low = "blue", mid = "#7f8fff", high = "red", midpoint = 0),
    #title = "Trends in Water Temperature",
    #subtitle = "Filled circles indicate p < 0.05",
    color_label = "Temperature trend (°C/yr)"
)

p2

p3<-plot_variable_map(
    data = merged_po4,
    coords = coords,
    lon_adjustments = c("kac" = -114, "job" = -90),
    lat_adjustments = c("kac" = 24, "job" = 25),
    variable_name = "trend_pctPerYear",
    color_gradient = list(low = "darkblue", mid = "#7f8fff", high = "red", midpoint = 0),
    #title = "Trends in Phosphorus (PO4)",
    #subtitle = "Filled circles indicate p < 0.05",
    color_label = "PO4 trend (%/yr)"
)

p3

p4<-plot_variable_map(
    data = merged_nh4,
    coords = coords,
    lon_adjustments = c("kac" = -114, "job" = -90),
    lat_adjustments = c("kac" = 24, "job" = 25),
    variable_name = "trend_pctPerYear",
    color_gradient = list(low = "darkblue", mid = "#7f8fff", high = "red", midpoint = 0),
    #title = "Trends in Ammonium (NH4)",
    #subtitle = "Filled circles indicate p < 0.05",
    color_label = "NH4 trend (%/yr)"
)

p4

p5<-plot_variable_map(
    data = merged_no23,
    coords = coords,
    lon_adjustments = c("kac" = -114, "job" = -90),
    lat_adjustments = c("kac" = 24, "job" = 25),
    variable_name = "trend_pctPerYear",
    color_gradient = list(low = "darkblue", mid = "#7f8fff", high = "red", midpoint = 0),
    #title = "Trends in Nitrate (NO23)",
    #subtitle = "Filled circles indicate p < 0.05",
    color_label = "NO23 trend (%/yr)"
)




p6<-plot_variable_map(
    data = merged_chla,
    coords = coords,
    lon_adjustments = c("kac" = -114, "job" = -90),
    lat_adjustments = c("kac" = 24, "job" = 25),
    variable_name = "trend_pctPerYear",
    color_gradient = list(low = "darkblue", mid = "#7f8fff", high = "green", midpoint = 0),
    #title = "Trends in Chlorophyll-a",
    #subtitle = "Filled circles indicate p < 0.05",
    color_label = "Chla trend (%/yr)"
)

p6

Nutrients<-p3+p4+p5
Nutrients

Response<-p1+p2+p6
Response
