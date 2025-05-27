# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(tigris)
library(viridis)
library(scatterpie)

# Define cluster colors using viridis
cluster_colors <- c("A" = viridis(4)[3],
                    "B" = viridis(4)[1],
                    "C" = viridis(4)[2],
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

p_v2 <- ggplot() +
    # Base map
    geom_sf(data = us_sf, fill = "gray93", color = "white") +
    
    # Scatter pies for clusters
    geom_scatterpie(
        data = d_wide,
        aes(x = lon_jittered, y = lat_jittered, group = Reserve, r = 1.25),  # Adjust radius for visibility
        cols = c("A", "B", "C", "D"),
        color = "black",
        alpha = 0.8
    ) +
    
    # Add Reserve labels
    geom_text(
        data = d_wide,
        aes(x = lon_jittered, y = lat_jittered, label = Reserve),
        size = 3,  # Label size
        nudge_y = 1.75  # Nudge labels upward to avoid overlap
    ) +
    
    # Define color scale for clusters
    scale_fill_manual(values = cluster_colors) +
    
    # Add titles and labels
    labs(
        fill = "Cluster"
    ) +
    
    # Minimal theme for clean appearance
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),        # Remove all gridlines
        axis.text = element_blank(),         # Remove lat/lon text
        axis.ticks = element_blank(),        # Remove tick marks
        axis.title = element_blank(),        # Remove axis titles
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)
    ) +
    
    # Set coordinate limits and aspect ratio
    coord_sf(
        xlim = c(-127, -65),
        ylim = c(18, 51),
        expand = FALSE
    ) +
    
    # Adjust legend settings
    guides(
        fill = guide_legend(
            title.position = "top",
            title.hjust = 0.5
        )
    )+theme(legend.position = c(0.9, 0.2)) # c(0,0) bottom left, c(1,1) top-right.

# Render the plot
p_v2

ggsave("Scatterpie.png", plot = p_v2, width = 8, height = 5, dpi = 600)



#######################################################################################


###PCA Plots

library(dplyr)
library(viridis)
library(ggplot2)
library(tidyr)
library(patchwork)

# Set custom cluster colors
cluster_colors <- c("A" = viridis(4)[3],
                    "B" = viridis(4)[1],
                    "C" = viridis(4)[2],
                    "D" = viridis(4)[4])

# Load data
cluster_data <- read.csv("swmp_clstr_med_pc_stations_spc.csv")

# Deduplicate variable names (example logic for variables)
variables <- c("SpCond", "Temp", "DO", "pH", "Turb", "Chla", "NH4", "NO23", "PO4")
variables <- make.unique(variables)  # Ensures unique variable names

# Perform PCA
pca_model <- prcomp(cluster_data[, variables], scale. = TRUE)

# Prepare PCA loadings
loadings <- as.data.frame(pca_model$rotation)
loadings$variable <- rownames(loadings)

# Function to compute convex hull for each cluster
compute_hull <- function(df) {
    df[chull(df$PC1, df$PC2), ]
}

hulls <- cluster_data %>% group_by(cluster) %>% do(compute_hull(.))

# PCA Plot 1 (PC1 vs PC2)
pca_1 <- ggplot(cluster_data, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.2, color = NA) +
    geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5), 
                 arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1) +
    geom_text(data = loadings, aes(x = PC1 * 5, y = PC2 * 5, label = variable), 
              hjust = 1.2, vjust = 1.2, color = "black", size = 3, fontface = "bold") +
    labs(x = "PC1 (32.4%)", y = "PC2 (25.9%)", color = "Cluster", fill = "Cluster") +
    theme_minimal(base_size = 11) +
    scale_color_manual(values = cluster_colors) +
    scale_fill_manual(values = cluster_colors) +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          panel.border = element_blank())

# Function for PC1 vs PC3
compute_hull2 <- function(df) {
    df[chull(df$PC1, df$PC3), ]
}

hulls2 <- cluster_data %>% group_by(cluster) %>% do(compute_hull2(.))

# PCA Plot 2 (PC1 vs PC3)
pca_2 <- ggplot(cluster_data, aes(x = PC1, y = PC3, color = cluster)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_polygon(data = hulls2, aes(x = PC1, y = PC3, fill = cluster), alpha = 0.2, color = NA) +
    geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 5, yend = -PC3 * 5), 
                 arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1) +
    geom_text(data = loadings, aes(x = PC1 * 5, y = -PC3 * 5, label = variable), 
              hjust = 1.2, vjust = 1.2, color = "black", size = 3, fontface = "bold") +
    labs(x = "PC1 (32.4%)", y = "PC3 (15.9%)", color = "Cluster", fill = "Cluster") +
    theme_minimal(base_size = 11) +
    scale_color_manual(values = cluster_colors) +
    scale_fill_manual(values = cluster_colors) +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          panel.border = element_blank())


pca_1 <- pca_1 +
    theme(
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
    )

pca_2 <- pca_2 +
    theme(
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
    )



combined_plot <- (pca_1 | pca_2) +
    plot_layout(guides = "collect") & 
    theme(
        legend.position = 'none',
        legend.title = element_blank(),
        panel.background = element_rect(fill = 'transparent', color = NA),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent')
    )


# Save as a high-resolution file
ggsave("PCA_Combined_Publication.png", plot = combined_plot, bg = "transparent", width = 6.5, height = 3, dpi = 300)



# PCA Plot 1 (PC1 vs PC2) with labels
pca_1_labeled <- ggplot(cluster_data, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_polygon(data = hulls, aes(x = PC1, y = PC2, fill = cluster), alpha = 0.2, color = NA) +
    geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5), 
                 arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1) +
    geom_text(data = loadings, aes(x = PC1 * 5, y = PC2 * 5, label = variable), 
              hjust = 1.2, vjust = 1.2, color = "black", size = 4.5, fontface = "bold") +
    geom_text(aes(label = code), hjust = 0, vjust = -0.5, size = 3) +  # Add labels
    labs(x = "PC1 (32.4%)", y = "PC2 (25.9%)", color = "Cluster", fill = "Cluster") +
    theme_minimal(base_size = 14) +
    scale_color_manual(values = cluster_colors) +
    scale_fill_manual(values = cluster_colors) +
    theme(legend.position = "none",
          panel.grid = element_blank())

# PCA Plot 2 (PC1 vs PC3) with labels
pca_2_labeled <- ggplot(cluster_data, aes(x = PC1, y = PC3, color = cluster)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_polygon(data = hulls2, aes(x = PC1, y = PC3, fill = cluster), alpha = 0.2, color = NA) +
    geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 5, yend = -PC3 * 5), 
                 arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1) +
    geom_text(data = loadings, aes(x = PC1 * 5, y = -PC3 * 5, label = variable), 
              hjust = 1.2, vjust = 1.2, color = "black", size = 4.5, fontface = "bold") +
    geom_text(aes(label = code), hjust = 0, vjust = -0.5, size = 3) +  # Add labels
    labs(x = "PC1 (32.4%)", y = "PC3 (15.9%)", color = "Cluster", fill = "Cluster") +
    theme_minimal(base_size = 14) +
    scale_color_manual(values = cluster_colors) +
    scale_fill_manual(values = cluster_colors) +
    theme(legend.position = "bottom",
          panel.grid = element_blank())

# Combine the two labeled plots
combined_plot_labeled <- (pca_1_labeled | pca_2_labeled) +
    plot_layout(guides = "collect") & 
    theme(legend.position = 'bottom', legend.title = element_blank())

# Save as a high-resolution file
ggsave("PCA_Combined_WithLabels_Publication.png", plot = combined_plot_labeled,  bg = "transparent", width = 14, height = 8, dpi = 300)

combined_plot_labeled

############################################################
#GGRIDGES of Medians
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggridges)
library(patchwork)
library(scales)

# Load and preprocess data
cluster_data <- read.csv("swmp_clstr_med_pc_stations_spc_2.csv") %>%
    select(1:12) %>%
    mutate(across(last_col(4):ncol(.), ~ 10^.)) %>%
    mutate(across(last_col(2):ncol(.), ~ .*1000))

long_data <- pivot_longer(
    cluster_data, 
    cols = starts_with(c("SpCond", "Temp", "DO", "pH", "Turb", "Chla", "NH4", "NO23", "PO4")),
    names_to = "Parameter",
    values_to = "Value"
) %>%
    rename(station = code, reserve = Map.code)

# Split data into two groups
long_data_wq <- long_data %>%
    filter(!Parameter %in% c("Chla", "NH4", "NO23", "PO4", "Turb"))

long_data_nutchla <- long_data %>%
    filter(Parameter %in% c("Chla", "NH4", "NO23", "PO4", "Turb"))

# Ensure consistent factor levels for clusters
long_data_wq$cluster <- factor(long_data_wq$cluster, levels = rev(levels(factor(long_data_wq$cluster))))
long_data_nutchla$cluster <- factor(long_data_nutchla$cluster, levels = rev(levels(factor(long_data_nutchla$cluster))))

# Define custom labeller with "Log()" for log-transformed parameters
log_label_labeller <- as_labeller(c(
    SpCond = "bold('SpCond'~(mS/cm))",
    Temp = "bold('Temperature'~(degree*C))",
    DO = "bold('DO'~(mg/L))",
    pH = "bold('pH')",
    Chla = "bold('Chl-a'~(mu*g/L))",
    NH4 = "bold('NH'[4]~(mg/L))",
    NO23 = "bold('NO'[23]~(mg/L))",
    PO4 = "bold('PO'[4]~(mg/L))",
    Turb = "bold('Turbidity'~(NTU))"
), default = label_parsed)


custom_theme <- theme_minimal() +
    theme(
        strip.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),  # White background with black border
       # strip.text = element_text(size = 10, face = "bold", color = "black"),  # Bold text for facet labels
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 9, color = "black"),
        legend.title = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 9, color = "black"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )


# Water quality plots
p1 <- ggplot(long_data_wq, aes(x = Value, y = cluster, fill = cluster)) + 
    geom_density_ridges(scale = 2, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(~ Parameter, scales = "free", labeller = log_label_labeller) +
    xlab("") + ylab("") +
    scale_fill_manual(values = cluster_colors) +
    scale_y_discrete(expand = expansion(add = c(0.2, 1.9))) +  # Add space above
    custom_theme 


# Nutrient and chlorophyll plots
p2 <- ggplot(long_data_nutchla, aes(x = Value, y = cluster, fill = cluster)) + 
    geom_density_ridges(scale = 2, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(~ Parameter, scales = "free", labeller = log_label_labeller) +
    xlab("") + ylab("") +
    scale_fill_manual(values = cluster_colors) +
    scale_y_discrete(expand = expansion(add = c(0.2, 1.9))) +  # Add space above
    custom_theme +scale_x_log10(labels = trans_format("log10", label_math()))



####
#violin plots

p1.2 <- ggplot(long_data_wq, aes(y = Value, x = factor(cluster, levels = rev(levels(factor(cluster)))), fill = cluster)) + 
    geom_violin() +
    facet_wrap(~ Parameter, scales = "free", labeller = log_label_labeller) +
    xlab("") + ylab("") +
    scale_fill_manual(values = cluster_colors, guide = guide_legend(reverse = TRUE)) +
    custom_theme

p2.2 <- ggplot(long_data_nutchla, aes(y = Value, x = factor(cluster, levels = rev(levels(factor(cluster)))), fill = cluster)) + 
    geom_violin() +
    facet_wrap(~ Parameter, scales = "free", labeller = log_label_labeller, ncol = 3) +
    ylab("") + xlab("") +
    scale_fill_manual(values = cluster_colors, guide = guide_legend(reverse = TRUE)) +
    scale_y_log10() +
    custom_theme 



pca_ridges <- p1 | p2 
#pca_violin <- p1.2 | p2.2 


# Display combined plots
print(pca_ridges)
#print(pca_violin)

ggsave("pca_ridges.png", plot = pca_ridges, width = 8, height = 4, dpi = 300)
#ggsave("pca_violin.png", plot = pca_violin, width = 10, height = 4, dpi = 300)



# Combine the two plots
combined_plot <- (pca_1 / pca_2) +
    plot_layout(guides = "collect") & 
    theme(legend.position = 'none', legend.title = element_blank())

library(patchwork)

pca_wridges <- wrap_elements(combined_plot) + pca_ridges + plot_layout(widths = c(1.5, 3))

pca_wridges

pca_wridges<-combined_plot|pca_ridges

pca_wridges

ggsave("pca_wridges.png", plot = pca_wridges, width = 10, height = 10, dpi = 300)


##################

library(dplyr)
library(readr)

# Summary stats for water quality parameters
summary_wq <- long_data_wq %>%
    group_by(Parameter, cluster) %>%
    summarise(
        count = n(),
        mean = mean(Value, na.rm = TRUE),
        sd = sd(Value, na.rm = TRUE),
        min = min(Value, na.rm = TRUE),
        q25 = quantile(Value, 0.25, na.rm = TRUE),
        median = median(Value, na.rm = TRUE),
        q75 = quantile(Value, 0.75, na.rm = TRUE),
        max = max(Value, na.rm = TRUE),
        .groups = "drop"
    )

# Summary stats for nutrient + Chl a parameters
summary_nutchla <- long_data_nutchla %>%
    group_by(Parameter, cluster) %>%
    summarise(
        count = n(),
        mean = mean(Value, na.rm = TRUE),
        sd = sd(Value, na.rm = TRUE),
        min = min(Value, na.rm = TRUE),
        q25 = quantile(Value, 0.25, na.rm = TRUE),
        median = median(Value, na.rm = TRUE),
        q75 = quantile(Value, 0.75, na.rm = TRUE),
        max = max(Value, na.rm = TRUE),
        .groups = "drop"
    )

# Export to CSV
write_csv(summary_wq, "summary_stats_wq_by_cluster.csv")
write_csv(summary_nutchla, "summary_stats_nutchla_by_cluster.csv")

