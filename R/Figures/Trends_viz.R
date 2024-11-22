# Load necessary libraries
library(dplyr)
library(viridis)
library(ggfortify)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ggridges)

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

unique(all_trends$parameter)

subset_trends0 <- subset_trends %>%
    filter(!parameter %in% c("dailyPAR_median", "do_pct_median",
                             "do_proportion_below5","sal_median"))

subset_trends <- subset_trends %>%
  filter(!parameter %in% c("dailyPAR_median", "do_pct_median",
                           "do_proportion_below5","sal_median","nh4f_mdl",
                           "po4f_mdl","no23f_mdl","chla_n","atemp_median"))

subset_nut_trends <- nut_trends %>%
  mutate(station = substr(station, 1, 5))


merged_df <- subset_trends0 %>%
  inner_join(cluster, by = "station")

merged_nut_df <- subset_nut_trends %>%
  inner_join(cluster, by = "station")

######################

remove_stns <- c("pdbgd", "kachd", "kacsd", "sfbfm","lksbl", "lksol", "lksba", "lkspo")

# weed out the problem stations - too deep, or missing trend(s)  
subset_trends <- subset_trends |> 
    filter(!(station %in% remove_stns))

subset_nut_trends <- subset_nut_trends |> 
    filter(!(station %in% remove_stns))

merged_df <- merged_df |> 
    filter(!(station %in% remove_stns))

merged_nut_df <- merged_nut_df |> 
    filter(!(station %in% remove_stns))




# General slope visualization
colors <- viridis::viridis(5)
custom_colors <- colors[c(1, 3, 4, 5)]

# Create a named vector for renaming facets
facet_labels <- c(
    "do_mgl_median" = "Dissolved Oxygen (mg/L)",
    "do_proportion_below2" = "DO Proportion Below 2 mg/L",
    "spcond_median" = "Specific Conductivity (mS/cm)",
    "chla_n" = "Chlorophyll-a (ug/L)",
    "nh4f_mdl" = "Ammonium (mg/L)",
    "sqrt_precp_total" = "Sqrt Precipitation (mm)",
    "temp_median" = "Temperature (°C)",
    "turb_median" = "Turbidity (NTU)",
    "no23f_mdl" = "Nitrate + Nitrite (mg/L)",
    "po4f_mdl" = "Phosphate (mg/L)"
)


plot1 <- ggplot() +
    geom_histogram(data = subset_trends, aes(x = Slope, fill = sig_trend), color = "black") +
    facet_wrap(parameter ~ ., scales = "free", labeller = labeller(parameter = facet_labels)) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    scale_fill_manual(values = custom_colors) +
    theme_minimal() +
    theme(
        strip.text = element_text(size = 10, face = "bold", color = "black"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 9, color = "black"),
        legend.title = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 9, color = "black")
    ) +
    labs(
        x = "Slope (Unit per Year)",
        y = "Count",
        fill = "Significant Trend (p<0.05)"
    )

subset_nut_trends$sig_trend<-ifelse(subset_nut_trends$p_trend<0.05,"yes","no")

# Update the second plot with black text
plot2 <- ggplot() +
    geom_histogram(data = subset_nut_trends, aes(x = trend_pctPerYear, fill = sig_trend), color = "black") +
    facet_wrap(param ~ ., scales = "free", labeller = labeller(param = facet_labels)) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    scale_fill_manual(values = custom_colors) + ylab ("")+
    theme_minimal() +
    theme(
        strip.text = element_text(size = 10, face = "bold", color = "black"),
        axis.title = element_text(size = 10, color = "black"),
        axis.text = element_text(size = 9, color = "black"),
        legend.title = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 9, color = "black")
    ) +
    labs(
        x = "Trend (% per Year)",
        y = "Count",
        fill = "Significant Trend (p<0.05)"
    )

# Combine the plots
combined_plot <- plot1 + plot2 +
    plot_layout(guides = "collect", widths = c(2, 1)) & 
    theme(
        legend.position = "bottom",
        text = element_text(color = "black")
    )

# Display the combined plot
combined_plot

###########################################
##Slope distributions by cluster

# Define cluster colors
cluster_colors <- viridis::viridis(4)
names(cluster_colors) <- c("A", "B", "C", "D")

# Chlorophyll-a (Chla) trends
# Filter and merge data for Chla trends
chla_trend_log <- merged_df %>%
    filter(parameter == "chla_n")

chla_sig <- chla_trend_log %>%
    select(station, sig_trend)

chla_trend_pct <- merged_nut_df %>%
    filter(param == "chla_n")

merged_chla <- chla_trend_pct %>%
    inner_join(chla_sig, by = "station")

# Order data by cluster and slope
ordered_chla <- merged_chla %>%
    arrange(cluster, desc(trend_pctPerYear)) %>%
    mutate(
        station = factor(station, levels = unique(station)),
        cluster = factor(cluster, levels = rev(unique(cluster)))
    )

# Plot Chla slopes with confidence intervals by station
p1 <- ggplot(ordered_chla, aes(x = trend_pctPerYear, y = reorder(station, trend_pctPerYear), color = cluster)) +
    geom_point(aes(shape = sig_trend), size = 2) +
    geom_segment(aes(x = ciLow_pctPerYear, xend = ciHigh_pctPerYear, y = station, yend = station, linetype = sig_trend), linewidth = 1) +
    labs(title = "Chla Slopes with Confidence Intervals by Station", x = "Trend (% per Year)", y = "Station") +
    theme_minimal() +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_color_manual(values = cluster_colors)

# Ridge plot of Chla trends by cluster
p3 <- ggplot(ordered_chla, aes(x = trend_pctPerYear, y = cluster, fill = cluster)) +
    geom_density_ridges(scale = 2, alpha = 0.7) +
    labs(x = "Chla Trend (% per Year)", y = "Cluster") +
    scale_fill_manual(values = cluster_colors) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    theme_bw() +
    theme(
        text = element_text(color = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none"
    )

# Combine Chla plots
combined_plot1 <- (p1 | p3) +
    plot_layout(guides = "collect") & 
    theme(legend.position = 'bottom')

# Dissolved Oxygen (DO) trends
# Filter and order DO data
do_trend <- merged_df %>%
    filter(parameter == "do_mgl_median") %>%
    arrange(Slope) %>%
    mutate(
        station = factor(station, levels = unique(station)),
        cluster = factor(cluster, levels = rev(unique(cluster)))
    )

# Plot DO slopes with confidence intervals by station
p4 <- ggplot(do_trend, aes(x = Slope, y = station, color = cluster)) +
    geom_point(aes(shape = sig_trend), size = 2) +
    geom_segment(aes(x = conf.low, xend = conf.high, y = station, yend = station, linetype = sig_trend), linewidth = 1) +
    labs(title = "DO Slopes with Confidence Intervals by Station", x = "DO Trend (mg/L per Year)", y = "Station") +
    theme_minimal() +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_color_manual(values = cluster_colors) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1)

# Ridge plot of DO trends by cluster
p5 <- ggplot(do_trend, aes(x = Slope, y = cluster, fill = cluster)) +
    geom_density_ridges(scale = 2, alpha = 0.7) +
    labs(x = "DO Trend (mg/L per Year)", y = "Cluster") +
    scale_fill_manual(values = cluster_colors) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    theme_bw() +
    theme(
        text = element_text(color = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none"
    )

# Combine DO plots
patchwork_DO <- (p4 | p5) +
    plot_layout(guides = "collect") & 
    theme(legend.position = 'bottom')

# Combine Chla and DO ridge plots
patchwork_DOChla_Ridge <- (p3 | p5) +
    plot_layout(guides = "collect") & 
    theme(legend.position = 'none')

# Display combined plots
combined_plot1
patchwork_DO
patchwork_DOChla_Ridge


####################################
# Load necessary libraries
library(dplyr)
library(viridis)
library(ggfortify)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ggridges)

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

############################################################
#Chla Trends

chla_df <- combined_data[, c(1:4, 11, 55:61,71:75)]


# Scatter Plot Function
scatter_plot <- function(data, x_col, y_col, x_label, y_label, color_col = "cluster") {
    ggplot(data, aes_string(x = x_col, y = y_col, color = color_col)) +
        geom_point(size = 3) +
        scale_color_manual(values = cluster_colors) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        xlab(x_label) +
        ylab(y_label) +
        theme_minimal()
}

# Boxplot Function
boxplot_with_jitter <- function(data, x_col, y_col, x_label, y_label, color_col = "cluster") {
    ggplot(data, aes_string(x = x_col, y = y_col)) +
        geom_boxplot() +
        geom_jitter(aes_string(color = color_col), size = 3, width = 0.1) +
        scale_color_manual(values = cluster_colors) +
        scale_fill_manual(values = cluster_colors) +
        xlab(x_label) +
        ylab(y_label) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Annotated Scatter Plot Function
annotated_scatter_plot <- function(data, x_col, y_col, x_label, y_label, color_col = "cluster", annotations) {
    ggplot(data, aes_string(x = x_col, y = y_col, color = color_col)) +
        geom_point(size = 3) +
        scale_color_manual(values = cluster_colors) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        xlab(x_label) +
        ylab(y_label) +
        theme_minimal() +
        annotate("text", x = Inf, y = Inf, label = annotations[1], hjust = 1.1, vjust = 1.1) +
        annotate("text", x = -Inf, y = Inf, label = annotations[2], hjust = -0.1, vjust = 1.1) +
        annotate("text", x = -Inf, y = -Inf, label = annotations[3], hjust = -0.1, vjust = -0.1) +
        annotate("text", x = Inf, y = -Inf, label = annotations[4], hjust = 1.1, vjust = -0.1)
}


scatter_trend_median_plot <- function(data, x_col, y_col, x_label, y_label, color_col = "cluster", smooth = FALSE) {
    p <- ggplot(data, aes_string(x = x_col, y = y_col, color = color_col)) +
        geom_point(size = 3) +
        scale_color_manual(values = cluster_colors) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        xlab(x_label) +
        ylab(y_label) +
        theme_minimal()
    if (smooth) {
        p <- p + geom_smooth(method = "gam")
    }
    return(p)
}

scatter_vars <- list(
    list(x = "chla_median", y = "chla_n", x_label = "Median Chla (ug/L)", y_label = "Chla Trend (%/yr)"),
    list(x = "spcond_median", y = "chla_n", x_label = "Median SpCond (mS/cm)", y_label = "Chla Trend (%/yr)"),
    list(x = "turb_median", y = "chla_n", x_label = "Median Turbidity (NTU)", y_label = "Chla Trend (%/yr)"),
    list(x = "domgl_median", y = "chla_n", x_label = "Median DO (mg/L)", y_label = "Chla Trend (%/yr)"),
    list(x = "temp_median", y = "chla_n", x_label = "Median Temp (°C)", y_label = "Chla Trend (%/yr)"),
    list(x = "po4f_median", y = "chla_n", x_label = "Median PO4 (ug/L)", y_label = "Chla Trend (%/yr)"),
    list(x = "no23f_median", y = "chla_n", x_label = "Median NO23 (ug/L)", y_label = "Chla Trend (%/yr)"),
    list(x = "nh4f_median", y = "chla_n", x_label = "Median NH4 (ug/L)", y_label = "Chla Trend (%/yr)"),
    list(x = "precp_median", y = "chla_n", x_label = "Median Precipitation", y_label = "Chla Trend (%/yr)"),
    list(x = "chla_median", y = "domgl_trend", x_label = "Median Chla (ug/L)", y_label = "DO Trend (mg/L/yr)"),
    list(x = "temp_median", y = "domgl_trend", x_label = "Median Temp (°C)", y_label = "DO Trend (mg/L/yr)")
)

for (vars in scatter_vars) {
    print(scatter_trend_median_plot(combined_data, vars$x, vars$y, vars$x_label, vars$y_label, smooth = vars$smooth %||% FALSE))
}

boxplot_vars <- c(
    "TidalFlowType", "AquaticSystem", "Ecoregion",
    "SalinityRegime", "TidalRegime", "PrimaryWaterSource", "NERR_BioRegion"
)

# For both Chla and DO trends
for (x_var in boxplot_vars) {
    print(boxplot_with_jitter(combined_data, x_var, "chla_n", x_var, "Chla Trend (%/yr)"))
    print(boxplot_with_jitter(combined_data, x_var, "domgl_trend", x_var, "DO Trend (mg/L/yr)"))
}

annotated_comparisons <- list(
    list(
        x = "po4f_mdl", y = "chla_n",
        x_label = "PO4 Trend (%/yr)", y_label = "Chla Trend (%/yr)",
        annotations = c("+ PO4 & Chla", "- PO4, + Chla", "- PO4 & Chla", "+ PO4, - Chla")
    ),
    list(
        x = "nh4f_mdl", y = "chla_n",
        x_label = "NH4 Trend (%/yr)", y_label = "Chla Trend (%/yr)",
        annotations = c("+ NH4 & Chla", "- NH4, + Chla", "- NH4 & Chla", "+ NH4, - Chla")
    ),
    list(
        x = "turb_trend", y = "chla_n",
        x_label = "Turbidity Trend (NTU/yr)", y_label = "Chla Trend (%/yr)",
        annotations = c("+ Turb & Chla", "- Turb, + Chla", "- Turb & Chla", "+ Turb, - Chla")
    ),
    list(
        x = "chla_trend", y = "domgl_trend",
        x_label = "Chla Trend (%/yr)", y_label = "DO Trend (mg/L/yr)",
        annotations = c("+ Chla & DO", "- Chla, + DO", "- Chla & DO", "+ Chla, - DO")
    ),
    list(
        x = "temp_trend", y = "domgl_trend",
        x_label = "Temp Trend (°C/yr)", y_label = "DO Trend (mg/L/yr)",
        annotations = c("+ Temp & DO", "- Temp, + DO", "- Temp & DO", "+ Temp, - DO")
    )
)

for (vars in annotated_comparisons) {
    print(annotated_scatter_plot(combined_data, vars$x, vars$y, vars$x_label, vars$y_label, annotations = vars$annotations))
}


nutrient_comparisons <- list(
    list(
        x = "po4f_mdl_trend", y = "nh4f_mdl_trend",
        x_label = "PO4 Trend (%/yr)", y_label = "NH4 Trend (%/yr)",
        annotations = c("+ PO4 & NH4", "- PO4, + NH4", "- PO4 & NH4", "+ PO4, - NH4")
    ),
    list(
        x = "po4f_mdl_trend", y = "no23f_mdl_trend",
        x_label = "PO4 Trend (%/yr)", y_label = "NO23 Trend (%/yr)",
        annotations = c("+ PO4 & NO23", "- PO4, + NO23", "- PO4 & NO23", "+ PO4, - NO23")
    ),
    list(
        x = "no23f_mdl_trend", y = "nh4f_mdl_trend",
        x_label = "NO23 Trend (%/yr)", y_label = "NH4 Trend (%/yr)",
        annotations = c("+ NO23 & NH4", "- NO23, + NH4", "- NO23 & NH4", "+ NO23, - NH4")
    )
)

for (vars in nutrient_comparisons) {
    print(annotated_scatter_plot(combined_data, vars$x, vars$y, vars$x_label, vars$y_label, annotations = vars$annotations))
}

