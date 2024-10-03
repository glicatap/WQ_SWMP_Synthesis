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

#Remove LKS Stations
remove_stns <- c("pdbgd", "kachd", "kacsd", "sfbfm","lksbl", "lksol", "lksba", "lkspo")


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

subset_nut_trends <- subset_nut_trends |> 
    filter(!(station %in% remove_stns))

subset_trends <- subset_trends |> 
    filter(!(station %in% remove_stns))

merged_df <- subset_trends %>%
    inner_join(cluster, by = "station")

merged_nut_df <- subset_nut_trends %>%
    inner_join(cluster, by = "station")


# Count occurrences where std.error exceeds Slope
count_results_do <- all_trends %>%
    filter(parameter == "temp_median", sig_trend == "yes", Slope > 0) %>%
    count()


# Count occurrences where std.error exceeds Slope
count_results <- all_trends %>%
    filter(!is.na(std.error) & !is.na(Slope)) %>%
    mutate(significance = if_else(sig_trend == 'yes', "Significant", "Non-significant"),
           std_exceeds_slope = if_else(std.error > abs(Slope), "Exceeds", "Does not exceed")) %>%
    count(parameter, significance, std_exceeds_slope)

count_results_wide <- count_results %>%
    pivot_wider(names_from = std_exceeds_slope, values_from = n, values_fill = 0)


# Plot counts of occurrences where std.error exceeds Slope
plot1 <- ggplot(count_results, aes(x = parameter, y = n, fill = std_exceeds_slope)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ significance) +
    labs(title = "Count of Occurrences where std.error Exceeds Slope",
         x = "Parameter",
         y = "Count",
         fill = "Std. Error vs Slope") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot1

# General slope visualization
colors <- viridis::viridis(5)
custom_colors <- colors[c(1, 3, 4, 5)]

# Histogram of slopes
plot2 <- ggplot() +
    geom_histogram(data = subset_trends, aes(x = Slope, fill = sig_trend), color = "black") +
    facet_wrap(parameter ~ ., scales = "free") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    scale_fill_manual(values = custom_colors) +
    theme_minimal()

plot2

# Scatter plot of slopes vs std.error
plot3 <- ggplot() +
    geom_point(data = subset_trends, aes(x = Slope, y = std.error, color = sig_trend)) +
    facet_wrap(parameter ~ ., scales = "free") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    scale_color_manual(values = custom_colors) +
    theme_minimal()

plot3

# Scatter plot of slopes vs std.error
plot4.1 <- ggplot() +
    geom_point(data = subset_trends, aes(x = Slope, y = std.error, color = reserve, shape=sig_trend), size = 2) +
    geom_text(data = subset_trends, aes(x = Slope, y = std.error, label = station, color = reserve), 
              vjust = 1, hjust = 0.5, size = 3, check_overlap = TRUE) + 
    facet_wrap(parameter ~ ., scales = "free") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    #scale_color_manual(values = custom_colors) +
    theme_minimal() +
    guides(color = "none", shape = guide_legend(override.aes = list(color = "black"))) +  # Remove color legend, keep shape
    theme(legend.position = "bottom")

plot4.1

nonsig<- subset_trends %>%
    filter(sig_trend =="no")


# Scatter plot of slopes vs std.error
plot4.1.1 <- ggplot() +
    geom_point(data = nonsig, aes(x = Slope, y = std.error, color = reserve, shape=sig_trend), size = 2) +
    geom_text(data = nonsig, aes(x = Slope, y = std.error, label = station, color = reserve), 
              vjust = 1, hjust = 0.5, size = 3, check_overlap = TRUE) + 
    facet_wrap(parameter ~ ., scales = "free") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    #scale_color_manual(values = custom_colors) +
    theme_minimal() +
    guides(color = "none", shape = guide_legend(override.aes = list(color = "black"))) +  # Remove color legend, keep shape
    theme(legend.position = "bottom")

plot4.1.1

# Scatter plot of slopes vs std.error
plot4.2 <- ggplot() +
    geom_point(data = subset_trends, aes(x = Slope, y = std.error, color = reserve)) +
    facet_wrap(parameter ~ ., scales = "free") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    theme_minimal()

plot4.2


plot4.3 <- ggplot() +
    geom_boxplot(data = subset_trends, aes(x = reserve, y = std.error, color = reserve)) +
    geom_point(data = subset_trends, aes(x = reserve, y = std.error, color = reserve)) +
    facet_wrap(parameter ~ ., scales = "free") +
    # geom_text(data = subset_trends, aes(x = Slope, y = ratio_SE_Slope, label = station, color = reserve), 
    #          vjust = -1, hjust = 0.5, size = 3, check_overlap = TRUE) + 
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme(legend.position = 'none')

plot4.3+ggtitle("SE for Slope by Parameter")


plot4.4 <- ggplot() +
    geom_point(data = subset_trends, aes(x = reserve, y = ratio_SE_Slope, color = reserve)) +
    geom_boxplot(data = subset_trends, aes(x = reserve, y = ratio_SE_Slope, color = reserve)) +
    facet_wrap(parameter ~ ., scales = "free") +
    geom_text(data = subset_trends, aes(x = Slope, y = ratio_SE_Slope, label = station, color = reserve), 
              vjust = -1, hjust = 0.5, size = 3, check_overlap = TRUE) + 
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme(legend.position = 'none')

plot4.4+ggtitle("Ratio SE to Slope by Parameter")


plot4.5 <- ggplot() +
    geom_point(data = subset_trends, aes(x = Slope, y = ratio_SE_Slope, color = reserve), size = 2) +
    geom_text(data = subset_trends, aes(x = Slope, y = ratio_SE_Slope, label = station, color = reserve), 
              vjust = 1, hjust = 0.5, size = 3, check_overlap = TRUE) + 
    facet_wrap(parameter ~ ., scales = "free") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    geom_hline(yintercept = 1, color = "black", linetype = "dashed", size = 1) +
    #scale_color_manual(values = custom_colors) +
    theme_minimal()

plot4.5+ggtitle("Ratio SE to Slope by Parameter vs Slope value")


# Scatter plot of slopes vs std.error
plot5 <- ggplot() +
    geom_point(data = all_trends, aes(x = Slope, y = std.error, color = sig_trend)) +
    facet_wrap(parameter ~ ., scales = "free") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    scale_color_manual(values = custom_colors) +
    theme_minimal()

plot5



#####################################

percentiles <- subset_trends %>%
    group_by(parameter) %>%
    summarize(p95 = quantile(std.error, 0.95, na.rm = TRUE))

# Join with the original dataframe to filter out rows
subset_trends_SE <- subset_trends %>%
    inner_join(percentiles, by = "parameter") %>%
    filter(std.error <= p95) %>%
    select(-p95)

# To get the removed data, use anti_join
removed_data <- subset_trends %>%
    inner_join(percentiles, by = "parameter") %>%
    anti_join(subset_trends_SE, by = names(subset_trends_SE))

unique(removed_data$station)

unique_reserves <- length((removed_data$station))
unique_reserves


# Scatter plot of slopes vs std.error
plot4.2 <- ggplot() +
    geom_point(data = subset_trends_SE, aes(x = Slope, y = std.error, color = sig_trend)) +
    facet_wrap(parameter ~ ., scales = "free") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    scale_color_manual(values = custom_colors) +
    theme_minimal()

plot4.2

plot4.3<-ggplot(subset_trends, aes(x = Slope, y = sig_trend,fill=sig_trend)) + 
    geom_density_ridges(scale = 2, alpha = 0.7)+theme(legend.position="none")+facet_wrap(~parameter, scales="free")+
    scale_fill_viridis_d(option = "D") + 
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    theme( strip.background = element_rect(fill = "lightgrey", color = "black"))

plot4.3


########################

# Step 1: Filter the data where sig_trend == "no"
filtered_data <- subset_trends %>%
    filter(sig_trend == "no")

# Step 2: Calculate the standard deviation of slopes for each parameter
slope_stats <- filtered_data %>%
    group_by(parameter) %>%
    summarise(sd_slope = sd(Slope, na.rm = TRUE))

# View the calculated standard deviations
print(slope_stats)

# Step 3: Merge the SD information back with the original filtered data
slopes_with_sd <- slope_stats %>%
    inner_join(filtered_data, by = "parameter")

# Step 3: Identify slopes more than 2 SD away from 0
slopes_more_than_2SD <- filtered_data %>%
    inner_join(slope_stats, by = "parameter") %>%
    mutate(is_outlier = abs(Slope) > 2 * sd_slope)

slopes_more_than_2SD_data<- slopes_more_than_2SD %>%
    filter(is_outlier=="TRUE")

slopes_2SD_data <- slopes_more_than_2SD_data %>%
    select('reserve', 'station', 'parameter','Slope','std.error') 

write.csv(slopes_2SD_data,"slopes_2SD_data.csv")

unique(slopes_more_than_2SD_data$station)

length(unique(slopes_more_than_2SD_data$station))


plot4.4 <- ggplot() +
    geom_point(data = subset_trends %>% filter(!is.na(sig_trend)), aes(x = Slope, y = std.error, color = sig_trend)) +
    geom_point(data = slopes_more_than_2SD_data, aes(x = Slope, y = std.error), color = "red", size = 2) +
    facet_wrap(parameter ~ ., scales = "free") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    scale_color_manual(values = custom_colors) +
    theme_minimal()


plot4.4+ggtitle("NS slopes more than 2 SD from zero")


slopes_2SD_data_weights<-read.csv("slopes_2SD_data_weights.csv")


plot4.4 <- ggplot() +
    geom_point(data = subset_trends %>% filter(!is.na(sig_trend)), aes(x = Slope, y = std.error, color = sig_trend)) +
    geom_point(data = slopes_more_than_2SD_data, aes(x = Slope, y = std.error), color = "red", size = 2) +
    geom_point(data = slopes_2SD_data_weights, aes(x = Slope, y = std.error), color = "green", size = 2) +
    facet_wrap(parameter ~ ., scales = "free") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    scale_color_manual(values = custom_colors) +
    theme_minimal()


plot4.4+ggtitle("NS slopes more than 2 SD from zero divided by SE")


###################
#Drop LKS and MAR


plot4.5 <- ggplot() +
    geom_point(data = slopes_more_than_2SD_data, aes(x = station, y = p.value), size = 2) +
    facet_wrap(parameter ~ ., scales = "free") +
    geom_hline(yintercept = 0.2, color = "black", linetype = "dashed", size = 1) +
    scale_color_manual(values = custom_colors) +
    theme_minimal()

plot4.5



