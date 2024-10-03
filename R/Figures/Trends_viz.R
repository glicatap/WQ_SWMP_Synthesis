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

# Histogram of slopes
plot1 <- ggplot() +
  geom_histogram(data = subset_trends, aes(x = Slope, fill = sig_trend), color = "black") +
  facet_wrap(parameter ~ ., scales = "free") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()

plot1

subset_nut_trends$sig_trend<-ifelse(subset_nut_trends$p_trend<0.05,"yes","no")

plot2 <- ggplot() +
    geom_histogram(data = subset_nut_trends, aes(x = trend_pctPerYear, fill = sig_trend), color = "black") +
    facet_wrap(param ~ ., scales = "free") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
    scale_fill_manual(values = custom_colors) +
    theme_minimal()

plot2

# Combine plots with the first plot being larger
combined_plot <- plot1 + plot2 +
    plot_layout(guides = "collect", widths = c(2, 1)) & 
    theme(legend.position = "bottom")

# Display the combined plot
combined_plot


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

# Order data by cluster and then by slope in descending order
ordered_chla <- merged_chla %>%
  arrange(cluster, desc(trend_pctPerYear))

# Plot slopes with confidence intervals by station
p1 <- ggplot(ordered_chla, aes(x = trend_pctPerYear, y = reorder(station, trend_pctPerYear), color = cluster)) +
  geom_point(aes(shape=sig_trend),size=2) +
  geom_segment(aes(x = ciLow_pctPerYear, xend = ciHigh_pctPerYear, y = station, yend = station,linetype=sig_trend), linewidth = 1) +
  labs(title = "Chla slopes with Confidence Intervals by Station", x = "Trend Percentage per Year", y = "Station") +
  theme_minimal() +
  scale_linetype_manual(values = c("dashed","solid"))+
  scale_color_manual(values = cluster_colors)

p1

# Reorder the 'station' factor based on the sorted data
ordered_chla$station <- factor(ordered_chla$station, levels = unique(ordered_chla$station))

# Plot chla slopes with confidence intervals by station, with facets
p2 <- ggplot(ordered_chla, aes(x = trend_pctPerYear, y = station, color = cluster)) +
  geom_point(aes(shape = sig_trend), size = 2) +
  geom_segment(aes(x = ciLow_pctPerYear, xend = ciHigh_pctPerYear, y = station, yend = station, linetype = sig_trend), linewidth = 1) +
  labs(title = "Chla slopes with Confidence Intervals by Station", x = "Chla trend Percentage per Year", y = "Station") +
  theme_minimal() +
  scale_linetype_manual(values = c("solid","dashed"))+
  geom_vline(xintercept=0, color="black", linetype="dashed", size=1) +
  scale_color_manual(values = cluster_colors)#+facet_wrap(~cluster,scales="free_y")

ordered_chla$cluster <- 
    factor(ordered_chla$cluster, levels = rev(levels(factor(ordered_chla$cluster))))


# Ridge plot of chlorophyll-a trends by cluster
p3 <- ggplot(ordered_chla, aes(x = trend_pctPerYear, y = cluster, fill = cluster)) +
  geom_density_ridges(scale = 2, alpha = 0.7) +
  theme(legend.position = "none") +
  ylab("Cluster") +
  xlab("Chl-a Trend Percentage per Year") +
  scale_fill_manual(values = cluster_colors) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
  theme(strip.background = element_rect(fill = "lightgrey", color = "black"))

p3

# Combine plots for chlorophyll-a trends
combined_plot1 <- (p1 | p3) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')


combined_plot1

# Filter and merge data for dissolved oxygen trends
do_trend <- merged_df %>%
  filter(parameter == "do_mgl_median")

# Order data by cluster and then by slope in descending order
ordered_do <- do_trend %>%
  arrange(Slope)

# Reorder the 'station' factor based on the sorted data
ordered_do$station <- factor(ordered_do$station, levels = unique(ordered_do$station))

ordered_do$cluster <- 
    factor(ordered_do$cluster, levels = rev(levels(factor(ordered_do$cluster))))


# Plot DO slopes with confidence intervals by station, with facets
p4<-ggplot(ordered_do, aes(x = Slope, y = station, color = cluster)) +
  geom_point(aes(shape=sig_trend),size=2) +  # Plot points  
  geom_segment(aes(x = conf.low, xend = conf.high, y = station, yend = station,linetype=sig_trend), linewidth=1) +  # Confidence intervals
  labs(title = "Slopes with Confidence Intervals by Station", x = "DO trend mg/L per year", y = "Station") +
  theme_minimal() +
  scale_linetype_manual(values = c("dashed","solid"))+
  geom_vline(xintercept=0, color="black", linetype="dashed", size=1) +
  scale_color_manual(values = cluster_colors)#+facet_wrap(~cluster,scales="free_y")

p4

# Plot slopes with confidence intervals by station for dissolved oxygen (DO)
p5 <- ggplot(ordered_do, aes(x = Slope, y = cluster, fill = cluster)) + 
  geom_density_ridges(scale = 2, alpha = 0.7) +
  theme(legend.position = "none") +
  ylab("Cluster") +
  xlab("DO trend mg/L per year") +
  scale_fill_manual(values = cluster_colors) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 1) +
  theme(strip.background = element_rect(fill = "lightgrey", color = "black"))

p5

patchwork_DO <- (p4 |p5) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

patchwork_DO


patchwork_DOChla_Ridge <- (p3 /p5) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

patchwork_DOChla_Ridge
