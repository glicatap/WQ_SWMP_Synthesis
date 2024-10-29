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
  theme(strip.background = element_rect(fill = "lightgrey", color = "black"))+
    theme_bw()

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
  theme(strip.background = element_rect(fill = "lightgrey", color = "black"))+
    theme_bw()

p5

patchwork_DO <- (p4 |p5) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

patchwork_DO


patchwork_DOChla_Ridge <- (p3 |p5) +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

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


chla_long <- chla_df %>%
    pivot_longer(
        cols = ends_with("pctTotal"), # Adjust based on your land use columns' naming pattern
        names_to = "landuse",
        values_to = "value"
    )

ggplot(chla_long)+
    geom_point(aes(x=chla_trend, y=value,color=cluster),size=3)+facet_wrap(~landuse)+
    scale_color_manual(values = cluster_colors)+theme_bw()+xlab("Chl-a Trend %/Yr")

chla_df_v2 <- combined_data[, c(1:4, 11, 62:67,71:75)]


chla_long_v2 <- chla_df_v2 %>%
    pivot_longer(
        cols = ends_with("pctLand"), # Adjust based on your land use columns' naming pattern
        names_to = "landuse",
        values_to = "value"
    )

ggplot(chla_long_v2)+
    geom_point(aes(x=chla_n, y=value,color=cluster),size=3)+facet_wrap(~landuse)+
    scale_color_manual(values = cluster_colors)+xlab("Chla Trend %/yr")+
    ylab("% Cover")+theme_bw()+xlab("Chl-a Trend %/Yr")


ggplot(combined_data,aes(x=TidalFlowType, y=chla_n))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("Chla Trend %/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_data,aes(x=AquaticSystem, y=chla_n))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("Chla Trend %/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_data,aes(x=Ecoregion, y=chla_n))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("Chla Trend %/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_data,aes(x=SalinityRegime, y=chla_n))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("Chla Trend %/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_data,aes(x=TidalRegime, y=chla_n))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("Chla Trend %/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_data,aes(x=PrimaryWaterSource, y=chla_n))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("Chla Trend %/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(combined_data,aes(x=NERR_BioRegion, y=chla_n))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("Chla Trend %/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Tends vs top predictors


ggplot(combined_data, aes(x = po4f_mdl, y = chla_n)) +
    geom_point(aes(color = cluster), size = 3) +
    scale_color_manual(values = cluster_colors) +
    xlab("PO4 Trend %/yr") + ylab("Chla Trend %/yr") +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    scale_fill_manual(values = cluster_colors) +
    theme_minimal() +
    annotate("text", x = Inf, y = Inf, label = "+ PO4 & Chla", hjust = 1.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = Inf, label = "- PO4, + Chla", hjust = -0.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = -Inf, label = "- PO4 & Chla", hjust = -0.1, vjust = -0.1) +
    annotate("text", x = Inf, y = -Inf, label = "+ PO4, - Chla", hjust = 1.1, vjust = -0.1)


ggplot(combined_data,aes(x=nh4f_mdl, y=chla_n))+
    geom_point(aes(color=cluster),size=3)+
    scale_color_manual(values = cluster_colors)+
    # geom_smooth(method="lm")+
    xlab("NH4 Trend %/yr")+ylab("Chla Trend %/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+
    annotate("text", x = Inf, y = Inf, label = "+ NH4 & Chla", hjust = 1.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = Inf, label = "- NH4, + Chla", hjust = -0.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = -Inf, label = "- NH4 & Chla", hjust = -0.1, vjust = -0.1) +
    annotate("text", x = Inf, y = -Inf, label = "+ NH4, - Chla", hjust = 1.1, vjust = -0.1)


ggplot(combined_data,aes(x=turb_trend, y=chla_n))+
    geom_point(aes(color=cluster),size=3)+
    scale_color_manual(values = cluster_colors)+
    #geom_smooth(method="gam")+
    xlab("Turbidiy Trend NTU/yr")+ylab("Chla Trend %/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+
    annotate("text", x = Inf, y = Inf, label = "+ Turb & Chla", hjust = 1.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = Inf, label = "- Turb, + Chla", hjust = -0.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = -Inf, label = "- Turb & Chla", hjust = -0.1, vjust = -0.1) +
    annotate("text", x = Inf, y = -Inf, label = "+ Turb, - Chla", hjust = 1.1, vjust = -0.1)

##Plotting trend medians

ggplot(combined_data,aes(x=chla_median, y=chla_n))+
    geom_point(aes(color=cluster),size=3)+
    #geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median Chla ug/L")+ylab("Chla Trend %/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=spcond_median, y=chla_n))+
    geom_point(aes(color=cluster),size=3)+
    geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median SpCond mS/cm")+ylab("Chla Trend %/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=turb_median, y=chla_n))+
    geom_point(aes(color=cluster),size=3)+
    geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median Turbidity NTU")+ylab("Chla Trend %/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=domgl_median, y=chla_n))+
    geom_point(aes(color=cluster),size=3)+
    #geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median DO mg/L")+ylab("Chla Trend %/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=temp_median, y=chla_n))+
    geom_point(aes(color=cluster),size=3)+
    geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median Temp C")+ylab("Chla Trend %/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=po4f_median, y=chla_n))+
    geom_point(aes(color=cluster),size=3)+
    #geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median PO4 ug/L")+ylab("Chla Trend %/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=no23f_median, y=chla_n))+
    geom_point(aes(color=cluster),size=3)+
    geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median NO23 ug/L")+ylab("Chla Trend %/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=nh4f_median, y=chla_n))+
    geom_point(aes(color=cluster),size=3)+
    geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median NH4 ug/L")+ylab("Chla Trend %/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=precp_median, y=chla_n))+
    geom_point(aes(color=cluster),size=3)+
    geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median precp unit")+ylab("Chla Trend %/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

#######################################
#DO Trends

do_df <- combined_data[, c(1:4,6, 11, 55:61,71:75)]


do_long <- do_df %>%
    pivot_longer(
        cols = ends_with("pctTotal"), # Adjust based on your land use columns' naming pattern
        names_to = "landuse",
        values_to = "value"
    )

ggplot(do_long)+
    geom_point(aes(x=domgl_trend, y=value,color=cluster),size=3)+facet_wrap(~landuse)+
    scale_color_manual(values = cluster_colors)+theme_bw()+xlab("DO Trend mg/L/yr")

do_df_v2 <- combined_data[, c(1:4,6, 11, 62:67,71:75)]


do_long_v2 <- do_df_v2 %>%
    pivot_longer(
        cols = ends_with("pctLand"), # Adjust based on your land use columns' naming pattern
        names_to = "landuse",
        values_to = "value"
    )

ggplot(do_long_v2)+
    geom_point(aes(x=domgl_trend, y=value,color=cluster),size=3)+facet_wrap(~landuse)+
    scale_color_manual(values = cluster_colors)+xlab("DO Trend mg/L/yr")+
    ylab("% Cover")+theme_bw()


ggplot(combined_data,aes(x=TidalFlowType, y=domgl_trend))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("DO Trend mg/L/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_data,aes(x=AquaticSystem, y=domgl_trend))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("DO Trend mg/L/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_data,aes(x=Ecoregion, y=domgl_trend))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("DO Trend mg/L/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_data,aes(x=SalinityRegime, y=domgl_trend))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("DO Trend mg/L/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_data,aes(x=TidalRegime, y=domgl_trend))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("DO Trend mg/L/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_data,aes(x=PrimaryWaterSource, y=domgl_trend))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("DO Trend mg/L/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_data,aes(x=NERR_BioRegion, y=domgl_trend))+
    geom_boxplot()+
    geom_jitter(aes(color=cluster),size=3,width=0.1)+
    scale_color_manual(values = cluster_colors)+ylab("DO Trend mg/L/yr")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Tends vs top predictors


ggplot(combined_data,aes(x=chla_trend, y=domgl_trend))+
    geom_point(aes(color=cluster),size=3)+
    scale_color_manual(values = cluster_colors)+
    #geom_smooth(method="lm")+
    xlab("Chla Trend %/yr")+ylab("DO Trend mg/L/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+
    annotate("text", x = Inf, y = Inf, label = "+ Chla & DO", hjust = 1.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = Inf, label = "- Chla, + DO", hjust = -0.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = -Inf, label = "- Chla & DO", hjust = -0.1, vjust = -0.1) +
    annotate("text", x = Inf, y = -Inf, label = "+ Chla, - DO", hjust = 1.1, vjust = -0.1)

ggplot(combined_data,aes(x=temp_trend, y=domgl_trend))+
    geom_point(aes(color=cluster),size=3)+
    scale_color_manual(values = cluster_colors)+
    # geom_smooth(method="lm")+
    xlab("Temp Trend C/yr")+ylab("DO Trend mg/L/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+
    annotate("text", x = Inf, y = Inf, label = "+ Temp & DO", hjust = 1.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = Inf, label = "- Temp, + DO", hjust = -0.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = -Inf, label = "- Temp & DO", hjust = -0.1, vjust = -0.1) +
    annotate("text", x = Inf, y = -Inf, label = "+ Temp, - DO", hjust = 1.1, vjust = -0.1)



##Plotting trend medians

ggplot(combined_data,aes(x=chla_median, y=domgl_trend))+
    geom_point(aes(color=cluster),size=3)+
    #  geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median Chla ug/L")+ylab("DO Trned mg/L/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=spcond_median, y=domgl_trend))+
    geom_point(aes(color=cluster),size=3)+
    #geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median SpCond mS/cm")+ylab("DO Trned mg/L/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=turb_median, y=domgl_trend))+
    geom_point(aes(color=cluster),size=3)+
    geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median Turbidity NTU")+ylab("DO Trned mg/L/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=domgl_median, y=domgl_trend))+
    geom_point(aes(color=cluster),size=3)+
    #geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median DO mg/L")+ylab("DO Trned mg/L/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=temp_median, y=domgl_trend))+
    geom_point(aes(color=cluster),size=3)+
    geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median Temp C")+ylab("DO Trned mg/L/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=po4f_median, y=domgl_trend))+
    geom_point(aes(color=cluster),size=3)+
    #geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median PO4 ug/L")+ylab("DO Trned mg/L/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=no23f_median, y=domgl_trend))+
    geom_point(aes(color=cluster),size=3)+
    geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median NO23 ug/L")+ylab("DO Trned mg/L/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=nh4f_median, y=domgl_trend))+
    geom_point(aes(color=cluster),size=3)+
    geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median NH4 ug/L")+ylab("DO Trned mg/L/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

ggplot(combined_data,aes(x=precp_median, y=domgl_trend))+
    geom_point(aes(color=cluster),size=3)+
    geom_smooth(method="gam")+
    scale_color_manual(values = cluster_colors)+
    xlab("Median precp unit")+ylab("DO Trned mg/L/yr")+
    geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
    scale_fill_manual(values = cluster_colors)+theme_minimal()

###Nutrients

ggplot(combined_data, aes(x = po4f_mdl_trend, y = nh4f_mdl_trend)) +
    geom_point(aes(color = cluster), size = 3) +
    scale_color_manual(values = cluster_colors) +
    xlab("PO4 Trend %/yr") + ylab("NH4 Trend %/yr") +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    scale_fill_manual(values = cluster_colors) +
    theme_minimal() +
    annotate("text", x = Inf, y = Inf, label = "+ PO4 & NH4", hjust = 1.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = Inf, label = "- PO4, + NH4", hjust = -0.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = -Inf, label = "- PO4 & NH4", hjust = -0.1, vjust = -0.1) +
    annotate("text", x = Inf, y = -Inf, label = "+ PO4, - NH4", hjust = 1.1, vjust = -0.1)

ggplot(combined_data, aes(x = po4f_mdl_trend, y = no23f_mdl_trend)) +
    geom_point(aes(color = temp_trend), size = 4) +
    #scale_color_manual(values = cluster_colors) +
    xlab("PO4 Trend %/yr") + ylab("no23 Trend %/yr") +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    scale_fill_manual(values = cluster_colors) +
    theme_minimal()+
    scale_color_gradient(high = "red", low = "blue", name = "Temp trend, C/yr") +
    annotate("text", x = Inf, y = Inf, label = "+ PO4 & NO23", hjust = 1.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = Inf, label = "- PO4, + NO23", hjust = -0.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = -Inf, label = "- PO4 & NO23", hjust = -0.1, vjust = -0.1) +
    annotate("text", x = Inf, y = -Inf, label = "+ PO4, - NO23", hjust = 1.1, vjust = -0.1)


ggplot(combined_data, aes(x = temp_median, y = temp_trend)) +
    geom_point(aes(color = cluster), size = 4) +
    scale_color_manual(values = cluster_colors) +
    xlab("Median Temp") + ylab("Temp Trend C/yr") +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    scale_fill_manual(values = cluster_colors) +
    theme_minimal()
   # scale_color_gradient(high = "red", low = "blue", name = "Temp trend, C/yr") 


ggplot(combined_data, aes(x = no23f_mdl_trend, y = nh4f_mdl_trend)) +
    geom_point(aes(color = cluster), size = 3) +
    scale_color_manual(values = cluster_colors) +
    xlab("no23 Trend %/yr") + ylab("NH4 Trend %/yr") +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
    scale_fill_manual(values = cluster_colors) +
    theme_minimal() +
    annotate("text", x = Inf, y = Inf, label = "+ NO23 & NH4", hjust = 1.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = Inf, label = "- NO23, + NH4", hjust = -0.1, vjust = 1.1) +
    annotate("text", x = -Inf, y = -Inf, label = "- NO23 & NH4", hjust = -0.1, vjust = -0.1) +
    annotate("text", x = Inf, y = -Inf, label = "+ NO23, - NH4", hjust = 1.1, vjust = -0.1)


N.P_subset<-combined_data[(combined_data$no23f_mdl_trend<0 & combined_data$po4f_mdl_trend>0),]

N.P_stations<-N.P_subset[c("reserve","station")]

write.csv(N.P_stations, "NP_stations.csv")

