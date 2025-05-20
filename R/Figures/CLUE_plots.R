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


po4_df <- combined_data[, c(1:4, 14, 55:61,71:75)]


po4_long <- po4_df %>%
    pivot_longer(
        cols = ends_with("pctTotal"), # Adjust based on your land use columns' naming pattern
        names_to = "landuse",
        values_to = "value"
    )

ggplot(po4_long)+
    geom_point(aes(x=po4f_mdl_trend, y=value,color=cluster),size=3)+facet_wrap(~landuse)+
    scale_color_manual(values = cluster_colors)+theme_bw()+xlab("PO4 Trend %/Yr")


nh4_df <- combined_data[, c(1:4, 13, 55:61,71:75)]


nh4_long <- nh4_df %>%
    pivot_longer(
        cols = ends_with("pctTotal"), # Adjust based on your land use columns' naming pattern
        names_to = "landuse",
        values_to = "value"
    )

ggplot(nh4_long)+
    geom_point(aes(x=nh4f_mdl_trend, y=value,color=cluster),size=3)+facet_wrap(~landuse)+
    scale_color_manual(values = cluster_colors)+theme_bw()+xlab("NH4 Trend %/Yr")


no23_df <- combined_data[, c(1:4, 12, 55:61,71:75)]


no23_long <- no23_df %>%
    pivot_longer(
        cols = ends_with("pctTotal"), # Adjust based on your land use columns' naming pattern
        names_to = "landuse",
        values_to = "value"
    )

ggplot(no23_long)+
    geom_point(aes(x=no23f_mdl_trend, y=value,color=cluster),size=3)+facet_wrap(~landuse)+
    scale_color_manual(values = cluster_colors)+theme_bw()+xlab("NO23 Trend %/Yr")


##########################
#Chla CLUE


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
    scale_color_manual(values = cluster_colors)+ylab("Chla Trend %/yr")+xlab ("NERR Bioregion")+
    scale_fill_manual(values = cluster_colors)+theme_minimal()+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

