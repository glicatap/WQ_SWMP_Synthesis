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
                             "compiled_predictors_withExternalInfo_v3.csv"))


chla_df <- data[, c(1:4, 11, 55:61,71)]


chla_long <- chla_df %>%
    pivot_longer(
        cols = ends_with("pctTotal"), # Adjust based on your land use columns' naming pattern
        names_to = "landuse",
        values_to = "value"
    )

ggplot(chla_long)+
    geom_point(aes(x=chla_trend, y=value,color=cluster))+facet_wrap(~landuse)+
    scale_color_manual(values = cluster_colors)

chla_df_v2 <- data[, c(1:4, 11, 62:67,71)]


chla_long_v2 <- chla_df_v2 %>%
    pivot_longer(
        cols = ends_with("pctLand"), # Adjust based on your land use columns' naming pattern
        names_to = "landuse",
        values_to = "value"
    )

ggplot(chla_long_v2)+
    geom_point(aes(x=chla_trend, y=value,color=cluster))+facet_wrap(~landuse)+
    scale_color_manual(values = cluster_colors)
