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
    
    