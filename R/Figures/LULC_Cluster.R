library(foreign)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(dplyr)
library(trend)
library(zoo)
library(lubridate)
library(tis)
library(viridis)

cluster_colors <- c("A" = viridis(4)[3],
                    "B" = viridis(4)[1],
                    "C" = viridis(4)[2],
                    "D" = viridis(4)[4])

clusters<-read.csv("swmp_clstr_med_pc_stations_spc.csv")

clusters <- clusters %>%
  select(station = code, Reserve = Map.code,cluster=cluster)

cluster_upper <-clusters %>%
  mutate(across(where(is.character), toupper))

gridcodes<-read.csv("gridcodes.csv")

stations<-read.csv("NLCD2021_HUC12_NameCrosswalk.csv")

statons_sub<-stations %>% 
  select(station, Reserve, lon, lat, name)
  
station_merge <- inner_join(statons_sub, cluster_upper, by = c("station","Reserve"))
#Note that there are less stations because AL, HI, and PR are not included in the NLCD data

Reserves <- stations %>%
  select(Reserve, name) %>%
  unique()

LULC21_HUC12<-read.csv("NLCD2021_HUC12_Summary.csv")

# Merge the gridcodes data using the appropriate column
merged_data_HUC12 <- merge(LULC21_HUC12, gridcodes, by = "gridcode")

final_merged_data_HUC12 <- merge(merged_data_HUC12, station_merge, by = "name")
# Display the final merged data
head(final_merged_data_HUC12)  # Display the first few rows of the final merged data frame


##########################################
# Load the required packages
library(dplyr)

# Calculate the total count for each Reserve and name combination
total_counts_WBD <- final_merged_data_HUC12 %>%
  group_by(name) %>%
  summarise(total_sum_shape = sum(SUM_Shape_Area))

# Join the total counts with the original data
final_merged_data_with_totals_WBD <- left_join(final_merged_data_HUC12, total_counts_WBD, by = c("name"))

# Calculate the percentage for each category
final_merged_data_with_percentage_WBD <- final_merged_data_with_totals_WBD %>%
  mutate(percent = (SUM_Shape_Area/ total_sum_shape) * 100)


###########

#Check
apalachicola_data <- final_merged_data_with_percentage_WBD %>%
  filter(name == "Apalachicola Bay")

sum(apalachicola_data$percent)

library(stringr)

# Abbreviate x-axis labels to the first 5 characters
final_merged_data_with_percentage_WBD$name_abbr <- 
  str_sub(final_merged_data_with_percentage_WBD$name, start = 1, end = 5)


library(ggridges)
theme_set(theme_bw())

plot2<-ggplot(final_merged_data_with_percentage_WBD, aes(x = percent, y = cluster,fill=cluster)) + 
  geom_density_ridges(scale = 2, alpha = 0.7)+theme(legend.position="none")+facet_wrap(~category,scales="free_x")+
  ylab("Cluster")+xlab("Percent cover for HUC12 watersheds associated with stations") +
  scale_fill_manual(values = cluster_colors)+
    scale_x_log10(labels = scales::scientific)+
  theme( strip.background = element_rect(fill = "lightgrey", color = "black"))

plot2


#####################################

percent_binned <- final_merged_data_with_percentage_WBD %>%
  mutate(BinnedCategory = case_when(
    category %in% c("Developed, Open Space", "Developed, Low Intensity",
                    "Developed, Medium Intensity","Developed, High Intensity") ~ "Developed",
    category %in% c("Woody Wetlands","Emergent Herbaceous Wetlands") ~ "Wetlands",
    category %in% c("Deciduous Forest","Evergreen Forest","Mixed Forest") ~ "Forest",
    category %in% c("Cultivated Crops","Pasture/Hay") ~ "Agriculture",
    TRUE ~ category  # This ensures other values are retained if they exist
  ))

plot4 <- ggplot(percent_binned, aes(x = percent, y = cluster, fill = cluster)) + 
 # geom_density_ridges(scale = 2, alpha = 0.7, trim = TRUE) +
    stat_density_ridges(quantile_lines = TRUE, quantiles = 2,jittered_points = TRUE, )+
  theme(legend.position = "none") +
  facet_wrap(~BinnedCategory, nrow = 2, scales = "free_x") +
  ylab("Cluster") +
  xlab("Percent cover for HUC12 watersheds associated with stations") +
  scale_fill_manual(values = cluster_colors) +
    scale_x_log10(labels = scales::scientific)+
  theme(strip.background = element_rect(fill = "lightgrey", color = "black"))  

plot4

plot6<-ggplot(percent_binned, aes(x = percent, y = cluster,fill=cluster)) + 
  geom_density_ridges(scale = 2, alpha = 0.7)+theme(legend.position="none")+xlim(0,20)+facet_wrap(~BinnedCategory, nrow=2)+
  ylab("Cluster")+xlab("Percent cover for HUC12 watersheds associated with stations") +
  scale_fill_viridis_d(option = "D") + 
  theme( strip.background = element_rect(fill = "lightgrey", color = "black"))

plot6

############
#HUC8



stations<-read.csv("HUC8_Coords_wq_data.csv")

statons_sub<-stations %>% 
  select(station, Reserve, lon, lat, name)

station_merge <- inner_join(statons_sub, cluster_upper, by = c("station","Reserve"))

##

LULC21_HUC8<-read.csv("NLCD2021_HUC8_Summary.csv")

# Merge the gridcodes data using the appropriate column
merged_data_HUC8 <- merge(LULC21_HUC8, gridcodes, by = "gridcode")

final_merged_data_HUC8 <- merge(merged_data_HUC8, station_merge, by = "name")
# Display the final merged data
head(final_merged_data_HUC8)  # Display the first few rows of the final merged data frame


##########################################
# Load the required packages
library(dplyr)

# Calculate the total count for each Reserve and name combination
total_counts_WBD <- final_merged_data_HUC8 %>%
  group_by(name) %>%
  summarise(total_sum_shape = sum(SUM_Shape_))

# Join the total counts with the original data
final_merged_data_with_totals_WBD <- left_join(final_merged_data_HUC8, total_counts_WBD, by = c("name"))

# Calculate the percentage for each category
final_merged_data_with_percentage_WBD <- final_merged_data_with_totals_WBD %>%
  mutate(percent = (SUM_Shape_ / total_sum_shape) * 100)


###########

#Check
apalachicola_data <- final_merged_data_with_percentage_WBD %>%
  filter(name == "Apalachicola")

sum(apalachicola_data$percent)


library(ggridges)
theme_set(theme_minimal())

plot3<-ggplot(final_merged_data_with_percentage_WBD, aes(x = percent, y = cluster,fill=cluster)) + 
  geom_density_ridges(scale = 2, alpha = 0.7)+theme(legend.position="none")+facet_wrap(~category,scales="free_x")+
  ylab("Cluster")+xlab("Percent cover for HUC8 watersheds associated with stations") +
  scale_fill_viridis_d(option = "D") + 
  theme( strip.background = element_rect(fill = "lightgrey", color = "black"))

plot3



#####################################

percent_binned <- final_merged_data_with_percentage_WBD %>%
  mutate(BinnedCategory = case_when(
    category %in% c("Developed, Open Space", "Developed, Low Intensity",
                    "Developed, Medium Intensity","Developed, High Intensity") ~ "Developed",
    category %in% c("Woody Wetlands","Emergent Herbaceous Wetlands") ~ "Wetlands",
    category %in% c("Deciduous Forest","Evergreen Forest","Mixed Forest") ~ "Forest",
    category %in% c("Cultivated Crops","Pasture/Hay") ~ "Agriculture",
    TRUE ~ category  # This ensures other values are retained if they exist
  ))


plot5<-ggplot(percent_binned, aes(x = percent, y = cluster,fill=cluster)) + 
  geom_density_ridges(scale = 2, alpha = 0.7)+theme(legend.position="none")+facet_wrap(~BinnedCategory, nrow=2,scales="free_x")+
  ylab("Cluster")+xlab("Percent cover for HUC8 watersheds associated with stations") +
  scale_fill_viridis_d(option = "D") + 
  theme( strip.background = element_rect(fill = "lightgrey", color = "black"))

plot5


plot7<-ggplot(percent_binned, aes(x = percent, y = cluster,fill=cluster)) + 
  geom_density_ridges(scale = 2, alpha = 0.7)+theme(legend.position="none")+xlim(0,20)+facet_wrap(~BinnedCategory, nrow=2)+
  ylab("Cluster")+xlab("Percent cover for HUC8 watersheds associated with stations") +
  scale_fill_viridis_d(option = "D") + 
  theme( strip.background = element_rect(fill = "lightgrey", color = "black"))

plot7

library(patchwork)


patchwork <- (plot2/plot3) 
patchwork+plot_annotation(caption = '2021 NLCD for CONUS')

patchwork <- (plot4/plot5) 
patchwork+plot_annotation(caption = '2021 NLCD for CONUS')

patchwork <- (plot6/plot7) 
patchwork+plot_annotation(caption = '2021 NLCD for CONUS')


####################################################

cluster_data<-read.csv("swmp_clstr_med_pc_stations_spc.csv")

cluster_data<-cluster_data[,1:12]

long_data <- pivot_longer(
  cluster_data, 
  cols = starts_with(c("Sal", "Temp", "DO", "pH", "Turb", "Chla", "NH4", "NO23", "PO4")),
  names_to = "Parameter",
  values_to = "Value"
)

library(viridis)

cluster_colors <- c("A" = viridis(4)[3],
                    "B" = viridis(4)[1],
                    "C" = viridis(4)[2],
                    "D" = viridis(4)[4])

plot_c<-ggplot(long_data, aes(x = Value, y = cluster, fill = Parameter)) + 
  geom_density_ridges(scale = 2, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~ Parameter, scales = "free", labeller = labeller(Parameter = c(
    Sal = "Sal ppt",
    Temp = "Temp deg C",
    DO = "DO mg/L",
    pH = "pH",
    Turb = "Log(Turb) NTU",
    Chla = "Log(Chl-a) ug/L",
    NH4 = "Log(NH4) mg/L",
    NO23 = "Log(NO23) mg/L",
    PO4 = "Log(PO4) mg/L"
  ))) +
  ylab("Cluster") + xlab("Median values")+
scale_fill_manual(values = cluster_colors)+  # Apply Viridis color scale for fill
  theme_minimal() +
  theme(strip.background = element_rect(fill = "lightgrey", color = "black"))

plot_c


patchwork <- (plot_c|plot4) 
patchwork


########################

