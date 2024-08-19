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

gridcodes<-read.csv("gridcodes.csv")

stations<-read.csv("HUC8_Coords_wq_data.csv")

Reserves <-stations[, !(names(stations) == "station")]

LULC21_HUC8<-read.csv("NLCD2021_HUC8_Summary.csv")

# Merge the gridcodes data using the appropriate column
final_merged_data <- merge(LULC21_HUC8, gridcodes, by = "gridcode")

# Display the final merged data
head(final_merged_data)  # Display the first few rows of the final merged data frame


##########################################
# Load the required packages
library(dplyr)

# Calculate the total count for each Reserve and name combination
total_counts_WBD <- final_merged_data %>%
  group_by(name) %>%
  summarise(total_sum_shape = sum(SUM_Shape_))

# Join the total counts with the original data
final_merged_data_with_totals_WBD <- left_join(final_merged_data, total_counts_WBD, by = c("name"))

# Calculate the percentage for each category
final_merged_data_with_percentage_WBD <- final_merged_data_with_totals_WBD %>%
  mutate(percent = (SUM_Shape_ / total_sum_shape) * 100)


###########

#Check
apalachicola_data <- final_merged_data_with_percentage_WBD %>%
  filter(name == "Apalachicola")

sum(apalachicola_data$percent)

#########################################

# Create a new column 'Reserve' by grouping 'name' column

final_merged_data_with_percentage_WBD$Reserve<-c('')
#################


final_merged_data_with_percentage_WBD <- final_merged_data_with_percentage_WBD %>%
  mutate(Reserve = case_when(
    name %in% c("Cooper", "Edisto River", "Salkehatchie") ~ "ACE",
    name %in% c("Apalachicola Bay", "Apalachicola") ~ "APA",
    name == "Cape Cod" ~ "WQB",
    name %in% c("Patuxent", "Tangier", "Gunpowder-Patapsco") ~ "CBM",
    name %in% c("Pamunkey", "York", "Lynnhaven-Poquoson") ~ "CBV",
    name %in% c("Broadkill-Smyrna", "Brandywine-Christina") ~ "DEL",
    name == "Monterey Bay" ~ "ELK",
    name %in% c("Mississippi Coastal") ~ "GND",
    name == "Hudson-Wappinger" ~ "HUD",
    name %in% c("Lower Kenai Peninsula") ~ "KAC",
    name == "St. Louis" ~ "LKS",
    name %in% c("Daytona-St. Augustine") ~ "GTM",
    name %in% c("East San Antonio Bay", "Aransas Bay", "South Corpus Christi Bay") ~ "MAR",
    name == "Narragansett" ~ "NAR",
    name %in% c("Coastal Carolina", "Waccamaw", "Carolina Coastal-Sampit") ~ "NIW",
    name %in% c("New River", "Lower Cape Fear") ~ "NOC",
    name == "Huron-Vermilion" ~ "OWC",
    name == "Strait of Georgia" ~ "PDB",
    name == "Big Cypress Swamp" ~ "RKB",
    name == "Ogeechee Coastal" ~ "SAP",
    name %in% c("Suisun Bay", "San Pablo Bay") ~ "SFB",
    name == "Coos" ~ "SOS",
    name == "San Diego" ~ "TJR",
    name %in% c("Piscataqua-Salmon Falls") ~ "WEL",
    name == "Mobile Bay" ~ "WKB",
    name == "Cottonwood-Tijuana" ~ "TJR",
    name %in% c("Lower Hudson", "Middle Hudson")  ~ "HUD",
    name == "Mullica-Toms" ~ "JAC",
   TRUE ~ NA_character_
  ))




plot1<-ggplot(final_merged_data_with_percentage_WBD, aes(x = name, y = percent, fill = category)) +
  geom_bar(stat = "identity",color="black") +
  labs(x = "Watershed (names removed)", y = "Percentage", fill = "Land Use Category") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  ggtitle("LULC using HUC8 WBD")+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Reserve, nrow = 1, scales = "free_x")+theme(legend.position = "none")

plot1

#########################
final_merged_data_RES<-final_merged_data
final_merged_data_RES$Reserve <- c('')


final_merged_data_RES <- final_merged_data_RES %>%
  mutate(Reserve = case_when(
    name %in% c("Cooper", "Edisto River", "Salkehatchie") ~ "ACE",
    name %in% c("Apalachicola Bay", "Apalachicola") ~ "APA",
    name == "Cape Cod" ~ "WQB",
    name %in% c("Patuxent", "Tangier", "Gunpowder-Patapsco") ~ "CBM",
    name %in% c("Pamunkey", "York", "Lynnhaven-Poquoson") ~ "CBV",
    name %in% c("Broadkill-Smyrna", "Brandywine-Christina") ~ "DEL",
    name == "Monterey Bay" ~ "ELK",
    name %in% c("Mississippi Coastal") ~ "GND",
    name == "Hudson-Wappinger" ~ "HUD",
    name %in% c("Lower Kenai Peninsula") ~ "KAC",
    name == "St. Louis" ~ "LKS",
    name %in% c("Daytona-St. Augustine") ~ "GTM",
    name %in% c("East San Antonio Bay", "Aransas Bay", "South Corpus Christi Bay") ~ "MAR",
    name == "Narragansett" ~ "NAR",
    name %in% c("Coastal Carolina", "Waccamaw", "Carolina Coastal-Sampit") ~ "NIW",
    name %in% c("New River", "Lower Cape Fear") ~ "NOC",
    name == "Huron-Vermilion" ~ "OWC",
    name == "Strait of Georgia" ~ "PDB",
    name == "Big Cypress Swamp" ~ "RKB",
    name == "Ogeechee Coastal" ~ "SAP",
    name %in% c("Suisun Bay", "San Pablo Bay") ~ "SFB",
    name == "Coos" ~ "SOS",
    name == "San Diego" ~ "TJR",
    name %in% c("Piscataqua-Salmon Falls") ~ "WEL",
    name == "Mobile Bay" ~ "WKB",
    name == "Cottonwood-Tijuana" ~ "TJR",
    name %in% c("Lower Hudson", "Middle Hudson")  ~ "HUD",
    name == "Mullica-Toms" ~ "JAC",
    TRUE ~ NA_character_
  ))

# Calculate the total count for each Reserve and name combination
total_counts_RES_HUC8 <- final_merged_data_RES %>%
  group_by(Reserve) %>%
  summarise(total_sum_shape = sum(SUM_Shape_))

# Join the total counts with the original data
final_merged_data_with_totals_RES <- left_join(final_merged_data_RES, total_counts_RES, by = c("Reserve"))

# Calculate the percentage for each category
final_merged_data_with_percentage_RES <- final_merged_data_with_totals_RES %>%
  mutate(percent = (SUM_Shape_ / total_sum_shape) * 100)


ggplot(final_merged_data_with_percentage_RES, aes(x = Reserve, y = percent, fill = category)) +
  geom_bar(stat = "identity",color="black") +
  labs(x = "Reserve", y = "Percentage", fill = "Land Use Category") +
  theme_minimal() +
# facet_wrap(~Reserve, nrow = 1, scales = "free_x")+
theme(legend.position = "bottom")


########################################

#HUC12 Comparison

gridcodes<-read.csv("gridcodes.csv")

stations<-read.csv("NLCD2021_HUC12_NameCrosswalk.csv")

Reserves <- stations %>%
  select(Reserve, name) %>%
  unique()

LULC21_HUC12<-read.csv("NLCD2021_HUC12_Summary.csv")

# Merge the gridcodes data using the appropriate column
merged_data_HUC12 <- merge(LULC21_HUC12, gridcodes, by = "gridcode")

final_merged_data_HUC12 <- merge(merged_data_HUC12, Reserves, by = "name")
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


plot2<-ggplot(final_merged_data_with_percentage_WBD, aes(x = name, y = percent, fill = category)) +
  geom_bar(stat = "identity",color="black") +
  labs(x = "Watershed (names removed)", y = "Percentage", fill = "Land Use Category") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  ggtitle("LULC using HUC12 WBD")+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Reserve, nrow = 1, scales = "free_x")+theme(legend.position = "bottom")

plot2

library(patchwork)

patchwork <- (plot1/plot2) 
patchwork

######################

total_counts_RES_HUC12 <- final_merged_data_HUC12 %>%
  group_by(Reserve) %>%
  summarise(total_sum_shape_12 = sum(SUM_Shape_Area))

total_counts_RES_HUC8 <- final_merged_data_RES %>%
  group_by(Reserve) %>%
  summarise(total_sum_shape_8 = sum(SUM_Shape_))


merged_area_data <- merge(total_counts_RES_HUC12, total_counts_RES_HUC8, by = "Reserve")

merged_area_data$ratio<-c(merged_area_data$total_sum_shape_12/merged_area_data$total_sum_shape_8)

ggplot(data=merged_area_data)+
  geom_bar(aes(x=Reserve, y=total_sum_shape_8),color="blue")+
  geom_bar(aes(x=Reserve, y=total_sum_shape_12),color="red")


ggplot(data = merged_area_data) +
  geom_bar(aes(x = Reserve, y = total_sum_shape_8, fill = total_sum_shape_8), color = "blue", position = "dodge") +
  geom_bar(aes(x = Reserve, y = total_sum_shape_12, fill = total_sum_shape_12), color = "red", position = "dodge") +
  scale_fill_manual(values = c("blue", "red"))


  