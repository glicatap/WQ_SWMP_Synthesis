library(here)
library(dplyr)
library(reshape2) 
library (ggplot2)
library(lubridate)
library(SWMPr)
library(data.table)
library(tidyr)

#Set your working directory

##All files saved in the same github folder, including data downloaded from box
here()
data<-read.csv("long-term-trends.csv")
coords <- read.csv("coords.csv", header = TRUE)
coords <- coords %>%
  mutate(station = tolower(station))


temp<-data %>% 
filter(parameter==c("temp_median"))
temp<-merge(temp,coords,by=("station"))

chla<-dplyr::filter(data, 
                    grepl('chla', parameter))
chla<-merge(chla,coords,by=("station"))

DO<-data %>% 
  filter(parameter==c("do_mgl_median"))
DO<-merge(DO,coords,by=("station"))

DO2<-data %>% 
  filter(parameter==c("do_proportion_below2"))
DO2<-merge(DO2,coords,by=("station"))

DO5<-data %>% 
  filter(parameter==c("do_proportion_below5"))
DO5<-merge(DO5,coords,by=("station"))

temp_sub<-temp[,c(1,4,38)]
names<-c("station_wq","temp_slope","temp_sig")
setnames(temp_sub,names)
temp_sub$stationcode<-substr(temp_sub$station, 0, 5)

chla_sub<-chla[,c(1,4,38)]
names<-c("station_nut","chla_slope","chla_sig")
setnames(chla_sub,names)
chla_sub$stationcode<-substr(chla_sub$station, 0, 5)

DO_sub<-DO[,c(1,4,38)]
names<-c("station_DO","DO_slope","DO_sig")
setnames(DO_sub,names)
DO_sub$stationcode<-substr(DO_sub$station, 0, 5)

DO2_sub<-DO2[,c(1,4,38)]
names<-c("station_DO2","DO2_slope","DO2_sig")
setnames(DO2_sub,names)
DO2_sub$stationcode<-substr(DO2_sub$station, 0, 5)

DO5_sub<-DO5[,c(1,4,38)]
names<-c("station_DO5","DO5_slope","DO5_sig")
setnames(DO5_sub,names)
DO5_sub$stationcode<-substr(DO5_sub$station, 0, 5)

temp_chla<-merge(temp_sub, chla_sub, by=("stationcode"))
temp_chla$fig_code<-c("")

temp_chla$fig_code[temp_chla$temp_sig == "yes" & temp_chla$chla_sig == "no"] <- "Temp"
temp_chla$fig_code[temp_chla$temp_sig == "no" & temp_chla$chla_sig == "yes"] <- "Chla"
temp_chla$fig_code[temp_chla$temp_sig == "yes" & temp_chla$chla_sig == "yes"] <- "Both"
temp_chla$fig_code[temp_chla$temp_sig == "no" & temp_chla$chla_sig == "no"] <- "None"

clean_data<-na.omit(temp_chla)

manual_colors <- c("Temp" = "red", "Chla" = "blue", "Both" = "purple", "None" = "lightgrey")


ggplot(data = clean_data, aes(x = temp_slope, y = chla_slope, color = fig_code)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "black",lwd=1.5,linetype = 2) +
  geom_vline(xintercept = 0, color = "black",lwd=1.5, linetype= 2) +
  ylab("Chl-a Trend ppb/yr") + xlab("Temp Trend deg C/yr") +
  theme_bw() +
  scale_color_manual(values = manual_colors, name = "Sig. Trend")+
  theme(text = element_text(size = 14)) 

#############
DO_chla<-merge(DO_sub, chla_sub, by=("stationcode"))
DO_chla$fig_code<-c("")

DO_chla$fig_code[DO_chla$DO_sig == "yes" & DO_chla$chla_sig == "no"] <- "DO"
DO_chla$fig_code[DO_chla$DO_sig == "no" & DO_chla$chla_sig == "yes"] <- "Chla"
DO_chla$fig_code[DO_chla$DO_sig == "yes" & DO_chla$chla_sig == "yes"] <- "Both"
DO_chla$fig_code[DO_chla$DO_sig == "no" & DO_chla$chla_sig == "no"] <- "None"

clean_data<-na.omit(DO_chla)

manual_colors <- c("DO" = "red", "Chla" = "blue", "Both" = "purple", "None" = "lightgrey")


ggplot(data = clean_data, aes(x = DO_slope, y = chla_slope, color = fig_code)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "black",lwd=1.5,linetype = 2) +
  geom_vline(xintercept = 0, color = "black",lwd=1.5, linetype= 2) +
  ylab("Chl-a Trend ppb/yr") + xlab("DO Trend mg/L/yr") +
  theme_bw() +
  scale_color_manual(values = manual_colors, name = "Sig. Trend")+
  theme(text = element_text(size = 14)) 

###########

DO2_chla<-merge(DO2_sub, chla_sub, by=("stationcode"))
DO2_chla$fig_code<-c("")

DO2_chla$fig_code[DO2_chla$DO2_sig == "yes" & DO2_chla$chla_sig == "no"] <- "DO< 2"
DO2_chla$fig_code[DO2_chla$DO2_sig == "no" & DO2_chla$chla_sig == "yes"] <- "Chla"
DO2_chla$fig_code[DO2_chla$DO2_sig == "yes" & DO2_chla$chla_sig == "yes"] <- "Both"
DO2_chla$fig_code[DO2_chla$DO2_sig == "no" & DO2_chla$chla_sig == "no"] <- "None"

clean_data<-na.omit(DO2_chla)

manual_colors <- c("DO< 2" = "red", "Chla" = "blue", "Both" = "gold", "None" = "black")


ggplot(data = clean_data, aes(x = DO2_slope, y = chla_slope, color = fig_code)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "black",lwd=1.5,linetype = 2) +
  geom_vline(xintercept = 0, color = "black",lwd=1.5, linetype= 2) +
  ylab("Chl-a Trend ppb/yr") + xlab("DO< 2 Trend proportion/yr") +
  theme_bw() +
  scale_color_manual(values = manual_colors, name = "Sig. Trend")+
  theme(text = element_text(size = 14)) 

#####

temp_DO<-merge(temp_sub, DO_sub, by=("stationcode"))
temp_DO$fig_code<-c("")

temp_DO$fig_code[temp_DO$temp_sig == "yes" & temp_DO$DO_sig == "no"] <- "Temp"
temp_DO$fig_code[temp_DO$temp_sig == "no" & temp_DO$DO_sig == "yes"] <- "DO"
temp_DO$fig_code[temp_DO$temp_sig == "yes" & temp_DO$DO_sig == "yes"] <- "Both"
temp_DO$fig_code[temp_DO$temp_sig == "no" & temp_DO$DO_sig == "no"] <- "None"

clean_data<-na.omit(temp_DO)

manual_colors <- c("Temp" = "red", "DO" = "blue", "Both" = "gold", "None" = "black")


ggplot(data = clean_data, aes(x = temp_slope, y = DO_slope, color = fig_code)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "black",lwd=1.5,linetype = 2) +
  geom_vline(xintercept = 0, color = "black",lwd=1.5, linetype= 2) +
  ylab("DO Trend mg/L/yr") + xlab("Temp Trend deg C/yr") +
  theme_bw() +
  scale_color_manual(values = manual_colors, name = "Sig. Trend")+
  theme(text = element_text(size = 14)) 
#####

temp_DO2<-merge(temp_sub, DO2_sub, by=("stationcode"))
temp_DO2$fig_code<-c("")

temp_DO2$fig_code[temp_DO2$temp_sig == "yes" & temp_DO2$DO2_sig == "no"] <- "Temp"
temp_DO2$fig_code[temp_DO2$temp_sig == "no" & temp_DO2$DO2_sig == "yes"] <- "DO< 2"
temp_DO2$fig_code[temp_DO2$temp_sig == "yes" & temp_DO2$DO2_sig == "yes"] <- "Both"
temp_DO2$fig_code[temp_DO2$temp_sig == "no" & temp_DO2$DO2_sig == "no"] <- "None"

clean_data<-na.omit(temp_DO2)

manual_colors <- c("Temp" = "red", "DO< 2" = "blue", "Both" = "gold", "None" = "black")


ggplot(data = clean_data, aes(x = temp_slope, y = DO2_slope, color = fig_code)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "black",lwd=1.5,linetype = 2) +
  geom_vline(xintercept = 0, color = "black",lwd=1.5, linetype= 2) +
  ylab("DO< 2 Trend proportion/yr") + xlab("Temp Trend deg C/yr") +
  theme_bw() +
  scale_color_manual(values = manual_colors, name = "Sig. Trend")+
  theme(text = element_text(size = 14)) 




library(sf)
library(ggplot2)
library(tigris)

  us_sf <- states(cb = TRUE, resolution = "20m") %>%
    shift_geometry()
  df_sf <- st_as_sf(temp, coords = c("lon", "lat"), crs = 4326) %>% 
    tigris::shift_geometry()
  
  ggplot(temp, aes(x=ts_length)) + 
    geom_histogram(color="black", fill="gray",binwidth=1)+
    xlab("Time Series Length (years)")+theme_bw()
  
  ggplot() +
    geom_sf(data = us_sf) +
    labs(title = "Trends in eutrophication and hypoxia in the NERRS",
         subtitle = "Filled circles indicate p<0.05") +
    theme(panel.background = element_blank()) +
    geom_sf(data = df_sf, aes(color = Slope, shape = as.factor(sig_trend)),size=4) +
    scale_color_gradient(low = "blue", high = "red", name = "Temp deg C/yr") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21),guide="none") 
  
  #######
  
  us_sf <- states(cb = TRUE, resolution = "20m") %>%
    shift_geometry()
  
  df_sf <- st_as_sf(temp, coords = c("lon", "lat"), crs = 4326) %>% 
    tigris::shift_geometry()
  
  ggplot() +
    geom_sf(data = us_sf, fill = NA, color = "black") +
    labs(title = "Trends in eutrophication and hypoxia in the NERRS",
         subtitle = "Filled circles indicate p<0.05") +
    theme(panel.background = element_blank()) +
    geom_sf(data = df_sf, aes(color = Slope, shape = as.factor(sig_trend)), size = 4)+
    scale_color_gradient(low = "blue", high = "red", name = "Temp deg C/yr") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none")
  
  #############
  
  us_sf <- states(cb = TRUE, resolution = "20m") %>%
    shift_geometry()
  
  df_sf <- st_as_sf(chla, coords = c("lon", "lat"), crs = 4326) %>% 
    tigris::shift_geometry()
  
  ggplot() +
    geom_sf(data = us_sf, color = "black") +
    labs(title = "Trends in eutrophication and hypoxia in the NERRS",
         subtitle = "Filled circles indicate p<0.05") +
    theme(panel.background = element_blank()) +
    geom_sf(data = df_sf, aes(color = Slope, shape = as.factor(sig_trend)), size = 4)+
    scale_color_gradient(low = "skyblue", high = "darkgreen", name = "Chla ug/L/yr") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none")
  
  
  #############
  
  us_sf <- states(cb = TRUE, resolution = "20m") %>%
    shift_geometry()
  
  df_sf <- st_as_sf(DO, coords = c("lon", "lat"), crs = 4326) %>% 
    tigris::shift_geometry()
  
  ggplot() +
    geom_sf(data = us_sf,  color = "black") +
    labs(title = "Trends in eutrophication and hypoxia in the NERRS",
         subtitle = "Filled circles indicate p<0.05") +
    theme(panel.background = element_blank()) +
    geom_sf(data = df_sf, aes(color = Slope, shape = as.factor(sig_trend)), size = 4)+
    scale_color_gradient(low = "red", high = "darkblue", name = "DO mg/L/yr") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none")
  
  #############
  
  us_sf <- states(cb = TRUE, resolution = "20m") %>%
    shift_geometry()
  
  df_sf <- st_as_sf(DO2, coords = c("lon", "lat"), crs = 4326) %>% 
    tigris::shift_geometry()
  
  ggplot() +
    geom_sf(data = us_sf, color = "black") +
    labs(title = "Trends in eutrophication and hypoxia in the NERRS",
         subtitle = "Filled circles indicate p<0.05") +
    theme(panel.background = element_blank()) +
    geom_sf(data = df_sf, aes(color = Slope, shape = as.factor(sig_trend)), size = 4)+
    scale_color_gradient(low = "white", high = "orangered", name = "DO< 2 proportion/yr") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none")
  
 ############
  
  
  #############
  
  us_sf <- states(cb = TRUE, resolution = "20m") %>%
    shift_geometry()
  
  df_sf <- st_as_sf(DO5, coords = c("lon", "lat"), crs = 4326) %>% 
    tigris::shift_geometry()
  
  ggplot() +
    geom_sf(data = us_sf, color = "black") +
    labs(title = "Trends in eutrophication and hypoxia in the NERRS",
         subtitle = "Filled circles indicate p<0.05") +
    theme(panel.background = element_blank()) +
    geom_sf(data = df_sf, aes(color = Slope, shape = as.factor(sig_trend)), size = 4)+
    scale_color_gradient(low = "white", high = "brown2", name = "DO< 5 proportion/yr") +
    scale_shape_manual(values = c("yes" = 16, "no" = 21), guide = "none")
  
  ############
  #Time series length

  temp_data<-data %>% 
    filter(parameter==c("temp_median"))
  
ggplot()+
    geom_point(data=temp_data, aes(x=ts_length, y=Slope,color=sig_trend),size=3)

  #############

ggplot()+
  geom_histogram(data=data, aes(x=Slope,fill=sig_trend),color="black")+facet_wrap(parameter~.,scales="free")+
 # theme(legend.position="none")+ 
  geom_vline(xintercept=0, color="black", linetype="dashed", size=1)

ggplot()+
  geom_histogram(data=data, aes(x=Slope,fill=sig_trend),color="black")+facet_wrap(parameter~.,scales="free")+
  # theme(legend.position="none")+ 
  geom_vline(xintercept=0, color="black", linetype="dashed", size=1)+theme_minimal()

ggplot()+
  geom_histogram(data=data, aes(x=Slope,fill=sig_trend),color="black")+facet_wrap(parameter~.,scales="free")+
  # theme(legend.position="none")+ 
  geom_vline(xintercept=0, color="black", linetype="dashed", size=1)+theme_bw()

