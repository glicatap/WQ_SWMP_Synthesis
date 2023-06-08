library(SWMPr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(kableExtra)
library(here)
library(data.table)
library(trend)
library(zoo)
library(lubridate)

data_path <- here("Data/Nutrients")

setwd(data_path)

NUT_data <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))


data_path <- here("Data/WQ")

setwd(data_path)

WQ <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))


WQ_long <- melt(setDT(WQ), id.vars = c("StationCode","DateTimeStamp"), 
                measure.vars = c("Temp","SpCond", "Sal","DO_mgl","pH","Turb"))

WQ_long_flags <- melt(setDT(WQ), id.vars = c("StationCode","DateTimeStamp"), 
                      measure.vars = c("F_Temp","F_SpCond", "F_Sal","F_DO_mgl","F_pH","F_Turb"))

colnames(WQ_long_flags)<-c("Station","DateTime","variable","flag")

WQ_long$flag<-WQ_long_flags$flag

WQ_long<-separate(WQ_long, DateTimeStamp, into = c("Date", "Time"), sep = " ")
WQ_long<-separate(WQ_long, Date, into = c("Month", "Day","Year"), sep = "/",remove=F)

WQ_long$Date<- as.Date(WQ_long$Date,format="%m/%d/%Y")

#WQ_long_clean<-WQ_long[(WQ_long$Month>"05"& WQ_long$Month<"11"),]

Summarize_codes<-WQ_long %>% 
  group_by(StationCode, Month,Year,variable,flag)%>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

unique(Summarize_codes$StationCode)


Flag_subset <- Summarize_codes[grepl("<0>", Summarize_codes$flag)|
                            grepl("<1>", Summarize_codes$flag), ]

Summarize_flags<-Flag_subset %>% 
  group_by(StationCode, Month,Year,variable)%>% 
  summarise(sum_freq = sum(freq)) 

unique(Summarize_flags$StationCode)

Check_data<-Summarize_flags %>% 
  group_by(StationCode, Month,Year,variable)%>% 
  filter(sum_freq<0.65)
