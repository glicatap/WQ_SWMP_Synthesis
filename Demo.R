a<-1
b<-2
c<-1+2
a + b


library(tidyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(dplyr)
library(trend)
library(zoo)
library(lubridate)

#change wd for your computer

setwd("C:/Users/kreinl1/OneDrive/OneDrive - UW-Madison/GitHub/WQ_SWMP_Synthesis/Data")

WQ<-read_csv("lksbawq2012.csv")

WQ_long <- melt(setDT(WQ), id.vars = c("StationCode","DateTimeStamp"), 
                measure.vars = c("Temp","SpCond", "Sal","DO_mgl","pH","Turb"))

WQ_long_flags <- melt(setDT(WQ), id.vars = c("StationCode","DateTimeStamp"), 
                      measure.vars = c("F_Temp","F_SpCond", "F_Sal","F_DO_mgl","F_pH","F_Turb"))

colnames(WQ_long_flags)<-c("Station","DateTime","variable","flag")

WQ_long$flag<-WQ_long_flags$flag

WQ_long_subset <- WQ_long[grep("<0>", WQ_long$flag), ]

WQ_long_subset<-separate(WQ_long_subset, DateTimeStamp, into = c("Date", "Time"), sep = " ")
WQ_long_subset<-separate(WQ_long_subset, Date, into = c("Month", "Day","Year"), sep = "/",remove=F)

WQ_long_subset$Date<- as.Date(WQ_long_subset$Date,format="%m/%d/%Y")


ggplot()+
  geom_boxplot(data=WQ_long_subset, aes(x=Month,y=value))+
  theme_bw()+facet_wrap(~variable, scales="free")
  theme(axis.text.x = element_text(color="black"), text = element_text(size = 16,color="black"))

