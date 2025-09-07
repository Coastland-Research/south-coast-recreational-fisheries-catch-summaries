setwd("C:/Users/Andy1/Documents/R/coastland/SBC CHINOOK/")

options(scipen=1000)
library(data.table)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(stringr)
library(patchwork)
library(ggplot2)
library(dplyr)
require(readxl)

dir()
data<-fread("updated catch and release data.csv",header=TRUE)

gg.data<-data%>%pivot_longer(3:13,names_to="Year",values_to="Fish")
gg.data$Disposition<-factor(gg.data$Disposition,levels=c("Released Sub-legal","Released Legal","Kept"))

ggplot(gg.data,aes(x=Year,y=Fish,fill=Disposition))+
  geom_col()+
  facet_wrap(~Area,ncol=2,scales="free_y")+
  labs(y="Number of Chinook")+
  theme_bw()+
  theme(legend.position = "bottom",axis.text.x = element_text(angle=90))
  
ggsave("catch and releases-by region-updated 2021 info.png",dpi=600,height=9,width=7)


gg.data
