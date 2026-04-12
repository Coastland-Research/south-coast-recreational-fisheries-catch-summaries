#script to create plots for SRKW critical habitat letter

library(data.table)
library(tidyverse)

data<-fread("data/2025_data/SCA_REC_CATCH_ESTIMATES_20March2025.csv")%>%
  filter(PFMA%in%c("PFMA 18","PFMA 19","PFMA 20","PFMA 21","PFMA 121","PFMA 29"))%>%
  filter(YEAR>=2010)%>%
  mutate(MONTH=factor(MONTH,levels=c("January","February","March","April","May","June","July",
                                     "August","September","October","November","December")))

species="CHINOOK SALMON"
disposition="Released"

releases<-data%>%
  filter(SPECIES==species,DISPOSITION==disposition)

#plot of releases columns all areas all months since 2010
ggplot(releases,aes(x=YEAR,y=ESTIMATE))+
  geom_col()+
  facet_grid(PFMA~MONTH,scale="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90),
        strip.text.x.top = element_text(angle=90),
        strip.text.y.right = element_text(angle=0))

ggsave("figures/releases 18-29 columns.png")

#plot of releases points/lines with trends since 2010
ggplot(releases,aes(x=YEAR,y=ESTIMATE))+
  geom_point()+
  geom_smooth(linewidth=.5,SE=FALSE)+
  ylim(0,NA)+
  facet_grid(PFMA~MONTH,scale="free_y")+
  theme_bw()+
  labs(y="RELEASES")+
  theme(axis.text.x = element_text(angle=90),
        strip.text.x.top = element_text(angle=90),
        strip.text.y.right = element_text(angle=0))

ggsave("figures/releases 18-29 points with loess.png")

#plot of effort by month 
species="BOAT TRIPS"
disposition="Effort"

effort<-data%>%
  filter(SPECIES==species,DISPOSITION==disposition)

ggplot(effort,aes(x=YEAR,y=ESTIMATE))+
  geom_point(alpha=.5)+
  geom_smooth(linewidth=.5,se=FALSE)+
  ylim(0,NA)+
  facet_grid(PFMA~MONTH,scale="free_y")+
  theme_bw()+
  labs(y="BOAT TRIPS")+
  theme(axis.text.x = element_text(angle=90),
        strip.text.x.top = element_text(angle=90),
        strip.text.y.right = element_text(angle=0))

ggsave("figures/effort 18-29 points with loess.png")

#### base period versus recent years ####
# base period 2010-2017
# 2025 data added

thisyear<-fread("data/2025_data/catch and release data september 2025 version.csv")

pre<-fread("data/2025_data/SCA_REC_CATCH_ESTIMATES_20March2025.csv")%>%
  filter(PFMA%in%c("PFMA 18","PFMA 19","PFMA 20","PFMA 21","PFMA 121","PFMA 29"))%>%
  filter(SPECIES%in%c("CHINOOK SALMON","BOAT TRIPS"))%>%
  filter(YEAR>=2010&YEAR<=2018)%>%
  mutate(MONTH=factor(MONTH,levels=c("January","February","March","April","May","June","July",
                                     "August","September","October","November","December")))%>%
  group_by(YEAR,PFMA,DISPOSITION)%>%
  summarize(u=sum(ESTIMATE,na.rm=TRUE))%>%
  mutate(GROUP="2010-2018")

recent.2024<-fread("data/2025_data/SCA_REC_CATCH_ESTIMATES_20March2025.csv")%>%
  filter(PFMA%in%c("PFMA 18","PFMA 19","PFMA 20","PFMA 21","PFMA 121","PFMA 29"))%>%
  filter(SPECIES%in%c("CHINOOK SALMON","BOAT TRIPS"))%>%
  mutate(YEAR=as.character(YEAR))%>%
  filter(YEAR%in%c("2019","2020","2021","2022","2023","2024"))%>%
  mutate(MONTH=factor(MONTH,levels=c("January","February","March","April","May","June","July",
                                     "August","September","October","November","December")))%>%
  select(GROUP=YEAR,MONTH,PFMA,DISPOSITION,u=ESTIMATE)%>%
  group_by(GROUP,PFMA,DISPOSITION)%>%
  summarise(u=sum(u,na.rm=TRUE))

creel.2025<-fread("data/2025_data/srkw areas 2025 data.csv")

recent.2025<-rbind(recent.2024,creel.2025%>%mutate(GROUP=as.character(GROUP)))

ggplot(pre,aes(x=GROUP,y=u,group=GROUP))+
  geom_boxplot(fill="grey80")+
  geom_col(data=recent.2025)+
  facet_grid(PFMA~DISPOSITION,scale="free_y")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(x="Year",y="Estimate")

ggsave("figures/effort catch releases by PFMA 19-25 versus 2010-18.png",units="in",dpi=600,width=6,height=7)


#### summary table for june and july 2019 ####
jj2019<-fread("data/2025_data/SCA_REC_CATCH_ESTIMATES_20March2025.csv")%>%
  filter(PFMA%in%c("PFMA 18","PFMA 19","PFMA 20","PFMA 21","PFMA 121","PFMA 29"))%>%
  filter(SPECIES%in%c("CHINOOK SALMON","BOAT TRIPS"))%>%
  filter(YEAR==2019)%>%
  mutate(MONTH=factor(MONTH,levels=c("January","February","March","April","May","June","July",
                                     "August","September","October","November","December")))%>%
  filter(MONTH%in%c("June","July"))%>%
  group_by(PFMA,DISPOSITION)%>%
  summarize(Estimate=sum(ESTIMATE,na.rm=TRUE))

write.csv(jj2019,"data/2019 june and july summary.csv",row.names = FALSE)



ggplot(pre,aes(x=GROUP,y=u,group=GROUP))+
  geom_boxplot(fill="grey80")+
  geom_col(data=recent.2024)+
  facet_grid(PFMA~DISPOSITION)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(x="YEAR",y="Estimate")

ggsave("figures/effort catch releases by PFMA 20-24 versus 2010-2019 same scale.png",units="in",dpi=300,width=6,height=7)

recent.2025<-fread("data/2025_data/srkw areas 20205 data.csv")

recent<-rbind(recent.2024,recent.2025)


props<-fread("data/2025_data/SCA_REC_CATCH_ESTIMATES_20March2025.csv")%>%
  filter(PFMA%in%c("PFMA 18","PFMA 19","PFMA 20","PFMA 21","PFMA 121","PFMA 29"))%>%
  filter(SPECIES%in%c("CHINOOK SALMON","BOAT TRIPS"))%>%
  filter(YEAR<2025)%>%
  mutate(MONTH=factor(MONTH,levels=c("January","February","March","April","May","June","July",
                                     "August","September","October","November","December")))%>%
  group_by(YEAR,PFMA,DISPOSITION)%>%
  summarize(u=sum(ESTIMATE,na.rm=TRUE))

ggplot(props,aes(x=YEAR,y=u,fill=PFMA))+
  geom_col()+
  facet_wrap(~DISPOSITION,ncol=1)+
  theme_bw()+
  labs(x="Year",y="Estimate",fill="Area")

ggsave("figures/effort catch releases by 1980-2024 stacked bar.png",units="in",dpi=300,width=6,height=7)

ggplot(props,aes(x=YEAR,y=u,fill=PFMA))+
  geom_col(position="fill")+
  facet_wrap(~DISPOSITION,ncol=1)+
  theme_bw()+
  labs(x="Year",y="Estimate",fill="Area")

ggsave("figures/effort catch releases by 1980-2024 full bar.png",units="in",dpi=300,width=6,height=7)


props
  
  mutate(era=case_when(YEAR>=2010&YEAR<2015~"2010-2015",
                       YEAR>=2010&YEAR<2015~"2011-2015",
                       YEAR>=2010&YEAR<2015~"2010-2015"))
  
  group_by(YEAR,PFMA,DISPOSITION)%>%
  summarize(u=sum(ESTIMATE,na.rm=TRUE))%>%
  mutate(GROUP="2010-2019")