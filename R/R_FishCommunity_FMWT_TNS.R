library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(sf)
library(maptools)
library(ggrepel)
library(readxl)

#Path to local drive
root <- "~/GitHub/Summer_Fall_Action_2022"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

#Summary for POD species (Striped Bass and Threadfin Shad)
tns <- read.csv(file.path(data_root,"TNS", "STNCatchPerTow1959-2022.csv"))%>%
  filter(Year>2010) %>% 
  select(Year, Survey, Station.Code, Age.0.Striped.Bass, Threadfin.Shad) %>%
  group_by(Year,Survey,Station.Code) %>% summarise(Age0Stripedbass=sum(Age.0.Striped.Bass), Threadfin.Shad=sum(Threadfin.Shad))

fmwt <- read.csv(file.path(data_root,"FMWT", "FMWT 1967-2022 Catch Matrix_updated.csv"))


#Summer Townet Survey---------
#Text from CDFW website:
#Beginning in 2003, CDFG standardized sampling to 6 surveys annually, starting in early June and running on alternate weeks through August.
#In 2011, to better describe delta smelt distribution, as well as that of other fishes, 
#STN added 8 supplemental, non-index, stations in Cache Slough (3 stations) and the Sacramento Deep Water Ship Channel (5 stations).

#Fall Midwater Trawl---------
#Text from CDFW website:
#Latest non-core stations were added in 2010, 122 stations in total

list(fmwt)
fmwt_edit<-fmwt %>% filter(Year>=2010) %>% select(Year,SampleDate,SurveyNumber,StationCode,Threadfin.Shad,Striped.Bass.age.0)
unique(fmwt_edit$Survey)

fmwt_edit$SampleDate<-as.Date(fmwt_edit$SampleDate,"%Y-%m-%d")

#THREADFIN SHAD

#Summer Townet Survey-----------
#Summarize mean and standard deviation for catch per tow
tns_tfs_2011_by_station<- tns %>% group_by(Year,Survey,Station.Code) %>% summarise(TFS_catch = mean(Threadfin.Shad)) 
#tns_tfs_2011_by_survey<- tns_tfs_2011_by_station %>% add_column(samplesize=1) %>% group_by(Year,Survey) %>% summarise(TFS_catch = mean(TFS_catch),samplesize=sum(samplesize))
tns_tfs_2011_by_year<-tns_tfs_2011_by_station %>% group_by(Year) %>% summarise(TFS_catch_mean = mean(TFS_catch),TFS_sd= sd(TFS_catch))

#Add upper and lower bounds
tns_tfs_2011_by_year$upper_bnd<-tns_tfs_2011_by_year$TFS_catch_mean+tns_tfs_2011_by_year$TFS_sd
tns_tfs_2011_by_year$lower_bnd<-tns_tfs_2011_by_year$TFS_catch_mean-tns_tfs_2011_by_year$TFS_sd
tns_tfs_2011_by_year$lower_bnd<-ifelse(tns_tfs_2011_by_year$lower_bnd<0,0,tns_tfs_2011_by_year$lower_bnd)

#Create threadfin shad plot for TNS
threadfin_tns_plot<-ggplot2::ggplot()+
  ggplot2::geom_point(data=tns_tfs_2011_by_year, ggplot2::aes(x=.data$Year, y=.data$TFS_catch_mean), color="darkorchid4",size=2)+
  ggplot2::geom_errorbar(data=tns_tfs_2011_by_year, ggplot2::aes(x=.data$Year, ymax=.data$upper_bnd, ymin=.data$lower_bnd,width=0.3))+
  ggplot2::theme_classic()+
  ggplot2::scale_x_continuous(breaks =c(min(tns_tfs_2011_by_year$Year):max(tns_tfs_2011_by_year$Year)))+
  ggplot2::ylab("Threadfin Shad catch per tow")+
  ggplot2::ggtitle("Summer Townet Survey")+
  ggplot2::theme(plot.title=element_text(size=9), 
               axis.text.x=element_text(size=9, color="black"), 
               axis.text.y = element_text(size=8, color="black"), 
               axis.title.x = element_blank(), 
               axis.title.y = element_text(size = 9, angle = 90),
               strip.text = element_text(size = 7),
               strip.background = element_rect(size=0.3),
               legend.position = "none")

threadfin_tns_plot


#Fall Midwater Trawl--------------
#We will use just 2010 data and on
#AND USE SURVEY 3 and 4 only to keep it consistent with 2020's limited data
#Summarize mean and standard deviation for catch per tow
fmwt_tfs_2010_by_year<-fmwt_edit %>% filter(SurveyNumber <=4) %>% group_by(Year) %>% summarise(TFS_catch_mean = mean(Threadfin.Shad),TFS_sd= sd(Threadfin.Shad))

#Add upper and lower bounds
fmwt_tfs_2010_by_year$upper_bnd<-fmwt_tfs_2010_by_year$TFS_catch_mean+fmwt_tfs_2010_by_year$TFS_sd
fmwt_tfs_2010_by_year$lower_bnd<-fmwt_tfs_2010_by_year$TFS_catch_mean-fmwt_tfs_2010_by_year$TFS_sd
fmwt_tfs_2010_by_year$lower_bnd<-ifelse(fmwt_tfs_2010_by_year$lower_bnd<0,0,fmwt_tfs_2010_by_year$lower_bnd)

#Create threadfin shad plot for FMWT
threadfin_fmwt_plot<-ggplot2::ggplot()+
  ggplot2::geom_point(data=fmwt_tfs_2010_by_year, ggplot2::aes(x=.data$Year, y=.data$TFS_catch_mean), color="darkorchid4",size=2)+
  ggplot2::geom_errorbar(data=fmwt_tfs_2010_by_year, ggplot2::aes(x=.data$Year, ymax=.data$upper_bnd, ymin=.data$lower_bnd,width=0.3))+
  ggplot2::theme_classic()+
  ggplot2::scale_x_continuous(breaks =c(min(fmwt_tfs_2010_by_year$Year):max(fmwt_tfs_2010_by_year$Year)))+
  ggplot2::ylab("Threadfin Shad catch per tow")+
  ggplot2::ggtitle("Fall Midwater Trawl")+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3),
                 legend.position = "none")

threadfin_fmwt_plot

#Print figure
tiff(filename=file.path(output_root,"Figure_ThreadfinShad.tiff"),
     type="cairo",
     units="in", 
     width=10, #10*1, 
     height=4, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
grid.arrange(threadfin_tns_plot, threadfin_fmwt_plot, ncol=2,nrow=1)

dev.off()

#STRIPED BASS

#Summer Townet Survey-----------

#Perhaps we will just use CPUE for consistency sake. We can switch to index if necessary. 
#Summarize mean and standard deviation for catch per tow
tns_stb_2011_by_station<- tns %>% group_by(Year,Survey,Station.Code) %>% summarise(STB_catch = mean(Age0Stripedbass)) 
tns_stb_2011_by_year<-tns_stb_2011_by_station %>% group_by(Year) %>% summarise(STB_catch_mean = mean(STB_catch),STB_sd= sd(STB_catch))

#Add upper and lower bounds
tns_stb_2011_by_year$upper_bnd<-tns_stb_2011_by_year$STB_catch_mean+tns_stb_2011_by_year$STB_sd
tns_stb_2011_by_year$lower_bnd<-tns_stb_2011_by_year$STB_catch_mean-tns_stb_2011_by_year$STB_sd
tns_stb_2011_by_year$lower_bnd<-ifelse(tns_stb_2011_by_year$lower_bnd<0,0,tns_stb_2011_by_year$lower_bnd)

#Create striped bass plot for TNS
stripedbass_tns_plot<-ggplot2::ggplot()+
  ggplot2::geom_point(data=tns_stb_2011_by_year, ggplot2::aes(x=.data$Year, y=.data$STB_catch_mean), color="darkorchid4",size=2)+
  ggplot2::geom_errorbar(data=tns_stb_2011_by_year, ggplot2::aes(x=.data$Year, ymax=.data$upper_bnd, ymin=.data$lower_bnd,width=0.3))+
  ggplot2::theme_classic()+
  ggplot2::scale_x_continuous(breaks =c(min(tns_stb_2011_by_year$Year):max(tns_stb_2011_by_year$Year)))+
  ggplot2::ylab("Striped Bass catch per tow")+
  ggplot2::ggtitle("Summer Townet Survey")+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3),
                 legend.position = "none")

stripedbass_tns_plot

#Fall Midwater Trawl--------------
#We will use just 2010 data and on
#AND USE SURVEY 3 and 4 only to keep it consistent with 2020's limited data
#Summarize mean and standard deviation for catch per tow
fmwt_stb_2010_by_year<-fmwt_edit %>% filter(SurveyNumber <=4) %>% group_by(Year) %>% summarise(STB_catch_mean = mean(Striped.Bass.age.0),STB_sd= sd(Striped.Bass.age.0))

#Add upper and lower bounds
fmwt_stb_2010_by_year$upper_bnd<-fmwt_stb_2010_by_year$STB_catch_mean+fmwt_stb_2010_by_year$STB_sd
fmwt_stb_2010_by_year$lower_bnd<-fmwt_stb_2010_by_year$STB_catch_mean-fmwt_stb_2010_by_year$STB_sd
fmwt_stb_2010_by_year$lower_bnd<-ifelse(fmwt_stb_2010_by_year$lower_bnd<0,0,fmwt_stb_2010_by_year$lower_bnd)

#Create striped bass plot for FMWT
stripedbass_fmwt_plot<-ggplot2::ggplot()+
  ggplot2::geom_point(data=fmwt_stb_2010_by_year, ggplot2::aes(x=.data$Year, y=.data$STB_catch_mean), color="darkorchid4",size=2)+
  ggplot2::geom_errorbar(data=fmwt_stb_2010_by_year, ggplot2::aes(x=.data$Year, ymax=.data$upper_bnd, ymin=.data$lower_bnd,width=0.3))+
  ggplot2::theme_classic()+
  ggplot2::scale_x_continuous(breaks =c(min(fmwt_stb_2010_by_year$Year):max(fmwt_stb_2010_by_year$Year)))+
  ggplot2::ylab("Striped Bass catch per tow")+
  ggplot2::ggtitle("Fall Midwater Trawl")+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 strip.background = element_rect(size=0.3),
                 legend.position = "none")

stripedbass_fmwt_plot

#Print figure
tiff(filename=file.path(output_root,"Figure_StripedBass.tiff"),
     type="cairo",
     units="in", 
     width=10, #10*1, 
     height=4, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
grid.arrange(stripedbass_tns_plot, stripedbass_fmwt_plot, ncol=2,nrow=1)

dev.off()
