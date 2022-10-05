library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(gtable)
library(sharpshootR)
library(RcppRoll)
library(ggpubr)

#Path to local drive
root <- "~/GitHub/Summer_Fall_Action_2021"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

#################### Read dayflow data
data_dayflow<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1997-2020.csv"))
data_dayflow$Date <- as.Date(data_dayflow$Date,"%m/%d/%Y")

#Add julian day
data_dayflow$julianday<-yday(data_dayflow$Date)

#Add water year to dayflow
data_dayflow$WY<-as.numeric(ifelse(month(data_dayflow$Date)>9,data_dayflow$Year+1,data_dayflow$Year))

#################### Add 2021 data
#Load WY2021 Net Delta Outflow Index (NDOI) as given to Rosemary from Jessie Cheng:Jie.Cheng@water.ca.gov


NDOI_2021 <- read.csv(file.path(data_root, "Dayflow", "NDOI_WY2021.csv"))
NDOI_2021$Date <- as.Date(NDOI_2021$Date,"%m/%d/%Y")

#Edit 2021 data from CDEC to match Dayflow
data_outflow_2021<-NDOI_2021 %>% mutate(OUT=NDOIcfs,WY=2021,Year=year(Date)) %>% select(Date,OUT,Year,WY)
data_outflow_2021$Month<-month(data_outflow_2021$Date)

#Add 2021 to the dayflow data
data_dayflow_added <- dplyr::bind_rows(data_dayflow,data_outflow_2021)
#Order full data set by date
data_dayflow_added <- data_dayflow_added[order(data_dayflow_added$Date),]
#Locate start row for 2021 WY data
which(data_dayflow_added$Date=="2020-10-01")
#8767


#Calculate X2 based on DAYFLOW documentation:
###
#The 1994 Bay-Delta agreement established standards for salinity in the estuary. 
#Specifically, the standards determine the degree to which salinity is allowed 
#to penetrate up-estuary, with salinity to be controlled through delta outflow. 
#The basis for the standards is a series ofrelationships between the salinity 
#pattern and the abundance or survival of various species of fish and 
#invertebrates. These relationships have been expressed in terms of X2, 
#the distance from the Golden Gate to the point where daily average salinity is 
#2 parts per thousand at 1 meter off the bottom (Jassby et. al. 1995).
#In Dayflow, X2 is estimated using the Autoregressive Lag Model:

#X2(t) = 10.16 + 0.945*X2(t-1) - 1.487log(QOUT(t)) 
#NOTE: It seems like the log in the DAYFLOW notation is referring to Log10 (i.e., not ln)

#Fill in X2 data for most recent WY data
for (i in 8767:nrow(data_dayflow_added)) {
  data_dayflow_added$X2[i] = 10.16 + (0.945*(data_dayflow_added$X2[i-1]))+(-1.487*log10(data_dayflow_added$OUT[i]))
}

data_dayflow_added[c(8767),]


#################### Add Water Year information

#Data from https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
#Copy and pasted into excel
data_wateryear<-read.csv(file.path(data_root, "Dayflow", "DWR_WSIHIST.csv"))

#Add WY type information
data_dayflow_added<-left_join(data_dayflow_added,data_wateryear[,c("WY","Sac_WY")])

#################### Graph for the last 10 years of outflow and X2 data

#Subset just June to October
data_dayflow_summerfall<-data_dayflow_added %>% filter(Month %in% c(6:10))

#Subset just last 5 years
data_dayflow_summerfall_5yrs<-data_dayflow_summerfall %>% filter(Year %in% c(2016:2021))

data_dayflow_summerfall_5yrs$WY<-as.factor(data_dayflow_summerfall_5yrs$WY)

data_dayflow_summerfall_5yrs$Year<-as.factor(data_dayflow_summerfall_5yrs$Year)

##Quick average X2 summary for Armin
X2_2021<- data_dayflow_added %>% filter(Year==2021&Month %in% c(6:10))
mean(X2_2021$X2)
#88.79056

#Plot for Outflow
plot_outflow_5yrs <- ggplot2::ggplot()+
  ggplot2::theme_bw()+
  ggplot2::geom_line(data=data_dayflow_summerfall_5yrs, ggplot2::aes(x=as.Date(paste(2021,strftime(Date,format="%m-%d"),sep="-")), y=OUT, color=Year, size=Year))+
  #ggplot2::scale_linetype_manual(values = c("dotdash","dotdash","dotdash","dotdash","dotdash","solid"),guide=FALSE) +
  ggplot2::scale_colour_manual(values = c("#003E51","#007396","#C69214","#DDCBA4","#FF671F","#9A3324"),name="",labels=c(2016:2021)) +
  ggplot2::scale_size_manual(values = c(0.5,0.5,0.5,0.5,0.5,1),name="",labels=c(2016:2021)) +
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::ylab(bquote("Delta"~outflow~"("*ft^3*"/s)"))+
  ggplot2::xlab("Date")

plot_outflow_5yrs

#Plot for X2
plot_X2_5yrs <- ggplot2::ggplot()+
  ggplot2::theme_bw()+
  ggplot2::geom_line(data=data_dayflow_summerfall_5yrs, ggplot2::aes(x=as.Date(paste(2021,strftime(Date,format="%m-%d"),sep="-")), y=X2, color=Year, size=Year))+
  #ggplot2::scale_linetype_manual(values = c("dotdash","dotdash","dotdash","dotdash","dotdash","solid"),name="",labels=c(2015:2020)) +
  ggplot2::scale_colour_manual(values = c("#003E51","#007396","#C69214","#DDCBA4","#FF671F","#9A3324"),name="",labels=c(2016:2021))+
  ggplot2::scale_size_manual(values = c(0.5,0.5,0.5,0.5,0.5,1),name="",labels=c(2016:2021)) +
  #ggplot2::scale_linetype(guide = FALSE)+
  ggplot2::theme(plot.title=element_text(size=9), 
          axis.text.x=element_text(size=9, color="black"), 
          axis.text.y = element_text(size=8, color="black"), 
          axis.title.x = element_text(size = 9, angle = 00), 
          axis.title.y = element_text(size = 9, angle = 90),
          strip.text = element_text(size = 7),
          legend.text=element_text(size = 9),
          strip.background = element_rect(size=0.3),
          legend.position="bottom") + 
  guides(size=guide_legend(keywidth = 6, keyheight = 1),
          colour=guide_legend(keywidth = 6, keyheight = 1))+
  ggplot2::ylab("X2 (km)")+
  ggplot2::xlab("Date")

plot_X2_5yrs

gA <- ggplotGrob(plot_outflow_5yrs)
gB <- ggplotGrob(plot_X2_5yrs)
maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3])

# Set the widths
gA$widths[2:3] <- maxWidth
gB$widths[2:3] <- maxWidth

#Print figure
tiff(filename=file.path(output_root,"Figure_Dayflow_5yrs.tiff"),
     type="cairo",
     units="in", 
     width=6, #10*1, 
     height=8, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
#pushViewport(viewport(layout = grid.layout(2, 1)))
#print(gA, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
#print(gB, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
grid.arrange(gA, gB, ncol=1,nrow=2)

dev.off()

#################### Graph for the last 5 critically dry years of outflow and X2 data

#Subset just dry years
critically_dry_years<-unique(data_dayflow_summerfall[data_dayflow_summerfall$Sac_WY=="C",c("WY")])
critically_dry_years

data_dayflow_summerfall_c_dry<-data_dayflow_summerfall %>% filter(Year %in% critically_dry_years)
unique(data_dayflow_summerfall_c_dry$Year)

data_dayflow_summerfall_c_dry$Year<-as.factor(data_dayflow_summerfall_c_dry$Year)

#Plot for Outflow
plot_outflow_critically_dry <- ggplot2::ggplot()+
  ggplot2::theme_bw()+
  ggplot2::geom_line(data=data_dayflow_summerfall_c_dry, ggplot2::aes(x=as.Date(paste(2021,strftime(Date,format="%m-%d"),sep="-")), y=OUT, color=Year, size=Year))+
  #Need to change below when 2020 data is in
  #ggplot2::scale_linetype_manual(values = c("dotdash","dotdash","dotdash","dotdash","dotdash","solid"),guide=FALSE) +
  ggplot2::scale_colour_manual(values = c("#003E51","#007396","#C69214","#9A3324"),guide=FALSE) +
  ggplot2::scale_size_manual(values = c(0.5,0.5,0.5,1),name="",labels=critically_dry_years) +
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3)) + 
  ggplot2::ylab(bquote("Delta"~outflow~"("*ft^3*"/s)"))+
  ggplot2::xlab("Date")

plot_outflow_critically_dry

#Plot for X2
plot_X2_critically_dry <- ggplot2::ggplot()+
  ggplot2::theme_bw()+
  ggplot2::geom_line(data=data_dayflow_summerfall_c_dry, ggplot2::aes(x=as.Date(paste(2021,strftime(Date,format="%m-%d"),sep="-")), y=X2, color=Year, size=Year))+
  #Need to change below when 2020 data is in
  #ggplot2::scale_linetype_manual(values = c("dotdash","dotdash","dotdash","dotdash","dotdash","solid"),name="",labels=dry_years) +
  ggplot2::scale_colour_manual(values = c("#003E51","#007396","#C69214","#9A3324"),name="",labels=critically_dry_years) +
  ggplot2::scale_size_manual(values = c(0.5,0.5,0.5,1),name="",labels=critically_dry_years) +
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 legend.text=element_text(size = 9),
                 legend.position="bottom",
                 strip.background = element_rect(size=0.3)) + 
  guides(size=guide_legend(keywidth = 6, keyheight = 1),
         colour=guide_legend(keywidth = 6, keyheight = 1))+
  ggplot2::ylab("X2 (km)")+
  ggplot2::xlab("Date")

plot_X2_critically_dry

gC <- ggplotGrob(plot_outflow_critically_dry)
gD <- ggplotGrob(plot_X2_critically_dry)
maxWidth = unit.pmax(gC$widths[2:3], gD$widths[2:3])

# Set the widths
gC$widths[2:3] <- maxWidth
gD$widths[2:3] <- maxWidth


#Print figure
tiff(filename=file.path(output_root,"Figure_Dayflow_critically_dry_years.tiff"),
     type="cairo",
     units="in", 
     width=7, #10*1, 
     height=8, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
#pushViewport(viewport(layout = grid.layout(1, 2)))
#print(plot_outflow_dry, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
#print(plot_X2_dry, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
grid.arrange(gC, gD, ncol=1,nrow=2)

dev.off()

### To check min and max X2
DT2021<-data_dayflow_added %>% filter(Year==2021)
View(DT2021)
DT2021<-DT2021 %>% filter(Month %in% c(6:10))


#################### Graph of outflow vs D-1641



d1641<-data.frame(Month=c(5,6,7,8,9,10),Outflow_required=c(3000,3000,3000,3000,3000,3000))

data_outflow_2021$Month=month(data_outflow_2021$Date)

d1641_2021<-left_join(data_outflow_2021,d1641) %>% filter(Month %in% c(5:10), Year==2021)

d1641_2021$d14_rollavg <- roll_mean(d1641_2021$OUT, n = 14, align = "right", fill = NA)

d1641_2021_june <- d1641_2021 %>% filter(Month==6)

  
#D1641 plot for June (14-day rolling average)
plot_d1641 <- ggplot2::ggplot()+
  ggplot2::theme_bw()+
  ggplot2::geom_line(data=d1641_2021_june, ggplot2::aes(x=as.Date(paste(2021,strftime(Date,format="%m-%d"),sep="-")), y=d14_rollavg),color="black",size=0.6)+
  ggplot2::geom_line(data=d1641_2021_june, ggplot2::aes(x=as.Date(paste(2021,strftime(Date,format="%m-%d"),sep="-")), y=Outflow_required),color="red",size=2,alpha=0.5)+
  #Need to change below when 2020 data is in
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3))+
  ggplot2::ylab(bquote("Delta"~outflow~"("*ft^3*"/s)"))+
  ggplot2::xlab("")
plot_d1641


#D1641 plot for July to September (monthly averages)
d1641_2021_jul_sep <- d1641_2021 %>% mutate(Month_name=format(Date,"%B")) %>% group_by(Month,Month_name) %>% 
  summarise(OUT=mean(OUT),Outflow_required=mean(Outflow_required)) %>% filter(Month>6)

d1641_2021_jul_sep$Month_name<-factor(d1641_2021_jul_sep$Month_name, levels = c("July", "August", "September"))

plot_d1641_02 <- ggplot2::ggplot(data=d1641_2021_jul_sep,ggplot2::aes(x=Month_name))+
  ggplot2::theme_bw()+
  ggplot2::geom_bar(data=d1641_2021_jul_sep, ggplot2::aes(y=OUT),size=0.6,stat="identity")+
  #ggplot2::geom_line(data=d1641_2021_jul_sep, ggplot2::aes(x=as.numeric(Month_name),y=Outflow_required),color="red",size=2,alpha=0.5)+
  ggplot2::geom_hline(yintercept = 3000,size=2,alpha=0.5,color="red")+
  #Need to change below when 2020 data is in
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 legend.position = "none",
                 strip.background = element_rect(size=0.3))+
  ggplot2::ylab(bquote("Delta"~outflow~"("*ft^3*"/s)"))+
  ggplot2::xlab("")
plot_d1641_02

plot_d1641_combined<-ggarrange(plot_d1641, plot_d1641_02, widths = c(2,1),labels=c("A","B"))
plot_d1641_combined

#Print figure
tiff(filename=file.path(output_root,"Figure_Dayflow_D1641.tiff"),
     type="cairo",
     units="in", 
     width=8, #10*1, 
     height=4, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
print(plot_d1641_combined)
dev.off()
