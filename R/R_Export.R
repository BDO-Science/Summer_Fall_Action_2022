library(tidyverse)
library(lubridate)
library(viridis)

#Path to local drive
root <- "~/GitHub/Summer_Fall_Action_2022"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")


data_export <- read_csv(file.path(data_root, "Dayflow", "NDOI.csv"))
data_export$Date<-as.Date(data_export$Date,"%m/%d/%Y")
data_export_edit<-data_export %>% mutate (Federal = CVP, State = SWP) %>% select("Date","State","Federal") %>%
  tidyr::gather("Facility","Export",2:3) %>% #Subset to June to September
  filter(month(Date) %in% c(6:9))

data_export_edit$Facility<-ordered(data_export_edit$Facility, levels = c("State", "Federal"))

#Armin indicated that CVO wanted the export from two facilities combined
data_export_sum <- data_export_edit %>% group_by(Date) %>% summarise(Export=sum(Export))
  
export_plot<-ggplot(data_export_sum, aes(y=Export, x=Date)) + 
  geom_point(size=0.5) + geom_line() +
  scale_fill_viridis(discrete = T) +
  ggtitle("Summer-Fall 2022") +
  theme_bw() +
  ggplot2::ylab(bquote("Daily"~export~"("*ft^3*"/s)"))+
  xlab("")+
  ggplot2::theme(plot.title=element_text(size=9), 
                 axis.text.x=element_text(size=9, color="black"), 
                 axis.text.y = element_text(size=8, color="black"), 
                 axis.title.x = element_text(size = 9, angle = 00), 
                 axis.title.y = element_text(size = 9, angle = 90),
                 strip.text = element_text(size = 7),
                 legend.position = c(0.8, 0.8),
                 legend.background = element_rect(fill="white",
                                                  size=0.5, linetype="solid", 
                                                  colour ="black"),
                 strip.background = element_rect(size=0.3)) 
export_plot
#############Print figure

tiff(filename=file.path(output_root,"Figure_Export.png"), 
     type="cairo",
     units="in", 
     width=6, #10*1, 
     height=4, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
print(export_plot)
dev.off()