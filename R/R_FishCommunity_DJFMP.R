library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(readxl)

#Path to local drive
root <- "~/GitHub/Summer_Fall_Action_2022"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

#Read data from EDI posting and copy rds (compressed) file into data-raw folder
#data_djfmp<- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.9&entityid=147fd5e2c7db15913b2ffa44410dc7f9")
#saveRDS(data_djfmp, file.path(data_root,"DJFMP","DJFMP_beachseine.rds"))

#Reload data
data_djfmp<-readRDS(file.path(data_root,"DJFMP","DJFMP_beachseine.rds"))

data_djfmp$SampleDate<-as.Date(data_djfmp$SampleDate,"%Y-%m-%d")
str(data_djfmp)

data_djfmp_2022<-read_excel(file.path(data_root,"DJFMP","BrianM_DJFMP_FY22.xlsx"))
str(data_djfmp_2022)

data_djfmp_combined<-bind_rows(data_djfmp %>% select(Location, StationCode, SampleDate, SampleTime, GearConditionCode, Volume, OrganismCode, CommonName, ForkLength, Count) %>%
                                 mutate(StationCode=as.character(StationCode))
                                 ,data_djfmp_2022 %>% select(Location, StationCode, SampleDate, SampleTime, GearConditionCode, Volume, OrganismCode, CommonName, ForkLength, Count) %>%
                                 mutate(GearConditionCode=as.numeric(GearConditionCode)))
#Subset to select stations, months and years

select_stations<-c("DS002S", "GS010E", "LP003E","MK004W", "MR010W", "MS001N","MS001A","OR003W","OR014W","SF014E","SJ001S", "SJ005N", "SJ032S","SJ041N","SJ051E", "SJ056E", "SJ058W", "SJ058E","SR012E","SR012W","SR014W",  "SR017E", "SR024E", "SR043W", "SR049E", "SS011N","TM001N", "WD002W","WD002E","XC001N" )
#Stations from Mahardja et al. 2017 paper
data_djfmp_edit <- data_djfmp_combined %>% filter(StationCode %in% select_stations, year(SampleDate)>=1995, month(SampleDate) %in% c(3:8), GearConditionCode <=2)


#List of different equations for transforming length to weights (to grams)
americanshad_function<-function(x){(0.0074*((x)^3.09))/1000} #Kimmerer American Shad
largemouth_function<-function(x){10^(3.12735*(log10(x))-5.16885)} #Schneider LMB
blacksidedarter_function<-function(x){10^(3.236*(log10(x))-5.4899)} #Schneider BlacksideDarter
bullhead_function<-function(x){10^(2.88495*(log10(x))-4.60512)} #Schneider Bullhead
blackcrappie_function<-function(x){10^(3.17980*(log10(x))-5.24330)} #Schneider BlackCrappie
bluegill_function<-function(x){10^(3.17266*(log10(x))-5.10377)} #Schneider Bluegill
splittail_function<-function(x){(0.0030*((x)^3.27))/1000} #Kimmerer Splittail
shimofurigoby_function<-function(x){(0.0017*((x)^3.47))/1000} #Kimmerer Shimofuri Goby
channelcatfish_function<-function(x){10^(3.2764*(log10(x))-5.8116)} #Schneider ChannelCat
carp_function<-function(x){(0.0670*((x)^2.85))/1000} #Kimmerer carp
deltasmelt_function<-function(x){(0.0018*((x)^3.38))/1000} #Kimmerer delta smelt
goldenshiner_function<-function(x){10^(3.08217*(log10(x))-5.24775)} #Schneider Golden Shiner
greensunfish_function<-function(x){10^(3.1644*(log10(x))-5.0697)} #Schneider Green Sunfish
silverside_function<-function(x){(0.0097*((x)^2.87))/1000} #Kimmerer Silverside
longfinsmelt_function<-function(x){(0.0005*((x)^3.69))/1000} #Kimmerer Longfin
pacherring_function<-function(x){(0.0015*((x)^3.44))/1000} #Kimmerer Pacific herring
pacstaghornsculpin_function<-function(x){(0.0090*((x)^3.06))/1000} #Kimmerer Pacific staghorn
pricklysculpin_function<-function(x){(0.0037*((x)^3.30))/1000} #Kimmerer Prickly Sculpin
rainwaterkillifish_function<-function(x){(0.0061*((x)^3.18))/1000} #Kimmerer Rainwater killifish
redearsunfish_function<-function(x){10^(3.33276*(log10(x))-5.46370)} #Schneider Redear
pikeminnow_function<-function(x){exp(2.915*(log(x))-11.10)} #Nobriga et al. 2006
sacsucker_function<-function(x){(0.0146*((x)^3.01))/1000} #Kimmerer Sac Sucker
shimofurigoby_function<-function(x){(0.0017*((x)^3.47))/1000} #Kimmerer Shimofuri Goby
smallmouth_function<-function(x){10^(3.02635*(log10(x))-4.91466)} #Schneider Smallmouth
starryflounder_function<-function(x){(0.0082*((x)^3.13))/1000} #Kimmerer Starry Flounder
stripedbass_function<-function(x){(0.0066*((x)^3.12))/1000} #Kimmerer Striped Bass
threadfinshad_function<-function(x){(0.0072*((x)^3.16))/1000} #Kimmerer Threadfin Shad
threespinestickleback_function<-function(x){(0.0086*((x)^3.04))/1000} #Kimmerer Threespine Stickleback
tuleperch_function<-function(x){(0.0204*((x)^3.03))/1000} #Kimmerer Tule Perch
westmosquitofish_function<-function(x){(0.0066*((x)^3.15))/1000} #Kimmerer Western Mosquitofish
warmouth_function<-function(x){10^(3.20625*(log10(x))-5.12390)} #Schneider Warmouth
whitecrappie_function<-function(x){10^(3.3835*(log10(x))-5.8236)} #Schneider White Crappie
yellowfingoby_function<-function(x){(0.0087*((x)^2.98))/1000} #Kimmerer Yellowfin Goby


#Apply function and get total weight
sort(unique(data_djfmp_edit$CommonName))
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="American shad",americanshad_function(data_djfmp_edit$ForkLength),0)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="bass unknown",largemouth_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="bigscale logperch",blacksidedarter_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="black bullhead",bullhead_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="black crappie",blackcrappie_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="Bluefin Killifish",rainwaterkillifish_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="bluegill",bluegill_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="brown bullhead",bullhead_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="California roach",splittail_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="chameleon goby",shimofurigoby_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="channel catfish",channelcatfish_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="common carp",carp_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="Delta smelt",deltasmelt_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="fathead minnow",goldenshiner_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="golden shiner",goldenshiner_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="goldfish",carp_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="green sunfish",greensunfish_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="hardhead",splittail_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="hitch",splittail_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="Mississippi silverside",silverside_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="largemouth bass",largemouth_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="longfin smelt",longfinsmelt_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="Pacific herring",pacherring_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="Pacific staghorn sculpin",pacstaghornsculpin_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="prickly sculpin",pricklysculpin_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="rainwater killifish",rainwaterkillifish_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="red shiner",goldenshiner_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="redear sunfish",redearsunfish_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="redeye bass",largemouth_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="Sacramento blackfish",splittail_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="Sacramento pikeminnow",pikeminnow_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="Sacramento sucker",sacsucker_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="shimofuri goby",shimofurigoby_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="Shokihaze goby",shimofurigoby_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="smallmouth bass",smallmouth_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="splittail",splittail_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="spotted bass",smallmouth_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="starry flounder",starryflounder_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="striped bass",stripedbass_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="threadfin shad",threadfinshad_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="threespine stickleback",threespinestickleback_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="tule perch",tuleperch_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="wakasagi",deltasmelt_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="warmouth",warmouth_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="western mosquitofish",westmosquitofish_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="white catfish",channelcatfish_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="white crappie",whitecrappie_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)
data_djfmp_edit$Weight<-ifelse(data_djfmp_edit$CommonName=="yellowfin goby",yellowfingoby_function(data_djfmp_edit$ForkLength),data_djfmp_edit$Weight)

data_djfmp_edit$Biomass<-data_djfmp_edit$Weight*data_djfmp_edit$Count
data_djfmp_edit$Weight<-NULL
#Restore the zeroes
data_djfmp_edit$Biomass[is.na(data_djfmp_edit$Biomass)] <- 0
#Add plus count
data_djfmp_edit$PlusCount<-ifelse(data_djfmp_edit$ForkLength==0,data_djfmp_edit$Count,0)
data_djfmp_edit$MeasuredCount<-ifelse(data_djfmp_edit$ForkLength>0,data_djfmp_edit$Count,0)

#Summarize by organism and sampling occassion
data_djfmp_summary <- data_djfmp_edit %>% group_by(Location,StationCode,SampleDate,SampleTime,OrganismCode, CommonName) %>%
  summarise(MeasuredCount=sum(MeasuredCount),Biomass=sum(Biomass),Volume=mean(Volume),PlusCount=sum(PlusCount))
#Multiply biomass by total count/measured count
mean(data_djfmp_summary$Biomass)
data_djfmp_summary$AdjustedBiomass<-data_djfmp_summary$Biomass*((data_djfmp_summary$MeasuredCount+data_djfmp_summary$PlusCount)/data_djfmp_summary$MeasuredCount)
data_djfmp_summary$AdjustedBiomass[is.na(data_djfmp_summary$AdjustedBiomass)] <- 0

mean(data_djfmp_summary$AdjustedBiomass)

unique(data_djfmp_summary$StationCode)
#Fill volume with mean volume
data_djfmp_summary$Volume<-ifelse(is.na(data_djfmp_summary$Volume),mean(data_djfmp_summary$Volume, na.rm = T),data_djfmp_summary$Volume)
summary(data_djfmp_summary$Volume)
str(data.frame(data_djfmp_summary))

#Change alternate station names into a single station
data_djfmp_summary$StationCode<-as.factor(data_djfmp_summary$StationCode)
levels(data_djfmp_summary$StationCode)[levels(data_djfmp_summary$StationCode)=="SR012W"] <- "SR012E"
levels(data_djfmp_summary$StationCode)[levels(data_djfmp_summary$StationCode)=="SJ058E"] <- "SJ058W"
levels(data_djfmp_summary$StationCode)[levels(data_djfmp_summary$StationCode)=="MS001A"] <- "MS001N"
levels(data_djfmp_summary$StationCode)[levels(data_djfmp_summary$StationCode)=="WD002E"] <- "WD002W"

#Check sampling effort
DJFMP_seine_sampling_effort<-data_djfmp_summary %>% mutate(Year=year(SampleDate),Month=month(SampleDate)) %>% group_by(Year,Month,Location,StationCode,SampleDate,SampleTime) %>%
  summarise(Count=sum(MeasuredCount)) %>% mutate(sample_size=1) %>% group_by(Year,Month,StationCode) %>% summarise(sample_size=sum(sample_size)) 

ggplot(DJFMP_seine_sampling_effort,aes(x=Year,y=StationCode,fill=sample_size))+geom_tile()+facet_wrap(~ Month)+
  scale_x_continuous(breaks=c(1970,1980,1990,2000,2010,2015,2020)) + labs(title= "DJFMP station sampling effort by year")


#Spread data wide to get zeroes
data_djfmp_wide <- data_djfmp_summary %>% mutate(Year=year(SampleDate),Month=month(SampleDate),BPUE=AdjustedBiomass/Volume)%>%
  select(-CommonName,-MeasuredCount,-Biomass,-Volume,-PlusCount,-AdjustedBiomass)

data_djfmp_wide<-  spread(data_djfmp_wide,OrganismCode,BPUE)
data_djfmp_wide[is.na(data_djfmp_wide)] <- 0

#Summarize by station, month, and year
data_djfmp_annual<- data_djfmp_wide %>% ungroup() %>% select(-Location,-SampleTime) %>% group_by(StationCode,Year,Month) %>%
  summarise_all(mean) %>% 
  ungroup() %>% select(-StationCode,-SampleDate) %>% group_by(Year,Month) %>%
  summarise_all(mean) %>%
  ungroup() %>% select(-Month) %>% group_by(Year) %>%
  summarise_all(mean)

data_djfmp_annual<-gather(data_djfmp_annual,"OrganismCode","BPUE",2:length(data_djfmp_annual))
#Add common name back
fish_taxa<-read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.9&entityid=17c9974d9b7b0125c146a887f3c64bd8")
data_djfmp_annual<-left_join(data_djfmp_annual,fish_taxa[,c("OrganismCode","CommonName")])

#Add species categories
data_djfmp_annual<- data_djfmp_annual %>% mutate(Species_Status= case_when(
  CommonName %in% c("American shad","bluegill","black crappie","common carp","chameleon goby",
                    "fathead minnow","goldfish","green sunfish","golden shiner","largemouth bass",
                    "bigscale logperch","western mosquitofish","Mississippi silverside",
                    "No catch","redear sunfish","rosyface shiner","red shiner",
                    "shimofuri goby","smallmouth bass","striped bass","threadfin shad",
                    "yellowfin goby","black bullhead","wakasagi","white catfish","white crappie","bass unknown",
                    "rainwater killifish","channel catfish","brown bullhead",
                    "spotted bass","unid fish","yellow bullhead","Shokihaze goby",
                    "warmouth","redeye bass","Bluefin Killifish","no catch" ) ~ "Introduced",
  CommonName %in% c("California roach","Chinook salmon","Delta smelt","hitch","hardhead","lamprey unknown",
                    "longfin smelt","prickly sculpin","Pacific staghorn sculpin",
                    "steelhead trout","Sacramento pikeminnow","Sacramento sucker","Sacramento blackfish",
                    "splittail","tule perch","threespine stickleback","starry flounder","striped mullet",
                    "minnow unknown","arrow goby","Pacific herring","river lamprey","bay goby","cheekspot goby",
                    "topsmelt"
                    ) ~ "Native"
), 
Taxa= case_when(
  CommonName %in% c("bluegill","black crappie","largemouth bass","green sunfish","redear sunfish",
                    "smallmouth bass","white crappie","bass unknown","spotted bass","warmouth","redeye bass") ~ "Centrarchids",
  CommonName %in% c("Mississippi silverside") ~ "Mississippi Silverside",
  CommonName %in% c("American shad","common carp","chameleon goby",
                    "fathead minnow","goldfish","golden shiner",
                    "bigscale logperch","western mosquitofish",
                    "No catch","rosyface shiner","red shiner",
                    "shimofuri goby","striped bass","threadfin shad",
                    "yellowfin goby","black bullhead","wakasagi","white catfish",
                    "rainwater killifish","channel catfish","brown bullhead",
                    "unid fish","yellow bullhead","Shokihaze goby",
                    "Bluefin Killifish") ~ "Other introduced species",
  CommonName %in% c("California roach","Chinook salmon","Delta smelt","hitch","hardhead","lamprey unknown",
                    "longfin smelt","prickly sculpin","Pacific staghorn sculpin",
                    "steelhead trout","Sacramento pikeminnow","Sacramento sucker","Sacramento blackfish",
                    "splittail","tule perch","threespine stickleback","starry flounder","striped mullet",
                    "minnow unknown","arrow goby","Pacific herring","river lamprey","bay goby","cheekspot goby",
                    "topsmelt","no catch") ~ "Native species"
))

annual_summary_status<-data_djfmp_annual %>% group_by(Year,Species_Status) %>% summarise(BPUE=sum(BPUE))


annual_summary_taxa<-data_djfmp_annual %>% group_by(Year,Taxa) %>% summarise(BPUE=sum(BPUE))
annual_summary_taxa$Taxa <- factor(annual_summary_taxa$Taxa, levels = c("Centrarchids", "Mississippi Silverside", "Other introduced species","Native species"))
#There were non-fish species in the most recent EDI posting in 2022, check that there were 0 BPUE and remove
annual_summary_status<-na.omit(annual_summary_status)
annual_summary_taxa<-na.omit(annual_summary_taxa)

str(annual_summary_status)
#Construct barplots

biomassplot_native_introduced<-ggplot(annual_summary_status,aes(x = Year, y = BPUE, fill=Species_Status,order=Species_Status))+
  geom_bar(position = 'stack', stat="identity", colour="black", show_guide=TRUE)+ theme_bw() +
  theme(axis.title.y = element_text(size = rel(1.8)),axis.title.x = element_text(size = rel(1.8), angle = 00),
        axis.text.x = element_text(size=14, color = "black",angle=45, vjust = 1, hjust=1),axis.text.y = element_text(size=23, color = "black"))+
  labs(title=NULL, x=NULL, y=expression(paste("Biomass per volume (grams/", m^3, ")",sep = "")))+
  scale_x_continuous(breaks=seq(min(annual_summary_status$Year),max(annual_summary_status$Year),1),labels = c(1995:2019,"2020*",2021:2022))+
  scale_y_continuous(breaks=seq(0,7,1))+ 
  theme(legend.title=element_blank(),legend.text=element_text(size=14),
        legend.justification=c(1,0), legend.position=c(0.35,0.65),legend.background = element_rect(colour = "black"))+
  scale_fill_manual(name="Status",values=c("red","blue"))
biomassplot_native_introduced

biomassplot_taxa<-ggplot(annual_summary_taxa,aes(x = Year, y = BPUE, fill=Taxa,order=Taxa))+
  geom_bar(position = 'stack', stat="identity", colour="black", show_guide=TRUE)+ theme_bw() +
  theme(axis.title.y = element_text(size = rel(1.8)),axis.title.x = element_text(size = rel(1.8), angle = 00),
        axis.text.x = element_text(size=18, color = "black",angle=45, vjust = 1, hjust=1),axis.text.y = element_text(size=18, color = "black"))+
  labs(title=NULL, x=NULL, y=expression(paste("Biomass per volume (grams/", m^3, ")",sep = "")))+
  scale_x_continuous(breaks=seq(min(annual_summary_status$Year),max(annual_summary_status$Year),1),labels = c(1995:2019,"2020*",2021:2022))+
  scale_y_continuous(breaks=seq(0,7,1))+ 
  theme(legend.title=element_blank(),legend.text=element_text(size=14),
        legend.justification=c(1,0), legend.position=c(0.45,0.75),legend.background = element_rect(colour = "black"))+
  scale_fill_manual(name="Taxon",values=c("green2","gold1","darkorange","darkblue"))
biomassplot_taxa


#Print figure
tiff(filename=file.path(output_root,"Figure_NearshoreFishes.tiff"),
     type="cairo",
     units="in", 
     width=14, #10*1, 
     height=8, #22*1, 
     pointsize=5, #12, 
     res=600,
     compression="lzw")
#grid.arrange(biomassplot_native_introduced, biomassplot_taxa, ncol=2,nrow=1)
biomassplot_taxa
dev.off()
