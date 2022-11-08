library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
library(deltafish)
#Path to local drive
root <- "~/GitHub/Summer_Fall_Action_2022"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

#Use deltafish
surv <- open_survey()
fish <- open_fish()


# filter for sources and taxa of interest
surv_20mm <- surv %>% 
  filter(Source == "20mm") 

surv_EDSM <- surv %>% 
  filter(Source == "EDSM") 

fish_deltasmelt <- fish %>% 
  filter(Taxa %in% c("Hypomesus transpacificus"))


# do a join and collect the resulting data frame
# collect executes the sql query and gives you a table
data_EDSM_20mm<-left_join(surv_EDSM, fish_deltasmelt) %>% 
  collect() %>% filter(Method == "20mm net") %>% filter(Count>0)

data <- left_join(surv_20mm, fish_deltasmelt) %>% 
  collect()

data<-bind_rows(data, data_EDSM_20mm)

data$Year<-year(data$Date)
data$julianday<-yday(data$Date)
data<-data %>% filter(Count>0)

#Check for adults
ggplot(aes(y=Length,x=julianday),data=data)+geom_point()

#Remove adults
data_juv <- data %>% filter(Length<50 & julianday<150|Length<60&julianday<175&julianday>=150|Length<62.5&julianday>=175)

ggplot(aes(y=Length,x=julianday),data=data_juv)+geom_point()

#Multiply by count
data_juv<- setDT(expandRows(data_juv, "Count")) 

data_split <- split(data_juv, f = data_juv$Year)
#View(data_split$`2018`)

#Remove 2021 because only a single fish was collected
data_split$`2021`<-NULL

#Perform ols on every single year
lm_results <- lapply(data_split, function(x) lm(Length ~ julianday, data=x))

#lapply(lm_results,function(x) summary(x))
#lapply(lm_results,function(x) plot(x))

lm_results_list<-lapply(lm_results, function(x) data.frame(intercept=x$coefficients[[1]],coefficient = x$coefficients[[2]], adj_rsquared= summary(x)$adj.r.squared))

#bind data
lm_results <- do.call("rbind", lm_results_list)
lm_results$year <- rownames(lm_results) 

#assume 5.4 mm at birth (even though Xieu et al. says standard length)
lm_results$hatch_julianday<-(5.4-lm_results$intercept)/lm_results$coefficient

#Convert back to date
lm_results$hatch_date<-parse_date_time(x = paste(lm_results$year, as.integer(lm_results$hatch_julianday)), orders = "yj")

#write out csv
write.csv(lm_results,file.path(output_root,"20mm_OLS_hatchdate.csv"),row.names=F)

#Graph inspection
#lapply(data_split,function(x) ggplot(aes(y=Length,x=julianday),data=x)+
#         geom_point()+
#         stat_smooth(method = "lm", col = "red"))

summary(lm(hatch_julianday~as.integer(year),data=lm_results))
