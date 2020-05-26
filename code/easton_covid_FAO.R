##Froehlich FAO prodcution time series (& map?)
#Started March 25, 2020

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(dplyr) #has to be opened after tidyverse or else summarise wont work
#library(tidyr)
#library(lme4)
library(multcomp)
library(patchwork)
library(janitor)

setwd("~/Desktop/github/covid19/data/FAO_usa")

#Load FAO times estimates for USA
prod.data<-read_csv("USA_FAO_aq&cap_FIG.csv") #production 1950-2017
consmp.data <- read.csv("FAO_us_consumption_ts.csv") #consumption 1961-2013
head(prod.data)
head(consmp.data)

unique(prod.data$major_grp)

food.prod <- filter(prod.data, major_grp != "MAMMALIA")
food.prod <- filter(food.prod, major_grp != "AMPHIBIA, REPTILIA")

unique(consmp.data$Unit)

consmp.tot <- filter(consmp.data, Unit == "tonnes")
consmp.cap <- filter(consmp.data, Unit == "kg/capita/yr")

#prod.data[is.na(prod.data)] <- 0

cl_prod <- food.prod %>%
  clean_names
cl_tot <- consmp.tot %>%
  clean_names
cl_cap <- consmp.cap %>%
  clean_names

head(cl_cap)
unique(cl_prod$major_grp)

grp_prod <- cl_prod %>%
  group_by(year, type, envt)%>%
  summarise(n_spp = n(), total_tonnes = sum(tonnes))
head(grp_prod)

cl_prod %>%
  filter(year==2017)%>%
  group_by(type)%>%
  summarise(n_spp = n(), total_tonnes = sum(tonnes))

#stats
439670/(5040435+ 439670)
5040435/(5040435+ 439670)

quartz()
pO<-ggplot(grp_prod, aes(x= year, y=(total_tonnes/1000000) , group=type)) +
  geom_line(aes(color=type), size=1)+
  labs(x = "Year", y="Harvest (million tonnes)")+
  #geom_vline(xintercept = as.numeric(as.Date(c("2020-03-11"))), linetype=2,color="red")+
  theme_classic()+
  facet_wrap(~envt, scales = "free")+
  #geom_smooth(aes(color = as.factor(year), fill = as.factor(year)), method = "loess") +
  scale_fill_manual(values = c("#FFC300", "#00AFBB"))+
  scale_color_manual(values = c("#FFC300", "#00AFBB"))
  #scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_fill_viridis(discrete = TRUE) 


head(cl_cap)
head(cl_tot)


p1<-ggplot(cl_cap, aes(x= year, y=(value) , group=element)) +
  geom_line(aes(color=element), size=1)+
  labs(x = "Year", y="Seafood supply (g/capita/day)")+
  #geom_vline(xintercept = as.numeric(as.Date(c("2020-03-11"))), linetype=2,color="red")+
  theme_classic()+
  facet_wrap(~element, scales = "free")+
  #geom_smooth(aes(color = as.factor(year), fill = as.factor(year)), method = "loess") +
  scale_fill_manual(values = c("#DAF7A6", "#00AFBB"))+
  scale_color_manual(values = c("#DAF7A6", "#00AFBB"))+
  theme(legend.position = "none")
#scale_color_viridis(discrete = TRUE, option = "D")+
#scale_fill_viridis(discrete = TRUE) 

p2<-ggplot(cl_tot, aes(x= year, y=(value/1000000) , group=element)) +
  geom_line(aes(color=element), size=1)+
  labs(x = "Year", y="Consumption (million tonnes)")+
  #geom_vline(xintercept = as.numeric(as.Date(c("2020-03-11"))), linetype=2,color="red")+
  theme_classic()+
  facet_wrap(~element, scales = "free")+
  #geom_smooth(aes(color = as.factor(year), fill = as.factor(year)), method = "loess") +
  scale_fill_manual(values = c( "#1ABC9C"))+
  scale_color_manual(values = c("#1ABC9C"))+
  theme(legend.position = "none")
#scale_color_viridis(discrete = TRUE, option = "D")+
#scale_fill_viridis(discrete = TRUE) 

quartz()
pO/p2
