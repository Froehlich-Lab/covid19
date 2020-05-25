##Froehlich FAO prodcution time series (& map?)
#Started March 25, 2020

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(dplyr) #has to be opened after tidyverse or else summarise wont work
library(plyr) #ldply
library(readr) #ldply
library(tidyr)
library(lme4)
library(multcomp)
library(patchwork)
library(janitor)

setwd("~/Desktop/github/covid19/data/FAO_usa")
prod.data<-read_csv("USA_FAO_aq&cap_FIG.csv")
head(prod.data)

prod.data[is.na(prod.data)] <- 0

cl_prod <- prod.data %>%
  clean_names
head(cl_prod)

grp_prod <- cl_prod %>%
  group_by(year, type)%>%
  summarise(total_tonnes = sum(tonnes))
head(grp_prod)

quartz()
ggplot(cl_prod, aes(x= year, y=log(tonnes+1) , group=type)) +
  geom_line(aes(color=type), size=0.2)+
  labs(x = "Year", y="LN(Harvest (tonnes))")+
  #geom_vline(xintercept = as.numeric(as.Date(c("2020-03-11"))), linetype=2,color="red")+
  theme_classic()+
  facet_wrap(~envt, scales = "free")

  #geom_smooth(aes(color = as.factor(year), fill = as.factor(year)), method = "loess") +
  scale_fill_manual(values = c("#FFC300", "#00AFBB"))+
  scale_color_manual(values = c("#FFC300", "#00AFBB"))
  #scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_fill_viridis(discrete = TRUE) 