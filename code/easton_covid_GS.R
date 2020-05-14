rm(list=ls())

library(chron)
library(here)
library(RColorBrewer)
library(lattice)
library(ncdf4)
library(oce)
library(raster)
library(rgdal)
library(stats)
library(ggmap)
library(rworldmap)
library(tidyverse)
library(janitor)
library(lubridate)
library(patchwork)
library(viridis)
library(grid)

#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library("rnaturalearth")
library("rnaturalearthdata")
theme_set(theme_bw())
library("sf")

setwd("~/Desktop/github/covid19/data/Google trends/search term_specific")

data<-read_csv("st_spec_COMBINED.csv") #time trends from Jan 1 - May 13 2020 & 2019
head(data)

data$Day <- mdy(data$day)
head(data)

cl_data <- data %>%
  clean_names()
head(cl_data)

dates <- unique(cl_data$day)
dates

quartz()
ggplot(cl_data, aes(x= std_day, y=gs_trend, group=search_term, color=search_term)) +
  geom_line(aes(color=search_term), size=1)+
  labs(x = "Time", y="Google Trend Value")+
  #scale_x_date(date_labels = "%Y %b %d")+
  #geom_vline(xintercept = as.numeric(as.Date(c("2020-03-11"))), linetype=2,color="red")+
  geom_vline(xintercept = 70, linetype=1,color="red")+
  theme_classic()+
  scale_color_viridis(discrete = TRUE, option = "D")+
  facet_wrap(~year)+
  geom_smooth()
