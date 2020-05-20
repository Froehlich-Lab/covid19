##Froehlich Google Search time series & map
#Started May 13, 2020

rm(list=ls())

library(ggplot2)
library(tidyverse)
library(dplyr) #has to be opened after tidyverse or else summarise wont work
library(plyr) #ldply
library(readr) #ldply
library(tidyr)
library(multcomp)

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
library(janitor)
library(lubridate)
library(patchwork)
library(viridis)
library(grid)

#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library("gghighlight")
library("rnaturalearth")
library("rnaturalearthdata")
theme_set(theme_bw())
library("sf")

setwd("~/Desktop/github/covid19/data/Google trends/search term")

#list of files of interest (2016-2020) times series and map google trends
myfiles_ts = list.files(path=getwd(), pattern="multi.*.csv", full.names=TRUE)
myfiles_map = list.files(path=getwd(), pattern="geo.*.csv", full.names=TRUE)

#Read every csv within the designated folder
dat_csv_ts = ldply(myfiles_ts, read_csv)
dat_csv_map = ldply(myfiles_map, read_csv)

head(dat_csv_ts)
unique(dat_csv_ts$year)

cl_ts <- dat_csv_ts %>%
  clean_names
cl_ts$day <- mdy(cl_ts$day)
head(cl_ts)

cl_ts$std_day<-c(0:135, 0:134, 0:134, 0:134, 0:132)
head(cl_ts)

lg_ts<-gather(cl_ts, search_term, google_value, seafood_restaurant, seafood_delivery,seafood_recipe,
              sushi_take_out, bbq_restaurant, factor_key=TRUE)
head(lg_ts)

dates <- unique(cl_ts$day)
dates

quartz()
ggplot(lg_ts, aes(x= std_day, y=google_value, group=as.factor(year), fill=as.factor(year))) +
  geom_line(aes(color=as.factor(year)), size=0.2)+
  labs(x = "Time", y="Google Trend Value")+
  #scale_x_date(date_labels = "%Y %b %d")+
  #geom_vline(xintercept = as.numeric(as.Date(c("2020-03-11"))), linetype=2,color="red")+
  geom_vline(xintercept = 70, linetype=2,color="#FC4E07", cex=0.8)+
  theme_classic()+
  facet_wrap(~search_term, scales = "free")+
  geom_smooth(aes(color = as.factor(year), fill = as.factor(year)), method = "loess") +
  scale_fill_manual(values = c("gray", "gray","gray", "gray", "#00AFBB"))+
  scale_color_manual(values = c("gray", "gray","gray", "gray", "#00AFBB"))
  #scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_fill_viridis(discrete = TRUE) 
