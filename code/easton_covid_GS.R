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

setwd("~/Desktop/github/covid19/data/Google trends/search term_new")

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
cl_map <- dat_csv_map %>%
  clean_names

cl_ts$day <- mdy(cl_ts$day)

#cl_ts$std_day<-c(0:135, 0:134, 0:134, 0:134, 0:132)
#head(cl_ts)

lg_ts<-gather(cl_ts, search_term, google_value, seafood_restaurant, seafood_delivery,seafood_recipe,
              sushi_take_out, bbq_restaurant, factor_key=TRUE)
head(lg_ts)

lg_ts <- lg_ts %>%
         mutate(m_day = as.Date(m_day, format = "%m/%d")) #just put everything on the same "month and day" for plotting

dates <- unique(cl_ts$day)
dates


quartz()
ggplot(lg_ts, aes(x= m_day, y=google_value, group=as.factor(year), fill=as.factor(year))) +
  geom_line(aes(color=as.factor(year)), size=0.2)+
  labs(x = "Time", y="Google Trend Value")+
  scale_x_date(date_labels = "%Y %b %d")+
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-11"))), linetype=2,color="red")+
  #geom_vline(xintercept = 70, linetype=2,color="#FC4E07", cex=0.8)+
  theme_classic()+
  facet_wrap(~search_term, scales = "free")+
  geom_smooth(aes(color = as.factor(year), fill = as.factor(year)), method = "loess") +
  scale_fill_manual(values = c("gray", "gray","gray", "gray", "#00AFBB"))+
  scale_color_manual(values = c("gray", "gray","gray", "gray", "#00AFBB"))
  #scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_fill_viridis(discrete = TRUE) 

library(usmap)

head(cl_map)

#remove % and make value numeric
cl_map$seafood_restaurant <- as.numeric(sub("%", "",cl_map$seafood_restaurant))
cl_map$seafood_delivery <- as.numeric(sub("%", "",cl_map$seafood_delivery))
cl_map$seafood_recipe <- as.numeric(sub("%", "",cl_map$seafood_recipe))
cl_map$sushi_take_out <- as.numeric(sub("%", "",cl_map$sushi_take_out))
cl_map$bbq_restaurant <- as.numeric(sub("%", "",cl_map$bbq_restaurant))

#Make NA = 0
cl_map[is.na(cl_map)] <- 0

#gather data for ggplot
lg_map<-gather(cl_map, search_term, google_value, seafood_restaurant, seafood_delivery,seafood_recipe,
              sushi_take_out, bbq_restaurant, factor_key=TRUE)
lg_map$full <- lg_map$region

head(lg_map)
head(statepop)

full_map<-left_join(statepop,lg_map, by="full")
head(full_map)



unique(full_map$search_term)
sub_map <- filter(full_map, search_term=="seafood_restaurant")


#plot
quartz()
plot_usmap("states", data=sub_map, values = "google_value", color = "gray") + 
  theme(legend.position = "right")+
  labs()+
  facet_wrap(. ~ search_term + year)+
  scale_color_binned()+
  scale_color_viridis(discrete = FALSE, option = "D")+
  scale_fill_viridis(discrete = FALSE) 
  
