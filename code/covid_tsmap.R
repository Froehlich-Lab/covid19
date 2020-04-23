rm(list=ls())

library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)
library(oce)
library(raster)
library(rgdal)
library(stats)
library(ggmap)
library(rworldmap)
library(dplyr)
library(tidyr)
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

setwd("~/Desktop/github/covid19/data")

#colors
cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme for plotting

#Our World in Data COVID (pulled both per million and totals)
covid_permil<-read.csv("covid-confirmed-cases-per-million-since-1-per-million.csv") 
head(covid_permil)
covid_tot<-read.csv("total-cases-covid-19.csv") 
head(covid_tot)
chn_news<-read.csv("china_news.csv") 
head(chn_news)

#NEWS BACK BONE OF DATES AND COUNTRIES OF INTEREST
chn_news$clean_date <- mdy(chn_news$clean_date)
head(chn_news)

  dates <- na.omit(unique(chn_news$clean_date))
  dates #10 total dates of interest dates[c(1,4,5,7:10)]
  link<- na.omit(unique(chn_news$linked_countires))
  link #10 countries

#DEAL WITH DATE
covid_permil$Date <- mdy(covid_permil$Date)
head(covid_permil)

#CLEAN NAMES
cl_covid <- covid_permil %>%
  clean_names()
head(cl_covid)
unique(cl_covid$iso3)

#PICK STATES
id<- c("USA","CHN","IND","AUS", "CHL", "NOR",
       "PER","CAN", "ZAF", "KEN")
cnty_covid<-filter(cl_covid, iso3 %in% id)
head(cnty_covid)
max_pt<-log(max(cnty_covid$x_cases_per_million+1))

#PLOT WITH EXAMPLE DATES (MODIFY BASED ON NEWS SELECTION)
max_day<-max(as.Date(cnty_covid$date))
quartz()
p1<-ggplot(cnty_covid, aes(x=date, y=log(x_cases_per_million+1), group=entity)) +
  geom_line(aes(color=entity), size=2)+
  labs(x = "Time", y="LN(Confirmed Cases per Million)")+
  scale_x_date(date_labels = "%Y %b %d")+
  #geom_vline(xintercept = as.numeric(as.Date(c("2020-01-23", "2020-03-01"))), linetype=2,color="black", "gray")+
  geom_vline(xintercept = as.numeric(as.Date(dates[c(1,4,5,7:10)])), linetype=2,color="black")+  
  theme_classic()+
  scale_color_viridis(discrete = TRUE, option = "D")+
  geom_text(data = filter(cnty_covid, date == max_day), aes(label = entity, colour = entity, x = max_day, y = log(x_cases_per_million+1)), hjust = -.1)+
  theme(legend.position = 'none', plot.margin = unit(c(1,4,1,1), "lines"))+
  annotate("text", x = as.Date(dates[c(1,4,5,7:10)]-2), y=c(max_pt), label = c("A.","B.","C.","D.","E.","F.","G."))
 
quartz()
gt <- ggplotGrob(p1)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
  
    

#OUR WORLD IN DATA SEAFOOD PER CAP MAP
#RICH, I JUST PUT IN A BASIC MAP TO SEE HOW THE PATCHWORK PLOT LOOKS, MODIFY AS YOU SEE FIT!

sea_data<-read.csv("fish-and-seafood-consumption-per-capita.csv") 
head(sea_data)

sea_2013<-filter(sea_data, Year==2013)
head(sea_data)
names(sea_data) <- c("region","iso3","year","per_cap_kg")
head(sea_data)

world_map <- map_data("world")
head(world_map)

comb_data<- left_join(world_map, sea_data, by="region")
head(comb_data)
comb_data$per_cap_kg

p2<-ggplot(comb_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = per_cap_kg), colour = "white")+
  scale_fill_viridis_c(option = "C")
p2

#PLOT TIME-LINE AND MAP TOGETHER
p1/p2
