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


setwd("~/Desktop/github/covid19/data")

#colors
cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme for plotting

#Our World in Data COVID (pulled both per million and totals)
covid_permil<-read.csv("covid-confirmed-cases-per-million-since-1-per-million.csv") 
head(covid_permil)
covid_tot<-read.csv("total-cases-covid-19.csv") 
head(covid_tot)

#DEAL WITH DATE
covid_permil$Date <- mdy(covid_permil$Date)
head(covid_permil)

#CLEAN NAMES
cl_covid <- covid_permil %>%
  clean_names()
head(cl_covid)

#PICK STATES
id<- c("USA","CHN","IND")
cnty_covid<-filter(cl_covid, iso3 %in% id)
head(cnty_covid)

#PLOT WITH EXAMPLE DATES (MODIFY BASED ON NEWS SELECTION)
ggplot(cnty_covid, aes(x=date, y=log(x_cases_per_million+1), group=entity)) +
  geom_line(aes(color=entity), size=2)+
  labs(x = "Time", y="LN(Confirmed Cases per Million")+
  scale_x_date(date_labels = "%Y %b %d")+
  geom_vline(xintercept = as.numeric(as.Date(c("2020-01-23", "2020-03-01"))), linetype=2, 
             color=c("black", "gray"))+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_color_brewer(palette="PuBu")

p

#Our World in Data seafood per captia
sea_data<-read.csv("fish-and-seafood-consumption-per-capita.csv") 
head(sea_data)

sPDF.imp <- joinCountryData2Map(sea_data ,joinCode = "ISO3", nameJoinColumn = "ISO3", verbose=TRUE)
catmethod=seq(1, 6, by=1) 
numCats <- 6
colourPalette <- colorRampPalette(brewer.pal(11, "Spectral")) (6)

#quartz()
mapCountryData(sPDF.imp,nameColumnToPlot='Check', colourPalette=colourPalette, catMethod=catmethod,
               missingCountryCol="gray",addLegend=F,oceanCol="white", mapTitle="")
