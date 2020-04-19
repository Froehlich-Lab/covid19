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


#colors
cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme for plotting

#Our World in Data COVID (pulled both per million and totals)
covid_permil<-read.csv("covid-confirmed-cases-per-million-since-1-per-million.csv") 
head(covid_permil)
covid_totl<-read.csv("total-cases-covid-19.csv") 
head(covid_tot)

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
