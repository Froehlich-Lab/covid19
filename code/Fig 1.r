#=============================================================
#Data from Halley Froehlich 28 May 2020 (HF edit 2 June 2020)
#For paper on executive order on fish and aquaculture
#plot by Trevor Branch tbranch@uw.edu
#=============================================================
library(tidyverse)

setwd("~/Desktop/github/covid19/data/FAO_usa")

   XX <- read.csv(file="all_FAO_data.csv", stringsAsFactors=FALSE)
   #Three panels: aquaculture, capture, and consumption
   aq.fresh <- filter(XX, envt=="freshwater", type=="aquaculture")
   aq.salt <- filter(XX, envt=="marine", type=="aquaculture")
   cap.fresh <- filter(XX, envt=="freshwater", type=="capture")
   cap.salt <- filter(XX, envt=="marine", type=="capture")
   eaten <- filter(XX, element=="Food supply quantity (tonnes)")
   eaten.yr <- sort(unique(XX$year))
   eaten.amount <- tapply(X=XX$consumption_tonnes, FUN=mean, INDEX=XX$year)
   
   max.in.panels <- c(max(aq.fresh$prod_tonnes, cap.fresh$prod_tonnes),
                      max(aq.salt$prod_tonnes, cap.salt$prod_tonnes),
                      max(eaten.amount, na.rm=TRUE))
   
   line.colors <- c("#e41a1c","#377eb8","#984ea3")
   
   scaling <- 1e06 #divide by 1 million
   max.in.panels <- 1.1*max.in.panels/scaling
   
   quartz()
   par(mar=c(0,0,0,0), oma=c(5,5,1,1))
   layout(mat=matrix(1:3,nrow=3, ncol=1),widths=1, 
          heights=c(3,max.in.panels[2:3]))
   plot(aq.fresh$year, aq.fresh$prod_tonnes/scaling, type="l", 
        col=line.colors[1], lwd=2,
        ylim=c(0,max.in.panels[1]), yaxs="i", xaxs="i", xpd=NA,
        las=1, axes=FALSE, ann=FALSE)
   lines(x=cap.fresh$year, y=cap.fresh$prod_tonnes/scaling, 
         col=line.colors[2],
         lwd=2)
   axis(side=2, at=c(0,0.2,0.4), las=1,col="gray30",col.axis="gray30")
   box(col="gray30")
   text(x=c(1977.5, 1965.5), y=c(0.31,0.13), 
        labels=c("Freshwater aquaculture", "Freshwater capture"),
        col=c(line.colors[1:2]), cex=1.3)
   
   plot(cap.salt$year, cap.salt$prod_tonnes/scaling, type="l", 
        col=line.colors[2], lwd=2,
        ylim=c(0,max.in.panels[2]), yaxs="i", xaxs="i", xpd=NA,
        las=1,axes=FALSE, ann=FALSE)
   axis(side=2, at=c(0,2,4), las=1,col="gray30",col.axis="gray30")
   lines(x=aq.salt$year, y=aq.salt$prod_tonnes/scaling, 
         col=line.colors[1], lwd=2)
   box(col="gray30")
   text(x=c(1963.5, 1967), y=c(3.1,0.5), 
        labels=c("Marine capture", "Marine aquaculture"),
        col=c(line.colors[2:1]), cex=1.3)
   
   
   plot(x=eaten.yr, y=eaten.amount/scaling, type="l", 
        col=line.colors[3],
        ylim=c(0,max.in.panels[3]), yaxs="i", xaxs="i", xpd=NA,
        las=1,axes=FALSE, ann=FALSE, lwd=2)
   axis(side=2, at=c(0,2,4,6), las=1, col="gray30",col.axis="gray30")
   axis(side=1, col="gray30",col.axis="gray30")
   box(col="gray30")
   mtext(side=1, outer=TRUE, "Year", cex=1, line=3)
   mtext(side=2, outer=TRUE, "Millions of tonnes", cex=1, line=3)
   text(x=1983, y=6.3, 
        labels=c("Total consumption"),
        col=c(line.colors[3]), cex=1.3)


