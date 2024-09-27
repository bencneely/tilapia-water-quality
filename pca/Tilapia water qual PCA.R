#--------------------------------------------------------------
#Ben Neely
#09/19/2024
#Look at similarities with EREP and FHTC water quality data
#--------------------------------------------------------------

## Clear R
cat("\014")  
rm(list=ls())

## Install and load packages
## Checks if package is installed, installs if not, activates for current session
if("FSA" %in% rownames(installed.packages()) == FALSE) {install.packages("FSA")}
library(FSA)

if("rio" %in% rownames(installed.packages()) == FALSE) {install.packages("rio")}
library(rio)

if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
library(lubridate)

if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}
library(patchwork)

if("ggbiplot" %in% rownames(installed.packages()) == FALSE) {install.packages("ggbiplot")}
library(ggbiplot)

if("vegan" %in% rownames(installed.packages()) == FALSE) {install.packages("vegan")}
library(vegan)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

## Set seed for repeatability
set.seed(919)

## Set ggplot theme
pubtheme=theme_classic()+
  theme(panel.grid=element_blank(), 
        panel.background=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(fill="transparent"),
        axis.title=element_text(size=16,color="black",face="bold"),
        axis.text=element_text(size=12,color="black"),
        legend.position=c(0.01,0.99),
        legend.justification=c("left","top"),
        legend.title=element_blank(),
        legend.text=element_text(size=20))
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/Tilapia experiment/EREP FHTC water quality analysis/")

## Read in data with import
dat=import("cleandat.csv")

## Look at temporal autocorrelation of data
## Eyeball test to see where correlation falls between blue lines
erep=subset(dat,impd=="EREP")
acf(erep$tn)           ## 3
acf(erep$tp)           ## 4
acf(erep$tntp)         ## 1 
acf(erep$secchi)       ## 4
acf(erep$chloro)       ## 4
acf(erep$phyco)        ## 2
acf(erep$ph)           ## 2
acf(erep$orp_top)      ## 6
acf(erep$orp_bottom)   ## 6 
acf(erep$do_top)       ## 3  
acf(erep$do_bottom)    ## 2
acf(erep$tds)          ## 2
acf(erep$temp_top)     ## 5
acf(erep$temp_bottom)  ## 5

## Mean number of consecutive samples we could average
(3+4+1+4+4+2+2+6+6+3+2+2+5+5)/14

## Take mean of every THREE samples to mitigate temporal autocorrelation
dat1=dat%>%
  group_by(impd,grp)%>%
  summarize(tn=mean(tn),
            tp=mean(tp),
            tntp=mean(tntp),
            secchi=mean(secchi),
            chloro=mean(chloro),
            phyco=mean(phyco),
            ph=mean(ph),
            orp_top=mean(orp_top),
            orp_bottom=mean(orp_bottom),
            do_top=mean(do_top),
            do_bottom=mean(do_bottom),
            tds=mean(tds),
            temp_top=mean(temp_top),
            temp_bottom=mean(temp_bottom))%>%
  ungroup()

########################################################################################
## Conduct PERMANOVA
vars=dat1[,3:16]
vars_scaled=scale(vars)

## Permanova
adonis2(vars_scaled~impd,data=dat1,scale=T,method="euclidian")
#F = 1.705
#P = 0.124

########################################################################################
## Plot PCA for variables water quality variables
moddat=dat[4:17]
mod=prcomp(moddat,center=T,scale=T)

ggbiplot(mod,
         obs.scale=1,
         var.scale=1,
         point.size=2,
         alpha=0.25,
         groups=dat1$impd,
         ellipse=T,
         ellipse.prob=0.75,
         ellipse.alpha=0.1,
         varname.size=6)+
  scale_color_manual(values=c("#931314","#00008b"))+
  scale_fill_manual(values=c("#931314","#00008b"))+
  annotate("text",x=5.3,y=-2.4,label="italic(F)==1.705",hjust=1,vjust=1,size=8,parse=T)+
  annotate("text",x=5.3,y=-2.8,label="italic(P)==0.124",hjust=1,vjust=1,size=8,parse=T)+
  pubtheme

ggsave(plot=last_plot(),"pca/pca.svg",height=8,width=8,bg="white")
