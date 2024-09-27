#--------------------------------------------------------------
#Ben Neely
#09/23/2024
#Analyze EREP and FHTC water quality data
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

if("zoo" %in% rownames(installed.packages()) == FALSE) {install.packages("zoo")}
library(zoo)

if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
library(lubridate)

if("patchwork" %in% rownames(installed.packages()) == FALSE) {install.packages("patchwork")}
library(patchwork)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)

## Set ggplot theme
pubtheme=theme_classic()+
  theme(panel.grid=element_blank(), 
        panel.background=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(fill="transparent"),
        axis.title=element_text(size=14,color="black",face="bold"),
        axis.text=element_text(size=12,color="black"),
        legend.position="none")
options(scipen=999)

## Set working directory
setwd("C:/Users/Ben.Neely/OneDrive - State of Kansas, OITS/Desktop/Tilapia experiment/EREP FHTC water quality analysis")

## Read in data with import
dat=import("cleandat.csv")%>%
  mutate(date=as.Date(date,tryFormats="%m/%d/%Y"))

################################################################################
## Create data set for plotting three observation moving averages
pdat=dat%>%
  group_by(impd)%>%
  mutate(tn_ma=rollmean(tn,k=3,fill=NA),
         tp_ma=rollmean(tp,k=3,fill=NA),
         tntp_ma=rollmean(tntp,k=3,fill=NA),
         secchi_ma=rollmean(secchi,k=3,fill=NA),
         chloro_ma=rollmean(chloro,k=3,fill=NA),
         phyco_ma=rollmean(phyco,k=3,fill=NA),
         ph_ma=rollmean(ph,k=3,fill=NA),
         orp_top_ma=rollmean(orp_top,k=3,fill=NA),
         orp_bottom_ma=rollmean(orp_bottom,k=3,fill=NA),
         do_top_ma=rollmean(do_top,k=3,fill=NA),
         do_bottom_ma=rollmean(do_bottom,k=3,fill=NA),
         tds_ma=rollmean(tds,k=3,fill=NA),
         temp_top_ma=rollmean(temp_top,k=3,fill=NA),
         temp_bottom_ma=rollmean(temp_bottom,k=3,fill=NA))%>%
  ungroup()

################################################################################
## Create plots
## TN plot
tn_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=tn_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=tn,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(0.6,3,0.2),
                     name="Total nitrogen (ppm)")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(0.58,3.1),
                  expand=F)+
  pubtheme

## TP plot
tp_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=tp_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=tp,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(0,0.9,0.1),
                     name="Total phosphorus (ppm)")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(0,0.83),
                  expand=F)+
  pubtheme

## TN/TP plot
tntp_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=tntp_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=tntp,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(0,100,10),
                     name="TN/TP")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(0,105),
                  expand=F)+
  pubtheme

## Secchi plot
secchi_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=secchi_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=secchi,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(0,180,20),
                     name="Secchi depth (cm)")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(0,185),
                  expand=F)+
  pubtheme

## Chlorophyll plot
chloro_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=chloro_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=chloro,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(0,90,10),
                     name="Chlorophyll (µg/L)")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(0,91),
                  expand=F)+
  pubtheme

## Phycocyanin plot
phyco_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=phyco_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=phyco,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(0,200,20),
                     name=paste("Phycocyanin (µg/L)"))+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(0,201),
                  expand=F)+
  pubtheme

## pH plot
ph_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=ph_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=ph,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(6.5,10,0.5),
                     name="pH")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(6.4,10.2),
                  expand=F)+
  pubtheme

## ORP top plot
orp_top_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=orp_top_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=orp_top,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(-200,300,100),
                     name="Top ORP (mV)")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(-210,310),
                  expand=F)+
  pubtheme

## ORP bottom plot
orp_bottom_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=orp_bottom_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=orp_bottom,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(-200,300,100),
                     name="Bottom ORP (mV)")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(-210,310),
                  expand=F)+
  pubtheme

## DO top plot
do_top_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=do_top_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=do_top,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(0,12,2),
                     name="Top dissolved oxygen (ppm)")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(0,12.1),
                  expand=F)+
  pubtheme

## DO bottom plot
do_bottom_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=do_bottom_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=do_bottom,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(0,12,2),
                     name="Bottom dissolved oxygen (ppm)")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(0,12.1),
                  expand=F)+
  pubtheme

## TDS plot
tds_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=tds_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=tds,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(50,250,25),
                     name="Total dissolved solids (ppm)")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(48,252),
                  expand=F)+
  pubtheme

## Water temperature top plot
temp_top_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=temp_top_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=temp_top,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(5,35,5),
                     name="Top water temp (\u00B0C)")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(4.5,33.5),
                  expand=F)+
  pubtheme

## Water temperature bottom plot
temp_bottom_plot=ggplot(pdat)+
  geom_line(aes(x=date,y=temp_bottom_ma,color=impd),linewidth=2)+
  geom_line(aes(x=date,y=temp_bottom,color=impd),alpha=0.2)+
  scale_color_manual(values=c("#cb283f","#102092"))+
  scale_x_date(date_labels="%b",
               date_breaks="month",
               name="")+
  scale_y_continuous(breaks=seq(5,35,5),
                     name="Bottom water temp (\u00B0C)")+
  geom_vline(aes(xintercept=as.Date("2023-05-11")),linetype="solid")+
  geom_vline(aes(xintercept=as.Date("2023-10-24")),linetype="dashed")+
  coord_cartesian(xlim=c(as.Date("2023-04-01"),as.Date("2023-12-10")),
                  ylim=c(4.5,33.5),
                  expand=F)+
  pubtheme+
  theme(legend.position=c(0.5,0.15),
        legend.text=element_text(size=24),
        legend.title=element_blank())

################
## Combine plots and export
out=tn_plot+tp_plot+tntp_plot+secchi_plot+
  chloro_plot+phyco_plot+ph_plot+orp_top_plot+
  orp_bottom_plot+do_top_plot+do_bottom_plot+tds_plot+
  temp_top_plot+temp_bottom_plot+plot_layout(ncol=4)

ggsave(plot=out,"time series/chem_ts.svg",width=14,height=14,bg="white")