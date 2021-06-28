library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(sp)
library(spData)
library(tmap)
library(leaflet)
library(stringr)
library(plotly)
library(ggfortify)
library(naniar)
library(ggalt)
library(shiny)

download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip")
data("World")
names(World)[1]<-"open_covid_region_code"

dat<-read.csv("C:\\Users\\LENOVO\\Downloads\\finalexam.csv")
dat<-as_tibble(dat)
dat$date<-as.Date(dat$date)
options(scipen = 999)
dat1<-inner_join(dat,World,by= "open_covid_region_code")%>%
  dplyr::select(-continent,-geometry,-X,-region_name,-area,-pop_est,-pop_est_dens,-economy,-income_grp,-gdp_cap_est,-life_exp,-well_being,-footprint,-inequality,-HPI)

dat1<-dat1[,c(50,51,52,1:49)]
dat1$open_covid_region_code<-as.character(dat1$open_covid_region_code)
dat1$cases_cumulative<-as.numeric(dat1$cases_cumulative)
dat1$cases_new<-as.numeric(dat1$cases_new)
dat1$deaths_cumulative<-as.numeric(dat1$deaths_cumulative)
dat1$deaths_new<-as.numeric(dat1$deaths_new)
dat1$confirmed_cases<-as.numeric(dat1$confirmed_cases)
dat1$confirmed_deaths<-as.numeric(dat1$confirmed_deaths)


jan<-dat1%>%group_by(open_covid_region_code)%>%
  filter(str_detect(date,"2020-01-"))%>%
  slice_max(date)
feb<-dat1%>%group_by(open_covid_region_code)%>%
  filter(str_detect(date,"2020-02-"))%>%
  slice_max(date)
mar<-dat1%>%group_by(open_covid_region_code)%>%
  filter(str_detect(date,"2020-03-"))%>%
  slice_max(date)
apr<-dat1%>%group_by(open_covid_region_code)%>%
  filter(str_detect(date,"2020-04-"))%>%
  slice_max(date)
may<-dat1%>%group_by(open_covid_region_code)%>%
  filter(str_detect(date,"2020-05-"))%>%
  slice_max(date)
jun<-dat1%>%group_by(open_covid_region_code)%>%
  filter(str_detect(date,"2020-06-"))%>%
  slice_max(date)
jul<-dat1%>%group_by(open_covid_region_code)%>%
  filter(str_detect(date,"2020-07-"))%>%
  slice_max(date)
aug<-dat1%>%group_by(open_covid_region_code)%>%
  filter(str_detect(date,"2020-08-"))%>%
  slice_max(date)
sep<-dat1%>%group_by(open_covid_region_code)%>%
  filter(str_detect(date,"2020-09-"))%>%
  slice_max(date)
oct<-dat1%>%group_by(open_covid_region_code)%>%
  filter(str_detect(date,"2020-10-"))%>%
  slice_max(date)
nov<-dat1%>%group_by(open_covid_region_code)%>%
  filter(str_detect(date,"2020-11-"))%>%
  slice_max(date)
dec<-dat1%>%group_by(open_covid_region_code)%>%
  filter(str_detect(date,"2020-12-"))%>%
  slice_max(date)

dat1<-rbind(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)
remove(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)

dat1$date<-factor(dat1$date,levels=c("มกราคม","กุมภาพันธ์","มีนาคม","เมษายน","พฤษภาคม","มิถุนายน","กรกฎาคม","สิงหาคม","กันยายน","ตุลาคม","พฤศจิกายน","ธันวาคม"))
###By the end of 2020,  total number of cases 

dat1<-dat1%>%
  mutate(hover=paste0(sovereignt,"\n stringency index =",stringency_index,"\n containment health index =",containment_health_index,"\n economic support index =",economic_support_index))

fontStyle=list(family="TH Sarabun New",size=26,color="black")
label=list(bgcolor="#EEEEEE",bordercolor="transparent",font=list(family="TH Sarabun New",size=16,color="black"))


fig_case<-plot_ly(dat1,type='choropleth',zauto=F,text=~hover,hoverinfo=text,locations=~open_covid_region_code,z=~confirmed_cases,zmin=0,zmax=max(dat1$confirmed_cases),color=~confirmed_cases,colorscale="Viridis",frame=~date)%>%
  animation_slider(currentvalue = list(prefix = "", font = list(color="transparent")))%>%
  layout(margin=list(l=50,r=50,b=100,t=100),font=fontStyle,title='<b>Total COVID-19 Cases in 2020</b>')%>%
  style(hoverlabel=label)%>%
  config(displayModeBar=F)
                     
fig_death<-plot_ly(dat1,type='choropleth',zauto=F,text=~hover,hoverinfo=text,locations=~open_covid_region_code,z=~confirmed_deaths,zmin=0,zmax=max(dat1$confirmed_deaths),color=~confirmed_deaths,colorscale="Viridis",frame=~date)%>%
  animation_slider(currentvalue = list(prefix = "", font = list(color="transparent")))%>%
  layout(margin=list(l=50,r=50,b=100,t=100),font=fontStyle,title='<b>Total COVID-19 Casualties in 2020</b>')%>%
  style(hoverlabel=label)%>%
  config(displayModeBar=F)


htmlwidgets::saveWidget(as_widget(fig_case), "figcase.html")
htmlwidgets::saveWidget(as_widget(fig_death), "figdeath.html")

