install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggmap")
install.packages("tmap")
install.packages("maptools")
install.packages("tigris")
install.packages("leaflet")
install.packages("geo_join")
install.packages("sp")
library(leaflet)
library(xlsx)
library(readr)
library(dplyr)
library(tidyverse)
library(tmap)
library(maptools)
library(tigris)
library(sp)
#read data 
ds_svi<-read_csv("Social Vulnerability.csv", col_types=cols("FIPS"=col_character()))
fiptozip<-read.delim("ZIP_TRACT_032020.txt", colClasses = c("character","character","character","character","character","character"))
nyc_zips<-read_csv("nyc_zips.csv")

#select svi variables
colnames(ds_svi)
col_svi<-c("STCNTY","COUNTY","FIPS","RPL_THEME1","RPL_THEME2","RPL_THEME3","RPL_THEME4","RPL_THEMES")
ds_svi<-ds_svi[col_svi]
n_distinct(ds_svi$FIPS) #FIPS unique 1 per obv
n_distinct(fiptozip$FIPS) #multiple zip codes per FIPS

#select fiptozip variables
fiptozip<-fiptozip[1:2]

#nyc zip - separate comma sep values into individual rows
nyc_zips<-separate_rows(nyc_zips, `ZIP Codes`, convert=FALSE)
colnames(nyc_zips)[3]<-"ZIP"
n_distinct(nyc_zips$ZIP) #178 zip codes in nyc

#Join svi and fiptozip for crosswalk
#colnames(fiptozip)[2]<-"FIPS"
#ds_merged<-left_join(ds_svi, fiptozip, by="FIPS")
#glimpse(ds_merged)
#join ds_merged and nyc_zips for crosswalk -assuming all other zips belong to areas outside of NYC
#ds_merged<-left_join(ds_merged, nyc_zips, by="ZIP")
#glimpse(ds_merged)
#ds_merged%>%filter(!is.na(Borough))%>%distinct(ZIP)%>%nrow() #177 nyc zip codes were mapped

#NYC specifc SVI dataset 
#ds_nyc<-ds_merged%>%filter(!is.na(Borough))

#nyc zip code not mapped
#zips_mapped<-unique(ds_nyc$ZIP)
#zips_nyc<-unique(nyc_zips$ZIP)
#setdiff(zips_nyc, zips_mapped) #zipcode = 11695 was not mapped

#attempt at only NYC counties: New York, Kings, Bronx, Richmond, and Queens
#This causes NAs within the leaflet color range 
#may need to attempt after geo merge
counties_nyc<-c("New York", "Kings", "Bronx", "Richmond", "Queens")
nyc_svi<-ds_svi%>%filter(COUNTY %in% counties_nyc)
nyc_svi$RPL_THEMES[nyc_svi$RPL_THEMES==-999]<-0
range(nyc_svi$RPL_THEMES)
glimpse(nyc_svi)

#bring in spatial data on  NY w/Tigris 
tracts <- tracts(state = 'NY', cb=TRUE)
names(nyc_svi)[3]<-"GEOID"
#social vulnerability index to percent
nyc_svi$RPL_THEMES <- 100*(nyc_svi$RPL_THEMES)


#sequence for ALL counties in NY
names(ds_svi)[3]<-"GEOID"
ds_svi$RPL_THEMES[ds_svi$RPL_THEMES==-999]<-0
ds_svi$RPL_THEMES <- 100*(ds_svi$RPL_THEMES)
range(ds_svi$RPL_THEMES)


#merge svi and spatial ny data to create a enriched spatial dataset
ds_merged<-geo_join(tracts, ds_svi, "GEOID", "GEOID")
range(ds_merged$RPL_THEMES)
#hover tool options
popup <- paste0("GEOID: ", ds_merged$GEOID, "<br>", "Percentile of Vulnerability: ", ds_merged$RPL_THEMES,"<br>", "COUNTY: ", ds_merged$COUNTY)
#color range
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = ds_merged$RPL_THEMES
)
#leaflet plot of NY SVI data by TRACT
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = ds_merged, 
              fillColor = ~pal(RPL_THEMES), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = ds_merged$RPL_THEMES, 
            position = "bottomright", 
            title = "Percentile of Vulnerability",
            labFormat = labelFormat(suffix = "%")) 







