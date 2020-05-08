# Load packages
library(readr) # input/output
library(RSocrata) # input/output
library(dplyr) # data manipulation
library(tidyverse) # data manipulation
library(DescTools) # data manipulation
library(tigris) # geospatial data
library(sp) # visualization

# Load data 
ds_svi <- read_csv("../RawData/Social Vulnerability.csv", col_types=cols("FIPS"=col_character()))
nyc_zips <- read_csv("../RawData/nyc_zips.csv")
child_opp <- read_csv("../RawData/ny_child_opp.csv")
covid <- read_csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/tests-by-zcta.csv")
schools <- read.socrata("https://data.cityofnewyork.us/resource/wg9x-4ke6.json")
# tigris NYS county spatial data
options(tigris_use_cache = TRUE)
county_sp <- counties(state="NY")
nyc_cfips <- c("36005", "36047","36061", "36085", "36081" )
county_sp <- county_sp[county_sp@data$GEOID %in% nyc_cfips,] # nyc county fips only
# tigris NYS census tract spatial data
tracts <- tracts(state = 'NY', cb=TRUE)
# tigris NY + NJ zctas spatial data
zctas_ny <- zctas(cb=TRUE, starts_with=c("10","11"))

# creating svi by census tract spatial
col_svi <- c("STCNTY","COUNTY", "LOCATION", "FIPS", "RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4", "RPL_THEMES")
ds_svi <- ds_svi[col_svi]
ds_svi$RPL_THEMES[ds_svi$RPL_THEMES==-999]<-0 # missing values set to zero 
ds_svi <- ds_svi %>% # assigning vulnerability cat based on quantiles
  filter(STCNTY %in% nyc_cfips) %>% 
  mutate(cat = case_when(
    RPL_THEMES >= 0.84460 ~ "Highest Vulnerability",
    RPL_THEMES >= 0.65910 & RPL_THEMES < 0.84460 ~ "High Medium Vulnerability",
    RPL_THEMES >= 0.39635 & RPL_THEMES < 0.65910 ~ "Low Medium Vulnerability",
    RPL_THEMES >= 0.000 & RPL_THEMES < 0.39635 ~ "Lowest Vulnerability"
  ))
ds_svi$cat <- ordered(ds_svi$cat, levels = c("Lowest Vulnerability", "Low Medium Vulnerability", "High Medium Vulnerability", "Highest Vulnerability")) # order levels of svi cat
svi_tracts <- geo_join(tracts, ds_svi, "GEOID", "FIPS") # enrich tigris w/svi 
svi_tracts <- svi_tracts[svi_tracts@data$STCNTY %in% nyc_cfips,] # nyc counties only

# creating covid by zip spatial 
nyc_zips <- separate_rows(nyc_zips, `ZIP Codes`, convert=FALSE)# separate csv into individual rows
colnames(nyc_zips)[3]<-"ZIP"
x <- nyc_zips$ZIP
nyc_zips <- zctas_ny[zctas_ny@data$ZCTA5CE10 %in% x, ] # nyc zips only
nyc_covid <- geo_join(nyc_zips, covid, "ZCTA5CE10", "MODZCTA")

# creating child opportunity index spatial
y <- c(2,3,6,7,15:18) # select variables
child_opp <- child_opp[y]
colnames(child_opp)[1] <- "GEOID" 
child_opp$c5_COI_nat <- as.factor(child_opp$c5_COI_nat) 
child_opp$c5_COI_nat <- ordered(child_opp$c5_COI_nat, levels=c("Very Low", "Low", "Moderate", "High", "Very High")) # set factor order in plot value
merge_opp <- geo_join(tracts, child_opp, "GEOID", "GEOID")
merge_opp <- merge_opp[merge_opp@data$countyfips %in% nyc_cfips,] # only NYC County FIPS

# lat lng convert to numeric
schools$longitude <- as.numeric(schools$longitude)
schools$latitude <- as.numeric(schools$latitude)


# Save RDS
saveRDS(schools, file = "rds_files/schools.rds")
saveRDS(merge_opp, file = "rds_files/merge_opp.rds")
saveRDS(nyc_covid, file = "rds_files/nyc_covid.rds")
saveRDS(nyc_zips, file = "rds_files/nyc_zips.rds")
saveRDS(county_sp, file = "rds_files/county_sp.rds")
saveRDS(svi_tracts, file = "rds_files/svi_tracts.rds")
saveRDS(ds_svi, file = "rds_files/ds_svi.rds")



