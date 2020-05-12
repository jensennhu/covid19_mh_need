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
lcgms <- read_csv("../RawData/LCGMS_SchoolData_20200509_1135.csv")
school_tracts <- read_csv("../RawData/school tract.csv")
nta <- read.socrata("https://data.cityofnewyork.us/resource/93vf-i5bz.json")

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

# merge svi and coi 
svi_opp <- left_join(svi_tracts@data, merge_opp@data, by="GEOID")

# lat lng convert to numeric
schools$longitude <- as.numeric(schools$longitude)
schools$latitude <- as.numeric(schools$latitude)
schools <- schools %>% filter(status_descriptions == "Open") # 1900 open schools
# identify schools in high impact regions through census tract
lcgms <- lcgms[c(1,13:18)]
colnames(lcgms)[1] <- "system_code"
schools <- left_join(schools, lcgms, by="system_code") # 2190 schools
schools <- tibble::rowid_to_column(schools, "index")
school_tracts <- left_join(schools, school_tracts, by="index")

# update partial tracts 
school_tracts$truetract <- ifelse(school_tracts$Census.Tract == school_tracts$key1, school_tracts$key1, school_tracts$key1)
# replace na values in updated track var w/older track var
school_tracts$truetract <- coalesce(school_tracts$key1, school_tracts$Census.Tract)
# only open and partial tract available schools n = 1899
school_tracts <- school_tracts %>% filter(status_descriptions == "Open" & !is.na(truetract))
# recreate full census tract to connect to SVI_OPP data
school_tracts$cnty_code <- paste0(school_tracts$state, "0", school_tracts$cnty)
school_tracts$tract_len <- lapply(school_tracts$truetract, nchar)
trans_tract <- function(x){
  if (x == 2) {
    result <- "0000"
  }
  else if (x == 3) {
    result <- "000"
  }
  else if (x == 4) {
    result <- "00"
  }
  else if (x == 5) {
    result <- "0"
  }
  else {
    result <- ""
  }
  return(result)
}
# add 
school_tracts$y <- lapply(school_tracts$tract_len, trans_tract)
school_tracts$GEOID <- paste0(school_tracts$cnty_code, school_tracts$y, school_tracts$truetract)
school_tracts <- school_tracts[c(3, 25, 26, 33, 43, 46, 61)]

# Save RDS
saveRDS(schools, file = "rds_files/schools.rds")
saveRDS(merge_opp, file = "rds_files/merge_opp.rds")
saveRDS(nyc_covid, file = "rds_files/nyc_covid.rds")
saveRDS(nyc_zips, file = "rds_files/nyc_zips.rds")
saveRDS(county_sp, file = "rds_files/county_sp.rds")
saveRDS(svi_tracts, file = "rds_files/svi_tracts.rds")
saveRDS(svi_opp, file = "rds_files/svi_opp.rds")
saveRDS(ds_svi, file = "rds_files/ds_svi.rds")
saveRDS(school_tracts, file = "rds_files/school_tracts.rds")


