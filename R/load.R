# Load packages
library(xlsx) # input/output
library(readr) # input/output
library(RSocrata) # input/output
library(naniar) # missing values
library(dplyr) # data manipulation
library(tidyverse) # data manipulation
library(DescTools) # data manipulation
library(tigris) # geospatial data
library(sp) # visualization
library(sf) # geospatial 

# Load data 
ds_svi <- read_csv("../RawData/Social Vulnerability.csv", col_types=cols("FIPS"=col_character()))
child_opp <- read_csv("../RawData/ny_child_opp.csv")
covid <- read_csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/data-by-modzcta.csv")
schools <- read.socrata("https://data.cityofnewyork.us/resource/wg9x-4ke6.json")
lcgms <- read_csv("../RawData/LCGMS_SchoolData_20200509_1135.csv")
school_tracts <- read_csv("../RawData/school tract.csv")
nta <- read.socrata("https://data.cityofnewyork.us/resource/93vf-i5bz.json")
nta_tract <- read.xlsx("../RawData/nyc2010census_tabulation_equiv.xlsx", sheetName = "NTA to 2010 CT equivalency")
nta_puma <- read.xlsx("../RawData/nyc2010census_tabulation_equiv.xlsx", sheetName = "NTA in PUMA_")
nta_shp <- st_read("../RawData/geo_export_a3daacc4-b572-40c6-a08d-71e44f8d4f8a.shp")
nyc <- st_read("../RawData/MODZCTA_2010.shp")

# tigris NYS county spatial data
options(tigris_use_cache = TRUE)
county_sp <- counties(state="NY")
nyc_cfips <- c("36005", "36047","36061", "36085", "36081" )
county_sp <- county_sp[county_sp@data$GEOID %in% nyc_cfips,] # nyc county fips only

# tigris NYS census tract spatial data
tracts <- tracts(state = 'NY', cb=TRUE)

# transform NYC sf layer to EPSG 4326 to reproject
nyc <- st_transform(nyc, 4326)

# creating svi by census tract spatial
col_svi <- c("STCNTY","COUNTY", "LOCATION", "FIPS", "RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4", "RPL_THEMES", "SPL_THEMES")
ds_svi <- ds_svi[col_svi]

# adjust SVI percent rank to only NYC census tracts
ds_svi <- ds_svi[ds_svi$STCNTY %in% nyc_cfips, ] # 2166 census tracts in NYC
ds_svi <- ds_svi %>% replace_with_na(replace = list(SPL_THEMES = -999)) # replace -999 w/NA
ds_svi <- ds_svi %>% # SVI percent rank of NYC tracts, ignore NA values
  mutate(RPL_THEMES_nycct = percent_rank(SPL_THEMES, na.rm = TRUE))
quantile(ds_svi$RPL_THEMES_nycct, na.rm = TRUE) # 0.00, 0.25, 0.50, 0.75, 1.00 
ds_svi <- ds_svi %>% # assigning vulnerability cat based on quantiles
  filter(STCNTY %in% nyc_cfips) %>% 
  mutate(cat = case_when(
    RPL_THEMES_nycct >= 0.75 ~ "Highest Vulnerability",
    RPL_THEMES_nycct >= 0.50 & RPL_THEMES_nycct < 0.75 ~ "High Medium Vulnerability",
    RPL_THEMES_nycct >= 0.25 & RPL_THEMES_nycct < 0.50 ~ "Low Medium Vulnerability",
    RPL_THEMES_nycct >= 0.000 & RPL_THEMES_nycct < 0.25 ~ "Lowest Vulnerability"
  ))
ds_svi$cat <- ordered(ds_svi$cat, levels = c("Lowest Vulnerability", "Low Medium Vulnerability", "High Medium Vulnerability", "Highest Vulnerability")) # order levels of svi cat
svi_tracts <- geo_join(tracts, ds_svi, "GEOID", "FIPS") # enrich tigris w/svi 
svi_tracts <- svi_tracts[svi_tracts@data$STCNTY %in% nyc_cfips,] # nyc counties only

# adjust SVI percent rank to NYC Neighborhood Tabulation Areas (NTA)
# recreate full census tract (11-digit)
nta_tract$STCNTY[nta_tract$Borough == "Bronx"] <- "36005"
nta_tract$STCNTY[nta_tract$Borough == "Brooklyn"] <- "36047"
nta_tract$STCNTY[nta_tract$Borough == "Manhattan"] <- "36061"
nta_tract$STCNTY[nta_tract$Borough == "Queens"] <- "36081"
nta_tract$STCNTY[nta_tract$Borough == "Staten Island"] <- "36085"
# paste state county fips w/6 digit census tract
nta_tract$GEOID <- paste0(nta_tract$STCNTY, nta_tract$X2010.Census.Tract)
# create svi by nta
svi_nta <- left_join(nta_tract, ds_svi, by = c("GEOID" = "FIPS"))
svi_nta <- svi_nta %>% replace_with_na(replace = list(SPL_THEMES = -999)) # missing values set to N
svi_nta_ranked <- svi_nta %>% # percent rank by nta, ignore na values
  select(NTA_Name, NTA_Code, GEOID, SPL_THEMES) %>% 
  group_by(NTA_Code) %>% 
  summarise(SPL_THEMES_NTA = mean(SPL_THEMES, na.rm = TRUE)) %>% 
  mutate(RPL_THEMES_NTA = percent_rank(SPL_THEMES_NTA, na.rm = TRUE))
svi_nta_ranked <- geo_join(nta_shp, svi_nta_ranked, "ntacode", "NTA_Code") # enrich spatial
quantile(svi_nta_ranked$RPL_THEMES_NTA, na.rm = TRUE) # 0.0000000 0.2448454 0.4896907 0.7345361 0.9793814
svi_nta_ranked <- svi_nta_ranked %>% mutate(cat_nta = case_when(
  RPL_THEMES_NTA >= 0.7345361 ~ "Highest Vulnerability",
  RPL_THEMES_NTA >= 0.4896907 & RPL_THEMES_NTA < 0.7345361 ~ "High Medium Vulnerability",
  RPL_THEMES_NTA >= 0.2448454 & RPL_THEMES_NTA < 0.4896907 ~ "Low Medium Vulnerability",
  RPL_THEMES_NTA >= 0.000 & RPL_THEMES_NTA < 0.2448454 ~ "Lowest Vulnerability"
))
svi_nta_ranked$cat_nta <- ordered(svi_nta_ranked$cat_nta, levels = c("Lowest Vulnerability", "Low Medium Vulnerability", "High Medium Vulnerability", "Highest Vulnerability")) # order levels

# creating covid by zip spatial 
covid <- covid %>% 
  group_by(MODIFIED_ZCTA) %>%
  summarise(total_case_rate = mean(COVID_CASE_RATE, na.rm = TRUE), total_death_rate = mean(COVID_DEATH_RATE)) %>% 
  mutate(rank_case_rate = min_rank(total_case_rate), rank_death_rate = min_rank(total_death_rate), sum_ranks = rank_case_rate + rank_death_rate, rank_impact = percent_rank(sum_ranks)) %>% 
  mutate(impact_cat = case_when(
    rank_impact >= 0.75 ~ "Highest Impact",
    rank_impact >= 0.50 & rank_impact < 0.75 ~ "High Medium Impact",
    rank_impact >= 0.25 & rank_impact < 0.50 ~ "Low Medium Impact",
    rank_impact >= 0.00 & rank_impact < 0.25 ~ "Lowest Impact"))
covid$MODIFIED_ZCTA <- as.factor(covid$MODIFIED_ZCTA)
covid$impact_cat <- ordered(covid$impact_cat, levels = c("Lowest Impact", "Low Medium Impact", "High Medium Impact", "Highest Impact")) # order levels
nyc_covid <- geo_join(nyc, covid, "MODZCTA", "MODIFIED_ZCTA") # Enrich NYC data w/COVID impact 

# creating child opportunity index spatial
y <- c(2,3,6,7,31:38) # select variables
child_opp <- child_opp[y]
colnames(child_opp)[1] <- "GEOID" 
child_opp$c5_COI_met <- as.factor(child_opp$c5_COI_met) 
child_opp$c5_COI_met <- ordered(child_opp$c5_COI_met, levels=c("Very Low", "Low", "Moderate", "High", "Very High")) # set factor order in plot value
child_opp <- filter(child_opp, child_opp$year == "2015")
merge_opp <- geo_join(tracts, child_opp, "GEOID", "GEOID")
merge_opp <- merge_opp[merge_opp@data$countyfips %in% nyc_cfips,] # only NYC County FIPS
# creating COI by NTA
child_opp$GEOID <- as.character(child_opp$GEOID)
child_opp <- child_opp[child_opp$countyfips %in% nyc_cfips,]
coi_nta <- left_join(nta_tract, child_opp, by = c("GEOID" = "GEOID"))
coi_nta <- coi_nta %>% 
  group_by(NTA_Code) %>% 
  summarise(r_COI_MET_NTA = mean(r_COI_met, na.rm = TRUE)) %>% 
  mutate(pernk_COI_MET_NTA = percent_rank(r_COI_MET_NTA, na.rm = TRUE))
quantile(coi_nta$pernk_COI_MET_NTA, na.rm = TRUE, seq(0, 1, 0.2)) # percentiles as noted in documentation. 0.0000000 0.1969072 0.3938144 0.5907216 0.7876289 0.9845361 
quantile(coi_nta$r_COI_MET_NTA, na.rm = TRUE, seq(0, 1, 0.2))
coi_nta <- coi_nta %>% 
  mutate(cat_coi_nta = case_when(
    pernk_COI_MET_NTA >= 0.7876289 ~ "Very High",
    pernk_COI_MET_NTA >= 0.5907216 & pernk_COI_MET_NTA < 0.7876289 ~ "High",
    pernk_COI_MET_NTA >= 0.3938144 & pernk_COI_MET_NTA < 0.5907216 ~ "Moderate",
    pernk_COI_MET_NTA >= 0.1969072 & pernk_COI_MET_NTA < 0.3938144 ~ "Low",
    pernk_COI_MET_NTA >= 0.0000000 & pernk_COI_MET_NTA < 0.1969072 ~ "Very Low",
  ))
coi_nta$cat_coi_nta <- ordered(coi_nta$cat_coi_nta, levels=c("Very Low", "Low", "Moderate", "High", "Very High")) # set factor order in plot value
coi_nta_shp <- geo_join(nta_shp, coi_nta, "ntacode", "NTA_Code") # enrich nta spatial

# merge svi and coi 
x <- svi_nta_ranked
y <- coi_nta_shp
st_geometry(x) <- NULL
st_geometry(y) <- NULL
svi_opp <- left_join(x, y, by="ntacode")

# lat lng convert to numeric
schools$longitude <- as.numeric(schools$longitude)
schools$latitude <- as.numeric(schools$latitude)
schools <- schools %>% filter(status_descriptions == "Open") # 1900 open schools

# Save RDS
saveRDS(schools, file = "rds_files/schools.rds")
saveRDS(merge_opp, file = "rds_files/merge_opp.rds")
saveRDS(nyc_covid, file = "rds_files/nyc_covid.rds")
saveRDS(nyc, file = "rds_files/nyc_zips.rds")
saveRDS(county_sp, file = "rds_files/county_sp.rds")
saveRDS(svi_tracts, file = "rds_files/svi_tracts.rds")
saveRDS(svi_opp, file = "rds_files/svi_opp.rds")
saveRDS(ds_svi, file = "rds_files/ds_svi.rds")
saveRDS(school_tracts, file = "rds_files/school_tracts.rds")
saveRDS(svi_nta_ranked, file = "rds_files/svi_nta_ranked.rds")
saveRDS(coi_nta_shp, file = "rds_files/coi_nta_shp.rds")
