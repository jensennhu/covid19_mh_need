# covid19_mh_need
Updated 4/17/2020
Initial Outline for Discussion 

Topic: Identifying areas of potentially increased mental health needs as a result of COVID-19
-	Aim: routing public health efforts and MH resources to those areas of increased MH need during COVID-19
- Defining area by zip code
- Indicators used to define mental health need during COVID-19:
- Baseline MH Need
   - •	Child Opportunity Index (http://data.diversitydatakids.org/dataset/coi20-child-opportunity-index-2-0-database/resource/080cfe52-90aa-4925-beaa-90efb04ab7fb) 
   - •	Social vulnerability data (https://svi.cdc.gov/Documents/Data/2018_SVI_Data/SVI2018Documentation.pdf) 
   - •	YRBS and/or KIDS
   - •	Kimberly Sebek has other indices? Child mental health needs index

- How to predict areas of “increased” MH Need during COVID-19
  -  •	311 Complaint calls by zip https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9
     - o	Not looking at any specific compliant type, but viewing overall number of complaints as an indicator of chaos/stress in NY and therefore, potentially, an indicator of increased MH need
  -  •	Areas affected by COVID-19
     -  o	Number of cases
     -  o	Syndromic visits with ED visits open data
     -  o	Gothamist Fdny visits
     -  o	Number of hospitalizations
     -  o	Number of deaths
  -  o	ACS ? Child abuse mapping Open data?
  -  MH visits 
  -  Child outcomes specific data streaams

-  o	Other resources:
  -  	MH Service Finder: https://data.cityofnewyork.us/Health/Mental-Health-Service-Finder-Data/8nqg-ia7v
  -  	CMHS: https://data.cityofnewyork.us/Health/DOHMH-Community-Mental-Health-Survey/wi3r-8uzb

-  Methods:
   -	Prepare data and review MH need baseline and predicted MH need increase
   -	Build interactive
      -  o	R flexdashboard/shiny 
