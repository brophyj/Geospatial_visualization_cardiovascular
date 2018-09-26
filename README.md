# Geospatial_visualization_cardiovascular

###### The initial goal of this project was to build a mapping of AS incidence in Montreal region, later the project was expanded to investigate the cause of difference in AS incidence rate in Montreal

### Flow of data
##### Define AS cases and surgical procedure (SAVR) between 2000 and 2010 [01_define case and savr.R](https://github.com/nancyzhu24/Geospatial_visualization_cardiovascular/blob/master/01_define%20case%20and%20savr.R)
Cases of AS were determined from both in-hospital admission diagnostic data and out-patient physician billing data. The date at which a patient was diagnosed was defined as the cohort entry date. SAVR were determined from hospital intervention data confirmed with out-patient billing data.

##### Mapping of AS cases and SAVR
As a first attempt, the absolute count of AS cases over 10 years were mapping in Montreal based on CLSC regions [02_explore_geographic_AS.R](https://github.com/nancyzhu24/Geospatial_visualization_cardiovascular/blob/master/02_explore_geographic_AS.R) Following that, incidence rate of AS per 1000 pyr above 65 was mapped to CLSC regions [Mapping_AS.Rmd](https://github.com/nancyzhu24/Geospatial_visualization_cardiovascular/blob/master/Mapping_AS.Rmd) The 2017 Quebec CLSC map files are downloaded from [MSSS](http://www.msss.gouv.qc.ca/professionnels/informations-geographiques-et-de-population/information-geographique/)   are available in the folder [clsc_map](https://github.com/nancyzhu24/Geospatial_visualization_cardiovascular/tree/master/clsc_map) 

To further explore the cause of difference in AS cases over time in Montreal areas, age adjusted rate of AS and SAVR were calculated. The 2006 population data were extracted from census data provided by Statistic Canada [https://www12.statcan.gc.ca/datasets/Index-eng.cfm](https://www12.statcan.gc.ca/datasets/Index-eng.cfm). XML files were downloaded and parsed into R with script [03_parse_census_xml.R](https://github.com/nancyzhu24/Geospatial_visualization_cardiovascular/blob/master/03_parse_census_xml.R)

The 2006 Canadian age-specific population were used as the standard population in our age-specific rate calculation. Age-specific rate were calculated in each FSA region in Montreal. [03_age-adjusted-rate-calculation.R](https://github.com/nancyzhu24/Geospatial_visualization_cardiovascular/blob/master/03_age-adjusted-rate-calculation.R)

To map age-specific rate, FSA map are downloaded from [StatCan](https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-eng.cfm) and stored in [FSA_map](https://github.com/nancyzhu24/Geospatial_visualization_cardiovascular/tree/master/FSA_map) 
[05_Mapping_age_standarized_AS.Rmd](https://github.com/nancyzhu24/Geospatial_visualization_cardiovascular/blob/master/05_Mapping_age_standarized_AS.Rmd) includes all the script to map and report age-standarized rate of AS cases and SAVR for individuals over 65 between 2000 and 2010 in Greater Montreal area

##### Modelling AS cases and SAVR

