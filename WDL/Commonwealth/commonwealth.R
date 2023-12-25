
## misc code for Commonwealth Matrix ===========================================

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, data.table)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()

## creating vectors containing countries of interest
small_states <- c("Botswana",
                  "Swaziland",
                  "Gabon",
                  "Lesotho",
                  "Mauritius",
                  "Namibia",
                  "Seychelles",
                  "Gambia",
                  "Brunei Darussalam",
                  "Maldives",
                  "Antigua and Barbuda",
                  "Bahamas",
                  "Barbados",
                  "Belize",
                  #"Dominica",
                  "Grenada",
                  "Guyana",
                  "Jamaica",
                  #"St Kitts and Nevis",
                  "Saint Lucia",
                  "Saint Vincent and the Grenadines",
                  "Trinidad and Tobago",
                  "Cyprus",
                  "Malta",
                  "Fiji",
                  "Kiribati",
                  #"Nauru",
                  "Papua New Guinea",
                  "Samoa",
                  "Solomon Islands",
                  "Tonga",
                  #"Tuvalu",
                  "Vanuatu")

small_states_iso3c <- countrycode (small_states, "country.name", "iso3c")

## education -------------------------------------------------------------------

## loading in WCDE package
install.packages ("wcde")
library (wcde)

## finding relevant package code
find_indicator (x = "education")

## obtaining data 
edu_dat <- get_wcde (indicator = "prop",
                     country_name = small_states) # unavailable: Dominica, Saint Kitts and Nevis, Nauru, Tuvalu

## auditing available years 
df1 <- data.frame (country = small_states, years = NA)
for (i in small_states) {
  
  years <- edu_dat$year [edu_dat$name == i]
  years_string <- paste (min(years), max(years), sep = ", ")
  df1$years [df1$country == i] <- years_string
  
}

## jobs ------------------------------------------------------------------------

raw_jobs_1 <- read_excel ("~/Desktop/EMP_TEMP_SEX_AGE_NB_A_EN.xlsx")
raw_jobs_2 <- read_excel ("~/Desktop/EMP_TEMP_SEX_AGE_OCU_NB_A_EN.xlsx")

## emissions -------------------------------------------------------------------

## loading in WEC data
load_wec_dat = function (){
  
  require (worlddataverse)
  
  # loading paths
  base_path <- worlddataverse::get_wdl_path()
  if (is.na (base_path)) {base_path = file.path("/Users",
                                                "teddyyankov",
                                                "Library",
                                                "CloudStorage",
                                                "GoogleDrive-teodor.yankov@worlddata.io",
                                                "Shared drives", 
                                                "DATA_WDL")}
  
  # load binary (Rda) file
  load (file.path (base_path,
                   "world_emissions_clock",
                   "01_data",
                   "WEC_data_binary_20230425.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  .GlobalEnv$base_path = base_path
  
}

# loading WEC data
load_wec_dat()

## cleaning WEC data into what I need
wec_dat_smallstates = wec_dat %>%
  
  # isolating states of interest
  filter (iso3c %in% small_states_iso3c)


object = unique (wec_dat_smallstates$iso3c)
diff = setdiff (small_states_iso3c, object)
diff = countrycode (diff, "iso3c", "country.name")








