
## WEC: checking Switzerland ===================================================

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode)

## loading paths
base_path <- worlddataverse::get_wdl_path()

## building function to load WEC data
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

## cleaning and isolating Switzerland ------------------------------------------

wec_che_dat1 = wec_dat %>%
  
  # subsetting for CHE
  filter (iso3c == "CHE") %>%
  
  # aggregating 
  dplyr::group_by (iso3c, year, pop, gdp) %>%
  dplyr::summarise (base = sum (base, na.rm = T)) %>%
  
  # obtaining per capita emissions
  mutate (em_pc = base / pop)

wec_che_dat2 = wec_dat %>%
  
  # subsetting for CHE
  filter (iso3c == "CHE") %>%
  
  # aggregating 
  dplyr::group_by (iso3c, year, sector, pop, gdp) %>%
  dplyr::summarise (base = sum (base, na.rm = T)) %>%
  
  # obtaining per capita emissions
  mutate (em_pc = base / pop) %>%
  
  # isolate 2021
  filter (year == 2021) %>% ungroup () %>%
  
  # getting shares
  dplyr::group_by (iso3c, year) %>%
  mutate (share = em_pc / sum (em_pc, na.rm = T))















