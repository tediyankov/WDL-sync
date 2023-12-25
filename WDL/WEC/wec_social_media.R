
## WEC social media: oil and gas insights

## clean environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, worlddataverse, ggimage)

## set font
worlddataverse::font_wdl()

## loading data ----------------------------------------------------------------

# building function to load WEC data
load_wec_dat = function (){
  
  require (worlddataverse)
  
  # loading paths
  base_path <- worlddataverse::get_wdl_path()
  if (is.na (base_path)) {base_path = file.path("/Volumes/",
                                                "GoogleDrive-115239951252043738907",
                                                "Shared drives",
                                                "DATA_WDL")}
  
  # load binary (Rda) file
  load (file.path (base_path,
                   "world_emissions_clock",
                   "01_data",
                   "WEC_data_binary_new.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  
}

# loading WEC data
load_wec_dat()
