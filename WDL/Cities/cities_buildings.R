
## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, Hmisc, leaflet, geojsonio, broom)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')

## loading in WDPro latest version

# loading raw file
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))
open_buildings_v3_polygons_ne_110m_COD <- read_csv("open_buildings_v3_polygons_ne_110m_COD.csv")

# first cleaning of WDP dataset
wdp <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 8, 12, Inf))

# removing polygon column
open_buildings_v3_polygons_ne_110m_COD = open_buildings_v3_polygons_ne_110m_COD %>% dplyr::select (-geometry)

