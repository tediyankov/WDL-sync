
## WEC data insight check ======================================================

## Chapter 1: preliminaries ----------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools)

## system font 
worlddataverse::font_wdl()

## loading data 
load_wec_dat = function (){
  
  require (worlddataverse)
  
  # loading paths
  base_path <- worlddataverse::get_wdl_path()
  wec_path <- file.path (base_path, "world_emissions_clock", "01_data")
  
  # load binary (Rda) file
  load (file.path (wec_path,"WEC_data_binary_new.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  
}

load_wec_dat ()

## Chapter 2: working with the data --------------------------------------------

options(scipen=999)

wec_dat_delta_total = wec_dat %>%
  
  # computing totals per country-year
  group_by (iso3c, year) %>%
  summarise (base = sum (base, na.rm = T)) %>%
  
  # keeping relevant years only
  filter (year %in% c(2022,2023)) %>%
  
  # making data wider with one base var per year
  pivot_wider (names_from = year, values_from = base) %>%
  
  # renaming vars
  rename_with (
    ~ case_when (
      . == "2022" ~ "emissions_2022",
      . == "2023" ~ "emissions_2023",
      TRUE ~ .
    )
  ) %>%
  
  # computing percentage change in emissions 
  mutate (delta = ((emissions_2023 - emissions_2022) / emissions_2022 )*100) %>%
  
  # adding continent variable
  mutate (continent = countrycode (iso3c, "iso3c", "continent")) %>%
  drop_na (continent)

## Chapter 3: data visualisation -----------------------------------------------

wec_delta_plot = ggplot (wec_dat_delta_total, aes (x = iso3c, y = delta)) + 
  geom_bar (stat = "identity") +
  worlddataverse::theme_wdl() + 
  theme (axis.text.x = element_text (size = 6)) + 
  labs (title = "Change in Total Emissions per country",
        subtitle = "From 2022 to 2023",
        x = "country", 
        y = "percent change in emissions") + 
  facet_wrap (~continent, scales = "free")
wec_delta_plot
  
  
  
  
  


















