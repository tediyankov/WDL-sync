
## World Consumer Outlook 2023: first tasks =================================================================================================================

## Chapter 1: preliminaries ----------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')

## loading in WDPro latest version

# loading raw file
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))

## loading in IMF WEO April 2023 data
library (readxl)
weo_raw <- readxl::read_xls (file.path ("~/Desktop/WEO_Data.xls"))

## Chapter 2: data cleaning ----------------------------------------------------

## Cleaning WEO

weo_clean_22_24 = weo_raw %>%
  
  # isolating necessary vars
  dplyr::select (ISO, `Subject Descriptor`, 7:15) %>%
  
  # elongating to have one year per row
  tidyr::pivot_longer (3:11, names_to = "year", values_to = "value") %>%
  
  # expanding subject variable into individual metrics
  tidyr::pivot_wider (names_from = 2, values_from = "value") %>%
  
  # renaming metrics
  dplyr::rename_with (
    ~ case_when (
      . == "Gross domestic product, current prices" ~ "gdp",
      . == "Gross domestic product per capita, current prices" ~ "gdp_pc",
      . == "Gross domestic product based on purchasing-power-parity (PPP) share of world total" ~ "gdp_share",
      TRUE ~ .
    )
  ) %>%
  
  # replacing n/a with NAs 
  dplyr::mutate (gdp = ifelse (gdp == "n/a", NA, gdp),
                 gdp_pc = ifelse (gdp == "n/a", NA, gdp_pc),
                 gdp_share = ifelse (gdp == "n/a", NA, gdp_share)) %>%
  
  # removing phantom variable
  dplyr::select (-6) %>%
  
  # keeping only complete cases
  tidyr::drop_na () %>%
  
  # filtering years
  dplyr::filter (year %in% c(2022,2023,2024)) %>%
  
  # widening to have year+metric
  tidyr::pivot_wider (names_from = 2, values_from = 3:5) %>%
  
  # converting values to numeric
  dplyr::mutate_at (c(2:10), as.numeric) %>%
  
  # creating change variables
  dplyr::mutate (delta_gdp_22_23 = ((gdp_2023 - gdp_2022) / gdp_2022) * 100,
                 delta_gdp_23_24 = ((gdp_2024 - gdp_2023) / gdp_2023) * 100,
                 delta_gdp_pc_22_23 = ((gdp_pc_2023 - gdp_pc_2022) / gdp_pc_2022) * 100,
                 delta_gdp_pc_23_24 = ((gdp_pc_2024 - gdp_pc_2023) / gdp_pc_2023) * 100)


## cleaning WDPro

# sorting into desired spending groups
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 2, 5, 8, 12, 40, 80, 120, Inf))

# clean
wdp_clean = wdp %>%
  
  # country-year headcounts and spending per spending group and age
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T),
             exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # recoding age groups into 10-year scale
  dplyr::mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)"), '[00,10)', 
                                     ifelse (age_group %in% c("[10,15))","[15,20)"), '[10,20)', 
                                             ifelse (age_group %in% c("[20,25))","[25,30)"), '[20,30)', 
                                                     ifelse (age_group %in% c("[30,35))","[35,40)"), '[30,40)', 
                                                             ifelse (age_group %in% c("[40,45))","[45,50)"), '[40,50)', 
                                                                     ifelse (age_group %in% c("[50,55))","[55,60)"), '[50,60)', 
                                                                             ifelse (age_group %in% c("[60,65))","[65,70)"), '[60,70)', 
                                                                                     ifelse (age_group %in% c("[70,75))","[75,INF)"), '[70,INF)', age_group))))))))) %>%
  
  # aggregating into new groups
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T),
                    exp = sum (exp, na.rm = T






