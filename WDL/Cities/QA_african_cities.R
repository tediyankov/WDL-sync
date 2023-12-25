
## WDP QA on African countries =================================================

## Chapter 1: preliminaries ----------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, data.table)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
loreal_path <- file.path (base_path, "Data Request", "Loreal_2022-12-12")
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2022_10_11','2022_10_12_second_2017ppp','03_outputs')

## loading data

# Africa cities data 
loreal_afr_1 <- read.csv (file.path (loreal_path, "loreal_ssa_cities_updated.csv"))

# WDP raw data
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_10_11_2017ppp_1_USD.rds"))

## Chapter 2: cleaning data ----------------------------------------------------

# converting country names in Africa cities data to ISO3C for convenience
loreal_afr_1$Country <- countrycode (loreal_afr_1$Country, "country.name", "iso3c")

## computing required spending groups 
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 20, 40, Inf))

# cleaning WDP data
wdp_clean = wdp %>%
  
  # country-year headcounts per spending group
  group_by (ccode, year, daily_spending) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # computing variables for 12+, 20+ and 40+ 
  pivot_wider (names_from = daily_spending, values_from = pop) %>%
  rename_with (
    ~ case_when (
      . == "[0,12)" ~ "spend_0_12",
      . == "[12,20)" ~ "spend_12_20",
      . == "[20,40)" ~ "spend_20_40",
      . == "[40,Inf)" ~ "spend_40_inf",
      TRUE ~ .
    )
  ) %>%
  mutate (spend_12_ = spend_12_20 + spend_20_40 + spend_40_inf,
          spend_20_ = spend_20_40 + spend_40_inf,
          spend_40_ = spend_40_inf) %>%
  
  # keeping only needed vars
  dplyr::select (ccode, year, spend_12_, spend_20_, spend_40_) %>%
  
  # filtering to only relevant country-year observations
  filter (year %in% c(2022, 2030) & ccode %in% loreal_afr$Country) %>%
  
  # formatting to have one row per country
  pivot_wider (names_from = year, values_from = 3:5)

# cleaning African cities data
loreal_afr_clean_1 = loreal_afr_1 %>%
  
  # aggregating city-level variables for consumer class (total and F15+) at each level
  group_by (Country) %>%
  summarise (cc_12_2022 = sum (Consumer.Class..12...2022, na.rm = T), 
             cc_20_2022 = sum (Consumer.Class..20...2022, na.rm = T),
             cc_40_2022 = sum (Consumer.Class..40...2022, na.rm = T),
             cc_12_2030 = sum (Consumer.Class..12...2030, na.rm = T),
             cc_20_2030 = sum (Consumer.Class..20...2030, na.rm = T),
             cc_40_2030 = sum (Consumer.Class..40...2030, na.rm = T),
             fem_12_2022 = sum (Females..12...aged.15.or.older..2022, na.rm = T), 
             fem_20_2022 = sum (Females..20...aged.15.or.older..2022, na.rm = T),
             fem_40_2022 = sum (Females..40...aged.15.or.older..2022, na.rm = T),
             fem_12_2030 = sum (Females..12...aged.15.or.older..2030, na.rm = T),
             fem_20_2030 = sum (Females..20...aged.15.or.older..2030, na.rm = T),
             fem_40_2030 = sum (Females..40...aged.15.or.older..2030, na.rm = T)) %>%
  
  # renaming vars to match wdp_clean
  rename_with (~ case_when (. == "Country" ~ "ccode", TRUE ~ .))

## Chapter 3: getting shares ---------------------------------------------------

shares_1 = loreal_afr_clean_1 %>%
  
  # merging with WDP data
  left_join (wdp_clean, by = "ccode") %>%
  
  # computing shares variables
  mutate (shares_total_12_2022 = cc_12_2022 / spend_12__2022,
          shares_total_12_2030 = cc_12_2030 / spend_12__2030,
          shares_total_20_2022 = cc_20_2022 / spend_20__2022,
          shares_total_20_2030 = cc_20_2030 / spend_20__2030,
          shares_total_40_2022 = cc_40_2022 / spend_40__2022,
          shares_total_40_2030 = cc_40_2030 / spend_40__2030,
          shares_fem_12_2022 = fem_12_2022 / spend_12__2022,
          shares_fem_12_2030 = fem_12_2030 / spend_12__2030,
          shares_fem_20_2022 = fem_20_2022 / spend_20__2022,
          shares_fem_20_2030 = fem_20_2030 / spend_20__2030,
          shares_fem_40_2022 = fem_40_2022 / spend_40__2022,
          shares_fem_40_2030 = fem_40_2030 / spend_40__2030) %>%

  # keeping only ccode + shares variables
  dplyr::select (c(1, 20:31)) %>%
  
  # renaming back country variable for convenience
  mutate (ccode = countrycode (ccode, "iso3c", "country.name"))

## Chapter 4: conclusions ------------------------------------------------------

write.csv (shares, file.path (loreal_path, "shares.csv"), row.names = F)

  
  