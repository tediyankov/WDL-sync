
## Africa city shares V2 =======================================================

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
loreal_afr <- read.csv (file.path (loreal_path, "loreal_ssa_cities_updated_2022_12_12_2.csv"))

# WDP raw data
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_10_11_2017ppp_1_USD.rds"))

## Chapter 2: cleaning data ----------------------------------------------------

## converting country names in Africa cities data to ISO3C for convenience
loreal_afr$Country <- countrycode (loreal_afr$Country, "country.name", "iso3c")

## computing required spending groups 
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, Inf))

## cleaning WDP data
wdp_clean = wdp %>%
  
  # country-year headcounts per spending group
  group_by (ccode, year, daily_spending) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # computing variables for 12+, 20+ and 40+ 
  pivot_wider (names_from = daily_spending, values_from = pop) %>%
  rename_with (
    ~ case_when (
      . == "[0,12)" ~ "spend_0_12",
      . == "[12,40)" ~ "spend_12_40",
      . == "[40,80)" ~ "spend_40_80",
      . == "[80,Inf)" ~ "spend_80_inf",
      TRUE ~ .
    )
  ) %>%
  mutate (spend_12_ = spend_12_40 + spend_40_80 + spend_80_inf,
          spend_40_ = spend_40_80 + spend_80_inf,
          spend_80_ = spend_80_inf) %>%
  
  # keeping only needed vars
  dplyr::select (ccode, year, spend_12_, spend_40_, spend_80_) %>%
  
  # filtering to only relevant country-year observations
  filter (year %in% c(2022, 2030) & ccode %in% loreal_afr$Country) %>%
  
  # formatting to have one row per country
  pivot_wider (names_from = year, values_from = 3:5)

## cleaning female subset WDP data
wdp_clean_female = wdp %>%
  
  # subset for female
  filter (gender == 2) %>%
  
  # country-year headcounts per spending group
  group_by (ccode, year, daily_spending) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # computing variables for 12+, 20+ and 40+ 
  pivot_wider (names_from = daily_spending, values_from = pop) %>%
  rename_with (
    ~ case_when (
      . == "[0,12)" ~ "spend_0_12",
      . == "[12,40)" ~ "spend_12_40",
      . == "[40,80)" ~ "spend_40_80",
      . == "[80,Inf)" ~ "spend_80_inf",
      TRUE ~ .
    )
  ) %>%
  mutate (spend_12_ = spend_12_40 + spend_40_80 + spend_80_inf,
          spend_40_ = spend_40_80 + spend_80_inf,
          spend_80_ = spend_80_inf) %>%
  
  # keeping only needed vars
  dplyr::select (ccode, year, spend_12_, spend_40_, spend_80_) %>%
  
  # filtering to only relevant country-year observations
  filter (year %in% c(2022, 2030) & ccode %in% loreal_afr$Country) %>%
  
  # formatting to have one row per country
  pivot_wider (names_from = year, values_from = 3:5) %>%
  
  # renaming vars for joining later
  rename_with (
    ~ case_when (
      . == "spend_12__2022" ~ "fem_spend_12__2022",
      . == "spend_12__2030" ~ "fem_spend_12__2030",
      . == "spend_40__2022" ~ "fem_spend_40__2022",
      . == "spend_40__2030" ~ "fem_spend_40__2030",
      . == "spend_80__2022" ~ "fem_spend_80__2022",
      . == "spend_80__2030" ~ "fem_spend_80__2030",
      TRUE ~ .
    )
  )
  

## cleaning African cities data

# converting numerical vars to num from chr
cols = c(3:22)
for (i in cols) {
  
  loreal_afr [,i] <- gsub (",","",loreal_afr [,i])
  loreal_afr [,i] <- as.numeric (loreal_afr [,i])
  
  }

# clean
loreal_afr_clean = loreal_afr %>%
  
  # renaming vars 
  rename_with (
    ~ case_when (
      . == "Consumer.Class..12...2022" ~ "cc_12_2022",
      . == "Consumer.Class..12...2030" ~ "cc_12_2030",
      . == "Females..12...aged.15.or.older..2022" ~ "fem_12_2022",
      . == "Females..12...aged.15.or.older..2030" ~ "fem_12_2030",
      . == "Consumer.Class..80...2022" ~ "cc_80_2022",
      . == "Consumer.Class..80...2030" ~ "cc_80_2030",
      . == "Females..80...aged.15.or.older..2022" ~ "fem_80_2022",
      . == "Females..80...aged.15.or.older..2030" ~ "fem_80_2030",
      . == "Consumer.Class..40...2022" ~ "cc_40_2022",
      . == "Consumer.Class..40...2030" ~ "cc_40_2030",
      . == "Females..40...aged.15.or.older..2022" ~ "fem_40_2022",
      . == "Females..40...aged.15.or.older..2030" ~ "fem_40_2030",
      TRUE ~ .
    )
  ) %>%
  
  # aggregating city-level variables for consumer class (total and F15+) at each level
  dplyr::group_by (Country) %>%
  dplyr::summarise (cc_12_2022 = sum (cc_12_2022, na.rm = T), 
             cc_40_2022 = sum (cc_40_2022, na.rm = T),
             cc_80_2022 = sum (cc_80_2022, na.rm = T),
             cc_12_2030 = sum (cc_12_2030, na.rm = T),
             cc_40_2030 = sum (cc_40_2030, na.rm = T),
             cc_80_2030 = sum (cc_80_2030, na.rm = T),
             fem_12_2022 = sum (fem_12_2022, na.rm = T), 
             fem_40_2022 = sum (fem_40_2022, na.rm = T),
             fem_80_2022 = sum (fem_80_2022, na.rm = T),
             fem_12_2030 = sum (fem_12_2030, na.rm = T),
             fem_40_2030 = sum (fem_40_2030, na.rm = T),
             fem_80_2030 = sum (fem_80_2030, na.rm = T)) %>%
  
  # renaming vars to match wdp_clean
  rename_with (~ case_when (. == "Country" ~ "ccode", TRUE ~ .))

## Chapter 3: getting shares ---------------------------------------------------

shares = loreal_afr_clean %>%
  
  # merging with WDP data
  left_join (wdp_clean, by = "ccode") %>%
  left_join (wdp_clean_female, by = "ccode") %>%
  
  # computing shares variables
  mutate (shares_total_12_2022 = cc_12_2022 / spend_12__2022,
          shares_total_12_2030 = cc_12_2030 / spend_12__2030,
          shares_total_40_2022 = cc_40_2022 / spend_40__2022,
          shares_total_40_2030 = cc_40_2030 / spend_40__2030,
          shares_total_80_2022 = cc_80_2022 / spend_80__2022,
          shares_total_80_2030 = cc_80_2030 / spend_80__2030,
          shares_fem_12_2022 = fem_12_2022 / fem_spend_12__2022,
          shares_fem_12_2030 = fem_12_2030 / fem_spend_12__2030,
          shares_fem_40_2022 = fem_40_2022 / fem_spend_40__2022,
          shares_fem_40_2030 = fem_40_2030 / fem_spend_40__2030,
          shares_fem_80_2022 = fem_80_2022 / fem_spend_80__2022,
          shares_fem_80_2030 = fem_80_2030 / fem_spend_80__2030) %>%
  
  # keeping only ccode + shares variables
  dplyr::select (ccode, shares_total_12_2022, shares_total_12_2030, shares_total_40_2022, shares_total_40_2030, 
                 shares_total_80_2022, shares_total_80_2030, shares_fem_12_2022, shares_fem_12_2030, shares_fem_40_2022,
                 shares_fem_40_2030, shares_fem_80_2022, shares_fem_80_2030) %>%
  
  # renaming back country variable for convenience
  mutate (ccode = countrycode (ccode, "iso3c", "country.name"))

## Chapter 4: conclusions ------------------------------------------------------

write.csv (shares, file.path (loreal_path, "shares_updated_16_12_2022_v2.csv"), row.names = F)


## Chapter 5: sequel: comparing with old shares --------------------------------

shares_comp = shares %>% 
  dplyr::select (ccode, shares_total_12_2022, shares_total_12_2030, shares_total_40_2022, shares_total_40_2030) %>%
  rename_with (
    ~ case_when (
      . == "shares_total_12_2022" ~ "shares_total_12_2022_new",
      . == "shares_total_12_2030" ~ "shares_total_12_2030_new",
      . == "shares_total_40_2022" ~ "shares_total_40_2022_new",
      . == "shares_total_40_2030" ~ "shares_total_40_2030_new",
      TRUE ~ .
    )
  )

shares_1_comp = shares_1 %>%
  dplyr::select (ccode, shares_total_12_2022, shares_total_12_2030, shares_total_40_2022, shares_total_40_2030) %>%
  rename_with (
    ~ case_when (
      . == "shares_total_12_2022" ~ "shares_total_12_2022_old",
      . == "shares_total_12_2030" ~ "shares_total_12_2030_old",
      . == "shares_total_40_2022" ~ "shares_total_40_2022_old",
      . == "shares_total_40_2030" ~ "shares_total_40_2030_old",
      TRUE ~ .
    )
  )

shares_join_comp = shares_comp %>%
  left_join (shares_1_comp, by = "ccode") %>%
  mutate (shares_total_12_2022_diff = ( (shares_total_12_2022_new - shares_total_12_2022_old) / shares_total_12_2022_old ) * 100,
          shares_total_12_2030_diff = ( (shares_total_12_2030_new - shares_total_12_2030_old) / shares_total_12_2030_old ) * 100,
          shares_total_40_2022_diff = ( (shares_total_40_2022_new - shares_total_40_2022_old) / shares_total_40_2022_old ) * 100,
          shares_total_40_2030_diff = ( (shares_total_40_2030_new - shares_total_40_2030_old) / shares_total_40_2030_old ) * 100) %>%
  dplyr::select (ccode, shares_total_12_2022_diff, shares_total_12_2030_diff, shares_total_40_2022_diff, shares_total_40_2030_diff)


write.csv (shares_join_comp, file.path (loreal_path, "shares_delta_16_12_2022.csv"), row.names = F)

## comparing Maxi's updated file with the old data 

new <- read.csv (file.path (loreal_path, "loreal_ssa_cities_updated_2022_12_12_2.csv")) 
old <- read.csv (file.path (loreal_path, "loreal_ssa_cities_updated (3).csv"))

cols = c(3:22)
for (i in cols) {
  
  new [,i] <- gsub (",","", new [,i])
  new [,i] <- as.numeric (new [,i])
  
}

old = old %>% dplyr::select (-contains ("..20..."))

options (scipen = 999)

dat_compare = new %>%
  left_join (old,  by = c("Country", "City"), suffix = c("_new", "_old")) %>%
  mutate (cc_12_22_delta = ((Consumer.Class..12...2022_new - Consumer.Class..12...2022_old) / Consumer.Class..12...2022_old) * 100,
          cc_12_30_delta = ((Consumer.Class..12...2030_new - Consumer.Class..12...2022_old) / Consumer.Class..12...2030_old) * 100,
          cc_40_22_delta = ((Consumer.Class..40...2022_new - Consumer.Class..40...2022_old) / Consumer.Class..40...2022_old) * 100,
          cc_40_30_delta = ((Consumer.Class..40...2030_new - Consumer.Class..40...2030_old) / Consumer.Class..40...2030_old) * 100,
          cc_80_22_delta = ((Consumer.Class..80...2022_new - Consumer.Class..80...2022_old) / Consumer.Class..80...2022_old) * 100,
          cc_80_30_delta = ((Consumer.Class..80...2030_new - Consumer.Class..80...2030_old) / Consumer.Class..80...2030_old) * 100,
          fem_12_22_delta = ((Females..12...aged.15.or.older..2022_new - Females..12...aged.15.or.older..2022_old) / Females..12...aged.15.or.older..2022_old) * 100,
          fem_12_30_delta = ((Females..12...aged.15.or.older..2030_new - Females..12...aged.15.or.older..2022_old) / Females..12...aged.15.or.older..2022_old) * 100,
          fem_40_22_delta = ((Females..40...aged.15.or.older..2022_new - Females..40...aged.15.or.older..2022_old) / Females..40...aged.15.or.older..2022_old) * 100,
          fem_40_30_delta = ((Females..40...aged.15.or.older..2030_new - Females..40...aged.15.or.older..2030_old) / Females..40...aged.15.or.older..2030_old) * 100,
          fem_80_22_delta = ((Females..80...aged.15.or.older..2022_new - Females..80...aged.15.or.older..2022_old) / Females..80...aged.15.or.older..2022_old) * 100,
          fem_80_30_delta = ((Females..80...aged.15.or.older..2030_new - Females..80...aged.15.or.older..2030_old) / Females..80...aged.15.or.older..2030_old) * 100) %>%
  dplyr::select (Country, City, cc_12_22_delta, cc_12_30_delta, cc_40_22_delta, cc_40_30_delta, cc_80_22_delta, cc_80_30_delta,
                 fem_12_22_delta, fem_12_30_delta, fem_40_22_delta, fem_40_30_delta, fem_80_22_delta, fem_80_30_delta)
  #unite ("country_city", Country:City, sep = "_") %>%
  #pivot_longer (2:13, names_to = "category", values_to = "share")

dat_compare_plot = ggplot (data = dat_compare, aes (x = country_city, y = share)) +
  geom_bar (stat = "identity") + 
  facet_wrap (~category, scale = "free")
dat_compare_plot



write.csv (dat_compare, file.path (loreal_path, "citiesdat_compare_16_12_2022.csv"), row.names = F)


cols = c(3:14)
list = c()

for (i in cols) {
  
  list [i-2] <- mean (dat_compare [,i], na.rm = T)
  
}














