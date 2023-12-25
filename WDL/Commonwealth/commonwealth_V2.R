
## Commonwealth example data: Botswana =========================================

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, data.table)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')

## Poverty ---------------------------------------------------------------------

## loading data
poverty = read.csv (file.path (currentwdp_path, 
                               "01_csv", 
                               "07_002_poverty_clock_standard_ages_R_2023_04_11_2017ppp_1_USD.csv"))

## filtering for Botswana
poverty_bwa = poverty %>% filter (id == "BWA") %>%
  
  # renaming variables 
  rename (headcounts = hc, 
          total_population = pop)

## exporting 
write.csv (poverty_bwa, "~/Desktop/poverty_bwa.csv")

## Middle Class ----------------------------------------------------------------

## loading data
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))

## cleaning into 12-120 spending group
wdp_mc <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 120, Inf))

## isolating Botswana and Middle Class
middleclass_bwa <- wdp_mc %>% filter (daily_spending == "[12,120)", ccode == "BWA") %>%
  
  # renaming variables 
  rename (headcounts = hc.pdf.m, 
          expenditure = exp.pdf.m) %>%
  
  # recoding sex var
  mutate (gender = ifelse (gender == 1, "male", "female")) %>%
  
  # group to obtain totals 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (headcounts, na.rm = T),
                    exp = sum (expenditure, na.rm = T))

## exporting 
write.csv (middleclass_bwa, "~/Desktop/middleclass_bwa.csv")

## Life Expectancy -------------------------------------------------------------

## loading data
life_expectancy <- read.csv ("~/Desktop/life_expectancy_ages 2.csv")

## isolating Botswana
life_expectancy_bwa = life_expectancy %>% filter (region == "Botswana")

## exporting 
write.csv (life_expectancy_bwa, "~/Desktop/life_expectancy_bwa.csv")

## Life Expectancy -------------------------------------------------------------

## done via online data portal dirrect export

## education -------------------------------------------------------------------

## loading in WCDE package
install.packages ("wcde")
library (wcde)

## finding relevant package code
find_indicator (x = "education")

## obtaining data 
education_bwa <- get_wcde (indicator = "prop",
                           country_name = "Botswana")

## exporting 
write.csv (education_bwa, "~/Desktop/education_bwa.csv")

## emissions -------------------------------------------------------------------

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

## isolating Botswana
emissions_bwa = wec_dat %>% filter (iso3c == "BWA") %>%
  
  # total 
  dplyr::group_by (year, iso3c, pop) %>%
  dplyr::summarise (base = sum (base, na.rm = T)) %>%
  
  # filter 2023
  filter (year == 2023) %>%
  
  # computing per capita
  mutate (em_pc = base / pop)

## exporting 
write.csv (emissions_bwa, "~/Desktop/emissions_bwa.csv")

## water scarcity --------------------------------------------------------------

## loading data
water_scarcity <- read.csv ("~/Desktop/scarcity_countryaggregates.csv")

## isolating Botswana
water_scarcity_bwa <- water_scarcity %>% filter (ccode == "BWA")

## exporting 
write.csv (water_scarcity_bwa, "~/Desktop/water_scarcity_bwa.csv")

## internet poverty ------------------------------------------------------------

## loading data
internet_poverty <- read.csv ("~/Desktop/ipi_2_internet_poverty_country_level_2015_2023_2023_05_10.csv")

## isolating Botswana
internet_poverty_bwa <- internet_poverty %>% filter (ccode == "BWA")

## exporting 
write.csv (internet_poverty_bwa, "~/Desktop/internet_poverty_bwa.csv")



wdp_cc_africa = wdp %>%
  
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  dplyr::group_by (continent, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T))



## Commonwealth example data: Namibia ==========================================

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, data.table)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')

## Poverty ---------------------------------------------------------------------

## loading data
poverty = read.csv (file.path (currentwdp_path, 
                               "01_csv", 
                               "07_002_poverty_clock_standard_ages_R_2023_04_11_2017ppp_1_USD.csv"))

## filtering for Namibia
poverty_NAM = poverty %>% filter (id == "NAM") %>%
  
  # renaming variables 
  rename (headcounts = hc, 
          total_population = pop) %>%
  
  # filter
  filter (year == 2023) %>%
  
  # reaggregation into total
  dplyr::group_by (id, year, daily_spending) %>%
  dplyr::summarise (headcounts = sum (headcounts, na.rm = T))

## exporting 
write.csv (poverty_NAM, "~/Desktop/poverty_NAM.csv")

## Middle Class ----------------------------------------------------------------

## loading data
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))

## cleaning into 12-120 spending group
wdp_mc <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 120, Inf))

## isolating Namibia and Middle Class
middleclass_NAM <- wdp_mc %>% filter (daily_spending == "[12,120)", ccode == "NAM") %>%
  
  # renaming variables 
  rename (headcounts = hc.pdf.m, 
          expenditure = exp.pdf.m) %>%
  
  # recoding sex var
  mutate (gender = ifelse (gender == 1, "male", "female"))

## exporting 
write.csv (middleclass_NAM, "~/Desktop/middleclass_NAM.csv")

middleclass_NAM = middleclass_NAM %>% 
  dplyr::group_by (ccode, year, daily_spending) %>% 
  dplyr::summarise (hc = sum (headcounts, na.rm = T),
                    exp = sum (expenditure, na.rm = T))

## Life Expectancy -------------------------------------------------------------

## loading data
life_expectancy <- read.csv ("~/Desktop/life_expectancy_ages 2.csv")

## isolating Namibia
life_expectancy_NAM = life_expectancy %>% filter (region == "Namibia")

## exporting 
write.csv (life_expectancy_NAM, "~/Desktop/life_expectancy_NAM.csv")

## Life Expectancy -------------------------------------------------------------

## done via online data portal dirrect export

## education -------------------------------------------------------------------

## loading in WCDE package
install.packages ("wcde")
library (wcde)

## finding relevant package code
find_indicator (x = "education")

## obtaining data 
education_NAM <- get_wcde (indicator = "prop",
                           country_name = "Namibia")

## exporting 
write.csv (education_NAM, "~/Desktop/education_NAM.csv")

## emissions -------------------------------------------------------------------

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

## isolating Namibia
emissions_NAM = wec_dat %>% filter (iso3c == "NAM" & year == 2023)

emissions_NAM_total = emissions_NAM  %>%
  
  # total 
  dplyr::group_by (year, iso3c, pop) %>%
  dplyr::summarise (base = sum (base, na.rm = T))

emissions_NAM_sector = emissions_NAM  %>%
  
  # total 
  dplyr::group_by (year, iso3c, sector) %>%
  dplyr::summarise (base = sum (base, na.rm = T))
  
  
  # adding emissions per capita
  mutate (em_pc = )

## exporting 
write.csv (emissions_NAM, "~/Desktop/emissions_NAM.csv")

## water scarcity --------------------------------------------------------------

## loading data
water_scarcity <- read.csv ("~/Desktop/scarcity_countryaggregates.csv")

## isolating Namibia
water_scarcity_NAM <- water_scarcity %>% filter (ccode == "NAM")

## exporting 
write.csv (water_scarcity_NAM, "~/Desktop/water_scarcity_NAM.csv")

## internet poverty ------------------------------------------------------------

## loading data
internet_poverty <- read.csv ("~/Desktop/ipi_2_internet_poverty_country_level_2015_2023_2023_05_10.csv")

## isolating Namibia
internet_poverty_NAM <- internet_poverty %>% filter (ccode == "NAM")

## exporting 
write.csv (internet_poverty_NAM, "~/Desktop/internet_poverty_NAM.csv")









