
## WEC ZAF Subnational Data Work ===============================================

## Prologue --------------------------------------------------------------------

## clean environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, worlddataverse, ggimage, rworldmap)

## set font
worlddataverse::font_wdl()

## set paths
base_path <- worlddataverse::get_wdl_path()
wec_zaf_path <- file.path (base_path, "world_emissions_clock", "ZAF_subnational", "01_data", "database")

## loading WEC data function
load_wec_dat = function (){
  
  require (worlddataverse)
  
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

## loading WEC data
load_wec_dat()

## loading provincial data: GDP (constant 2010 prices)
raw_zaf_gdp_province <- read.csv (file.path (wec_zaf_path, "ZAF_GDP_provinces  - Sheet1.csv"))

## loading provincial data: industry GDP (constant 2010 prices)
raw_zaf_gdp_province_industry <- read.csv (file.path (wec_zaf_path, "ZAF_GDP_province_breakdown - Sheet1.csv"))

## loading provincial data: electricity
raw_zaf_electricity <- read.csv (file.path (wec_zaf_path, "ZAF_electricity - Sheet1.csv"))

## loading provincial data: demographic breakdown
raw_zaf_demography <- read.csv (file.path (wec_zaf_path, "ZAF_demography - Sheet1.csv"))


## Chapter 1: Data Cleaning ----------------------------------------------------

## GDP by province data
zaf_gdp_province = raw_zaf_gdp_province %>%
  
  # elongating data to create year variable
  pivot_longer (2:26, names_to = "year", values_to = "gdp") %>%
  
  # reformatting year values
  mutate (year = as.numeric (gsub ("X", "", year))) %>%
  
  # reformatting gdp values
  mutate (gdp = as.numeric (gsub (",", "", gdp))) %>%
  
  # adding province identifier
  mutate (Province = ifelse (Province == "Western Cape", "WCP",
                             ifelse (Province == "Eastern Cape", "ECP",
                                     ifelse (Province == "Northern Cape", "NCP",
                                             ifelse (Province == "Free State", "FRS",
                                                     ifelse (Province == "KwaZulu-Natal", "KZN",
                                                             ifelse (Province == "North West", "NW", 
                                                                     ifelse (Province == "Gauteng", "GAU",
                                                                             ifelse (Province == "Mpumalanga", "MPU",
                                                                                     ifelse (Province == "Limpopo", "LIM", NA
                                                                                     ))))))))))

## Industry GDP by province data
zaf_gdp_province_industry = raw_zaf_gdp_province_industry %>%
  
  # adding province identifier
  mutate (Province = ifelse (Province == "Western Cape", "WCP",
                             ifelse (Province == "Eastern Cape", "ECP",
                                     ifelse (Province == "Northern Cape", "NCP",
                                             ifelse (Province == "Free State", "FRS",
                                                     ifelse (Province == "KwaZulu-Natal", "KZN",
                                                             ifelse (Province == "North West", "NW", 
                                                                     ifelse (Province == "Gauteng", "GAU",
                                                                             ifelse (Province == "Mpumalanga", "MPU",
                                                                                     ifelse (Province == "Limpopo", "LIM", NA
                                                                                     )))))))))) %>%
  
  # removing summary rows
  filter (!(Industry %in% c("Primary Industries", "Secondary Industries", 
                            "Tertiary industries", "All industries at basic prices",
                            "GDPR at market prices"))) %>%
  
  # elongating data to add a year variable
  pivot_longer (3:27, names_to = "year", values_to = "gdp") %>%
  relocate (year, .after = "Province") %>%
  
  # reformatting year values
  mutate (year = as.numeric (gsub ("X", "", year))) %>%
  
  # reformatting gdp values
  mutate (gdp = as.numeric (gsub (",", "", gdp))) %>%
  
  # renaming industries to shorter names
  mutate (Industry = ifelse (Industry == "Agriculture, forestry and fishing", "agriculture_forest_fish",
                             ifelse (Industry == "Mining and quarrying", "mining_quarry",
                                     ifelse (Industry == "Manufacturing", "manufacturing",
                                             ifelse (Industry == "Electricity, gas and water", "electricity_gas_water",
                                                     ifelse (Industry == "Construction", "construction",
                                                             ifelse (Industry == "Trade, catering and accommodation", "trade_cater_accom",
                                                                     ifelse (Industry == "Transport, storage  and communication", "transport_store_comms",
                                                                             ifelse (Industry == "Finance, real estate and business services", "fin_realestate_biz",
                                                                                     ifelse (Industry == "Personal services", "personal_services",
                                                                                             ifelse (Industry == "General government services", "govt_services",
                                                                                                     ifelse (Industry == "General government services", "govt_services", 
                                                                                                             ifelse (Industry == "Taxes less subsidies on products", "tax", NA 
                                                                                                             ))))))))))))) %>%
  
  # widening data to include one column per sector
  pivot_wider (names_from = "Industry", values_from = "gdp") %>%
  
  # adding gdp_ prefix
  rename_with (.fn = ~ paste0("gdp_", .x, ""), .cols = 3:13)



## Electrcity data
zaf_electricity = raw_zaf_electricity %>%
  
  # adding province identifier
  mutate (Province = ifelse (Province == "Western Cape", "WCP",
                             ifelse (Province == "Eastern Cape", "ECP",
                                     ifelse (Province == "Northern Cape", "NCP",
                                             ifelse (Province == "Free State", "FRS",
                                                     ifelse (Province == "KwaZulu-Natal", "KZN",
                                                             ifelse (Province == "North west", "NW", 
                                                                     ifelse (Province == "Gauteng", "GAU",
                                                                             ifelse (Province == "Mpumalanga", "MPU",
                                                                                     ifelse (Province == "Limpopo", "LIM", NA
                                                                                     ))))))))))
  
## creating year sums from the monthly numbers

years = c(2002:2021)
empty_cols = c()


for (i in 1:20) {
  
  a <- paste0 ("electricity", "_", as.character (years[i]))
  empty_cols = c(empty_cols, a)
  
}

## adding vars that sum electricity use from monthly into yearly 

zaf_electricity$electricity_2002 = rowSums (select (zaf_electricity, matches("2002")), na.rm = TRUE)
zaf_electricity$electricity_2003 = rowSums (select (zaf_electricity, matches("2003")), na.rm = TRUE)
zaf_electricity$electricity_2004 = rowSums (select (zaf_electricity, matches("2004")), na.rm = TRUE)
zaf_electricity$electricity_2005 = rowSums (select (zaf_electricity, matches("2005")), na.rm = TRUE)
zaf_electricity$electricity_2006 = rowSums (select (zaf_electricity, matches("2006")), na.rm = TRUE)
zaf_electricity$electricity_2007 = rowSums (select (zaf_electricity, matches("2007")), na.rm = TRUE)
zaf_electricity$electricity_2008 = rowSums (select (zaf_electricity, matches("2008")), na.rm = TRUE)
zaf_electricity$electricity_2009 = rowSums (select (zaf_electricity, matches("2009")), na.rm = TRUE)
zaf_electricity$electricity_2010 = rowSums (select (zaf_electricity, matches("2010")), na.rm = TRUE)
zaf_electricity$electricity_2011 = rowSums (select (zaf_electricity, matches("2011")), na.rm = TRUE)
zaf_electricity$electricity_2012 = rowSums (select (zaf_electricity, matches("2012")), na.rm = TRUE)
zaf_electricity$electricity_2013 = rowSums (select (zaf_electricity, matches("2013")), na.rm = TRUE)
zaf_electricity$electricity_2014 = rowSums (select (zaf_electricity, matches("2014")), na.rm = TRUE)
zaf_electricity$electricity_2015 = rowSums (select (zaf_electricity, matches("2015")), na.rm = TRUE)
zaf_electricity$electricity_2016 = rowSums (select (zaf_electricity, matches("2016")), na.rm = TRUE)
zaf_electricity$electricity_2017 = rowSums (select (zaf_electricity, matches("2017")), na.rm = TRUE)
zaf_electricity$electricity_2018 = rowSums (select (zaf_electricity, matches("2018")), na.rm = TRUE)
zaf_electricity$electricity_2019 = rowSums (select (zaf_electricity, matches("2019")), na.rm = TRUE)
zaf_electricity$electricity_2020 = rowSums (select (zaf_electricity, matches("2020")), na.rm = TRUE)
zaf_electricity$electricity_2021 = rowSums (select (zaf_electricity, matches("2021")), na.rm = TRUE)

zaf_electricity = zaf_electricity %>%
  
  # cutting down dataframe to only sum variables
  dplyr::select (Province, all_of (empty_cols)) %>%
  
  # elongating to create year variable
  pivot_longer (2:21, names_to = "year", values_to = "electricity") %>%
  
  # reformatting year variable
  mutate (year = as.numeric (gsub ("electricity_", "", year)))


## Demography data

zaf_demography = raw_zaf_demography %>%
  
  # adding province identifier
  mutate (Province = ifelse (Name == "Western Cape", "WCP",
                             ifelse (Name == "Eastern Cape", "ECP",
                                     ifelse (Name == "Northern Cape", "NCP",
                                             ifelse (Name == "Free State", "FRS",
                                                     ifelse (Name == "KwaZulu-Natal", "KZN",
                                                             ifelse (Name == "North West", "NW", 
                                                                     ifelse (Name == "Gauteng", "GAU",
                                                                             ifelse (Name == "Mpumalanga", "MPU",
                                                                                     ifelse (Name == "Limpopo", "LIM", NA
                                                                                     )))))))))) %>%
  
  # elongating with year variable
  pivot_longer (4:21, names_to = "year", values_to = "pop") %>%
  mutate (year = as.numeric (gsub ("X", "", year))) %>%
  dplyr::select (Province, year, Sex, Age, pop) %>%
  
  # widening to create one var per demographic
  pivot_wider (names_from = "Sex", values_from = "pop") %>%
  pivot_wider (names_from = "Age", values_from = 4:5) %>% 
  
  # reformatting
  rename_with ( ~ paste0("pop_", .x)) %>%
  rename_with (
    ~ case_when (
      . == "pop_Province" ~ "Province",
      . == "pop_year" ~ "year",
      TRUE ~ .
    )
  )

## Chapter 2: Data Combining ---------------------------------------------------

df_list = c(zaf_gdp_province, zaf_gdp_province_industry, zaf_electricity, zaf_demography)
zaf_data <- df_list %>% reduce (inner_join, by = c("Province", "year"))

zaf_data = zaf_gdp_province %>%
  inner_join (zaf_gdp_province_industry, by = c("Province", "year")) %>%
  inner_join (zaf_electricity, by = c("Province", "year")) %>%
  inner_join (zaf_demography, by = c("Province", "year"))

write.csv (zaf_data, "zaf_data.csv", row.names = F)


## Chapter 3: aggregations -----------------------------------------------------

## shares of total provincial GDP

## to-do: shares for GDP categories once as shares of total provincial GDP (i.e. 
# GDP in province i for sector s divided by total GDP in province i) and once as 
# shares of national GDP within a given sector (i.e. GDP in province i for sector 
# s divided by GDP in sector s for ZAF)

zaf_data2 <- zaf_data %>%
  
  # isolating GDP vars
  dplyr::select (Province, year, gdp, gdp_agriculture_forest_fish, gdp_mining_quarry, gdp_manufacturing, gdp_electricity_gas_water,
                 gdp_construction, gdp_trade_cater_accom, gdp_transport_store_comms, gdp_fin_realestate_biz, gdp_personal_services,
                 gdp_govt_services, gdp_tax, electricity) %>%
  
  # renaming GDP var
  rename_with (
    ~ case_when (
      . == "gdp" ~ "gdp_total",
      TRUE ~ .
    )
  ) %>%
  
  # elongating data 
  pivot_longer (4:14, names_to = "sector", values_to = "gdp") %>%
  
  # computing provincial share
  mutate (share_provincial = gdp / gdp_total) %>%
  dplyr::select (Province, year, sector, gdp, gdp_total, share_provincial, electricity) %>%
  
  # compute sectoral share
  group_by (year, sector) %>%
  mutate (gdp_total_sector = sum (gdp, na.rm = T)) %>%
  ungroup () %>%
  mutate (share_sector = gdp / gdp_total_sector) %>%
  dplyr::select (Province, year, sector, gdp, gdp_total, share_provincial, share_sector, electricity) %>%
  
  ## to-do: compute shares for electricity consumption in province i on national total
  group_by (year) %>%
  mutate (sum = sum (unique (electricity), na.rm = T)) %>% ungroup () %>%
  mutate (share_electricity = electricity / sum) %>%
  dplyr::select (Province, year, sector, gdp, gdp_total, share_provincial, share_sector, electricity, share_electricity)

zaf_data2_onlygdp <- zaf_data %>%
  
  # isolating GDP vars
  dplyr::select (Province, year, gdp, gdp_agriculture_forest_fish, gdp_mining_quarry, gdp_manufacturing, gdp_electricity_gas_water,
                 gdp_construction, gdp_trade_cater_accom, gdp_transport_store_comms, gdp_fin_realestate_biz, gdp_personal_services,
                 gdp_govt_services, gdp_tax) %>%
  
  # renaming GDP var
  rename_with (
    ~ case_when (
      . == "gdp" ~ "gdp_total",
      TRUE ~ .
    )
  ) %>%
  
  # elongating data 
  pivot_longer (4:14, names_to = "sector", values_to = "gdp") %>%
  
  # computing provincial share
  mutate (share_provincial = gdp / gdp_total) %>%
  dplyr::select (Province, year, sector, gdp, gdp_total, share_provincial) %>%
  
  # compute sectoral share
  group_by (year, sector) %>%
  mutate (gdp_total_sector = sum (gdp, na.rm = T)) %>%
  ungroup () %>%
  mutate (share_sector = gdp / gdp_total_sector) %>%
  dplyr::select (Province, year, sector, gdp, gdp_total, share_provincial, share_sector)


## to-do: Aggregate the demographics data a bit, i.e. make only 3 age bins (0-19,20-64,65+) 
# and add up to total (i.e. female + male, and across age bins), then also compute shares 
# across age cohorts within province and across province

zaf_data3 <- zaf_data %>% 
  
  # computing demog variables
  mutate (pop_m_0_19 = `pop_Male_0-4`+`pop_Male_5-9`+`pop_Male_10-14`+`pop_Male_15-19`,
          pop_f_0_19 = `pop_Female_0-4`+`pop_Female_5-9`+`pop_Female_10-14`+`pop_Female_15-19`,
          
          pop_m_20_64 = `pop_Male_20-24`+`pop_Male_25-29`+`pop_Male_30-34`+`pop_Male_35-39`+`pop_Male_40-44`+`pop_Male_45-49`+`pop_Male_50-54`+`pop_Male_55-59`+`pop_Male_60-64`,
          pop_f_20_64 = `pop_Female_20-24`+`pop_Female_25-29`+`pop_Female_30-34`+`pop_Female_35-39`+`pop_Female_40-44`+`pop_Female_45-49`+`pop_Female_50-54`+`pop_Female_55-59`+`pop_Female_60-64`,
          
          pop_m_65_ = `pop_Male_65-69`+`pop_Male_70-74`+`pop_Male_75-79`+`pop_Male_80+`,
          pop_f_65_ = `pop_Female_65-69`+`pop_Female_70-74`+`pop_Female_75-79`+`pop_Female_80+`) %>%
  
  # isolating them
  dplyr::select (Province, year, pop_m_0_19, pop_f_0_19, pop_m_20_64, pop_f_20_64, pop_m_65_, pop_f_65_) %>%
  
  # computing population totals
  mutate (pop_total_province = pop_m_0_19 + pop_f_0_19 + pop_m_20_64 + pop_f_20_64 + pop_m_65_ + pop_f_65_) %>%
  group_by (year) %>%
  mutate (pop_total_national = sum (pop_total_province)) %>% ungroup () %>%
  
  # elongate data
  pivot_longer (3:8, names_to = "group", values_to = "pop") %>%
  dplyr::select (Province, year, group, pop, pop_total_province, pop_total_national) %>%
  
  # computing shares
  mutate (share_province = pop / pop_total_province, 
          share_national = pop / pop_total_national)
  

## to-do: create 3-year averages or sums across the individual variables, i.e. 
# summing or averaging GDP for the years 2017-2019, 2014-16 and so on, to get rid 
# of some of the short-term fluctuations. And after that compute shares as mentioned above

  
## exporting

write.csv (zaf_data2, 
           file.path (base_path, 
                      "world_emissions_clock", 
                      "ZAF_subnational", 
                      "02_output", 
                      "zaf_data_gdp_province.csv"),
           row.names = F)

save (zaf_data2, file = file.path (base_path, 
                                   "world_emissions_clock", 
                                   "ZAF_subnational", 
                                   "02_output", 
                                   "zaf_data_gdp_province.rda"))

write.csv (zaf_data3, 
           file.path (base_path, 
                      "world_emissions_clock", 
                      "ZAF_subnational", 
                      "02_output", 
                      "zaf_data_demog_province.csv"),
           row.names = F)

save (zaf_data3, file = file.path (base_path, 
                                   "world_emissions_clock", 
                                   "ZAF_subnational", 
                                   "02_output", 
                                   "zaf_data_demog_province.rda"))












