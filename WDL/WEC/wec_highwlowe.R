
## preliminaries ---------------------------------------------------------------

pacman::p_load(tidyverse, countrycode, data.table)

# building function to load WEC data 
load_wec_dat = function (){
  
  require (worlddataverse, tidyverse)
  
  # route to Google Drive
  base_path <- worlddataverse::get_wdl_path()
  
  # for German users
  get_drive_path <- function(){
    dplyr::case_when(
      dir.exists("G:/Geteilte Ablagen") ~ file.path("G:",
                                                    "Geteilte Ablagen",
                                                    "DATA_WDL"),
      dir.exists("G:/Shared Drives") ~ file.path("G:",
                                                 "Shared Drives",
                                                 "DATA_WDL"),
      dir.exists("/Volumes/GoogleDrive/Geteilte Ablagen") ~
        file.path("/Volumes",
                  "GoogleDrive",
                  "Geteilte Ablagen",
                  "DATA_WDL"))
  }
  
  if(is.na(base_path)) {
    base_path = get_drive_path()
  }
  
  # load binary (Rda) file
  load (file.path (base_path, 
                   "world_emissions_clock", 
                   "01_data", 
                   "WEC_data_binary_15072022.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  # clean environment
  rm (list = c("IIASA_data_consolidated",
               "IIASA_subsector_shares",
               "IIASA_WDL_data_consolidated",
               "IIASA_WDL_data_probabilities",
               "WDL_IIASA_data_consolidated",
               "WDL_IIASA_data_consolidated_ind_essd",
               "WDL_IIASA_data_probabilities"
  )
  )
  
  .GlobalEnv$wec_dat = wec_dat
  
}

# loading WEC data
load_wec_dat()

# changing all 0s to NAs

## simulating high-wealth low-emission scenario --------------------------------

## sub-setting for OECD countries

# building subset function
OECD_subset = function () {
  
  # OECD countries list
  OECD = c("Austria", "Belgium", "Czech Republic", "Denmark", "Estonia", 
           "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", 
           "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
           "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic", 
           "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", 
           "Canada", "Chile", "Colombia", "Mexico", "Costa Rica", 
           "United States", "Australia", "Japan", "Korea", "New Zealand", 
           "Israel", "Turkey")
  
  # converting to ISO3 country codes
  OECD_ISO3 = countrycode (sourcevar = OECD,
                           origin = "country.name",
                           destination = "iso3c",
                           warn = TRUE, 
                           nomatch = NA)
  
  # sub-setting
  wec_dat_oecd = wec_dat %>% 
    filter (iso3c %in% OECD_ISO3)
  
  # saving to environment
  .GlobalEnv$wec_dat_oecd = wec_dat_oecd
}

# subsetting
OECD_subset()

## simulating high-wealth-low-emission scenarios

## scenario 1: simple minimum emissions per sector

# building Scenario 1 function

highWlowE_1_fun = function () {

  # subset into year == 2020 as all the scenarios are equiv. in 2020
  wec_dat_oecd_1 = wec_dat_oecd %>% 
    filter (year == 2020) %>% 
    select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na()
  
  # keep only rows containing the min. emissions for each subsector
  highWlowE_1 = setDT(wec_dat_oecd_1)[,.SD[which.min(emissions)],by = subsector]
  
  # converting to per capita
  highWlowE_1 = highWlowE_1 %>%
    mutate (em_pc = emissions / pop) %>%
    select (c("year", "iso3c", "sector", "subsector", "em_pc", "gdp")) 
  
  # adding country names
  highWlowE_1$country = countrycode (sourcevar = highWlowE_1$iso3c,
                                     origin = "iso3c",
                                     destination = "country.name",
                                     warn = TRUE, 
                                     nomatch = NA)
  
  highWlowE_1 = highWlowE_1 %>% relocate (country, .before = sector)
  
  # saving to environment
  .GlobalEnv$highWlowE_1 = highWlowE_1
  
}

# executing Scenario 1
highWlowE_1_fun()

# total emissions of ideal OECD citizen by Scenario 1
sum (highWlowE_1$em_pc) # = 3.192415 tonnes per capita

## scenario 2: GDP threshold (picking from countries above the mean OECD gdp)

# building scenario 2 function 

highWlowE_2_fun = function (){
  
  # subset data 
  wec_dat_oecd_2 = wec_dat_oecd %>% 
    filter (year == 2020) %>% 
    select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na()
  
  # set GDP threshold
  wec_dat_oecd_2$gdp_threshold = mean (wec_dat_oecd_2$gdp)
  
  # keep only rows w/ minimum emissions AND GDP above threshold
  wec_dat_oecd_2 = wec_dat_oecd_2 %>% filter (gdp > gdp_threshold)
  highWlowE_2 = setDT(wec_dat_oecd_2)[,.SD[which.min(emissions)],by = subsector]
  
  # convert to per capita
  highWlowE_2 = highWlowE_2 %>%
    mutate (em_pc = emissions / pop) %>%
    select (c("year", "iso3c", "sector", "subsector", "em_pc", "gdp")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE, 
                                   nomatch = NA)) %>%
    relocate (country, .before = sector)

  # saving to environment
  .GlobalEnv$highWlowE_2 = highWlowE_2
  
}

# executing scenario 2 
highWlowE_2_fun()

# total emissions of ideal OECD citizen by Scenario 2
sum (highWlowE_2$em_pc) # = 2.446656 tonnes per capita

# note: when I run a scenario w/ a GDP and population threshold, I get the same result.

## scenario 3: population threshold (picking from countries above the mean OECD population)

# building scenario 3 function 

highWlowE_3_fun = function (){
  
  # subset data 
  wec_dat_oecd_3 = wec_dat_oecd %>% 
    filter (year == 2020) %>% 
    select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na()
  
  # set Population threshold
  wec_dat_oecd_3$pop_threshold = mean (wec_dat_oecd_3$pop)
  
  # keep only rows w/ minimum emissions AND GDP / pop above threshold
  wec_dat_oecd_3 = wec_dat_oecd_3 %>% 
    filter (pop > pop_threshold)
  highWlowE_3 = setDT(wec_dat_oecd_3)[,.SD[which.min(emissions)],by = subsector]
  
  # convert to per capita
  highWlowE_3 = highWlowE_3 %>%
    mutate (em_pc = emissions / pop) %>%
    select (c("year", "iso3c", "sector", "subsector", "em_pc", "gdp")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE, 
                                   nomatch = NA)) %>%
    relocate (country, .before = sector)
  
  # saving to environment
  .GlobalEnv$highWlowE_3 = highWlowE_3
  
}

# executing scenario 3
highWlowE_3_fun()

# total emissions of ideal OECD citizen by Scenario 3
sum (highWlowE_3$em_pc) # = 1.420799 tonnes per capita

## exporting results
write.csv(highWlowE_1, "highWlowE_1.csv", row.names = FALSE)
write.csv(highWlowE_2, "highWlowE_2.csv", row.names = FALSE)
write.csv(highWlowE_3, "highWlowE_3.csv", row.names = FALSE)



