
## preliminaries ---------------------------------------------------------------

pacman::p_load (worlddataverse, tidyverse, countrycode, data.table)

# building function to load WEC data 
load_wec_dat = function (){
  
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
                   "WEC_data_binary_new.Rda"))
  
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

## simulating high-wealth low-emission scenario --------------------------------

# building subset function
OECD_sim = function () {
  
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
  
  # sub-setting round 1
  wec_dat_oecd = wec_dat %>% 
    filter (iso3c %in% OECD_ISO3)
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>% 
    filter (year == 2022) %>% 
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = mean (wec_dat_oecd_2$gdp)
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2 %>% filter (gdp > gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_pc)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 25)) %>%
    relocate (rank, .before = iso3c)
  
  # saving to environment
  .GlobalEnv$highWlowE = highWlowE
  
  # creating df to store per capita totals
  em_pc_totals_new = data.frame (matrix (NA, nrow = 3, ncol = 3)) %>%
    rename_with(
      ~ case_when(
        . == "X1" ~ "high_income",
        . == "X2" ~ "mid_income",
        . == "X3" ~ "low_income",
        TRUE ~ .
      )
    )
  
  # saving totals to em_pc_totals
  em_pc_totals_new[1,1] = sum (highWlowE$em_pc [highWlowE$rank == 1])
  em_pc_totals_new[2,1] = sum (highWlowE$em_pc [highWlowE$rank == 2])
  em_pc_totals_new[3,1] = sum (highWlowE$em_pc [highWlowE$rank == 3])
  
  # saving to environment
  .GlobalEnv$em_pc_totals_new = em_pc_totals_new
  
}

# executing
OECD_sim()

## simulating  medium-wealth low-emission scenario -----------------------------

Mid_income_sim = function () {
  
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
  
  # countries list
  countries = read.csv (file.path (base_path, "world_emissions_clock", "countries.csv"))
  
  # converting to ISO3 country codes
  mid_ISO3 = countrycode (sourcevar = countries$IBRD,
                          origin = "country.name",
                          destination = "iso3c",
                          warn = TRUE, 
                          nomatch = NA)
  
  # sub-setting round 1
  data = wec_dat %>% 
    filter (iso3c %in% mid_ISO3)
  
  # sub-setting round 2
  data2 = data %>% 
    filter (year == 2022) %>% 
    select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)
  
  # setting GDP threshold
  data2$gdp_threshold = mean (data2$gdp)
  
  # first minimum emissions
  data3 = data2 %>% filter (gdp > gdp_threshold)
  midWlowE_1 = setDT(data3)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  data4 = data3 %>% filter (!(ID %in% midWlowE_1$ID))
  midWlowE_2 = setDT(data4)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  data5 = data4 %>% filter (!(ID %in% midWlowE_2$ID))
  midWlowE_3 = setDT(data5)[,.SD[which.min(em_pc)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  midWlowE = rbind (midWlowE_1, midWlowE_2, midWlowE_3) %>%
    select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 25)) %>%
    relocate (rank, .before = iso3c)
  
  # saving to environment
  .GlobalEnv$midWlowE = midWlowE
  
  # saving totals to em_pc_totals
  em_pc_totals_new[1,2] = sum (midWlowE$em_pc [midWlowE$rank == 1])
  em_pc_totals_new[2,2] = sum (midWlowE$em_pc [midWlowE$rank == 2])
  em_pc_totals_new[3,2] = sum (midWlowE$em_pc [midWlowE$rank == 3])
  
  # saving to environment
  .GlobalEnv$em_pc_totals_new = em_pc_totals_new
  
}

# execute 
Mid_income_sim()

## simulating low-wealth low-emission scenario ---------------------------------

Low_income_sim = function () {
  
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
  
  # countries list
  countries = read.csv (file.path (base_path, "world_emissions_clock", "countries.csv"))
  
  # converting to ISO3 country codes
  low_ISO3 = countrycode (sourcevar = countries$IDA,
                          origin = "country.name",
                          destination = "iso3c",
                          warn = TRUE, 
                          nomatch = NA)
  
  # sub-setting round 1
  data = wec_dat %>% 
    filter (iso3c %in% low_ISO3)
  
  # sub-setting round 2
  data2 = data %>% 
    filter (year == 2022) %>% 
    select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)
  
  # setting GDP threshold
  data2$gdp_threshold = mean (data2$gdp)
  
  # first minimum emissions
  data3 = data2 %>% filter (gdp > gdp_threshold)
  lowWlowE_1 = setDT(data3)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  data4 = data3 %>% filter (!(ID %in% lowWlowE_1$ID))
  lowWlowE_2 = setDT(data4)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  data5 = data4 %>% filter (!(ID %in% lowWlowE_2$ID))
  lowWlowE_3 = setDT(data5)[,.SD[which.min(em_pc)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  lowWlowE = rbind (lowWlowE_1, lowWlowE_2, lowWlowE_3) %>%
    select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 25)) %>%
    relocate (rank, .before = iso3c)
  
  # saving to environment
  .GlobalEnv$lowWlowE = lowWlowE
  
  # saving totals to em_pc_totals
  em_pc_totals_new[1,3] = sum (lowWlowE$em_pc [lowWlowE$rank == 1])
  em_pc_totals_new[2,3] = sum (lowWlowE$em_pc [lowWlowE$rank == 2])
  em_pc_totals_new[3,3] = sum (lowWlowE$em_pc [lowWlowE$rank == 3])
  
  # saving to environment
  .GlobalEnv$em_pc_totals_new = em_pc_totals_new
  
}

# execute 
Low_income_sim()

write.csv (lowWlowE, "lowWlowE_v3.csv")
write.csv (midWlowE, "midWlowE_v3.csv")
write.csv (highWlowE, "highWlowE_v3.csv")
write.csv (em_pc_totals_new, "em_pc_totals_v3.csv")


## preliminaries ---------------------------------------------------------------

pacman::p_load (worlddataverse, tidyverse, countrycode, data.table)

# building function to load WEC data 
load_wec_dat_old = function (){
  
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
                   "WEC_data_binary_old.Rda"))
  
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
  
  .GlobalEnv$wec_dat_old = wec_dat
  
}

# loading WEC data
load_wec_dat_old()

## simulating high-wealth low-emission scenario --------------------------------

# building subset function
OECD_sim_old = function () {
  
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
  
  # sub-setting round 1
  wec_dat_oecd = wec_dat_old %>% 
    filter (iso3c %in% OECD_ISO3)
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>% 
    filter (year == 2020) %>% 
    select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = mean (wec_dat_oecd_2$gdp)
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2 %>% filter (gdp > gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_pc)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 25)) %>%
    relocate (rank, .before = iso3c)
  
  # saving to environment
  .GlobalEnv$highWlowE_old = highWlowE
  
  # creating df to store per capita totals
  em_pc_totals = data.frame (matrix (NA, nrow = 3, ncol = 3)) %>%
    rename_with(
      ~ case_when(
        . == "X1" ~ "high_income",
        . == "X2" ~ "mid_income",
        . == "X3" ~ "low_income",
        TRUE ~ .
      )
    )
  
  # saving totals to em_pc_totals
  em_pc_totals[1,1] = sum (highWlowE$em_pc [highWlowE$rank == 1])
  em_pc_totals[2,1] = sum (highWlowE$em_pc [highWlowE$rank == 2])
  em_pc_totals[3,1] = sum (highWlowE$em_pc [highWlowE$rank == 3])
  
  # saving to environment
  .GlobalEnv$em_pc_totals_old = em_pc_totals
  
}

# executing
OECD_sim_old()

## simulating  medium-wealth low-emission scenario -----------------------------

Mid_income_sim_old = function () {
  
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
  
  # countries list
  countries = read.csv (file.path (base_path, "world_emissions_clock", "countries.csv"))
  
  # converting to ISO3 country codes
  mid_ISO3 = countrycode (sourcevar = countries$IBRD,
                          origin = "country.name",
                          destination = "iso3c",
                          warn = TRUE, 
                          nomatch = NA)
  
  # sub-setting round 1
  data = wec_dat_old %>% 
    filter (iso3c %in% mid_ISO3)
  
  # sub-setting round 2
  data2 = data %>% 
    filter (year == 2020) %>% 
    select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)
  
  # setting GDP threshold
  data2$gdp_threshold = mean (data2$gdp)
  
  # first minimum emissions
  data3 = data2 %>% filter (gdp > gdp_threshold)
  midWlowE_1 = setDT(data3)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  data4 = data3 %>% filter (!(ID %in% midWlowE_1$ID))
  midWlowE_2 = setDT(data4)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  data5 = data4 %>% filter (!(ID %in% midWlowE_2$ID))
  midWlowE_3 = setDT(data5)[,.SD[which.min(em_pc)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  midWlowE = rbind (midWlowE_1, midWlowE_2, midWlowE_3) %>%
    select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 25)) %>%
    relocate (rank, .before = iso3c)
  
  # saving to environment
  .GlobalEnv$midWlowE_old = midWlowE
  
  # saving totals to em_pc_totals
  em_pc_totals[1,2] = sum (midWlowE$em_pc [midWlowE$rank == 1])
  em_pc_totals[2,2] = sum (midWlowE$em_pc [midWlowE$rank == 2])
  em_pc_totals[3,2] = sum (midWlowE$em_pc [midWlowE$rank == 3])
  
  # saving to environment
  .GlobalEnv$em_pc_totals_old = em_pc_totals
  
}

# execute 
Mid_income_sim_old()

## simulating low-wealth low-emission scenario ---------------------------------

Low_income_sim_old = function () {
  
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
  
  # countries list
  countries = read.csv (file.path (base_path, "world_emissions_clock", "countries.csv"))
  
  # converting to ISO3 country codes
  low_ISO3 = countrycode (sourcevar = countries$IDA,
                          origin = "country.name",
                          destination = "iso3c",
                          warn = TRUE, 
                          nomatch = NA)
  
  # sub-setting round 1
  data = wec_dat_old %>% 
    filter (iso3c %in% low_ISO3)
  
  # sub-setting round 2
  data2 = data %>% 
    filter (year == 2020) %>% 
    select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)
  
  # setting GDP threshold
  data2$gdp_threshold = mean (data2$gdp)
  
  # first minimum emissions
  data3 = data2 %>% filter (gdp > gdp_threshold)
  lowWlowE_1 = setDT(data3)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  data4 = data3 %>% filter (!(ID %in% lowWlowE_1$ID))
  lowWlowE_2 = setDT(data4)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  data5 = data4 %>% filter (!(ID %in% lowWlowE_2$ID))
  lowWlowE_3 = setDT(data5)[,.SD[which.min(em_pc)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  lowWlowE = rbind (lowWlowE_1, lowWlowE_2, lowWlowE_3) %>%
    select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 25)) %>%
    relocate (rank, .before = iso3c)
  
  # saving to environment
  .GlobalEnv$lowWlowE_old = lowWlowE
  
  # saving totals to em_pc_totals
  em_pc_totals[1,3] = sum (lowWlowE$em_pc [lowWlowE$rank == 1])
  em_pc_totals[2,3] = sum (lowWlowE$em_pc [lowWlowE$rank == 2])
  em_pc_totals[3,3] = sum (lowWlowE$em_pc [lowWlowE$rank == 3])
  
  # saving to environment
  .GlobalEnv$em_pc_totals_old = em_pc_totals
  
}

# execute 
Low_income_sim_old()


## checking for differences 

highWlowE$em_pc = round (highWlowE$em_pc, 6)
highWlowE_old$em_pc = round (highWlowE_old$em_pc, 6)
midWlowE$em_pc = round (midWlowE$em_pc, 6)
midWlowE_old$em_pc = round (midWlowE_old$em_pc, 6)
lowWlowE$em_pc = round (lowWlowE$em_pc, 6)
lowWlowE_old$em_pc = round (lowWlowE_old$em_pc, 6)

high_diff = anti_join(highWlowE,highWlowE_old)
mid_diff = anti_join(midWlowE,midWlowE_old)
low_diff = anti_join(lowWlowE,lowWlowE_old)

highWlowE$em_pc_old = highWlowE_old$em_pc
highWlowE$em_pc_diff = highWlowE$em_pc - highWlowE$em_pc_old

highWlowE$country_old = highWlowE_old$country
highWlowE = highWlowE %>% relocate (country_old, .after = country)

write.csv (highWlowE, "highWlowE_diff.csv")



## preliminaries ---------------------------------------------------------------

pacman::p_load (worlddataverse, tidyverse, countrycode, data.table)

# building function to load WEC data 
load_wec_dat = function (){
  
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
                   "WEC_data_binary_old.Rda"))
  
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

## simulating high-wealth low-emission scenario --------------------------------

# building subset function
OECD_sim = function () {
  
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
  
  # sub-setting round 1
  wec_dat_oecd = wec_dat %>% 
    filter (iso3c %in% OECD_ISO3)
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>% 
    filter (year == 2020) %>% 
    select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = mean (wec_dat_oecd_2$gdp)
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2 %>% filter (gdp > gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_pc)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 25)) %>%
    relocate (rank, .before = iso3c)
  
  # saving to environment
  .GlobalEnv$highWlowE = highWlowE
  
  # creating df to store per capita totals
  em_pc_totals = data.frame (matrix (NA, nrow = 3, ncol = 3)) %>%
    rename_with(
      ~ case_when(
        . == "X1" ~ "high_income",
        . == "X2" ~ "mid_income",
        . == "X3" ~ "low_income",
        TRUE ~ .
      )
    )
  
  # saving totals to em_pc_totals
  em_pc_totals[1,1] = sum (highWlowE$em_pc [highWlowE$rank == 1])
  em_pc_totals[2,1] = sum (highWlowE$em_pc [highWlowE$rank == 2])
  em_pc_totals[3,1] = sum (highWlowE$em_pc [highWlowE$rank == 3])
  
  # saving to environment
  .GlobalEnv$em_pc_totals = em_pc_totals
  
}

# executing
OECD_sim()

## simulating  medium-wealth low-emission scenario -----------------------------

Mid_income_sim = function () {
  
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
  
  # countries list
  countries = read.csv (file.path (base_path, "world_emissions_clock", "countries.csv"))
  
  # converting to ISO3 country codes
  mid_ISO3 = countrycode (sourcevar = countries$IBRD,
                          origin = "country.name",
                          destination = "iso3c",
                          warn = TRUE, 
                          nomatch = NA)
  
  # sub-setting round 1
  data = wec_dat %>% 
    filter (iso3c %in% mid_ISO3)
  
  # sub-setting round 2
  data2 = data %>% 
    filter (year == 2020) %>% 
    select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)
  
  # setting GDP threshold
  data2$gdp_threshold = mean (data2$gdp)
  
  # first minimum emissions
  data3 = data2 %>% filter (gdp > gdp_threshold)
  midWlowE_1 = setDT(data3)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  data4 = data3 %>% filter (!(ID %in% midWlowE_1$ID))
  midWlowE_2 = setDT(data4)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  data5 = data4 %>% filter (!(ID %in% midWlowE_2$ID))
  midWlowE_3 = setDT(data5)[,.SD[which.min(em_pc)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  midWlowE = rbind (midWlowE_1, midWlowE_2, midWlowE_3) %>%
    select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 25)) %>%
    relocate (rank, .before = iso3c)
  
  # saving to environment
  .GlobalEnv$midWlowE = midWlowE
  
  # saving totals to em_pc_totals
  em_pc_totals[1,2] = sum (midWlowE$em_pc [midWlowE$rank == 1])
  em_pc_totals[2,2] = sum (midWlowE$em_pc [midWlowE$rank == 2])
  em_pc_totals[3,2] = sum (midWlowE$em_pc [midWlowE$rank == 3])
  
  # saving to environment
  .GlobalEnv$em_pc_totals = em_pc_totals
  
}

# execute 
Mid_income_sim()

## simulating low-wealth low-emission scenario ---------------------------------

Low_income_sim = function () {
  
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
  
  # countries list
  countries = read.csv (file.path (base_path, "world_emissions_clock", "countries.csv"))
  
  # converting to ISO3 country codes
  low_ISO3 = countrycode (sourcevar = countries$IDA,
                          origin = "country.name",
                          destination = "iso3c",
                          warn = TRUE, 
                          nomatch = NA)
  
  # sub-setting round 1
  data = wec_dat %>% 
    filter (iso3c %in% low_ISO3)
  
  # sub-setting round 2
  data2 = data %>% 
    filter (year == 2020) %>% 
    select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)
  
  # setting GDP threshold
  data2$gdp_threshold = mean (data2$gdp)
  
  # first minimum emissions
  data3 = data2 %>% filter (gdp > gdp_threshold)
  lowWlowE_1 = setDT(data3)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  data4 = data3 %>% filter (!(ID %in% lowWlowE_1$ID))
  lowWlowE_2 = setDT(data4)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  data5 = data4 %>% filter (!(ID %in% lowWlowE_2$ID))
  lowWlowE_3 = setDT(data5)[,.SD[which.min(em_pc)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  lowWlowE = rbind (lowWlowE_1, lowWlowE_2, lowWlowE_3) %>%
    select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 25)) %>%
    relocate (rank, .before = iso3c)
  
  # saving to environment
  .GlobalEnv$lowWlowE = lowWlowE
  
  # saving totals to em_pc_totals
  em_pc_totals[1,3] = sum (lowWlowE$em_pc [lowWlowE$rank == 1])
  em_pc_totals[2,3] = sum (lowWlowE$em_pc [lowWlowE$rank == 2])
  em_pc_totals[3,3] = sum (lowWlowE$em_pc [lowWlowE$rank == 3])
  
  # saving to environment
  .GlobalEnv$em_pc_totals = em_pc_totals
  
}

# execute 
Low_income_sim()


em_pc_totals_diff = data_frame(high_income_new = em_pc_totals$high_income,
                               high_income_old = em_pc_totals_old$high_income,
                               mid_income_new = em_pc_totals$mid_income,
                               mid_income_old = em_pc_totals_old$mid_income,
                               low_income_new = em_pc_totals$low_income,
                               low_income_old = em_pc_totals_old$low_income)

write.csv (em_pc_totals_diff, "em_pc_totals_diff.csv")

wec_dat_totals = wec_dat %>%
  select (-c("h_cpol", "o_1p5c", "ndc")) %>%
  add_column (total_emissions = NA) %>%
  group_by (iso3c, year, sector, subsector) %>%
  summarise (total_emissions = sum (base)) %>%
  filter (year == 2020) %>%
  filter (subsector == "LULUCF")

wec_dat_info = wec_dat %>%
  ungroup() %>%
  select (c(iso3c, year, pop, gdp)) %>%
  distinct()

wec_dat_totals = wec_dat_totals %>%
  left_join (wec_dat_info, by = c("iso3c", "year"))

ggplot (wec_dat_totals, aes (x = gdp, y = total_emissions)) +
  geom_jitter()









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

# sub-setting round 1
wec_dat_oecd = wec_dat %>% 
  filter (iso3c %in% OECD_ISO3)

# sub-setting round 2
wec_dat_oecd_2 = wec_dat_oecd %>% 
  filter (year == 2022) %>% 
  dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
  group_by (year, iso3c, sector, subsector, pop, gdp) %>%
  summarise (emissions = sum(base)) %>%
  drop_na() %>%
  unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
  mutate (em_pc = emissions / pop)

wec_dat_oecd_3 = wec_dat_oecd_2 %>% 
  group_by (subsector) %>% 
  summarise (avg_subsector_em_pc = mean(em_pc, na.rm = T))

oecd_mean <- sum (wec_dat_oecd_3$avg_subsector_em_pc)
