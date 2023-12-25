

## loading data ----------------------------------------------------------------

# building function to load WEC data
load_wec_dat = function (){
  
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

## model iterations ------------------------------------------------------------

## sub-sectoral using totals (standard)

fun_subsect_tot <- function (year) {
  
  year = year
  
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
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = mean (wec_dat_oecd_2$gdp)
  
  ## for all sectors except for energy_oil
  
  wec_dat_oecd_2_nooil = wec_dat_oecd_2 %>% filter (!(subsector == "energy_oil"))
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2_nooil %>% filter (gdp >= gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_pc)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE_nooil = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
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
    mutate (rank = rep(c(1,2,3), times = 24)) %>%
    relocate (rank, .before = iso3c)
  
  ## for energy_oil
  wec_dat_oecd_2_oil = wec_dat_oecd_2 %>% filter (subsector == "energy_oil")
  
  # first minimum emissions
  wec_dat_oecd_3_oil = wec_dat_oecd_2_oil
  highWlowE_1_oil = setDT(wec_dat_oecd_3_oil)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  wec_dat_oecd_4_oil = wec_dat_oecd_3_oil %>% filter (!(ID %in% highWlowE_1_oil$ID))
  highWlowE_2_oil = setDT(wec_dat_oecd_4_oil)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  wec_dat_oecd_5_oil = wec_dat_oecd_4_oil %>% filter (!(ID %in% highWlowE_2_oil$ID))
  highWlowE_3_oil = setDT(wec_dat_oecd_5_oil)[,.SD[which.min(em_pc)],by = subsector]
  
  highWlowE_oil = rbind (highWlowE_1_oil, highWlowE_2_oil, highWlowE_3_oil) %>%
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
    mutate (rank = rep(c(1,2,3), times = 1)) %>%
    relocate (rank, .before = iso3c)
  
  highWlowE = rbind (highWlowE_nooil, highWlowE_oil) %>% arrange (sector)
  
}
data_subsect_tot <- fun_subsect_tot (2022)

## sub-sectoral using totals (new GDP threshold)

fun_subsect_tot_newgdp <- function (year) {
  
  year = year
  
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
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop) %>%
    mutate (gdp_pc = gdp / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = 30000
  
  ## for all sectors except for energy_oil
  
  wec_dat_oecd_2_nooil = wec_dat_oecd_2 %>% filter (!(subsector == "energy_oil"))
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2_nooil %>% filter (gdp_pc >= gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_pc)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE_nooil = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
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
    mutate (rank = rep(c(1,2,3), times = 24)) %>%
    relocate (rank, .before = iso3c)
  
  ## for energy_oil
  wec_dat_oecd_2_oil = wec_dat_oecd_2 %>% filter (subsector == "energy_oil")
  
  # first minimum emissions
  wec_dat_oecd_3_oil = wec_dat_oecd_2_oil
  highWlowE_1_oil = setDT(wec_dat_oecd_3_oil)[,.SD[which.min(em_pc)],by = subsector]
  
  # second minimum emissions
  wec_dat_oecd_4_oil = wec_dat_oecd_3_oil %>% filter (!(ID %in% highWlowE_1_oil$ID))
  highWlowE_2_oil = setDT(wec_dat_oecd_4_oil)[,.SD[which.min(em_pc)],by = subsector]
  
  # third minimum emissions
  wec_dat_oecd_5_oil = wec_dat_oecd_4_oil %>% filter (!(ID %in% highWlowE_2_oil$ID))
  highWlowE_3_oil = setDT(wec_dat_oecd_5_oil)[,.SD[which.min(em_pc)],by = subsector]
  
  highWlowE_oil = rbind (highWlowE_1_oil, highWlowE_2_oil, highWlowE_3_oil) %>%
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
    mutate (rank = rep(c(1,2,3), times = 1)) %>%
    relocate (rank, .before = iso3c)
  
  highWlowE = rbind (highWlowE_nooil, highWlowE_oil) %>% arrange (sector)
  
}
data_subsect_tot_newgdp <- fun_subsect_tot_newgdp (2022)

## sub-sectoral using emissions intensity (standard)

fun_subsect_int <- function (year) {
  
  year = year
  
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
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_int = emissions / gdp)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = mean (wec_dat_oecd_2$gdp)
  
  ## for all sectors except for energy_oil
  
  wec_dat_oecd_2_nooil = wec_dat_oecd_2 %>% filter (!(subsector == "energy_oil"))
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2_nooil %>% filter (gdp >= gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_int)],by = subsector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_int)],by = subsector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_int)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE_nooil = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "subsector", "emissions", "pop")) %>%
    mutate (em_pc = emissions / pop) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    dplyr::select ("year", "sector", "subsector", "iso3c", "country", "em_pc") %>%
    mutate (rank = rep(c(1,2,3), times = 24)) %>%
    relocate (rank, .before = iso3c)
  
  ## for energy_oil
  wec_dat_oecd_2_oil = wec_dat_oecd_2 %>% filter (subsector == "energy_oil")
  
  # first minimum emissions
  wec_dat_oecd_3_oil = wec_dat_oecd_2_oil
  highWlowE_1_oil = setDT(wec_dat_oecd_3_oil)[,.SD[which.min(em_int)],by = subsector]
  
  # second minimum emissions
  wec_dat_oecd_4_oil = wec_dat_oecd_3_oil %>% filter (!(ID %in% highWlowE_1_oil$ID))
  highWlowE_2_oil = setDT(wec_dat_oecd_4_oil)[,.SD[which.min(em_int)],by = subsector]
  
  # third minimum emissions
  wec_dat_oecd_5_oil = wec_dat_oecd_4_oil %>% filter (!(ID %in% highWlowE_2_oil$ID))
  highWlowE_3_oil = setDT(wec_dat_oecd_5_oil)[,.SD[which.min(em_int)],by = subsector]
  
  highWlowE_oil = rbind (highWlowE_1_oil, highWlowE_2_oil, highWlowE_3_oil) %>%
    dplyr::select (c("year", "iso3c", "sector", "subsector", "emissions", "pop")) %>%
    mutate (em_pc = emissions / pop) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    dplyr::select ("year", "sector", "subsector", "iso3c", "country", "em_pc") %>%
    mutate (rank = rep(c(1,2,3), times = 1)) %>%
    relocate (rank, .before = iso3c)
  
  highWlowE = rbind (highWlowE_nooil, highWlowE_oil) %>% arrange (sector)
  
}
data_subsect_int <- fun_subsect_int (2022)

## sub-sectoral using emissions intensity (new GDP threshold)

fun_subsect_int_newgdp <- function (year) {
  
  year = year
  
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
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_int = emissions / gdp) %>%
    mutate (gdp_pc = gdp / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = 30000
  
  ## for all sectors except for energy_oil
  
  wec_dat_oecd_2_nooil = wec_dat_oecd_2 %>% filter (!(subsector == "energy_oil"))
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2_nooil %>% filter (gdp_pc >= gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_int)],by = subsector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_int)],by = subsector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_int)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE_nooil = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "subsector", "emissions", "pop")) %>%
    mutate (em_pc = emissions / pop) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    dplyr::select ("year", "sector", "subsector", "iso3c", "country", "em_pc") %>%
    mutate (rank = rep(c(1,2,3), times = 24)) %>%
    relocate (rank, .before = iso3c)
  
  ## for energy_oil
  wec_dat_oecd_2_oil = wec_dat_oecd_2 %>% filter (subsector == "energy_oil")
  
  # first minimum emissions
  wec_dat_oecd_3_oil = wec_dat_oecd_2_oil
  highWlowE_1_oil = setDT(wec_dat_oecd_3_oil)[,.SD[which.min(em_int)],by = subsector]
  
  # second minimum emissions
  wec_dat_oecd_4_oil = wec_dat_oecd_3_oil %>% filter (!(ID %in% highWlowE_1_oil$ID))
  highWlowE_2_oil = setDT(wec_dat_oecd_4_oil)[,.SD[which.min(em_int)],by = subsector]
  
  # third minimum emissions
  wec_dat_oecd_5_oil = wec_dat_oecd_4_oil %>% filter (!(ID %in% highWlowE_2_oil$ID))
  highWlowE_3_oil = setDT(wec_dat_oecd_5_oil)[,.SD[which.min(em_int)],by = subsector]
  
  highWlowE_oil = rbind (highWlowE_1_oil, highWlowE_2_oil, highWlowE_3_oil) %>%
    dplyr::select (c("year", "iso3c", "sector", "subsector", "emissions", "pop")) %>%
    mutate (em_pc = emissions / pop) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    dplyr::select ("year", "sector", "subsector", "iso3c", "country", "em_pc") %>%
    mutate (rank = rep(c(1,2,3), times = 1)) %>%
    relocate (rank, .before = iso3c)
  
  highWlowE = rbind (highWlowE_nooil, highWlowE_oil) %>% arrange (sector)
  
}
data_subsect_int_newgdp <- fun_subsect_int_newgdp (2022)

## sectoral using totals (standard)

fun_sect_tot <- function (year) {
  
  year = year
  
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
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:sector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = mean (wec_dat_oecd_2$gdp)
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2 %>% filter (gdp >= gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_pc)],by = sector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_pc)],by = sector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_pc)],by = sector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (sector) %>%
    dplyr::select ("year", "sector", "iso3c", "country", "em_pc") %>%
    mutate (rank = rep(c(1,2,3), times = 5)) %>%
    relocate (rank, .before = iso3c)
  
}
data_sect_tot <- fun_sect_tot (2022)

## sectoral using totals (new GDP threshold)

fun_sect_tot_newgdp <- function (year) {
  
  year = year
  
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
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:sector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop) %>%
    mutate (gdp_pc = gdp / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = 30000
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2 %>% filter (gdp_pc >= gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_pc)],by = sector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_pc)],by = sector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_pc)],by = sector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (sector) %>%
    dplyr::select ("year", "sector", "iso3c", "country", "em_pc") %>%
    mutate (rank = rep(c(1,2,3), times = 5)) %>%
    relocate (rank, .before = iso3c)
  
}
data_sect_tot_newgdp <- fun_sect_tot_newgdp (2022)

## sectoral using emissions intensity (standard)

fun_sect_int <- function (year) {
  
  year = year
  
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
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:sector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_int = emissions / gdp)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = mean (wec_dat_oecd_2$gdp)
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2 %>% filter (gdp > gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_int)],by = sector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_int)],by = sector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_int)],by = sector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "emissions", "pop")) %>%
    mutate (em_pc = emissions / pop) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (sector) %>%
    dplyr::select ("year", "sector", "iso3c", "country", "em_pc") %>%
    mutate (rank = rep(c(1,2,3), times = 5)) %>%
    relocate (rank, .before = iso3c)
  
}
data_sect_int <- fun_sect_int (2022)

## sectoral using emissions intensity (new GDP threshold)

fun_sect_int_newgdp <- function (year) {
  
  year = year
  
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
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:sector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_int = emissions / gdp) %>%
    mutate (gdp_pc = gdp / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = 30000
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2 %>% filter (gdp_pc > gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_int)],by = sector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_int)],by = sector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_int)],by = sector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "emissions", "pop")) %>%
    mutate (em_pc = emissions / pop) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (sector) %>%
    dplyr::select ("year", "sector", "iso3c", "country", "em_pc") %>%
    mutate (rank = rep(c(1,2,3), times = 5)) %>%
    relocate (rank, .before = iso3c)
  
}
data_sect_int_newgdp <- fun_sect_int_newgdp (2022)

## sums ------------------------------------------------------------------------

sum (data_subsect_tot_newgdp$em_pc [data_subsect_tot_newgdp$rank == 1])
sum (data_subsect_tot_newgdp$em_pc [data_subsect_tot_newgdp$rank == 2])
sum (data_subsect_tot_newgdp$em_pc [data_subsect_tot_newgdp$rank == 3])

sum (data_subsect_tot_newgdp$em_pc [data_subsect_tot_newgdp$rank == 1])
sum (data_subsect_tot_newgdp$em_pc [data_subsect_tot_newgdp$rank == 2])
sum (data_subsect_tot_newgdp$em_pc [data_subsect_tot_newgdp$rank == 3])

sum (data_subsect_int$em_pc [data_subsect_int$rank == 1])
sum (data_subsect_int$em_pc [data_subsect_int$rank == 2])
sum (data_subsect_int$em_pc [data_subsect_int$rank == 3])

sum (data_subsect_int_newgdp$em_pc [data_subsect_int_newgdp$rank == 1])
sum (data_subsect_int_newgdp$em_pc [data_subsect_int_newgdp$rank == 2])
sum (data_subsect_int_newgdp$em_pc [data_subsect_int_newgdp$rank == 3])

sum (data_sect_int$em_pc [data_sect_int$rank == 1])
sum (data_sect_int$em_pc [data_sect_int$rank == 2])
sum (data_sect_int$em_pc [data_sect_int$rank == 3])

sum (data_sect_int_newgdp$em_pc [data_sect_int_newgdp$rank == 1])
sum (data_sect_int_newgdp$em_pc [data_sect_int_newgdp$rank == 2])
sum (data_sect_int_newgdp$em_pc [data_sect_int_newgdp$rank == 3])

sum (data_sect_tot$em_pc [data_sect_tot$rank == 1])
sum (data_sect_tot$em_pc [data_sect_tot$rank == 2])
sum (data_sect_tot$em_pc [data_sect_tot$rank == 3])

sum (data_sect_tot_newgdp$em_pc [data_sect_tot_newgdp$rank == 1])
sum (data_sect_tot_newgdp$em_pc [data_sect_tot_newgdp$rank == 2])
sum (data_sect_tot_newgdp$em_pc [data_sect_tot_newgdp$rank == 3])





## exporting results into CSV --------------------------------------------------

# sub-sector data
write.csv (data_subsect_int, "data_subsect_int.csv")
write.csv (data_subsect_int_newgdp, "data_subsect_int_newgdp.csv")
write.csv (data_subsect_tot, "data_subsect_tot.csv")
write.csv (data_subsect_tot_newgdp, "data_subsect_tot_newgdp.csv")

# sector data
write.csv (data_sect_int, "data_sect_int.csv")
write.csv (data_sect_int_newgdp, "data_sect_int_newgdp.csv")
write.csv (data_sect_tot, "data_sect_tot.csv")
write.csv (data_sect_tot_newgdp, "data_sect_tot_newgdp.csv")

## double threshold model iteration (finalised method) -------------------------------------------------------------

fun_subsect_int_double <- function (year) {
  
  year = year
  
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
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_int = emissions / gdp) %>%
    mutate (gdp_pc = gdp / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = 30000
  
  # setting pop threshold
  wec_dat_oecd_2$pop_threshold = 5000000
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2 %>% filter (pop >= pop_threshold) %>% filter (gdp_pc >= gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_int)],by = subsector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_int)],by = subsector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_int)],by = subsector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "subsector", "emissions", "pop")) %>%
    mutate (em_pc = emissions / pop) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    dplyr::select ("year", "sector", "subsector", "iso3c", "country", "em_pc") %>%
    mutate (rank = rep(c(1,2,3), times = 25)) %>%
    relocate (rank, .before = iso3c)
  
}
data_subsect <- fun_subsect_int_double_2 (2022)

sum (data_subsect$em_pc [data_subsect$rank == 1])
sum (data_subsect$em_pc [data_subsect$rank == 2])
sum (data_subsect$em_pc [data_subsect$rank == 3])

write.csv (data_subsect, "dbl_subsect.csv")

fun_sect_int_double <- function (year) {
  
  year = year
  
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
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:sector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_int = emissions / gdp) %>%
    mutate (gdp_pc = gdp / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = 30000
  
  # setting pop threshold
  wec_dat_oecd_2$pop_threshold = 5000000
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2 %>% filter (pop >= pop_threshold) %>% filter (gdp_pc >= gdp_threshold)
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_int)],by = sector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_int)],by = sector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_int)],by = sector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "emissions", "pop")) %>%
    mutate (em_pc = emissions / pop) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (sector) %>%
    dplyr::select ("year", "sector", "iso3c", "country", "em_pc") %>%
    mutate (rank = rep(c(1,2,3), times = 5)) %>%
    relocate (rank, .before = iso3c)
  
}
data_sect <- fun_sect_int_double (2022)

sum (data_sect$em_pc [data_sect$rank == 1])
sum (data_sect$em_pc [data_sect$rank == 2])
sum (data_sect$em_pc [data_sect$rank == 3])

write.csv (data_sect, "dbl_sect.csv")



















## buidling bar charts ---------------------------------------------------------

# world average data input

world_avg = wec_dat %>%
  filter (year == 2022) %>%
  group_by (year, iso3c, sector) %>%
  summarise (base = sum (base, na.rm = T),
             pop = mean (pop, na.rm = T)) %>%
  ungroup () %>%
  group_by (year, sector) %>%
  summarise (base = sum (base, na.rm = T), 
             pop = sum (pop, na.rm = T)) %>%
  mutate (pop = replace (pop, sector == "Transport", pop [sector == "Transport"] / 2),
          em_pc = base / pop) %>%
  dplyr::select (sector, em_pc)

sum (world_avg$base_pc)

oecd_avg = wec_dat %>% 
  
  
  
  
  
  
  
  
  
  
  











fun_world = function (year) {
  
  year = year
  
  wec_dat = wec_dat [wec_dat$year == year,]
  
  world = wec_dat %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    mutate (base_pc = base / pop) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (em_pc = sum(base_pc, na.rm = T)) %>%
    group_by (year, sector) %>%
    summarise (em_pc = mean (em_pc, na.rm = T))
  
}

world_2022 <- fun_world (2022)
sum (world_2022$em_pc) # 9.037882

# OECD average data input

fun_OECD = function (year) {
  
  year = year
  
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
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  oecd = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    mutate (em_pc = emissions / pop) %>%
    group_by (year, sector) %>%
    summarise (em_pc = mean (em_pc, na.rm = T))
  
}
OECD_2022 <- fun_OECD (2022)
sum (OECD_2022$em_pc) # 12.16564

# EU data input

fun_EU = function (year) {
  
  year = year 
  
  EU = c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus", 
         "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", 
         "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", 
         "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", 
         "Slovakia", "Slovenia", "Spain", "Sweden")
  
  EU_ISO3 <- countrycode (sourcevar = EU,
                          origin = "country.name",
                          destination = "iso3c",
                          warn = TRUE,
                          nomatch = NA)
  
  wec_dat_eu = wec_dat %>% 
    filter (iso3c %in% EU_ISO3)
  
  wec_dat_eu = wec_dat_eu [wec_dat_eu$year == year, ]
  
  eu = wec_dat_eu %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    mutate (em_pc = emissions / pop) %>%
    group_by (year, sector) %>%
    summarise (em_pc = mean (em_pc, na.rm = T))
  
}
EU_2022 <- fun_EU (2022)
sum (EU_2022$em_pc) # 10.01915

# Pangea input (raw)

Pangea_2022 = data_sect %>% 
  filter (rank == 1) %>% 
  dplyr::select(-rank)
sum (Pangea_2022$em_pc) # 3.991277

# Pangea input (manual input)

Pangea_2022_2 = data_sect %>%
  filter (country %in% c("South Korea", "Sweden", "Slovakia", "United Kingdom", "Switzerland")) %>%
  slice (2,3,4,6,7) %>%
  dplyr::select(-rank)
sum (Pangea_2022_2$em_pc) # 4.159718

# creating unified input df

pan_1 = Pangea_2022 %>% dplyr::select (sector, em_pc)
pan_2 = Pangea_2022_2 %>% dplyr::select (sector, em_pc)
eu = EU_2022 %>% ungroup () %>% dplyr::select (sector, em_pc)
oecd = OECD_2022 %>% ungroup () %>% dplyr::select (sector, em_pc)
world = world_2022 %>% ungroup () %>% dplyr::select (sector, em_pc)

bar_input = rbind (world, eu, oecd, pan_1, pan_2)

bar_input$case = rep (c("World", "EU", "OECD", "Pangea 1.0", "Pangea 2.0"), each = 5)
bar_input$case = factor (bar_input$case, 
                         levels = c("World", "EU", "OECD", "Pangea 1.0", "Pangea 2.0"))

library (ggplot2)

pal_worlddataverse <- c("#0dcdc0", "#264653", "#e9c46a", "#f4a261", "#e76f51")

scale_fill_worlddataverse <- function(){
  structure(list(
    scale_fill_manual(values=pal_worlddataverse)
  ))
}

bar_plot = ggplot (bar_input, aes (fill = sector, y = em_pc, x = case)) + 
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl() + 
  scale_fill_worlddataverse() + 
  labs (title = "Emissions per capita: Pangea vs World, EU, OECD",
        subtitle = "Base scenario, 2022",
        x = "Case",
        y = "Emissions per capita, T pc py")

bar_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))




