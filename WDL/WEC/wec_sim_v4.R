
## preliminaries ---------------------------------------------------------------

pacman::p_load (worlddataverse, tidyverse, countrycode, data.table)

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

## base function for highWlowE -------------------------------------------------

highWlowE <- function (year) {

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
  em_pc_totals = data.frame (matrix (NA, nrow = 3, ncol = 1))
  colnames (em_pc_totals) = as.character (year)

  # saving totals to em_pc_totals
  em_pc_totals[1,1] = sum (highWlowE$em_pc [highWlowE$rank == 1])
  em_pc_totals[2,1] = sum (highWlowE$em_pc [highWlowE$rank == 2])
  em_pc_totals[3,1] = sum (highWlowE$em_pc [highWlowE$rank == 3])

  # saving to environment
  .GlobalEnv$em_pc_totals = em_pc_totals

}

# execute
highWlowE(2022)

## function for main data only -------------------------------------------------

highWlowE_nototal <- function (year) {

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
  wec_dat_oecd_3 = wec_dat_oecd_2_nooil %>% filter (gdp > gdp_threshold)
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

  # saving to environment
  .GlobalEnv$highWlowE = highWlowE

}

## iterating over 2022-2030 ----------------------------------------------------

values = c(2022:2050)
datalist = vector("list", length = length(unique(values)))

for (i in values) {

  datalist [[i]] <- highWlowE_nototal (i)

}

highWlowE_future = do.call(rbind, datalist)

## getting totals from highWlow£_future

values = unique (highWlowE_future$year)
n = length (values)
datalist = vector("list", length = n)

for (i in 1:n) {

  # subsetting highWlowE_future
  highWlowE_future_year = highWlowE_future [highWlowE_future$year == values[i],]

  # creating df to store per capita totals
  em_pc_totals = data.frame (matrix (NA, nrow = 3, ncol = 1))

  # saving totals to em_pc_totals
  em_pc_totals[1,1] = sum (highWlowE_future_year$em_pc [highWlowE_future_year$rank == 1])
  em_pc_totals[2,1] = sum (highWlowE_future_year$em_pc [highWlowE_future_year$rank == 2])
  em_pc_totals[3,1] = sum (highWlowE_future_year$em_pc [highWlowE_future_year$rank == 3])

  datalist [[i]] <- em_pc_totals

}

em_pc_totals_future = do.call(cbind, datalist)
colnames (em_pc_totals_future) = values



