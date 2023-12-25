
# read data & source function ---------------------------------------------------------------

# set based path
base_path <- worlddataverse::get_wdl_path()

# load binary (Rda) file
load (file.path (base_path,
                 "world_emissions_clock",
                 "01_data",
                 "WEC_data_binary_new.Rda"))
# emission data
dat_emis = WDL_IIASA_data_consolidated_ind_essd

# subset and aggregate
dat_emis_zaf_tmp <- subset(dat_emis, iso3c == 'ZAF')
dat_emis_zaf <- aggregate(dat_emis_zaf_tmp[6:8], by = list(dat_emis_zaf_tmp$year, dat_emis_zaf_tmp$subsector), sum)
names(dat_emis_zaf)[1:2] <- c('year', 'subsector')

# data for distribution
dat_dist2 <- read.csv (file.path (base_path,
                                 "world_emissions_clock",
                                 "ZAF_subnational",
                                 "02_output",
                                 "zaf_data.csv"))

dat_dist <- read.csv (file.path (base_path,
                                  "world_emissions_clock",
                                  "ZAF_subnational",
                                  "02_output",
                                  "zaf_data_gdp_province.csv")) %>%
  
  # keeping only sectoral GDP
  dplyr::select (Province, year, sector, gdp, gdp_total, electricity) %>%
  
  # widening data
  pivot_wider (names_from = "sector", values_from = "gdp") %>%
  
  # renaming gdp total 
  rename_with (
    ~ case_when (
      . == "gdp_total" ~ "gdp",
      TRUE ~ .
    )
  )

  
# source utility functions
source (file = './ZAF_subnational/code/auxilliary.R')


# mapping sector & sub sector to data --------------------------------------

## Agriculture
sec <-  rep("Agriculture", 3)
subsec <- c("CROPS", "LIVESTOCK", "LULUCF")
dat4sec <- rep("gdp_agriculture_forest_fish", 3)

## Building
sec <-  c(sec, rep("Building", 4))
subsec <- c(subsec, c("BUILD_FGAS", "buildings_gas", "buildings_liquids", "buildings_solids"))
dat4sec <- c(dat4sec, rep("gdp", 4))


## Energy
sec <-  c(sec, rep("Energy", 7))
subsec <- c(subsec, c("ENE_COMBUST", "energy_coal", "energy_gas", "energy_heating", "energy_oil", "energy_residual", "FF_PROD"))
dat4sec <- c(dat4sec, c(paste("gdp_electricity_gas_water", "gdp", sep = '&'), 
                        "electricity",
                        "electricity",
                        "gdp",
                        "electricity",
                        paste("gdp_electricity_gas_water", "gdp", sep = '&'),
                        paste("gdp_mining_quarry", "gdp", sep = '&')))

## Industry
sec <-  c(sec, rep("Industry", 5))
subsec <- c(subsec, c("Cement", "Chemicals", "Metals", "Other (industry)", "Waste"))
dat4sec <- c(dat4sec, c(paste("gdp_mining_quarry", "gdp_construction", sep = '&'),
                        paste("gdp_manufacturing", "gdp", sep = '&'),
                        "gdp_mining_quarry",
                        paste("gdp_manufacturing", "gdp", sep = '&'),
                        paste("gdp_manufacturing", "gdp", sep = '&')))

## Transport
sec <-  c(sec, rep("Transport", 6))
subsec <- c(subsec, c("transport_bus", "transport_air", "transport_cars", "TRANSPORT_COOL", "transport_off", "transport_ships"))
dat4sec <- c(dat4sec, c("gdp_transport_store_comms", rep('gdp', 5)))


## make a whole out of the parts
sec2dat <- data.frame(sec = sec, subsec = subsec, data = dat4sec)



# calculate sub national emissions --------------------------------------

emis_sub_nat <- emis2sub_national(dat_emis_zaf, dat_dist, sec2dat)


# aggregate by sector
emis_sub_nat_sec <- aggregate(emis_sub_nat[,4:6], by = list(emis_sub_nat$year, emis_sub_nat$prov, emis_sub_nat$sec), sum)
names(emis_sub_nat_sec)[1:3] <- c('year', 'prov', 'sec')


save(emis_sub_nat, emis_sub_nat_sec, 
     file = file.path(base_path,
                      "world_emissions_clock",
                      "ZAF_subnational",
                      "02_output",
                      "zaf_emis_subnational.rda"))





