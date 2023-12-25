

## loading data ----------------------------------------------------------------

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

## sectoral rankings

fun_sect_int_double <- function (year) {
  
  year = year
  
  # OECD countries list
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
    summarise (emissions = sum(base, na.rm = TRUE)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_int = emissions / gdp) %>%
    mutate (gdp_pc = gdp / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = 30000
  
  # setting pop threshold
  wec_dat_oecd_2$pop_threshold = 5000000
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2 %>%
    filter (pop >= pop_threshold) %>%
    filter (gdp_pc >= gdp_threshold) %>%
    filter (sector == "Industry")
  
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
    mutate (rank = rep(c(1,2,3), times = 5)) %>%
    relocate (rank, .before = iso3c)
  
}

data_sect_industry <- fun_sect_int_double (2022)
data_sect <- fun_sect_int_double (2022)

sum (data_sect$em_pc [data_sect$rank == 1])
sum (data_sect$em_pc [data_sect$rank == 2])
sum (data_sect$em_pc [data_sect$rank == 3])

pangea = data_sect %>%
  filter (country %in% c("South Korea", "Sweden", "Switzerland", "United Kingdom", "Netherlands")) %>%
  slice (1,2,3,6,8) %>%
  dplyr::select(-rank)

sum (pangea$em_pc) # 3.151785

write.csv (pangea, "pangea.csv")

pan = pangea %>% ungroup () %>% dplyr::select (sector, em_pc)
eu = EU_2022 %>% ungroup () %>% dplyr::select (sector, em_pc)
oecd = OECD_2022 %>% ungroup () %>% dplyr::select (sector, em_pc)
world = world_2022 %>% ungroup () %>% dplyr::select (sector, em_pc)

bar_input = rbind (world, eu, oecd, pan)

bar_input$case = rep (c("World", "EU", "OECD", "Pangea"), each = 5)
bar_input$case = factor (bar_input$case,
                         levels = c("World", "EU", "OECD", "Pangea"))

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

bar_plot_pangea = ggplot (bar_input %>% filter (case == "Pangea"), aes (fill = sector, y = em_pc, x = case)) +
  geom_bar (position = "stack", stat = "identity") +
  theme_wdl() +
  theme (legend.position = "none") +
  scale_fill_worlddataverse() +
  labs (title = "Emissions per capita: \nPangea",
        subtitle = "Base scenario, 2022",
        x = "Case",
        y = "Emissions per capita, T pc py")

bar_plot_pangea
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

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


