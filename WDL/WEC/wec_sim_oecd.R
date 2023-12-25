
## preliminaries ---------------------------------------------------------------

# building function to load WEC data 
load_wec_dat = function (){
  
  require (worlddataverse)
  
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

## sub-setting for OECD countries

# building subset function
OECD_subset = function () {
  
  require (countrycode)
  
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

## scenario 1: simple minimum

df1 = data.frame(subsector = unique(wec_dat_oecd$subsector),
                 country = NA,
                 value = NA
  )

wec_dat_oecd_1 = wec_dat_oecd %>% 
  filter (year == 2020 & subsector == "CROPS") %>%
  filter (which.min (base))

wec_dat_oecd_1 = wec_dat_oecd %>% 
  filter (year == 2020) %>% 
  select (-c("ndc", "o_1p5c", "h_cpol")) %>%
  group_by (year, iso3c, sector, subsector, pop, gdp) %>%
  summarise (emissions = sum(base))

wec_dat_oecd_1 = wec_dat_oecd_1 [wec_dat_oecd_1$subsector == "CROPS",]
wec_dat_oecd_1 = wec_dat_oecd_1 [which.min(wec_dat_oecd_1$emissions),]
  
require (foreach)

df1 = data.frame (year = NA,
                  iso3c = NA, 
                  sector = NA, 
                  subsector = NA,
                  pop = NA, 
                  gdp = NA,
                  emissions = NA
                  )


wec_dat_oecd_1 = wec_dat_oecd %>% 
  filter (year == 2020) %>% 
  select (-c("ndc", "o_1p5c", "h_cpol")) %>%
  group_by (year, iso3c, sector, subsector, pop, gdp) %>%
  summarise (emissions = sum(base))

require(data.table)
df2 = setDT(wec_dat_oecd_1)[ , .SD[which.min(emissions)], by = subsector]



subsectors = unique (wec_dat_oecd_1$subsector)
for (i in subsectors) {
  wec_dat_oecd_2 = wec_dat_oecd_1 [wec_dat_oecd_1$subsector == i,]
  output = wec_dat_oecd_2 [which.min(wec_dat_oecd_1$emissions),]
  df1 = rbind (df1, output)
}














wec_dat_oecd_1 = wec_dat_oecd_1 [which.min(wec_dat_oecd_1$base),]


min (wec_dat_oecd_1$base)

wec_dat_oecd_1 = wec_dat_oecd [wec_dat_oecd$year == 2020 &
                               wec_dat_oecd$subsector == "CROPS" & 
                               which.min(wec_dat_oecd$base),]





