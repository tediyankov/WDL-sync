
## Running IOM accelerator 6 regression analysis ===============================

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, data.table)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
iom_climate_path <- file.path (base_path, "IOM", "climate")

## raw datasets
em_dat_raw <- read_excel (file.path (iom_climate_path, "em_dat.xlsx"))
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
load_wec_dat()
human_mob <- read.csv (file.path (iom_climate_path, "human_mob.csv"))


## creating input data ---------------------------------------------------------

## cleaning EM-DAT
em_dat = em_dat_raw %>%
  
  # renaming columns
  dplyr::rename_with (
    ~ case_when (
      . == "Year" ~ "year",
      . == "Disaster Subtype" ~ "disaster_type",
      . == "ISO" ~ "ccode",
      . == "Total Deaths" ~ "deaths",
      . == "Total Affected" ~ "affected",
      . == "Total Damages, Adjusted ('000 US$)" ~ "damages",
      TRUE ~ .
    )
  ) %>%
  
  # removing the numbers column
  dplyr::select (-c(number, deaths)) %>%
  
  # filling the NAs by ID-specific mean wherever possible
  group_by (ID) %>%
  mutate (
    affected = ifelse (is.na (affected), mean (affected, na.rm = TRUE), affected)
  ) %>%
  
  # filling leftover NAs with type-specific mean
  group_by (disaster_type) %>%
  mutate (
    affected = ifelse (is.na (affected), mean (affected, na.rm = TRUE), affected)
  ) %>% ungroup() %>%
  
  # removing any remaining NaNs
  filter (!is.na (affected)) %>%
  
  # making year numeric 
  mutate (year = as.numeric (year)) %>%
  
  # adding info on GDP and pop
  left_join (wec_dat %>%
               
               # aggregating
               dplyr::group_by (year, iso3c) %>%
               dplyr::summarise (gdp = mean (gdp, na.rm = T), 
                                 pop = mean (pop, na.rm = T)) %>%
               
               # renaming to match EMDAT
               dplyr::rename_with (~ case_when (. == "iso3c" ~ "ccode", TRUE ~ .)), 
             by = c("year", "ccode")) %>%
  
  # converting damages to show the real thousands
  mutate (damages = damages * 1000) %>%
  
  # computing damage costs as % of GDP 
  mutate (damages_per_gdp = (damages / gdp) * 100) %>%
  
  # adding human mobility vars
  left_join (human_mob, by = c("year", "ccode"), relationship = "many-to-many") %>%
  
  # combined human mobility inclusion score
  mutate (human_mob_inclusion = migration + displacement + relocation) %>%
  
  # removing duplicates
  distinct() %>%
  
  # creating variable damages per affected
  mutate (damages_per_affected = damages / affected) %>%
  relocate (damages_per_affected, .after = damages)




## regression ------------------------------------------------------------------

summary (lm (damages_per_gdp ~ human_mob_inclusion +
               pop + 
               affected + 
               factor (disaster_type) + 
               factor (year), 
             data = em_dat))

summary (lm (damages_per_gdp ~ migration + displacement + relocation + 
               pop + 
               affected + 
               factor (disaster_type) + 
               factor (year), 
             data = em_dat))

summary (lm (damages ~ migration + displacement + relocation + 
               gdp + 
               pop + 
               affected + 
               factor (disaster_type) + 
               factor (year), 
             data = em_dat))

summary (lm (damages ~ human_mob_inclusion +
               gdp + 
               pop + 
               affected + 
               factor (disaster_type) + 
               factor (year), 
             data = em_dat))


## checking correlation
cor.test (em_dat$damages_per_gdp, em_dat$human_mob_inclusion, method = "pearson")






