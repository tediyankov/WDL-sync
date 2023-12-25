
## re-doing IOM action 6 modelling and pairs with new new approach

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
em_dat_raw <- read_excel (file.path (iom_climate_path, "emdat_public_2023_08_18_query_uid-KrtPjb.xlsx"))
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
human_mob <- read.csv (file.path (iom_climate_path, "human_mob_countries.csv")) %>%
  
  # combined human mobility inclusion score
  mutate (human_mob_inclusion = migration + displacement + relocation)

## cleaning --------------------------------------------------------------------

## first clean
em_dat_clean <- em_dat_raw %>%
  
  # removing top 6 rows
  dplyr::slice(-c(1:5)) %>%
  
  # making row 1 the header row
  janitor::row_to_names(1) %>%
  
  # keeping only relevant variables
  dplyr::select (c("Dis No", "Year", "Disaster Type", "ISO", "Continent", 
                   "Total Affected", "Total Damages, Adjusted ('000 US$)")) %>%
  
  # extracting disaster ID var
  mutate (disaster_id = substr (`Dis No`, 1, 9)) %>%
  relocate (disaster_id, .before = "Year") %>%
  dplyr::select (-`Dis No`) %>%
  
  # renaming
  dplyr::rename_with (
    ~ case_when (
      . == "Year" ~ "year",
      . == "Disaster Type" ~ "disaster_type",
      . == "ISO" ~ "ccode",
      . == "Total Affected" ~ "total_affected",
      . == "Total Damages, Adjusted ('000 US$)" ~ "total_damages",
      TRUE ~ .
    )
  ) %>%
  
  # converting number stuff to numeric
  mutate_at (vars (c("year", 
                     "total_affected", 
                     "total_damages")), 
             as.numeric)

## creating pairs by similar number of ppl affected and same disaster type
similar_affected_pairs <- em_dat_clean %>%
  
  # inner join to isolate pairs that share year and disaster type
  inner_join (em_dat_clean, by = c("year", 
                                   "disaster_type"), 
              relationship = "many-to-many") %>%
  
  # filtering for those whose difference in affected is below 500 or including
  filter (abs (total_affected.x - total_affected.y) / pmax(total_affected.x, total_affected.y) <= 0.05,
          ccode.x != ccode.y) %>%
  
  # concatenating the two country codes with a hyphen separator
  #unite ("ccode_pair", c(ccode.x, ccode.y), sep = "_") %>%
  
  # removing duplicate pairs
  distinct () %>%
  
  # keeping only where both damages and affected columns have values
  dplyr::filter_at (vars (c("total_affected.x", 
                            "total_affected.y", 
                            "total_damages.x", 
                            "total_damages.y")), 
                    all_vars (!is.na(.))) 

## obtaining which countries are contained
human_mob_countries = data.frame (ccode = unique (similar_affected_pairs$ccode.x, similar_affected_pairs$ccode.y),
                                  year = NA, 
                                  migration = NA, 
                                  displacement = NA, 
                                  relocation = NA) %>% distinct ()

## exporting to fill in manually in Excel
write.csv (human_mob_countries, "human_mob_countries.csv", row.names = F)

## adding human mobility
similar_affected_pairs_wmob = similar_affected_pairs %>%
  
  # joining with human_mob to see human mobility index for ccode.x and ccode.y
  left_join (human_mob %>% dplyr::select (-c(year)), by = c("ccode.x" = "ccode"), relationship = "many-to-many") %>%
  left_join (human_mob %>% dplyr::select (-c(year)), by = c("ccode.y" = "ccode"), relationship = "many-to-many") %>%
  
  # removing duplicate pairs
  distinct () %>%
  
  # keeping only pairs where there is info on human mobility index
  dplyr::filter_at (vars (c("human_mob_inclusion.x", "human_mob_inclusion.y")), all_vars (!is.na(.))) %>%
  
  # keeping only pairs which have a difference in their human mob inclusion index
  filter (human_mob_inclusion.x != human_mob_inclusion.y) %>%
  
  # filtering only for ones where damages in X are smaller than in y
  filter (total_damages.x < total_damages.y) %>%
  
  # filtering for where human mobility in x are more than in y
  filter (human_mob_inclusion.x > human_mob_inclusion.y) %>%
  
  # changing damages to damages per affected
  mutate (total_damages.x = (total_damages.x * 10^3) / total_affected.x,
          total_damages.y = (total_damages.y * 10^3) / total_affected.y) %>%
  
  # keeping only relevant variables
  dplyr::select (year, disaster_type, ccode.x, Continent.x, ccode.y, Continent.y, 
                 total_damages.x, total_damages.y, migration.x, displacement.x, 
                 relocation.x, human_mob_inclusion.x, migration.y, 
                 displacement.y, relocation.y, human_mob_inclusion.y)

## replicating regression

# input data
input = em_dat_clean %>%
  
  # keeping only rows which have affected and damages
  filter_at (vars (c("total_affected", "total_damages")), all_vars (!is.na(.))) %>%
  
  # total damages per affected
  mutate (damages_per_affected = total_damages / total_affected) %>%
  dplyr::select (-c(total_affected, total_damages)) %>%
  
  # adding pop and GDP from WEC database
  left_join (wec_dat %>%
               
               # aggregating
               dplyr::group_by (year, iso3c) %>%
               dplyr::summarise (gdp = mean (gdp, na.rm = T), 
                                 pop = mean (pop, na.rm = T)) %>%
               
               # renaming to match EMDAT
               dplyr::rename_with (~ case_when (. == "iso3c" ~ "ccode", TRUE ~ .)), 
             by = c("year", "ccode")) %>%
  
  # adding human mobility
  left_join (human_mob, by = c("year", "ccode")) %>%
  
  # removing NAs 
  filter (!is.na (human_mob_inclusion)) %>%
  
  # adding damages per gdp
  mutate (damages_per_gdp = damages_per_affected / gdp)
  
# models
summary (lm (damages_per_gdp ~ human_mob_inclusion +
               pop + 
               factor (disaster_type) + 
               factor (year) + 
               factor (Continent), 
             data = input))

summary (lm (damages_per_gdp ~ migration + displacement + relocation + 
               pop + 
               factor (disaster_type) + 
               factor (year)+ 
               factor (Continent), 
             data = input))

summary (lm (damages_per_affected ~ migration + displacement + relocation + 
               gdp + 
               pop + 
               factor (disaster_type) + 
               factor (year)+ 
               factor (Continent), 
             data = input))

summary (lm (damages_per_affected ~ human_mob_inclusion +
               gdp + 
               pop + 
               factor (disaster_type) + 
               factor (year)+ 
               factor (Continent), 
             data = input))

cor.test (input$damages_per_affected, input$human_mob_inclusion, method = "pearson")
cor.test (input$damages_per_affected, input$migration, method = "pearson")
cor.test (input$damages_per_affected, input$relocation, method = "pearson")
cor.test (input$damages_per_affected, input$displacement, method = "pearson")
  
  
  
  




