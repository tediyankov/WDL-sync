
## PRELIMS =====================================================================

## clean environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, worlddataverse, ggimage)
library (ggthemes)

## set font
worlddataverse::font_wdl()

## loading data

# building function to load WEC data
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
                   "WEC_data_binary_20230314.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  .GlobalEnv$base_path = base_path
  
}

# loading WEC data
load_wec_dat()

# loading in ZAF provincial data
s1 <- read.csv (file = file.path (base_path,
                                  "world_emissions_clock",
                                  "ZAF_subnational",
                                  "02_output",
                                  "scenario_1",
                                  "zaf_emis_subnational_s1.csv")) %>% dplyr::select (-X)

s2 <- read.csv (file = file.path (base_path,
                                  "world_emissions_clock",
                                  "ZAF_subnational",
                                  "02_output",
                                  "scenario_2",
                                  "zaf_emis_subnational_s2.csv")) %>% dplyr::select (-X)

s3 <- read.csv (file = file.path (base_path,
                                  "world_emissions_clock",
                                  "ZAF_subnational",
                                  "02_output",
                                  "scenario_3",
                                  "zaf_emis_subnational_s3.csv")) %>% dplyr::select (-X)

## Scenario 1 ==================================================================

## data transform

# ZAF total data
wec_zaf = wec_dat %>%
  
  #first aggregation
  group_by (year, iso3c, sector) %>%
  summarise (ndc = sum(ndc, na.rm = T), 
             o_1p5c = sum(o_1p5c, na.rm = T), 
             base = sum(base, na.rm = T)) %>%
  
  # filtering for ZAF 
  filter (iso3c == "ZAF") %>%
  
  # renaming
  rename_with (
    ~ case_when (
      . == "sector" ~ "sec",
      . == "ndc" ~ "ndc_zaf",
      . == "o_1p5c" ~ "o_1p5c_zaf",
      . == "base" ~ "base_zaf",
      TRUE ~ .
    )
  )

# joining to s1 province data
zaf_prov_s1_dat = s1 %>%
  
  # executing merge
  left_join (wec_zaf, by = c("year", "sec")) %>%
  
  # computing shares
  mutate (base_share = base / base_zaf) %>%
  
  # keeping only relevant vars
  dplyr::select (year, prov, sec, base_share) %>%
  
  # adding percentage variable 
  mutate (labels = scales::percent (round (base_share, 2)))


## plot
zaf_prov_pie = ggplot (data = zaf_prov_s1_dat %>% 
                         filter (year == 2023) %>%
                         filter (!sec == "Agriculture"), 
                       aes (x = "", y = base_share, fill = prov)) + 
  geom_col (color = "black") +
  geom_label (aes (label = labels),
              color = "white",
              position = position_stack (vjust = 0.5),
              show.legend = F) +
  coord_polar (theta = "y") + 
  facet_wrap (~sec) +
  labs (title = "Province contribtuion to sectoral GHG Emissions", 
        subtitle = "Showing data for 2023 (Scenario 1)") + 
  theme_void () +
  #worlddataverse::theme_wdl() +
  worlddataverse::scale_fill_wdl()

zaf_prov_pie

## bar plot for agriculture
zaf_prov_agri_s1 = ggplot (data = s1 %>% 
                             filter (year == 2023) %>%
                             filter (sec == "Agriculture"),
                           aes (x = prov, y = base)) + 
  geom_bar (stat = "identity") + 
  labs (title = "Emissions from Agriculture by ZAF Province", 
        subtitle = "Showing data for 2023 (Scenario 1)",
        x = "Province", 
        y = "Emissions / T") + 
  worlddataverse::theme_wdl() +
  worlddataverse::scale_fill_wdl() + 
  theme (axis.text.x = element_text (angle = 360))
zaf_prov_agri_s1


## Scenario 2 ==================================================================

## data transform

# joining to s1 province data
zaf_prov_s2_dat = s2 %>%
  
  # executing merge
  left_join (wec_zaf, by = c("year", "sec")) %>%
  
  # computing shares
  mutate (base_share = base / base_zaf) %>%
  
  # keeping only relevant vars
  dplyr::select (year, prov, sec, base_share) %>%
  
  # adding percentage variable 
  mutate (labels = scales::percent (round (base_share, 2)))


## plot
zaf_prov_pie_s2 = ggplot (data = zaf_prov_s2_dat %>% 
                         filter (year == 2023) %>%
                         filter (!sec == "Agriculture"), 
                       aes (x = "", y = base_share, fill = prov)) + 
  geom_col (color = "black") +
  geom_label (aes (label = labels),
              color = "white",
              position = position_stack (vjust = 0.5),
              show.legend = F) +
  coord_polar (theta = "y") + 
  facet_wrap (~sec) +
  labs (title = "Province contribtuion to sectoral GHG Emissions", 
        subtitle = "Showing data for 2023 (Scenario 2)") + 
  theme_void () +
  #worlddataverse::theme_wdl() +
  worlddataverse::scale_fill_wdl()

zaf_prov_pie_s2

## bar plot for agriculture
zaf_prov_agri_s2 = ggplot (data = s2 %>% 
                             filter (year == 2023) %>%
                             filter (sec == "Agriculture"),
                           aes (x = prov, y = base)) + 
  geom_bar (stat = "identity") + 
  labs (title = "Emissions from Agriculture by ZAF Province", 
        subtitle = "Showing data for 2023 (Scenario 2)",
        x = "Province", 
        y = "Emissions / T") + 
  #worlddataverse::theme_wdl() +
  #worlddataverse::scale_fill_wdl() + 
  theme_clean () +
  theme (axis.text.x = element_text (angle = 360))
zaf_prov_agri_s2

## Scenario 3 ==================================================================

## data transform

library (caret)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# joining to s1 province data
zaf_prov_s3_dat = s3 %>%
  
  # executing merge
  left_join (wec_zaf, by = c("year", "sec")) %>%
  
  # computing shares
  mutate (base_share = base / base_zaf) %>%
  
  # keeping only relevant vars
  dplyr::select (year, prov, sec, base_share) %>%
  
  # adding percentage variable 
  mutate (labels = scales::percent (round (base_share, 2))) 


## plot
zaf_prov_pie_s3 = ggplot (data = zaf_prov_s3_dat %>% 
                            filter (year == 2023),
                            filter (!sec == "Agriculture"), 
                          aes (x = "", y = base_share, fill = prov)) + 
  geom_col (color = "black") +
  geom_label (aes (label = labels),
              color = "white",
              position = position_stack (vjust = 0.5),
              show.legend = F) +
  coord_polar (theta = "y") + 
  facet_wrap (~sec) +
  labs (title = "Province contribtuion to sectoral GHG Emissions", 
        subtitle = "Showing data for 2023 (Scenario 3)") + 
  theme_void () +
  #worlddataverse::theme_wdl() +
  worlddataverse::scale_fill_wdl()

zaf_prov_pie_s3

## bar plot for agriculture
zaf_prov_agri_s3 = ggplot (data = s3 %>% 
                             filter (year == 2023) %>%
                             filter (sec == "Agriculture"),
                           aes (x = prov, y = base)) + 
  geom_bar (stat = "identity") + 
  labs (title = "Emissions from Agriculture by ZAF Province", 
        subtitle = "Showing data for 2023 (all Scenarios)",
        x = "Province", 
        y = "Emissions / T") + 
  #worlddataverse::theme_wdl() +
  #worlddataverse::scale_fill_wdl() + 
  theme_clean () + 
  theme (axis.text.x = element_text (angle = 360))
zaf_prov_agri_s3








































