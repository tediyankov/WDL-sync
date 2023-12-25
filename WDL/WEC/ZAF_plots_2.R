
## PRELIMS =====================================================================

## clean environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, worlddataverse, ggimage)

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

# loading in and reformatting ZAF files 
s1 <- read.csv (file = file.path (base_path,
                                  "world_emissions_clock",
                                  "ZAF_subnational",
                                  "02_output",
                                  "scenario_1",
                                  "zaf_emis_subnational_s1.csv")) 

s1_tot = s1 %>% 
  group_by (year, prov) %>% 
  summarise (ndc = sum (ndc, na.rm = T), 
             o_1p5c = sum (o_1p5c, na.rm = T), 
             base = sum (base, na.rm = T)) %>% 
  mutate(sec = "Total", .after = prov)

s1 <- rbind (s1_tot, s1)

s1$sec <- factor (s1$sec, 
                  levels = c("Total", "Agriculture", "Building", 
                             "Energy", "Industry", "Transport"))

s2 <- read.csv (file = file.path (base_path,
                                  "world_emissions_clock",
                                  "ZAF_subnational",
                                  "02_output",
                                  "scenario_2",
                                  "zaf_emis_subnational_s2.csv"))

s2_tot = s2 %>% 
  group_by (year, prov) %>% 
  summarise (ndc = sum (ndc, na.rm = T), 
             o_1p5c = sum (o_1p5c, na.rm = T), 
             base = sum (base, na.rm = T)) %>% 
  mutate(sec = "Total", .after = prov)

s2 <- rbind (s2_tot, s2)

s2$sec <- factor (s2$sec, 
                  levels = c("Total", "Agriculture", "Building", 
                             "Energy", "Industry", "Transport"))


s3 <- read.csv (file = file.path (base_path,
                                  "world_emissions_clock",
                                  "ZAF_subnational",
                                  "02_output",
                                  "scenario_3",
                                  "zaf_emis_subnational_s3.csv"))

s3_tot = s3 %>% 
  group_by (year, prov) %>% 
  summarise (ndc = sum (ndc, na.rm = T), 
             o_1p5c = sum (o_1p5c, na.rm = T), 
             base = sum (base, na.rm = T)) %>% 
  mutate(sec = "Total", .after = prov)

s3 <- rbind (s3_tot, s3)

s3$sec <- factor (s3$sec, 
                  levels = c("Total", "Agriculture", "Building", 
                             "Energy", "Industry", "Transport"))


## DATA TRANSFORM ==============================================================

## creating df with only ZAF
wec_dat_zaf_sec = wec_dat %>% ungroup () %>%
  
  # first aggregation
  group_by (year, iso3c, sector) %>%
  summarise (ndc = sum(ndc, na.rm = T), 
             o_1p5c = sum(o_1p5c, na.rm = T), 
             base = sum(base, na.rm = T)) %>%
  
  # filtering for ZAF 
  filter (iso3c == "ZAF")

## computing ZAF totals
wec_dat_zaf_totals = wec_dat_zaf_sec %>% 
  group_by (year, iso3c) %>%
  summarise(ndc = sum(ndc, na.rm = T), 
            o_1p5c = sum(o_1p5c, na.rm = T), 
            base = sum(base, na.rm = T)) %>% 
  mutate (sector = "Total", .after = iso3c)

## joining to make complete ZAf df
wec_dat_zaf = rbind (wec_dat_zaf_sec, wec_dat_zaf_totals) %>%
  
  # renaming vars
  rename_with (
    ~ case_when (
      . == "sector" ~ "sec",
      . == "ndc" ~ "ndc_tot",
      . == "o_1p5c" ~ "o_1p5c_tot",
      . == "base" ~ "base_tot",
      TRUE ~ .
    )
  )

## joining with province data (scenario 1)
wec_dat_zaf_prov_s1 = s1 %>%
  
  # executing join
  left_join (wec_dat_zaf, by = c("year", "sec")) %>%
  
  # computing shares 
  mutate (ndc_share = ndc / ndc_tot,
          o_1p5c_share = o_1p5c / o_1p5c_tot,
          base_share = base / base_tot) %>%
  
  # keeping only req vars
  dplyr::select (year, prov, sec, ndc_share, o_1p5c_share, base_share)

## joining with province data (scenario 2)
wec_dat_zaf_prov_s2 = s2 %>%
  
  # executing join
  left_join (wec_dat_zaf, by = c("year", "sec")) %>%
  
  # computing shares 
  mutate (ndc_share = ndc / ndc_tot,
          o_1p5c_share = o_1p5c / o_1p5c_tot,
          base_share = base / base_tot) %>%
  
  # keeping only req vars
  dplyr::select (year, prov, sec, ndc_share, o_1p5c_share, base_share)

## joining with province data (scenario 3)
wec_dat_zaf_prov_s3 = s3 %>%
  
  # executing join
  left_join (wec_dat_zaf, by = c("year", "sec")) %>%
  
  # computing shares 
  mutate (ndc_share = ndc / ndc_tot,
          o_1p5c_share = o_1p5c / o_1p5c_tot,
          base_share = base / base_tot) %>%
  
  # keeping only req vars
  dplyr::select (year, prov, sec, ndc_share, o_1p5c_share, base_share)

## pie chart (s1)

zaf_pie_s1 = wec_dat_zaf_prov_s1 %>%
  
  # keeping only 2023
  filter (year == 2023) %>%
  
  # keeping only base_share
  dplyr::select (-c(ndc_share, o_1p5c_share)) %>%
  
  # adding percentage variable 
  mutate (labels = scales::percent (round (base_share, 2))) %>%
  
  # creating chart 
  ggplot (aes (x = "", y = base_share, fill = prov)) + 
  geom_col (color = "black") +
  geom_label (aes (label = labels),
              color = "white",
              position = position_stack (vjust = 0.5),
              show.legend = F) +
  coord_polar (theta = "y") + 
  facet_wrap (~sec) +
  labs (title = "Province contribtuion to sectoral GHG Emissions", 
        subtitle = "Showing data for 2023")
  theme_void () +
  worlddataverse::theme_wdl() +
  worlddataverse::scale_fill_wdl()

zaf_pie_s1



















