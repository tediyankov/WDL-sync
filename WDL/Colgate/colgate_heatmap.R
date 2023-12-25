
## COLGATE HEAT MAPS ===========================================================

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, worlddataverse, hrbrthemes)
library (worlddataverse)
library(viridis)
library(scales)

## set font
worlddataverse::font_wdl()

## path
base_path <- worlddataverse::get_wdl_path ()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2022_12_29','2023_02_22_2017ppp','03_outputs')

## data ------------------------------------------------------------------------

## raw data 
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_12_29_2017ppp_1_USD.rds"))

## intermediary data 
wdp <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))

## putting data into heatmap format
heatmap_data = wdp %>%
  
  # first grouping and aggregating
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T),
             spend = sum (exp.pdf.m, na.rm = T)) %>%
  
  # age groups
  mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)","[10,15)","[15,20)","[20,25)","[25,30)"), "00-30",
                              ifelse (age_group %in% c("[30,35)","[35,40)"), "30-40",
                                      ifelse (age_group %in% c("[40,45)","[45,50)"), "40-50",
                                              ifelse (age_group %in% c("[50,55)","[55,60)"), "50-60",
                                                      ifelse (age_group %in% c("[60,65)","[65,70)"), "60-70",
                                                              ifelse (age_group %in% c("[70,75)","[75,INF)"), "70+", NA))))))) %>%
  
  # filter out NA age groups
  filter (!is.na (age_group)) %>%
  
  # second aggregation into new age groups
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (pop, na.rm = T),
             spend = sum (spend, na.rm = T)) %>%
  
  # filtering
  filter (ccode %in% c("BRA", "CHN", "FRA", "DEU", "IND", "MEX", "GBR", "USA")) %>%
  filter (year %in% c(2022,2030)) %>%
  
  # aggregating consumer spending
  mutate (daily_spending = ifelse (daily_spending == "[0,12)", "No_Cons", "Cons")) %>%
  
  # third aggregation
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (pop, na.rm = T),
             spend = sum (spend, na.rm = T)) %>%
  
  # filtering out non-cons
  filter (!daily_spending == "No_Cons") %>%
  
  # removing daily_spending variable
  dplyr::select (-daily_spending) %>%
  
  # pivot wider to compute deltas 
  pivot_wider (names_from = "year", values_from = c("pop", "spend")) %>%
  
  # computing deltas
  mutate (delta_hh_percent = ((pop_2030 - pop_2022) / pop_2022),
          delta_hh_abs = pop_2030 - pop_2022, 
          delta_exp_percent = ((spend_2030 - spend_2022) / spend_2022),
          delta_exp_abs = (spend_2030 - spend_2022) / 10^9,
          delta_hh_percent_label = paste0 (round (delta_hh_percent * 100, 1), "%"),
          delta_exp_percent_label = paste0 (round (delta_exp_percent * 100, 1), "%"),
          delta_hh_abs_label = paste0 (round (delta_hh_abs / 10^6, 1), "M"),
          delta_exp_abs_label = paste0 ("$", round (delta_exp_abs, 0), "B"))

## heat maps -------------------------------------------------------------------

options (scipen = 99)

## headcounts delta percent
#heatmap_hh_percent = ggplot (data = heatmap_data %>% 
                              # mutate (ccode = countrycode (ccode, "iso3c", "country.name")) %>%
                               #filter (!age_group == "00-30"), 
                            # aes (x = age_group, y = ccode)) + 
 # geom_tile (aes (fill = delta_hh_percent)) + 
 # geom_label (aes (label = delta_hh_percent_label), fill = "white", alpha = 0.65) +
 # scale_fill_gradient(low = "#cb1c0a", high = "#008a00") + 
 # worlddataverse::theme_wdl() + 
 # labs (title = "Percentage Change in Consumer Class Headcounts", 
       # subtitle = "2022 - 2030, for 8 markets of interest", 
       # x = '\nAge',
       # y = "") + 
  #theme (axis.text.x = element_text (angle = 360)) 
  #theme (legend.position = "none")

#heatmap_hh_percent

## headcounts delta absolute
heatmap_hh_abs = ggplot (data = heatmap_data %>% 
                               mutate (ccode = countrycode (ccode, "iso3c", "country.name")) %>%
                           filter (!age_group == "00-30"), 
                             aes (x = age_group, y = ccode)) + 
  geom_tile (aes (fill = delta_hh_abs)) + 
  geom_label (aes (label = delta_hh_abs_label), fill = "white", alpha = 0.65) +
  scale_fill_gradientn (name = "Absolute\n Change",
                        colours = c("red", "yellow", "green"),
                        values = scales::rescale(c(-1, 0, 5))) +
  worlddataverse::theme_wdl() + 
  labs (title = "Absolute Change in Consumer Class Headcounts", 
        subtitle = "2022 - 2030, for 8 countries of interest", 
        x = '\nAge',
        y = "") + 
  theme (axis.text.x = element_text (angle = 360)) 
  #theme (legend.position = "none")

heatmap_hh_abs

## spending delta percent
# heatmap_exp_pct = ggplot (data = heatmap_data %>% 
#                            mutate (ccode = countrycode (ccode, "iso3c", "country.name")) %>%
#                            filter (!age_group == "00-30"), 
#                          aes (x = age_group, y = ccode)) + 
#   geom_tile (aes (fill = delta_exp_percent)) + 
#   geom_label (aes (label = delta_exp_percent_label), fill = "white", alpha = 0.65) +
#   scale_fill_viridis (name = "Percentage change", option = "plasma") +
#   worlddataverse::theme_wdl() + 
#   labs (title = "Percentage Change in Consumer Class Spending", 
#         subtitle = "2022 - 2030, for 8 markets of interest", 
#         x = '\nAge',
#         y = "") + 
#   theme (axis.text.x = element_text (angle = 360)) 
#theme (legend.position = "none")

# heatmap_exp_pct

## spending delta abs
heatmap_exp_abs = ggplot (data = heatmap_data %>% 
                            mutate (ccode = countrycode (ccode, "iso3c", "country.name")) %>%
                            filter (!age_group == "00-30"), 
                          aes (x = age_group, y = ccode)) + 
  geom_tile (aes (fill = delta_exp_abs)) + 
  geom_label (aes (label = delta_exp_abs_label), fill = "white", alpha = 0.65) +
  scale_fill_gradientn (name = "Absolute Change\n in Billions $",
                        colours = c("red", "yellow", "green"),
                        values = scales::rescale(c(-1, 0, 5))) +
  worlddataverse::theme_wdl() + 
  labs (title = "Absolute Change in Consumer Class Spending", 
        subtitle = "2022 - 2030, for 8 markets of interest", 
        x = '\nAge',
        y = "") + 
  theme (axis.text.x = element_text (angle = 360)) 
#theme (legend.position = "none")

heatmap_exp_abs


## row and column totals -------------------------------------------------------

heatmap_data_2 = heatmap_data %>%
  
  # keep only relevant vars
  dplyr::select (ccode, age_group, delta_hh_abs, delta_exp_abs) %>%
  
  # removing 0-30 age group 
  filter (!age_group == "00-30") %>%
  
  # computing row totals
  group_by (ccode) %>%
  mutate (row_sums_hh = sum (delta_hh_abs, na.rm = T)) %>%
  mutate (row_sums_exp = sum (delta_exp_abs, na.rm = T)) %>%
  ungroup () %>%
  
  # computing column totals
  group_by (age_group) %>%
  mutate (col_sums_hh = sum (delta_hh_abs, na.rm = T)) %>%
  mutate (col_sums_exp = sum (delta_exp_abs, na.rm = T)) %>%
  ungroup () %>%
  
  # converting to millions and rounding
  mutate (row_sums_hh = round (row_sums_hh / 10^6, 1), 
          row_sums_exp = round (row_sums_exp, 1), 
          col_sums_hh = round (col_sums_hh / 10^6, 1), 
          col_sums_exp = round (col_sums_exp, 1))
  
  













