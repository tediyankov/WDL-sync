
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

## seeing if spending increases or decreases after 65+
spending = wdp %>%
  
  # country-year headcounts per spending group
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T),
             spend = sum (exp.pdf.m, na.rm = T)) %>%
  
  # filtering
  filter (ccode %in% c("BRA", "CHN", "FRA", "DEU", "IND", "MEX", "GBR", "USA")) %>%
  filter (year %in% 2022:2032) %>%
  
  # aggregating consumer spending
  mutate (daily_spending = ifelse (daily_spending == "[0,12)", "No_Cons", "Cons")) %>%
  
  # third aggregation
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (pop, na.rm = T) / 10^6,
             spend = sum (spend, na.rm = T) / 10^12) %>%
  
  # filtering out non-cons
  filter (!daily_spending == "No_Cons",
          year == 2022) %>%
  
  # final filtering
  filter (!age_group %in% c("[00,05)", "[05,10)", "[10,15)", "[15,20)", "[20,25)", "[25,30)", "[75,INF)")) %>%
  mutate (ccode = countrycode (ccode, "iso3c", "country.name"), 
          ccode = factor (ccode, levels = c("China", "India", "United States", "Brazil", "Mexico", "United Kingdom", "Germany", "France")))


spending_plot = ggplot (data = spending, 
                        aes (x = age_group, y = spend, fill = ccode))  + 
  geom_bar (position = "stack", stat = "identity") + 
  worlddataverse::theme_wdl() + 
  labs (title = "Consumer Spending per Age in 2022",
        x = "Age Group", 
        y = "Consumer Spending in USD trillions 2017 PPP") + 
  scale_fill_manual (values = c("#F3A947", "#FADC53", "#4785B6", "#5DC94C", "#8AD0E1", "#3B1F75", "#7248C8", "#D7ACF0")) +
  theme (axis.text.x = element_text (angle = 360))
spending_plot











