
## COLGATE VIZ =====================================================================

## Prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, worlddataverse)

## set font
worlddataverse::font_wdl()

## path
base_path <- worlddataverse::get_wdl_path ()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2022_12_29','2023_02_22_2017ppp','03_outputs')



## Raw data --------------------------------------------------------------------

wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_12_29_2017ppp_1_USD.rds"))



## Data transform --------------------------------------------------------------------

## WDPro data first clean into spending groups
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))

## WDPro data second clean for purpose
wdp_clean_1 = wdp %>%
  
  # country-year headcounts per spending group
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T),
             spend = sum (exp.pdf.m, na.rm = T)) %>%
  
  # age groups
  mutate (age_group = ifelse (age_group %in% c("[00,05)", "[05,10)", "[10,15)", "[15,20)", "[20,25)", "[25,30)"), "[00,30)",
                              ifelse (age_group %in% c("[30,35)", "[35,40)"), "[30,40)", 
                                      ifelse (age_group %in% c("[40,45)", "[45,50)", "[50,55)", "[55,60)", "[60,65)"), "[40,65)",
                                              ifelse (age_group %in% c("[65,70)", "[70,75)", "[75,INF)"), "[65, INF)",
                                                      NA
                                                      ))))) %>%
  
  # second aggregation into new age groups
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (pop, na.rm = T),
             spend = sum (spend, na.rm = T)) %>%
  
  # filtering
  filter (ccode %in% c("BRA", "CHN", "FRA", "DEU", "IND", "MEX", "GBR", "USA")) %>%
  filter (year %in% 2022:2032)

## creating df

dat_2023 = expand.grid (year = unique (wdp_clean_1$year), 
                        ccode = unique (wdp_clean_1$ccode)) %>%
  
  # adding vars
  left_join (wdp_clean_1 %>%
               filter (age_group == "[40,65)") %>%
               filter (!daily_spending == "[0,12)") %>%
               group_by (ccode, year) %>%
               summarise (pop_40_65 = sum (pop, na.rm = T) / 10^6,
                          spend_40_65 = sum (spend, na.rm = T) / 10^12), by = c("ccode", "year")) %>%
  
  left_join (wdp_clean_1 %>%
               filter (age_group %in% c("[30,40)", "[40,65)")) %>%
               filter (!daily_spending == "[0,12)") %>%
               group_by (ccode, year) %>%
               summarise (pop_30_65 = sum (pop, na.rm = T) / 10^6,
                          spend_30_65 = sum (spend, na.rm = T) / 10^12), by = c("ccode", "year")) %>%
  
  left_join (wdp_clean_1 %>%
               filter (age_group %in% c("[40,65)","[65, INF)")) %>%
               filter (!daily_spending == "[0,12)") %>%
               group_by (ccode, year) %>%
               summarise (pop_40_ = sum (pop, na.rm = T) / 10^6,
                          spend_40_ = sum (spend, na.rm = T) / 10^12), by = c("ccode", "year")) %>%
  
  left_join (wdp_clean_1 %>%
               filter (age_group %in% c( "[30,40)","[40,65)","[65, INF)")) %>%
               filter (!daily_spending == "[0,12)") %>%
               group_by (ccode, year) %>%
               summarise (pop_30_ = sum (pop, na.rm = T) / 10^6,
                          spend_30_ = sum (spend, na.rm = T) / 10^12), by = c("ccode", "year")) 


## Data visualisation --------------------------------------------------------------------


## line plots

# spending trend
spend_plot = ggplot (data = dat_2023, aes (x = year)) + 
  geom_line (aes (y = spend_40_65), col = "blue") + 
  geom_line (aes (y = spend_30_65), col = "red") + 
  geom_line (aes (y = spend_40_), col = "green") + 
  geom_line (aes (y = spend_30_), col = "orange") + 
  worlddataverse::theme_wdl () + 
  facet_wrap (~ccode, scales = "free")
spend_plot

# headcount trend
headcount_plot = ggplot (data = dat_2023, aes (x = year)) + 
  geom_line (aes (y = pop_40_65), col = "blue") + 
  geom_line (aes (y = pop_30_65), col = "red") + 
  geom_line (aes (y = pop_40_), col = "green") + 
  geom_line (aes (y = pop_30_), col = "orange") + 
  worlddataverse::theme_wdl () + 
  facet_wrap (~ccode, scales = "free")
headcount_plot

## delta bar plots

# spending
spend_delta_plot = dat_2023 %>%
  
  # computing delta vars
  mutate (spend_40_65_2 = Hmisc::Lag (spend_40_65, 1),
          spend_40_65_2 = ifelse (is.na (spend_40_65_2), 0, spend_40_65_2),
          spend_40_65_delta = ((spend_40_65 - spend_40_65_2) / spend_40_65_2) * 100) %>%
  
  mutate (spend_30_65_2 = Hmisc::Lag (spend_30_65, 1),
          spend_30_65_2 = ifelse (is.na (spend_30_65_2), 0, spend_30_65_2),
          spend_30_65_delta = ((spend_30_65 - spend_30_65_2) / spend_30_65_2) * 100) %>%
  
  mutate (spend_40_2 = Hmisc::Lag (spend_40_, 1),
          spend_40_2 = ifelse (is.na (spend_40_2), 0, spend_40_2),
          spend_40_delta = ((spend_40_ - spend_40_2) / spend_40_2) * 100) %>%
  
  mutate (spend_30_2 = Hmisc::Lag (spend_30_, 1),
          spend_30_2 = ifelse (is.na (spend_30_2), 0, spend_30_2),
          spend_30_delta = ((spend_30_ - spend_30_2) / spend_30_2) * 100) %>%
  
  # keeping only deltas 
  dplyr::select (ccode, year, spend_40_65_delta, spend_30_65_delta, spend_40_delta, spend_30_delta) %>%
  
  # filtering away 2022
  filter (!year == 2022) %>%
  
  # plotting 
  ggplot (aes (x = factor (year))) +
  geom_bar (aes (y = spend_40_65_delta), width = 0.8, stat = 'identity', fill = "#fbca7e") + 
  geom_bar (aes (y = spend_30_65_delta), width = 0.6, stat = 'identity', fill = "#eba979") + 
  geom_bar (aes (y = spend_40_delta), width = 0.4, stat = 'identity', fill = "#e76650") + 
  geom_bar (aes (y = spend_30_delta), width = 0.2, stat = 'identity', fill = "#408998") + 
  theme_wdl () + 
  theme (axis.text.x = element_text (angle = 360)) + 
  scale_fill_manual (name = 'Age group',
                     breaks = c('40-65', '30-65', '40+', '30+'),
                     values = c('40-65'='#fbca7e', '30-65'='#eba979', '40+'='#e76650', '30+'='#408998')) + 
  theme (legend.position = "bottom") +
  facet_wrap (~ccode, scale = "free") + 
  labs (title = "Change in consumer spending per age group", 
        x = "Year", 
        y = "% Change in Consumer Spending")
spend_delta_plot

# headcounts
pop_delta_plot = dat_2023 %>%
  
  # computing delta vars
  mutate (pop_40_65_2 = Hmisc::Lag (pop_40_65, 1),
          pop_40_65_2 = ifelse (is.na (pop_40_65_2), 0, pop_40_65_2),
          pop_40_65_delta = ((pop_40_65 - pop_40_65_2) / pop_40_65_2) * 100) %>%
  
  mutate (pop_30_65_2 = Hmisc::Lag (pop_30_65, 1),
          pop_30_65_2 = ifelse (is.na (pop_30_65_2), 0, pop_30_65_2),
          pop_30_65_delta = ((pop_30_65 - pop_30_65_2) / pop_30_65_2) * 100) %>%
  
  mutate (pop_40_2 = Hmisc::Lag (pop_40_, 1),
          pop_40_2 = ifelse (is.na (pop_40_2), 0, pop_40_2),
          pop_40_delta = ((pop_40_ - pop_40_2) / pop_40_2) * 100) %>%
  
  mutate (pop_30_2 = Hmisc::Lag (pop_30_, 1),
          pop_30_2 = ifelse (is.na (pop_30_2), 0, pop_30_2),
          pop_30_delta = ((pop_30_ - pop_30_2) / pop_30_2) * 100) %>%
  
  # keeping only deltas 
  dplyr::select (ccode, year, pop_40_65_delta, pop_30_65_delta, pop_40_delta, pop_30_delta) %>%
  
  # filtering away 2022
  filter (!year == 2022) %>%
  
  # plotting 
  ggplot (aes (x = factor (year))) +
  geom_bar (aes (y = pop_40_65_delta), width = 0.8, stat = 'identity', fill = "#fbca7e") + 
  geom_bar (aes (y = pop_30_65_delta), width = 0.6, stat = 'identity', fill = "#eba979") + 
  geom_bar (aes (y = pop_40_delta), width = 0.4, stat = 'identity', fill = "#e76650") + 
  geom_bar (aes (y = pop_30_delta), width = 0.2, stat = 'identity', fill = "#408998") + 
  theme_wdl () + 
  theme (axis.text.x = element_text (angle = 360)) + 
  scale_fill_manual (name = 'Age group',
                     breaks = c('40-65', '30-65', '40+', '30+'),
                     values = c('40-65'='#fbca7e', '30-65'='#eba979', '40+'='#e76650', '30+'='#408998')) + 
  theme (legend.position = "bottom") +
  facet_wrap (~ccode, scale = "free") + 
  labs (title = "Change in consumer headcounts per age group", 
        x = "Year", 
        y = "% Change in Consumer Headcounts")
pop_delta_plot


## Final bar plot (Definition - Using consumer spending)

hh_change_23_32_dat = wdp %>%
  
  # country-year headcounts per spending group
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T),
             spend = sum (exp.pdf.m, na.rm = T)) %>%
  
  # age groups
  mutate (age_group = ifelse (age_group %in% c("[30,35)"), "30_35",
                              ifelse (age_group %in% c("[35,40)"), "35_40",
                                      ifelse (age_group %in% c("[40,45)","[45,50)"), "40_50",
                                              ifelse (age_group %in% c("[50,55)","[55,60)"), "50_60",
                                                      ifelse (age_group %in% c("[65,70)","[70,75)","[75,INF)"), "65_", NA)))))) %>%
  
  # filter out NA age groups
  filter (!is.na (age_group)) %>%
  
  # second aggregation into new age groups
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (pop, na.rm = T),
             spend = sum (spend, na.rm = T)) %>%
  
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
  filter (!daily_spending == "No_Cons") %>%
  
  # computing changes
  filter (year %in% c(2022, 2032)) %>%
  pivot_wider (names_from = "year", values_from = c(5:6)) %>%
  mutate (hh_delta = pop_2032 - pop_2022, 
          exp_delta = spend_2032 - spend_2022) %>%
  
  # keeping only relevant vars
  dplyr::select (ccode, age_group, hh_delta, exp_delta)
  
  
hh_change_23_32_plot = ggplot (data = hh_change_23_32_dat, 
                               aes (fill = ccode, y = hh_delta, x = age_group)) +
  geom_bar (position = "stack", stat = "identity") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  labs (title = "Change in consumer headcounts",
        subtitle = "From 2022 to 2032",
        x = "Age Group",
        y = "Change in Consumer Headcounts, in Millions of people")

hh_change_23_32_plot

exp_change_23_32_plot = ggplot (data = hh_change_23_32_dat, 
                               aes (fill = ccode, y = exp_delta, x = age_group)) +
  geom_bar (position = "stack", stat = "identity") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  labs (title = "Change in consumer spending",
        subtitle = "From 2022 to 2032",
        x = "Age Group",
        y = "Change in Consumer Spending, in Trillions USD 2017 PPP")

exp_change_23_32_plot


library (ggpubr)

plotlist <- list (hh_change_23_32_plot, exp_change_23_32_plot)
colgate_plot1 <- ggarrange (plotlist = plotlist, nrow = 1, ncol = 2)
colgate_plot1  


## seeing if spending increases or decreases after 65+
spending_65 = wdp %>%
  
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
  group_by (year, age_group, daily_spending) %>%
  summarise (pop = sum (pop, na.rm = T) / 10^6,
             spend = sum (spend, na.rm = T) / 10^12) %>%
  
  # filtering out non-cons
  filter (!daily_spending == "No_Cons",
          year == 2022)

spending_65_plot = ggplot (data = spending_65 %>% filter (!age_group == "[75,INF)"), 
                           aes (x = age_group, y = spend))  + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  labs (title = "Consumer Spending per Age in 2022", 
        subtitle = "Spending decreases after 60, spikes up after 75",
        x = "Age Group", 
        y = "Consumer Spending in USD trillions 2017 PPP") + 
  theme (axis.text.x = element_text (angle = 360))
spending_65_plot

## PER CAPITA SPENDING PER AGE GROUPS

# computing total population
totals = wdp %>%
  
  # country-year headcounts per spending group
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T),
             spend = sum (exp.pdf.m, na.rm = T)) %>%
  
  # age groups
  mutate (age_group = ifelse (age_group %in% c("[30,35)"), "30_35",
                              ifelse (age_group %in% c("[35,40)"), "35_40",
                                      ifelse (age_group %in% c("[40,45)","[45,50)"), "40_50",
                                              ifelse (age_group %in% c("[50,55)","[55,60)"), "50_60",
                                                      ifelse (age_group %in% c("[65,70)","[70,75)","[75,INF)"), "65_", NA)))))) %>%
  
  # filter out NA age groups
  filter (!is.na (age_group)) %>%
  
  # second aggregation into new age groups
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (pop, na.rm = T),
             spend = sum (spend, na.rm = T)) %>%
  
  # filtering out non consumer spending 
  mutate (daily_spending = ifelse (daily_spending == "[0,12)", "No_Cons", "Cons")) %>%
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (pop, na.rm = T),
             spend = sum (spend, na.rm = T)) %>%
  filter (!daily_spending == "No_Cons") %>%

  # filtering
  filter (ccode %in% c("BRA", "CHN", "FRA", "DEU", "IND", "MEX", "GBR", "USA")) %>%
  filter (year %in% 2022) %>%
  
  # third aggregation
  group_by (year, age_group) %>%
  summarise (pop = sum (pop, na.rm = T),
             spend = sum (spend, na.rm = T)) %>%
  
  # computing per capita
  mutate (spend_pc = spend / pop) %>%
  dplyr::select (year, age_group, spend_pc)
  








