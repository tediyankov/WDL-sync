
## Mauritius PPT code for Teddy's slides =======================================

## Chapter 1: prelims ----------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, data.table, ggthemes)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')

## loading in WDPro latest version

# loading raw file
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))

# first cleaning of WDP dataset
wdp <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 5, 12, 20, Inf))

## Chapter 2: 4 categories 2023 vs 2030 (all of Africa) ------------------------

## building chart
slide_28_totalafrica = wdp %>%
  
  # re-coding the spending categories
  mutate (daily_spending = ifelse (daily_spending == "[0,5)", "Poor", 
                                   ifelse (daily_spending == "[5,12)", "Future \nConsumers", 
                                           ifelse (daily_spending == "[12,20)", "Low-income \nConsumers", 
                                                   ifelse (daily_spending == "[20,Inf)", "Affluent \nConsumers", NA 
                                                   ))))) %>%
  
  # aggregating into year-country-spendinggroup HC and EXP
  dplyr::group_by (year, ccode, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T), 
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # adding continent variable
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  
  # aggregating into continent
  dplyr::group_by (year, continent, daily_spending) %>%
  dplyr::summarise (headcounts = sum (hc, na.rm = T), 
                    expenditure = sum (exp, na.rm = T)) %>%
  
  # filtering for Africa and 2023 & 2030
  filter (continent == 'Africa' & year %in% c(2023, 2030)) %>%
  
  # rescaling HC in mil and EXP into tril
  mutate (headcounts = headcounts / 10^6, 
          expenditure = expenditure / 10^12) %>%
  
  # elongating 
  pivot_longer (4:5, names_to = 'cc_type', values_to = 'value')

ggplot (data = slide_28_totalafrica %>% 
          filter (cc_type == "headcounts") %>%
          mutate (label = paste0 (round (value, 0), " ", "M")), 
        aes (x = factor (daily_spending),
             y = value, 
             fill = factor (year))) + 
  geom_bar (stat = "identity", position = "dodge") +
  geom_text (aes (label = label), position = position_dodge (width = .9), vjust = -0.5) + 
  worlddataverse::theme_wdl() +
  theme (axis.text.x = element_text (angle = 360),
         legend.position = "bottom") + 
  labs (title = 'Africa: 2023 vs 2030 headcounts in key relevant segments',
        y = "Headcounts, in Millions\n", 
        x = "\nSpending group")

ggplot (data = slide_28_totalafrica %>% 
          filter (cc_type == "expenditure") %>%
          mutate (label = paste0 (round (value, 1), " ", "T")), 
        aes (x = factor (daily_spending),
             y = value, 
             fill = factor (year))) + 
  geom_bar (stat = "identity", position = "dodge") +
  geom_text (aes (label = label), position = position_dodge (width = .9), vjust = -0.5) + 
  worlddataverse::theme_wdl() +
  theme (axis.text.x = element_text (angle = 360),
         legend.position = "bottom") + 
  labs (title = 'Africa: 2023 vs 2030 spending in key relevant segments',
        y = "Spending, in Trillions USD 2017 PPP\n", 
        x = "\nSpending group")

## Chapter 3: 4 categories 2023 vs 2030 (top 3 in Africa) ----------------------

## checking which are the top 3 by low income cons headcounts in 2023
top3africa = wdp %>%
  
  # re-coding the spending categories
  mutate (daily_spending = ifelse (daily_spending == "[0,5)", "Poor", 
                                   ifelse (daily_spending == "[5,12)", "Future \nConsumers", 
                                           ifelse (daily_spending == "[12,20)", "Low-income \nConsumers", 
                                                   ifelse (daily_spending == "[20,Inf)", "Affluent \nConsumers", NA 
                                                   ))))) %>%
  
  # aggregating into year-country-spendinggroup HC and EXP
  dplyr::group_by (year, ccode, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T), 
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # adding continent variable
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  
  # filtering for Africa and 2023 and low income consumers
  filter (continent == 'Africa' & year == 2023 & daily_spending == "Low-income \nConsumers") %>%
  
  # arrange in desc order by hc 
  arrange (desc (hc))

inputdat = wdp %>%
  
  # re-coding the spending categories
  mutate (daily_spending = ifelse (daily_spending == "[0,5)", "Poor", 
                                   ifelse (daily_spending == "[5,12)", "Future \nConsumers", 
                                           ifelse (daily_spending == "[12,20)", "Low-income \nConsumers", 
                                                   ifelse (daily_spending == "[20,Inf)", "Affluent \nConsumers", NA 
                                                   ))))) %>%
  
  # aggregating into year-country-spendinggroup HC and EXP
  dplyr::group_by (year, ccode, daily_spending) %>%
  dplyr::summarise (headcounts = sum (hc.pdf.m, na.rm = T), 
                    expenditure = sum (exp.pdf.m, na.rm = T)) %>%
  
  # adding continent variable
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  
  # filtering for Africa and 2023 and top 3
  filter (continent == 'Africa' & year %in% c(2023, 2030) & ccode %in% c("NGA", "DZA", "EGY")) %>%
  
  # rescaling HC in mil and EXP into tril
  mutate (headcounts = headcounts / 10^6, 
          expenditure = expenditure / 10^9) %>%
  
  # elongating 
  pivot_longer (4:5, names_to = 'type', values_to = 'value')

## creating plot
ggplot (data = inputdat %>% 
          filter (type == "headcounts") %>%
          mutate (label = paste0 (round (value, 0), " ", "M")), 
        aes (x = factor (daily_spending),
             y = value, 
             fill = factor (year))) + 
  geom_bar (stat = "identity", position = "dodge") +
  geom_text (aes (label = label), position = position_dodge (width = .9), vjust = -0.5) + 
  worlddataverse::theme_wdl() +
  theme (axis.text.x = element_text (angle = 360),
         legend.position = "bottom") + 
  facet_wrap (~ccode, scales = "free", nrow = 1, ncol = 3) + 
  labs (title = 'Top 3 African markets by low-income consumer headcounts: \n2023 vs 2030 headcounts in key relevant segments',
        y = "Headcounts, in Millions\n", 
        x = "\nSpending group")

ggplot (data = inputdat %>% 
          filter (type == "expenditure") %>%
          mutate (label = paste0 (round (value, 0), " ", "B")), 
        aes (x = factor (daily_spending),
             y = value, 
             fill = factor (year))) + 
  geom_bar (stat = "identity", position = "dodge") +
  geom_text (aes (label = label), position = position_dodge (width = .9), vjust = -0.5) + 
  worlddataverse::theme_wdl() +
  theme (axis.text.x = element_text (angle = 360),
         legend.position = "bottom") + 
  facet_wrap (~ccode, scales = "free", nrow = 1, ncol = 3) + 
  labs (title = 'Top 3 African markets by low-income consumer spending: \n2023 vs 2030 spending in key relevant segments',
        y = "Spending, in Billions USD 2017 PPP\n", 
        x = "\nSpending group")
  
  
  
  
  
  
  
  



