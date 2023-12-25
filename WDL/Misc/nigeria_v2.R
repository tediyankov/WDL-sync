

## NIGERIA VALIDATION EXERCISE

## Chapter 1: preliminaries ----------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl)
library (Hmisc)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
if (is.na (base_path)) {base_path = file.path("/Users",
                                              "teddyyankov",
                                              "Library",
                                              "CloudStorage",
                                              "GoogleDrive-teodor.yankov@worlddata.io",
                                              "Shared drives", 
                                              "DATA_WDL")}

wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')
oldwdp_path <- file.path (wdp_path,'01_Data','R_2022_12_29','2023_03_13_2017ppp','03_outputs')

## loading in WDPro versions raw

# current
wdp_raw_new <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))

# old
wdp_raw_old <- readRDS (file.path (base_path, 
                                   "Mpro_2.0",
                                   "01_Data",
                                   "R_2022_12_29",
                                   "2023_03_13_2017ppp",
                                   "03_outputs", 
                                   "05_003_merge_standard_ages_R_2022_12_29_2017ppp_1_USD.rds"))

## Chapter 2: cleaning data ----------------------------------------------------

wdp_current = wdp_raw_new %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 2, 5, 8, 12, Inf))
wdp_old = wdp_raw_old %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 2, 5, 8, 12, Inf))

wdp_current_clean = wdp_current %>%
  
  # first aggregation 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # recoding spendng groups into CC and not
  dplyr::mutate (daily_spending = ifelse (daily_spending == "[12,Inf)", "CC", daily_spending)) %>%
  
  # aggregate 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>%
  
  # keeping only consumer class
  dplyr::filter (!daily_spending == "CC") %>%
  #dplyr::select (-daily_spending) %>%
  
  # filter for Nigeria
  filter (ccode == "NGA")

wdp_old_clean = wdp_old %>%
  
  # first aggregation 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # recoding spendng groups into CC and not
  dplyr::mutate (daily_spending = ifelse (daily_spending == "[12,Inf)", "CC", daily_spending)) %>%
  
  # aggregate 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>%
  
  # keeping only consumer class
  dplyr::filter (!daily_spending == "CC") %>%
  #dplyr::select (-daily_spending) %>%
  
  # filter for Nigeria
  filter (ccode == "NGA")

wdp_nga_comparison = wdp_current_clean %>%
  
  # executing merge
  left_join (wdp_old_clean, by = c("ccode", "year", "daily_spending"), suffix = c("_current", "_old")) %>%
  
  # computing difference variables
  dplyr::mutate (hc_diff = hc_current - hc_old, 
                 exp_diff = exp_current - exp_old)

# creating differences plot (hc)
ggplot (data = wdp_nga_comparison %>% 
          filter (year %in% 2023:2024) %>%
          mutate (hc_diff = hc_diff / 10^6) %>%
          mutate (exp_diff = hc_diff / 10^9) %>%
          mutate (label = paste0 (round (hc_diff, 3), " ", "M")),
        aes (x = factor(year), y = hc_diff, fill = daily_spending)) + 
  geom_bar (stat = "identity") + 
  #geom_label (aes (label = label,
                   #size = 16),
             # color = "white",
              #position = position_stack (vjust = 0.5),
              #show.legend = F) +
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl2() +
  facet_wrap (~daily_spending, scales = "free") + 
  labs (y = "Difference in CClass Headcounts (Millions)", 
        x = "Year", 
        title = "Nigeria: comparing headcounts across previous and \ncurrent WDPro versions", 
        subtitle = "Difference per year (in Millions) between the two WDPro versions") + 
  theme (#axis.text = element_blank(),
         axis.ticks = element_blank(),
         #axis.title = element_blank(),
         #axis.text.x = element_text (angle = 360),
         legend.position = "none",
         plot.background = element_blank(),
         )

nigeria_comparison_df = wdp_nga_comparison %>%
  
  # filering years
  filter (year %in% c(2019:2024, 2030)) %>%
  
  # renaming
  rename_with (
    ~ case_when (
      . == "hc_current" ~ "headcounts_WDP_2023_04_11",
      . == "exp_current" ~ "spending_WDP_2023_04_11",
      . == "hc_old" ~ "headcounts_WDP_2022_12_29",
      . == "exp_old" ~ "spending_WDP_2022_12_29",
      . == "hc_diff" ~ "heaadcounts_change",
      . == "exp_diff" ~ "spendng_change",
      TRUE ~ .
    )
  )

write.csv (nigeria_comparison_df, "nigeria_comparison_df.csv", row.names = F)

## creating the same but for consumer class

wdp_current_clean = wdp_current %>%
  
  # first aggregation 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # recoding spendng groups into CC and not
  dplyr::mutate (daily_spending = ifelse (daily_spending == "[12,Inf)", "Consumer Class", daily_spending)) %>%
  
  # aggregate 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>%
  
  # keeping only consumer class
  dplyr::filter (daily_spending == "Consumer Class") %>%
  #dplyr::select (-daily_spending) %>%
  
  # filter for Nigeria
  filter (ccode == "NGA")

wdp_old_clean = wdp_old %>%
  
  # first aggregation 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # recoding spendng groups into CC and not
  dplyr::mutate (daily_spending = ifelse (daily_spending == "[12,Inf)", "Consumer Class", daily_spending)) %>%
  
  # aggregate 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>%
  
  # keeping only consumer class
  dplyr::filter (daily_spending == "Consumer Class") %>%
  #dplyr::select (-daily_spending) %>%
  
  # filter for Nigeria
  filter (ccode == "NGA")

wdp_nga_comparison = wdp_current_clean %>%
  
  # executing merge
  left_join (wdp_old_clean, by = c("ccode", "year", "daily_spending"), suffix = c("_current", "_old")) %>%
  
  # computing difference variables
  dplyr::mutate (hc_diff = hc_current - hc_old, 
                 exp_diff = exp_current - exp_old)i

nigeria_comparison_df = wdp_nga_comparison %>%
  
  # filering years
  filter (year %in% c(2019:2024, 2030)) %>%
  
  # renaming
  rename_with (
    ~ case_when (
      . == "hc_current" ~ "headcounts_WDP_2023_04_11",
      . == "exp_current" ~ "spending_WDP_2023_04_11",
      . == "hc_old" ~ "headcounts_WDP_2022_12_29",
      . == "exp_old" ~ "spending_WDP_2022_12_29",
      . == "hc_diff" ~ "heaadcounts_change",
      . == "exp_diff" ~ "spendng_change",
      TRUE ~ .
    )
  )

write.csv (nigeria_comparison_df, "nigeria_comparison_df_consumerclass.csv", row.names = F)


## Chapter 3: comparison plots -------------------------------------------------

## raw data
wdp_current = wdp_raw_new %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 2.15, 12, Inf))
wdp_old = wdp_raw_old %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 2.15, 12, Inf))

# current WDP df
wdp_current_nga = wdp_current %>% 
  
  # first aggregation
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # keeping only relevant soending groups
  filter (!daily_spending == "[2.15,12)") %>%
  
  # keepng only Nigeria
  filter (ccode == "NGA") %>%
  
  # keepng only 2019-2030
  filter (year %in% 2019:2030)

# old WDP df
wdp_old_nga = wdp_old %>% 
  
  # first aggregation
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # keeping only relevant soending groups
  filter (!daily_spending == "[2.15,12)") %>%
  
  # keepng only Nigeria
  filter (ccode == "NGA") %>%
  
  # keepng only 2019-2030
  filter (year %in% 2019:2030)

# creating joint df 
wdp_nga = wdp_current_nga %>% 
  
  # execute join
  left_join (wdp_old_nga, by = c("ccode", "year", "daily_spending"), suffix = c("_current", "_old")) %>%
  
  # renaming
  rename_with (
    ~ case_when (
      . == "hc_current" ~ "Headcounts (April 2023)",
      . == "exp_current" ~ "Spending (April 2023)",
      . == "hc_old" ~ "Headcounts (March 2023)",
      . == "exp_old" ~ "Spending (March 2023)",
      TRUE ~ .
    )
  ) %>%
  
  # elongating for line chart
  pivot_longer (4:7, names_to = "Version:", values_to = "value")
  
## building plot
ggplot (data = wdp_nga %>% 
          
          # keeping only headcounts values
          filter (`Version:` %in% c("Headcounts (April 2023)", "Headcounts (March 2023)")) %>%
          
          # keeping poor and cc
          filter (daily_spending %in% c("[0,2.15)", "[12,Inf)")) %>%
          
          # changing income category names 
          mutate (daily_spending = ifelse (daily_spending == "[0,2.15)", "Extreme Poverty (<$2.15/day)",
                                           ifelse (daily_spending == "[12,Inf)", "Consumer Class (>$12/day)", daily_spending
                                           ))) %>%
          
          # converting to millions
          dplyr::mutate (value = value / 10^6)
          , 
        aes (x = year, y = value, group = `Version:`, linetype = `Version:`)) + 
  
  # creating lines
  geom_line (linewidth = 1.5) + 
  
  # theme settings
  worlddataverse::theme_wdl() + 
  theme (legend.position = "bottom") + 
  
  # title 
  labs (title = "Comparing Nigeria between current (April, 2023) and \nold (March, 2023) versions of WDPro",
        subtitle = "Headcounts for Extreme Poverty and Consumer Class",
        x = "\nYear", 
        y = "Headcounts (Millions)") + 
  
  # facetting
  facet_wrap (~daily_spending, scales = "free")

ggplot (data = wdp_nga %>% 
          
          # keeping only headcounts values
          filter (group %in% c("spending_WDP_2023_04_11", "spending_WDP_2022_12_29")) %>%
          
          # converting to millions
          dplyr::mutate (value = value / 10^9), 
        aes (x = year, y = value, group = group, linetype = group)) + 
  
  # creating lines
  geom_line (linewidth = 1.5) + 
  
  # theme settings
  worlddataverse::theme_wdl() + 
  theme (legend.position = "bottom") + 
  
  # title 
  labs (title = "Comparing Nigeria between \nWDP_2023_04_11 and WDP_2022_12_29",
        subtitle = "Spending for 0-2.15 and Consumer Class",
        x = "\nYear", 
        y = "Spending (Billions)") + 
  
  # facetting
  facet_wrap (~daily_spending, scales = "free", nrow = 1, ncol = 2)

ggplot (data = wdp_nga %>% 
          
          # keeping only headcounts values
          filter (`Version:` %in% c("Spending (April 2023)", "Spending (March 2023)")) %>%
          
          # keeping poor and cc
          filter (daily_spending %in% c("[0,2.15)", "[12,Inf)")) %>%
          
          # changing income category names 
          mutate (daily_spending = ifelse (daily_spending == "[0,2.15)", "Extreme Poverty (<$2.15/day)",
                                           ifelse (daily_spending == "[12,Inf)", "Consumer Class (>$12/day)", daily_spending
                                           ))) %>%
          
          # converting to millions
          dplyr::mutate (value = value / 10^9)
        , 
        aes (x = year, y = value, group = `Version:`, linetype = `Version:`)) + 
  
  # creating lines
  geom_line (linewidth = 1.5) + 
  
  # theme settings
  worlddataverse::theme_wdl() + 
  theme (legend.position = "bottom") + 
  
  # title 
  labs (title = "Comparing Nigeria between current (April, 2023) and \nold (March, 2023) versions of WDPro",
        subtitle = "Spending for Extreme Poverty and Consumer Class",
        x = "\nYear", 
        y = "Spending (Billions)") + 
  
  # facetting
  facet_wrap (~daily_spending, scales = "free")




  







