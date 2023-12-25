
## Africa city shares V2 =======================================================

## Chapter 1: preliminaries ----------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, data.table)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2022_10_11','2022_10_12_second_2017ppp','03_outputs')

## loading data
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_10_11_2017ppp_1_USD.rds"))



## Chapter 2: cleaning data ----------------------------------------------------

## computing desired spending groups
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 2, 12, 120, Inf))

## cleaning WDP data 

# WDP headcounts data with countries
wdp_clean_countries_hc = wdp %>%
  
  # first grouping: ccode, year, spending group --> headcounts and spending
  group_by (ccode, year, daily_spending) %>%
  summarise (hc = sum (hc.pdf.m, na.rm = T)) %>%
  
  # filter for required years
  filter (year %in% 2021:2023) %>%
  
  # renaming spending groups
  mutate (daily_spending = ifelse (daily_spending == "[0,2)", "poor", 
                                   ifelse (daily_spending == "[2,12)", "vulnerable", 
                                           ifelse (daily_spending == "[12,120)", "mc",
                                                   ifelse (daily_spending == "[120,Inf)", "rich", NA))))) %>%
  
  # ordering by country, year and spending group
  arrange (ccode, year, factor (daily_spending, levels = c("poor", "vulnerable", "mc", "rich"))) %>%
  
  # widening data by spending group
  pivot_wider (names_from = daily_spending, values_from = hc) %>%
  
  # widening data by year
  pivot_wider (names_from = year, values_from = 3:6) %>%
  
  # computing change variables 
  mutate (poor_hc_delta_21_22 = ((poor_2022 - poor_2021) / poor_2021) * 100,
          vulnerable_hc_delta_21_22 = ((vulnerable_2022 - vulnerable_2021) / vulnerable_2021) * 100,
          mc_hc_delta_21_22 = ((mc_2022 - mc_2021) / mc_2021) * 100,
          rich_hc_delta_21_22 = ((rich_2022 - rich_2021) / rich_2021) * 100,
          poor_hc_delta_22_23 = ((poor_2023 - poor_2022) / poor_2022) * 100,
          vulnerable_hc_delta_22_23 = ((vulnerable_2023 - vulnerable_2022) / vulnerable_2022) * 100,
          mc_hc_delta_22_23 = ((mc_2023 - mc_2022) / mc_2022) * 100,
          rich_hc_delta_22_23 = ((rich_2023 - rich_2022) / rich_2022) * 100) %>%
  
  # adding continent variable
  mutate (continent = countrycode (ccode, "iso3c", "continent"))

## graphing the deltas
hc_countries_delta_plot_21_22_Asia = ggplot (data = wdp_clean_countries_hc %>%
                                               filter (continent == "Asia") %>%
                                               dplyr::select (ccode, poor_hc_delta_21_22, vulnerable_hc_delta_21_22, mc_hc_delta_21_22,
                                                              rich_hc_delta_21_22) %>%
                                               pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                             aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in headcounts per spending group", 
        subtitle = "Asia, From 2021 until 2022", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
hc_countries_delta_plot_21_22_Asia

hc_countries_delta_plot_22_23_Asia = ggplot (data = wdp_clean_countries_hc %>%
                                               filter (continent == "Asia") %>%
                                               dplyr::select (ccode, poor_hc_delta_22_23, vulnerable_hc_delta_22_23,
                                                              mc_hc_delta_22_23, rich_hc_delta_22_23) %>%
                                               pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                             aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in headcounts per spending group", 
        subtitle = "Asia, From 2022 until 2023", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
hc_countries_delta_plot_22_23_Asia

hc_countries_delta_plot_21_22_Africa = ggplot (data = wdp_clean_countries_hc %>%
                                               filter (continent == "Africa") %>%
                                               dplyr::select (ccode, poor_hc_delta_21_22, vulnerable_hc_delta_21_22, mc_hc_delta_21_22,
                                                              rich_hc_delta_21_22) %>%
                                               pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                             aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
 # theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in headcounts per spending group", 
        subtitle = "Africa, From 2021 until 2022", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
hc_countries_delta_plot_21_22_Africa

hc_countries_delta_plot_22_23_Africa = ggplot (data = wdp_clean_countries_hc %>%
                                               filter (continent == "Africa") %>%
                                               dplyr::select (ccode, poor_hc_delta_22_23, vulnerable_hc_delta_22_23,
                                                              mc_hc_delta_22_23, rich_hc_delta_22_23) %>%
                                               pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                             aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in headcounts per spending group", 
        subtitle = "Africa, From 2022 until 2023", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
hc_countries_delta_plot_22_23_Africa

hc_countries_delta_plot_21_22_Europe = ggplot (data = wdp_clean_countries_hc %>%
                                                 filter (continent == "Europe") %>%
                                                 dplyr::select (ccode, poor_hc_delta_21_22, vulnerable_hc_delta_21_22, mc_hc_delta_21_22,
                                                                rich_hc_delta_21_22) %>%
                                                 pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                               aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (size = 6, angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in headcounts per spending group", 
        subtitle = "Europe, From 2021 until 2022", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
hc_countries_delta_plot_21_22_Europe

hc_countries_delta_plot_22_23_Europe = ggplot (data = wdp_clean_countries_hc %>%
                                                 filter (continent == "Europe") %>%
                                                 dplyr::select (ccode, poor_hc_delta_22_23, vulnerable_hc_delta_22_23,
                                                                mc_hc_delta_22_23, rich_hc_delta_22_23) %>%
                                                 pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                               aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in headcounts per spending group", 
        subtitle = "Europe, From 2022 until 2023", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
hc_countries_delta_plot_22_23_Europe

hc_countries_delta_plot_21_22_Americas = ggplot (data = wdp_clean_countries_hc %>%
                                                 filter (continent == "Americas") %>%
                                                 dplyr::select (ccode, poor_hc_delta_21_22, vulnerable_hc_delta_21_22, mc_hc_delta_21_22,
                                                                rich_hc_delta_21_22) %>%
                                                 pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                               aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (size = 6, angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in headcounts per spending group", 
        subtitle = "Americas, From 2021 until 2022", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
hc_countries_delta_plot_21_22_Americas

hc_countries_delta_plot_22_23_Americas = ggplot (data = wdp_clean_countries_hc %>%
                                                 filter (continent == "Americas") %>%
                                                 dplyr::select (ccode, poor_hc_delta_22_23, vulnerable_hc_delta_22_23,
                                                                mc_hc_delta_22_23, rich_hc_delta_22_23) %>%
                                                 pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                               aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in headcounts per spending group", 
        subtitle = "Americas, From 2022 until 2023", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
hc_countries_delta_plot_22_23_Americas
  
hc_countries_delta_plot_21_22_Oceania = ggplot (data = wdp_clean_countries_hc %>%
                                                   filter (continent == "Oceania") %>%
                                                   dplyr::select (ccode, poor_hc_delta_21_22, vulnerable_hc_delta_21_22, mc_hc_delta_21_22,
                                                                  rich_hc_delta_21_22) %>%
                                                   pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                                 aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (size = 6, angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in headcounts per spending group", 
        subtitle = "Oceania, From 2021 until 2022", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
hc_countries_delta_plot_21_22_Oceania

hc_countries_delta_plot_22_23_Oceania = ggplot (data = wdp_clean_countries_hc %>%
                                                   filter (continent == "Oceania") %>%
                                                   dplyr::select (ccode, poor_hc_delta_22_23, vulnerable_hc_delta_22_23,
                                                                  mc_hc_delta_22_23, rich_hc_delta_22_23) %>%
                                                   pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                                 aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in headcounts per spending group", 
        subtitle = "Oceania, From 2022 until 2023", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
hc_countries_delta_plot_22_23_Oceania
  
## WDP expenditure data with countries

wdp_clean_countries_exp = wdp %>%
  
  # first grouping: ccode, year, spending group --> headcounts and spending
  group_by (ccode, year, daily_spending) %>%
  summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # filter for required years
  filter (year %in% 2021:2023) %>%
  
  # renaming spending groups
  mutate (daily_spending = ifelse (daily_spending == "[0,2)", "poor", 
                                   ifelse (daily_spending == "[2,12)", "vulnerable", 
                                           ifelse (daily_spending == "[12,120)", "mc",
                                                   ifelse (daily_spending == "[120,Inf)", "rich", NA))))) %>%
  
  # ordering by country, year and spending group
  arrange (ccode, year, factor (daily_spending, levels = c("poor", "vulnerable", "mc", "rich"))) %>%
  
  # widening data by spending group
  pivot_wider (names_from = daily_spending, values_from = exp) %>%
  
  # widening data by year
  pivot_wider (names_from = year, values_from = 3:6) %>%
  
  # computing change variables 
  mutate (poor_exp_delta_21_22 = ((poor_2022 - poor_2021) / poor_2021) * 100,
          vulnerable_exp_delta_21_22 = ((vulnerable_2022 - vulnerable_2021) / vulnerable_2021) * 100,
          mc_exp_delta_21_22 = ((mc_2022 - mc_2021) / mc_2021) * 100,
          rich_exp_delta_21_22 = ((rich_2022 - rich_2021) / rich_2021) * 100,
          poor_exp_delta_22_23 = ((poor_2023 - poor_2022) / poor_2022) * 100,
          vulnerable_exp_delta_22_23 = ((vulnerable_2023 - vulnerable_2022) / vulnerable_2022) * 100,
          mc_exp_delta_22_23 = ((mc_2023 - mc_2022) / mc_2022) * 100,
          rich_exp_delta_22_23 = ((rich_2023 - rich_2022) / rich_2022) * 100) %>%
  
  # adding continent variable
  mutate (continent = countrycode (ccode, "iso3c", "continent"))

## graphing the deltas
exp_countries_delta_plot_21_22_Asia = ggplot (data = wdp_clean_countries_exp %>%
                                               filter (continent == "Asia") %>%
                                               dplyr::select (ccode, poor_exp_delta_21_22, vulnerable_exp_delta_21_22, mc_exp_delta_21_22,
                                                              rich_exp_delta_21_22) %>%
                                               pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                             aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in expenditure per spending group", 
        subtitle = "Asia, From 2021 until 2022", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
exp_countries_delta_plot_21_22_Asia

exp_countries_delta_plot_22_23_Asia = ggplot (data = wdp_clean_countries_exp %>%
                                                filter (continent == "Asia") %>%
                                                dplyr::select (ccode, poor_exp_delta_22_23, vulnerable_exp_delta_22_23, mc_exp_delta_22_23,
                                                               rich_exp_delta_22_23) %>%
                                                pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                              aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in expenditure per spending group", 
        subtitle = "Asia, From 2022 until 2023", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
exp_countries_delta_plot_22_23_Asia

exp_countries_delta_plot_21_22_Africa = ggplot (data = wdp_clean_countries_exp %>%
                                                filter (continent == "Africa") %>%
                                                dplyr::select (ccode, poor_exp_delta_21_22, vulnerable_exp_delta_21_22, mc_exp_delta_21_22,
                                                               rich_exp_delta_21_22) %>%
                                                pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                              aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in expenditure per spending group", 
        subtitle = "Africa, From 2021 until 2022", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
exp_countries_delta_plot_21_22_Africa

exp_countries_delta_plot_22_23_Africa = ggplot (data = wdp_clean_countries_exp %>%
                                                filter (continent == "Africa") %>%
                                                dplyr::select (ccode, poor_exp_delta_22_23, vulnerable_exp_delta_22_23, mc_exp_delta_22_23,
                                                               rich_exp_delta_22_23) %>%
                                                pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                              aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in expenditure per spending group", 
        subtitle = "Africa, From 2022 until 2023", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
exp_countries_delta_plot_22_23_Africa

exp_countries_delta_plot_21_22_Europe = ggplot (data = wdp_clean_countries_exp %>%
                                                  filter (continent == "Europe") %>%
                                                  dplyr::select (ccode, poor_exp_delta_21_22, vulnerable_exp_delta_21_22, mc_exp_delta_21_22,
                                                                 rich_exp_delta_21_22) %>%
                                                  pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                                aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in expenditure per spending group", 
        subtitle = "Europe, From 2021 until 2022", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
exp_countries_delta_plot_21_22_Europe

exp_countries_delta_plot_22_23_Europe = ggplot (data = wdp_clean_countries_exp %>%
                                                  filter (continent == "Europe") %>%
                                                  dplyr::select (ccode, poor_exp_delta_22_23, vulnerable_exp_delta_22_23, mc_exp_delta_22_23,
                                                                 rich_exp_delta_22_23) %>%
                                                  pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                                aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in expenditure per spending group", 
        subtitle = "Europe, From 2022 until 2023", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
exp_countries_delta_plot_22_23_Europe

exp_countries_delta_plot_21_22_Americas = ggplot (data = wdp_clean_countries_exp %>%
                                                  filter (continent == "Americas") %>%
                                                  dplyr::select (ccode, poor_exp_delta_21_22, vulnerable_exp_delta_21_22, mc_exp_delta_21_22,
                                                                 rich_exp_delta_21_22) %>%
                                                  pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                                aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in expenditure per spending group", 
        subtitle = "Americas, From 2021 until 2022", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
exp_countries_delta_plot_21_22_Americas

exp_countries_delta_plot_22_23_Americas = ggplot (data = wdp_clean_countries_exp %>%
                                                  filter (continent == "Americas") %>%
                                                  dplyr::select (ccode, poor_exp_delta_22_23, vulnerable_exp_delta_22_23, mc_exp_delta_22_23,
                                                                 rich_exp_delta_22_23) %>%
                                                  pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                                aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in expenditure per spending group", 
        subtitle = "Americas, From 2022 until 2023", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
exp_countries_delta_plot_22_23_Americas
  
exp_countries_delta_plot_21_22_Oceania = ggplot (data = wdp_clean_countries_exp %>%
                                                    filter (continent == "Oceania") %>%
                                                    dplyr::select (ccode, poor_exp_delta_21_22, vulnerable_exp_delta_21_22, mc_exp_delta_21_22,
                                                                   rich_exp_delta_21_22) %>%
                                                    pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                                  aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in expenditure per spending group", 
        subtitle = "Oceania, From 2021 until 2022", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
exp_countries_delta_plot_21_22_Oceania

exp_countries_delta_plot_22_23_Oceania = ggplot (data = wdp_clean_countries_exp %>%
                                                    filter (continent == "Oceania") %>%
                                                    dplyr::select (ccode, poor_exp_delta_22_23, vulnerable_exp_delta_22_23, mc_exp_delta_22_23,
                                                                   rich_exp_delta_22_23) %>%
                                                    pivot_longer (2:5, names_to = "category", values_to = "delta"),
                                                  aes (x = ccode, y = delta)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  #theme (axis.text.x = element_text (angle = 45)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in expenditure per spending group", 
        subtitle = "Oceania, From 2022 until 2023", 
        x = "Country", 
        y = "Percentage Change") + 
  facet_wrap (~category, scale = "free")
exp_countries_delta_plot_22_23_Oceania


## WDP headcount data total 

wdp_clean_total_hc = wdp %>%
  
  # first grouping: ccode, year, spending group --> headcounts and spending
  group_by (year, daily_spending) %>%
  summarise (hc = sum (hc.pdf.m, na.rm = T)) %>%
  
  # filter for required years
  filter (year %in% 2021:2023) %>%
  
  # renaming spending groups
  mutate (daily_spending = ifelse (daily_spending == "[0,2)", "poor", 
                                   ifelse (daily_spending == "[2,12)", "vulnerable", 
                                           ifelse (daily_spending == "[12,120)", "mc",
                                                   ifelse (daily_spending == "[120,Inf)", "rich", NA))))) %>%
  
  # ordering by country, year and spending group
  arrange (year, factor (daily_spending, levels = c("poor", "vulnerable", "mc", "rich"))) %>%
  
  # widening data by spending group
  pivot_wider (names_from = daily_spending, values_from = hc) 

# flipping rows and columns
wdp_clean_total_hc_2 <- data.frame (t (wdp_clean_total_hc[-1]))
colnames (wdp_clean_total_hc_2) <- c(2021,2022,2023)

# adding delta vars
wdp_clean_total_hc_2$change_21_22 <- wdp_clean_total_hc_2$`2022` - wdp_clean_total_hc_2$`2021`
wdp_clean_total_hc_2$change_22_23 <- wdp_clean_total_hc_2$`2023` - wdp_clean_total_hc_2$`2022`
wdp_clean_total_hc_2$percent_change_21_22 <- (wdp_clean_total_hc_2$change_21_22 / wdp_clean_total_hc_2$`2021`) * 100
wdp_clean_total_hc_2$percent_change_22_23 <- (wdp_clean_total_hc_2$change_22_23 / wdp_clean_total_hc_2$`2022`) * 100


## WDP expenditure data total

wdp_clean_total_exp = wdp %>%
  
  # first grouping: ccode, year, spending group --> headcounts and spending
  group_by (year, daily_spending) %>%
  summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # filter for required years
  filter (year %in% 2021:2023) %>%
  
  # renaming spending groups
  mutate (daily_spending = ifelse (daily_spending == "[0,2)", "poor", 
                                   ifelse (daily_spending == "[2,12)", "vulnerable", 
                                           ifelse (daily_spending == "[12,120)", "mc",
                                                   ifelse (daily_spending == "[120,Inf)", "rich", NA))))) %>%
  
  # ordering by country, year and spending group
  arrange (year, factor (daily_spending, levels = c("poor", "vulnerable", "mc", "rich"))) %>%
  
  # widening data by spending group
  pivot_wider (names_from = daily_spending, values_from = exp) 

# flipping rows and columns
wdp_clean_total_exp_2 <- data.frame (t (wdp_clean_total_exp[-1]))
colnames (wdp_clean_total_exp_2) <- c(2021,2022,2023)

# adding delta vars
wdp_clean_total_exp_2$change_21_22 <- wdp_clean_total_exp_2$`2022` - wdp_clean_total_exp_2$`2021`
wdp_clean_total_exp_2$change_22_23 <- wdp_clean_total_exp_2$`2023` - wdp_clean_total_exp_2$`2022`
wdp_clean_total_exp_2$percent_change_21_22 <- (wdp_clean_total_exp_2$change_21_22 / wdp_clean_total_exp_2$`2021`) * 100
wdp_clean_total_exp_2$percent_change_22_23 <- (wdp_clean_total_exp_2$change_22_23 / wdp_clean_total_exp_2$`2022`) * 100


  
  







