
## IPI2.0 vs IPI1.0 =======================================================

## Chapter 1: preliminaries ----------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, data.table)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
ipi1_path <- file.path (base_path, "IPI", "phase_1", "Output")
ipi2_path <- file.path (base_path, "IPI", "phase_2", "Data")
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2022_10_11','2022_10_12_second_2017ppp','03_outputs')

## loading data
ipi_1 <- read.csv (file.path (ipi1_path,"internet_index_2022_01.csv"))
ipi_2 <- read.csv (file.path (ipi2_path, "output", "internet_poverty_2023.csv"))



## Chapter 2: data wrangling ----------------------------------------------------

## cleaning IPI 1.0
ipi_1_clean = ipi_1 %>%
  
  # filtering 1.0 for both sexes 
  filter (gender == "both") %>%
  
  # filtering 1.0 to only contain countries in 2.0 
  filter (ccode %in% ipi_2$ccode) %>%
  
  # keeping only transferrable variables 
  dplyr::select (-gender) %>%
  
  # renaming internet poor variable
  rename_with (
    ~ case_when (
      . == "population" ~ "population_2022",
      . == "internet_poor_share" ~ "internet_poor_share_2022",
      . == "internet_poor" ~ "internet_poor_2022",
      TRUE ~ .
    )
  ) %>%
  
  # removing year
  dplyr::select (ccode, country, continent, population_2022, internet_poor_share_2022, internet_poor_2022)

## cleaning IPI 2.0
ipi_2_clean = ipi_2 %>%
  
  # renaming vars to match 1.0 
  rename_with (
    ~ case_when (
      . == "country_name" ~ "country",
      . == "share_internet_poor" ~ "internet_poor_share_2023",
      . == "internet_poor" ~ "internet_poor_2023",
      TRUE ~ .
    )
  ) %>%
  
  # computing total population variable
  mutate (population_2023 = internet_poor_2023 + not_internet_poor) %>%
  
  # removing not internet poor variable and reordering to match 1.0
  dplyr::select (ccode, country, population_2023, internet_poor_share_2023, internet_poor_2023) %>%
  
  # changing country name of Cote d'ivoire
  mutate (country = ifelse (ccode == "CIV", "Côte d’Ivoire", country))

## using WDP data for populaton figures validation

# loading data
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_10_11_2017ppp_1_USD.rds"))

# computing desired spending groups
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, Inf))

# cleaning WDP data to obtain population figures per country for desired years
wdp_clean = wdp %>%
  
  # grouping: ccode, year, spending group --> headcounts and spending
  group_by (ccode, year, daily_spending) %>%
  summarise (population = sum (hc.pdf.m, na.rm = T)) %>%
  
  # filter for required years
  filter (year %in% 2022:2023) %>%
  
  # filtering countries to only countain countries from 2.0 
  filter (ccode %in% ipi_2_clean$ccode) %>%
  
  # removing spending group column
  dplyr::select (-daily_spending) %>%
  
  # widening data to have one row per country
  pivot_wider (names_from = year, values_from = population) %>% 
  rename_with (
    ~ case_when (
      . == "2022" ~ "population_2022_WDP",
      . == "2023" ~ "population_2023_WDP",
      TRUE ~ .
    )
  )



## Chapter 3: joining and analysing data ---------------------------------------

## using WDP data for populaton figures validation

# loading data
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_10_11_2017ppp_1_USD.rds"))

# computing desired spending groups
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, Inf))

# cleaning WDP data to obtain population figures per country for desired years
wdp_clean = wdp %>%
  
  # grouping: ccode, year, spending group --> headcounts and spending
  group_by (ccode, year, daily_spending) %>%
  summarise (population = sum (hc.pdf.m, na.rm = T)) %>%
  
  # filter for required years
  filter (year %in% 2022:2023) %>%
  
  # filtering countries to only countain countries from 2.0 
  filter (ccode %in% ipi_2_clean$ccode) %>%
  
  # removing spending group column
  dplyr::select (-daily_spending) %>%
  
  # widening data to have one row per country
  pivot_wider (names_from = year, values_from = population) %>% 
  rename_with (
    ~ case_when (
      . == "2022" ~ "population_2022_WDP",
      . == "2023" ~ "population_2023_WDP",
      TRUE ~ .
    )
  )

## IPI joined data frame
ipi_delta = ipi_2_clean %>%
  
  # joining with 1.0
  left_join (ipi_1_clean, by = c("ccode", "country")) %>% 
  
  # joining with WDP data
  left_join (wdp_clean, by = "ccode") %>%
  
  # reordering to group variables for change computation
  dplyr::select (ccode, country, continent, population_2022, population_2023, internet_poor_share_2022, internet_poor_share_2023, internet_poor_2022, internet_poor_2023)

## validating population with WDP
ipi_popcheck = ipi_2_clean %>%
  
  # joining with 1.0
  left_join (ipi_1_clean, by = c("ccode", "country")) %>% 
  
  # joining with WDP data
  left_join (wdp_clean, by = "ccode") %>%
  
  # keeping only population vars
  dplyr::select (ccode, country, population_2022, population_2022_WDP, population_2023, population_2023_WDP) %>%
  
  # computing differences 
  mutate (pop_2022_dif = population_2022 - population_2022_WDP,
          pop_2023_dif = population_2023 - population_2023_WDP) %>%
  
  dplyr::select (ccode, country, pop_2022_dif, pop_2023_dif) 

## creating df with change vars in absolte numbers
ipi_delta_abs = ipi_delta %>%
  
  # computing change vars
  mutate (pop_delta = population_2023 - population_2022,
          share_delta = internet_poor_share_2023 - internet_poor_share_2022, 
          internet_poor_delta = internet_poor_2023 - internet_poor_2022) %>%
  
  # keeping only the change vars for easier analysis
  dplyr::select (ccode, country, continent, pop_delta, share_delta, internet_poor_delta)


## creating df with change vars in percentage change
ipi_delta_pct = ipi_delta %>%
  
  # computing change vars
  mutate (pop_delta = ((population_2023 - population_2022) / population_2022 ) * 100,
          share_delta = ((internet_poor_share_2023 - internet_poor_share_2022) / internet_poor_share_2022 ) * 100, 
          internet_poor_delta = ((internet_poor_2023 - internet_poor_2022) / internet_poor_2022 ) * 100) %>%
  
  # keeping only the change vars for easier analysis
  dplyr::select (ccode, country, continent, pop_delta, share_delta, internet_poor_delta)



## Chapter 4: visualising the changes ------------------------------------------

## graphs for number of internet poor

delta_internet_poor_abs = ggplot (data = ipi_delta_abs, 
                                  aes (x = ccode, y = internet_poor_delta)) +
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  theme (axis.text.x = element_text (size = 6)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in number of internet poor (absolute numbers)", 
        subtitle = "From 2022 to 2023 (IPI 1.0 to IPI 2.0)", 
        x = "Country", 
        y = "Difference in number of internet poor") + 
  facet_wrap (~continent, scale = "free")
delta_internet_poor_abs


delta_internet_poor_pct = ggplot (data = ipi_delta_pct, 
                                  aes (x = ccode, y = internet_poor_delta)) +
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  theme (axis.text.x = element_text (size = 6)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in number of internet poor (percentage change)", 
        subtitle = "From 2022 to 2023 (IPI 1.0 to IPI 2.0)", 
        x = "Country", 
        y = "Difference in number of internet poor") + 
  facet_wrap (~continent, scale = "free")
delta_internet_poor_pct

## graphs for internet poor share

delta_share_abs = ggplot (data = ipi_delta_abs, 
                                  aes (x = ccode, y = share_delta)) +
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  theme (axis.text.x = element_text (size = 6)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in share of internet poor (absolute difference)", 
        subtitle = "From 2022 to 2023 (IPI 1.0 to IPI 2.0)", 
        x = "Country", 
        y = "Change in share of internet poor") + 
  facet_wrap (~continent, scale = "free")
delta_share_abs


delta_share_pct = ggplot (data = ipi_delta_pct, 
                                  aes (x = ccode, y = share_delta)) +
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  theme (axis.text.x = element_text (size = 6)) +
  worlddataverse::scale_color_wdl() + 
  labs (title = "Change in share of internet poor (percentage change)", 
        subtitle = "From 2022 to 2023 (IPI 1.0 to IPI 2.0)", 
        x = "Country", 
        y = "Change in share of internet poor") + 
  facet_wrap (~continent, scale = "free")
delta_share_pct



## Chapter 5: creating final data frame ----------------------------------------

# united data frame with change variables as well as 2022 and 2023 numbers included
ipi_check = ipi_delta %>%
  
  # joining with absolute numbers
  left_join (ipi_delta_abs %>% 
               rename_with (
                 ~ case_when (
                   . == "pop_delta" ~ "pop_delta_abs",
                   . == "share_delta" ~ "share_delta_abs",
                   . == "internet_poor_delta" ~ "internet_poor_delta_abs",
                   TRUE ~ .
                 )
               ), by = c("ccode", "country", "continent")) %>%
  
  # joining with percentage changes
  left_join (ipi_delta_pct %>% 
               rename_with (
                 ~ case_when (
                   . == "pop_delta" ~ "pop_delta_pct",
                   . == "share_delta" ~ "share_delta_pct",
                   . == "internet_poor_delta" ~ "internet_poor_delta_pct",
                   TRUE ~ .
                 )
               ), by = c("ccode", "country", "continent"))

## exporting to CSV 
write.csv (ipi_check, file.path (ipi2_path, "ipi2_check.csv"), row.names = F)


## if we add the missing countries in 2.0, what do we get from 1.0?

missing <- setdiff (unique (ipi_1$ccode), unique (ipi_2$ccode))
missing_names = countrycode (missing, "iso3c", "country.name")

sum (ipi_1$internet_poor [ipi_1$gender == "both" & ipi_1$ccode %in% missing])



ipi_1_1 = ipi_1 %>% filter (gender == "both" & ccode %in% missing)
ipi_1_2 = ipi_1 %>% filter (gender == "both") %>% filter (!(ccode %in% missing))

sum (ipi_1_1$internet_poor) + sum (ipi_1_2$internet_poor)

  
  
  
  
  
  
  
  
  
  
  







  
  
