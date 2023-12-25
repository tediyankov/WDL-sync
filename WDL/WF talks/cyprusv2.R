
## chapter 1: setup ------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, data.table, ggpmisc, ggpubr, ggthemes, knitr, sjPlot, texreg, stargazer, corrplot,
                kableExtra, treemapify)

## system font 
worlddataverse::font_wdl()

EU = c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus",
       "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany",
       "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
       "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania",
       "Slovakia", "Slovenia", "Spain", "Sweden")

EU_ISO3 <- countrycode (sourcevar = EU,
                        origin = "country.name",
                        destination = "iso3c",
                        warn = TRUE,
                        nomatch = NA)

## cleaning tourism data
tourism = TOURISTS_A80_22_EN_100223 %>%
  
  # making 4th row the colnames
  janitor::row_to_names (4) %>%
  
  # isolating 2022
  dplyr::select (Residence, `2022`) %>%
  
  # cleaning up country names
  mutate (Residence = str_replace_all (Residence, "\\(\\d+\\)", "")) %>%
  mutate (Residence = str_replace_all (Residence, "\\(\\d+,\\d+\\)", "")) %>%
  mutate (Residence = str_replace_all (Residence, "\\(\\d+, \\d+\\)", "")) %>%
  
  # slice the bottom
  dplyr::slice (1:87) %>%
  
  # converting country names to ISO3 codes
  mutate (ccode = countrycode (Residence, "country.name", "iso3c")) %>%
  
  # remove non-countries
  filter (!is.na (ccode)) %>%
  
  # remove countries with no info
  filter (grepl("^\\d+$", `2022`)) %>%
  
  # keeping only ccode
  dplyr::select (ccode, `2022`) %>%
  
  # renaming var
  rename (tourists_2022 = `2022`) %>%
  
  # creating continent var
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  
  # aggregating into UK, EU, Israel, Asia, Rest of World
  mutate (ccode = ifelse (ccode == "GBR", "UK", 
                          ifelse (ccode %in% EU_ISO3, "EU", 
                                  ifelse (ccode == "ISR", "Israel", 
                                          ifelse (continent == "Asia", "Asia", "Rest of World"
                                          ))))) %>%
  
  # aggregate into new country groups
  dplyr::group_by (ccode) %>%
  dplyr::summarise (tourists_2022 = sum (as.numeric (tourists_2022), na.rm = T))

## plotting 
ggplot (tourism, aes (x = "", y = tourists_2022, fill = ccode)) +
  geom_bar (stat = "identity", width = 1) +
  coord_polar (theta = "y") +
  labs (title = "Tourists into Cyprus by Country of Origin, 2022",
        x = "",
        y = "") +
  theme_blank () +
  theme (axis.text.y = element_blank(),
         axis.text.x = element_blank()) +
  theme (legend.position = "right") +
  geom_text (aes (label = scales::percent (tourists_2022 / sum(tourists_2022)), y = tourists_2022), position = position_stack (vjust = 0.5))


## every country that has more than 1 million new consumers

base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')
cats_path1 <- file.path(base_path, 'product_categories', 'trade_model', '03_outputs')
cats_path2 <- file.path(base_path, 'product_categories', 'loreal', '2023-06-28')
pc_path <- file.path (base_path, "spending_categories","demographic_breakdown", "pc_and_beauty_breakdown","2023-09-07")




wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_07_26','2023_10_11_merge_test_2020_2017ppp','03_outputs')

wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_mpro2_ages_R_2023_07_26_2017ppp_1_USD.rds"))
wdp_raw = wdp_raw %>% rename (inc.pdf.lwr = inc_lwr, inc.pdf.upr = inc_upr) 
wdp_raw2 = wdp_raw %>%
  mutate(
    age_group = ifelse(age_upr == Inf, 
                       paste0("[", sprintf("%02d", age_lwr), ",INF]"),
                       paste0("[", sprintf("%02d", age_lwr), ",", sprintf("%02d", age_upr), ")")
    )
  )

wdp_raw2 = wdp_raw2 %>% dplyr::select (ccode, year, age_group, gender, inc.pdf.lwr, inc.pdf.upr, hc, exp) %>%
  rename (hc.pdf.m = hc, exp.pdf.m = exp)

# data cleaning
library (tidyverse)

wdp_millionrisers = wdp %>%
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T)) %>%
  mutate (daily_spending = ifelse (daily_spending == "[0,12)", "not", "CC")) %>%
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc, na.rm = T)) %>%
  filter (daily_spending == "CC" & year %in% c(2023, 2030)) %>%
  pivot_wider (names_from = "year", values_from = "hc") %>%
  mutate (delta = `2030` - `2023`) %>%
  filter (delta >= 10000000) %>%
  mutate (delta = delta / 10^6) %>%
  ggplot (aes (x = reorder (ccode, -delta), y = delta, fill = ccode)) + 
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (legend.position = "none") + 
  labs (title = "Countries adding at least 10M consumers between 2023 and 2030",
        x = "Country", 
        y = "Change in consumer headcounts, Millions")
wdp_millionrisers





