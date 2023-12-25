
################################################################################
-------------------- # WDL INSIGHTS PRESENTATION ACTUALISING # -----------------
----------------------------- # by: Teodor Yankov # ----------------------------
################################################################################

## Preamble ====================================================================

## clean environment
rm (list = ls())

## packages --------------------------------------------------------------------

pacman::p_load (worlddataverse, tidyverse, countrycode, qs)

## file paths ------------------------------------------------------------------

base_path <- worlddataverse::get_wdl_path()
pop_path <- file.path (base_path, 
                       "wittgenstein_iiasa_population")
wdp_path <- file.path (base_path, 
                       "Mpro_2.0", 
                       "01_Data", 
                       "R_2023_11_21", 
                       '2023_11_21_chn_sau_2020_2017ppp', 
                       "03_outputs")
wec_path <- file.path (base_path,
                       "world_emissions_clock",
                       "01_data")

## Slides 4 ====================================================================

## loading data in
popdat_raw = qs::qread (file.path (pop_path, "wittgenstein_population_one_year_age_groups_v2.qs"))

## country objects
northam = countrycode (c("Antigua and Barbuda", "Bahamas", 
                         "Barbados", "Belize", "Canada", 
                         "Costa Rica", "Cuba", 
                         "Dominica", "Dominican Republic", 
                         "El Salvador", "Grenada", 
                         "Guatemala", "Haiti", "Honduras", 
                         "Jamaica", "Mexico", "Nicaragua", 
                         "Panama", "Saint Kitts and Nevis", 
                         "Saint Lucia", 
                         "Saint Vincent and the Grenadines", 
                         "Trinidad and Tobago", 
                         "United States of America (USA)", "Anguilla", 
                         "Aruba", "Bermuda", "Bonaire", "British Virgin Islands", 
                         "Cayman Islands", "Clipperton Island", "Curacao", 
                         "Greenland", "Guadeloupe", "Martinique", "Montserrat", 
                         "Navassa Island", "Puerto Rico", "Saba", 
                         "Saint Barthelemy", "Saint Martin", 
                         "Saint Pierre and Miquelon", "Sint Eustatius", 
                         "Sint Maarten", "Turks and Caicos Islands", 
                         "US Virgin Islands"), "country.name", "iso3c")

southam = countrycode (c("Argentina", "Bolivia", "Brazil", "Chile", 
                         "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", 
                         "Suriname", "Uruguay", "Venezuela", "Falkland Islands", 
                         "French Guiana", "South Georgia and the South Sandwich Islands"), "country.name", "iso3c")

## regional data clean
popdat = popdat_raw %>%
  
  # filtering
  filter (year == 2023) %>%
  filter (!is.na (ccode)) %>%
  
  # getting region variable 
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  mutate (region = ifelse (ccode %in% northam, "North America",
                           ifelse (ccode %in% southam, "South America", 
                                   ifelse (continent == "Oceania", "Asia", continent)))) %>%
  
  # aggregate over region groupings
  dplyr::group_by (year, region, sex) %>%
  dplyr::summarise (pop = sum (population, na.rm = T)) %>%
  
  # delta vars
  pivot_wider (names_from = "sex", values_from = "pop") %>%
  mutate (delta = female - male) %>%
  
  # rescaling
  mutate (male = male / 10^3,
          female = female / 10^3,
          delta = delta / 10^3)

## world row
popdat_world = popdat_raw %>%
  
  # filtering
  filter (year == 2023) %>%
  filter (!is.na (ccode)) %>%
  
  # getting region variable 
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  mutate (region = ifelse (ccode %in% northam, "North America",
                           ifelse (ccode %in% southam, "South America", 
                                   ifelse (continent == "Oceania", "Asia", continent)))) %>%
  
  # aggregate over region groupings
  dplyr::group_by (year, sex) %>%
  dplyr::summarise (pop = sum (population, na.rm = T)) %>%
  
  # delta vars
  pivot_wider (names_from = "sex", values_from = "pop") %>%
  mutate (delta = female - male)  %>%
  
  # rescaling
  mutate (male = male / 10^3,
          female = female / 10^3,
          delta = delta / 10^3)
  
## combine
popdat_world$region = "World"
popdat_whole = rbind (popdat_world, popdat) %>%
  
  # changing order of countries
  mutate (region = factor (region,
                          levels = c("Asia", "Africa", "North America", "South America", "Europe", "World")))

## plotting
ggplot (data = popdat_whole,
        aes (x = region, y = delta, fill = region)) + 
  geom_bar (stat = "identity", width = 0.8) + 
  labs (title = "There are more males than females in the world (2023)", 
        subtitle = "Absolute difference in male and female population in 2023 per region",
        x = "", 
        y = "Female Population - Male Population, 2023") + 
  ylim (-150, 50) +
  coord_flip() + 
  worlddataverse::theme_wdl () + 
  worlddataverse::scale_fill_wdl() + 
  theme (legend.position = "none")

## Slides 5 ====================================================================

## data cleaning (age disagg)
popdat_age_disagg = popdat_raw %>%
  
  # filtering
  filter (!is.na (ccode)) %>%
  filter (year %in% 2000:2100) %>%
  
  # new age groupingsh
  mutate (age = as.numeric (age),
          age = ifelse (age < 15, "Children: 0-14", "Adults: 15+")) %>%
  
  # aggregate over age groupingsh
  dplyr::group_by (year, age) %>%
  dplyr::summarise (pop = sum (population, na.rm = T)) %>%
  
  # converting to billions 
  mutate (pop = pop / 10^6)

## data cleaning (total)
popdat_age_total = popdat_age_disagg %>%
  
  # getting sum across ages
  dplyr::group_by (year) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T))

## adding total name category
popdat_age_total$age = "Total"
popdat_age_total = popdat_age_total %>% dplyr::select (year, age, pop)

## combining
popdat_age_whole = rbind (popdat_age_total, popdat_age_disagg)

## plotting
ggplot (data = popdat_age_whole,
        aes (x = year, y = pop, col = age)) + 
  geom_line (linewidth = 1) + 
  labs (title = "Since 2000, global population growth is driven by adults – not children", 
        subtitle = "Population growth by age group, 2000 - 2100",
        x = "Year", 
        y = "Population (billions)") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl()

## Slides 6 ====================================================================

## raw data import
wdp_raw <- readRDS (file.path (wdp_path,
                               "05_003_merge_mpro2_ages_R_2023_11_21_2017ppp_1_USD_adjusted.rds"))

wdp_raw2 = wdp_raw %>%
  rename (inc.pdf.lwr = inc_lwr,
          inc.pdf.upr = inc_upr) %>%
  mutate(
    age_group = ifelse(age_upr == Inf, 
                       paste0("[", sprintf("%02d", age_lwr), ",INF]"),
                       paste0("[", sprintf("%02d", age_lwr), ",", sprintf("%02d", age_upr), ")")
    )
  ) %>%
  dplyr::select (-c(age_upr, age_lwr))

wdp_raw2 = wdp_raw2 %>% dplyr::select (ccode, year, age_group, gender, inc.pdf.lwr, inc.pdf.upr, hc, exp) %>%
  rename (hc.pdf.m = hc, exp.pdf.m = exp)

## data cleaning
wdp = wdp_raw2 %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, Inf))
wdp_clean = wdp %>%
  
  # isolating consumer class
  filter (daily_spending == "[12,Inf)") %>%
  dplyr::select (-daily_spending) %>%
  
  # aggregating headcounts across gender
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T)) %>%
  
  # isolating relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # recategorising age
  mutate (age_group = ifelse (age_group %in% c("[00,05)", "[05,10)", "[10,15)"), "Young", "Old")) %>%
  
  # isolating young
  filter (age_group == "Young") %>%
  dplyr::select (-age_group) %>%
  
  # recategorising countries into regions
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  mutate (region = ifelse (ccode %in% c("BGD", "BTN", "IND", "LKA", "MDV", "NPL"), "South Asia", 
                           ifelse (continent == "Asia" & !(ccode %in% c("BGD", "BTN", "IND", "LKA", "MDV", "NPL")), "Rest of Asia", 
                                   ifelse (!(continent %in% c("Europe", "Africa")), "Rest of World", continent)))) %>%
  
  # aggregating over regions
  dplyr::group_by (region, year) %>%
  dplyr::summarise (hc = sum (hc, na.rm = T)) %>%
  
  # obtaining 2023 -> 2030 delta
  pivot_wider (names_from = "year", values_from = "hc") %>%
  mutate (delta2330 = `2030` - `2023`) %>% 
  
  # rescaling delta var to millions
  mutate (delta2330 = delta2330 / 10^6)


## plotting
color_palette <- colorRampPalette (c("lightblue", "darkblue"))(length (unique (wdp_clean$region)))
ggplot (data = wdp_clean, 
        aes (x = reorder (region, -delta2330), y = delta2330, fill = region)) + 
  geom_bar (stat = "identity") + 
  labs (title = "Consumer Class Under 15, Growth 2023 - 2030",
        x = "", 
        y = "Consumer Class Headcount Growth (Millions)") +
  scale_fill_manual (values = color_palette) +
  geom_text (aes (label = paste0 (round (delta2330, 1), " M")), vjust = -0.5, color = "black", size = 4) +
  theme (axis.text.x = element_text (angle = 45)) + 
  theme (legend.position = "none") + 
  worlddataverse::theme_wdl()

## Slides 7-9 ==================================================================

## data cleaning
wdp2 = wdp_raw2 %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 2, 12, 120, Inf))
wdp_clean2 = wdp2 %>%
  
  # aggregating
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T)) %>%
  
  # renaming the spending groups 
  mutate (daily_spending = ifelse (daily_spending == "[0,2)", "Poor", 
                                   ifelse (daily_spending == "[2,12)", "Vulnerable", 
                                           ifelse (daily_spending == "[12,120)", "Middle Class", 
                                                   ifelse (daily_spending == "[120,Inf)", "Upper Class", NA
                                                   ))))) %>%
  
  # reaggregating over new spending groups just to make sure
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc, na.rm = T)) %>%
  
  # filtering for 2023
  filter (year == 2023) %>%
  
  # reformatting hc
  mutate (hc = ifelse (hc < 10^9, paste0 (round (hc / 10^6, 1), " M"), paste0 (round (hc / 10^9, 1), " B")))


## Slides 10 ===================================================================

## data cleaning
wdp_clean3 = wdp2 %>%
  
  # aggregating
  dplyr::group_by (year, ccode, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T)) %>%
  
  # keeping only rich and for 2023
  filter (daily_spending == "[120,Inf)") %>%
  filter (year == 2023) %>%
  
  # getting the top 10 
  arrange (desc (hc))

## slicing top 10
top10 = head (wdp_clean3, 10) %>%
  mutate (hc = hc / 10^6) # converting to millions

## plotting
ggplot (data = top10, 
        aes (x = reorder (ccode, -hc), y = hc)) +
  geom_bar (stat = "identity") +
  labs (title = "Rich (>$120/day) headcounts in 2023 - Top 10 Countries",
        x = "Country", 
        y = "Rich (>$120/day) Headcount (Millions)") + 
  worlddataverse::theme_wdl()
  
## Slides 15 (left) ============================================================

## data import
load_wec_dat = function (){
  
  # load binary (Rda) file
  load (file.path (wec_path, "WEC_data_binary_20231120_lulucfREG.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  
}
load_wec_dat()

## data cleaning
wec_dat_clean = wec_dat %>%
  
  # aggregating
  dplyr::group_by (iso3c, sector, year, pop) %>%
  dplyr::summarise (emissions = sum (base, na.rm = T)) %>%
  
  # computing emissions per capita
  mutate (em_pc = emissions / pop) %>%
  
  # filtering for 2023
  filter (year == 2023) %>%
  
  # summing over sector
  dplyr::group_by (sector) %>%
  dplyr::summarise (emissions = sum (emissions, na.rm = T), 
                    pop = sum (pop, na.rm = T))

wec_dat_clean$pop[wec_dat_clean$sector == "Transport"] = 7909458979
wec_dat_clean$em_pc = wec_dat_clean$emissions / wec_dat_clean$pop
  
  
## Slides 15 (right) ===========================================================

## data cleaning 
wec_dat_clean2 = wec_dat %>%
  
  # aggregating
  dplyr::group_by (iso3c, sector, year, pop) %>%
  dplyr::summarise (emissions = sum (base, na.rm = T)) %>%
  
  # computing emissions per capita
  mutate (em_pc = emissions / pop) %>%
  
  # filtering for 2023
  filter (year == 2023) %>%
  
  # only energy emissions
  filter (sector == "Energy") %>%
  
  # keeping select countries
  filter (iso3c %in% countrycode (c("Kenya", "Switzerland", "Spain", "Germany", "China", "South Africa", "Mongolia", "Australia"), 
                                  "country.name", 
                                  "iso3c"))

## plotting
ggplot (data = wec_dat_clean2, 
        aes (x = reorder (iso3c, em_pc), y = em_pc)) + 
  geom_bar (stat = "identity") + 
  geom_text (aes (label = paste0 (round (em_pc, 2), "\nT pc")), vjust = -0.5, color = "black", size = 4) + 
  labs (title = "Selected countries, per-capita energy emissions",
        x = "", 
        y = "Energy emissions per capita (T)") + 
  worlddataverse::theme_wdl() + 
  ylim (0,13)

