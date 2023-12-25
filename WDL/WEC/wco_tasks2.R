
## World Consumer Outlook 2023: first tasks =================================================================================================================

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
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')

## loading in WDPro latest version

# loading raw file
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))

## loading in IMF WEO April 2023 data
library (readxl)
weo_raw <- readxl::read_xls (file.path ("~/Desktop/WEO_Data_2.xls"))

to_qa <- readRDS (file.path (base_path, "wdp_city_spending", "wdpro_combination", "city_distribution_updated_LK_V2.rds"))


## Chapter 2: data cleaning ----------------------------------------------------

## Cleaning WEO
weo_clean = weo_raw %>%
  
  # isolating necessary vars
  dplyr::select (ISO, 8:15) %>%
  
  # elongating to have one year per row
  tidyr::pivot_longer (2:9, names_to = "year", values_to = "value") %>%
  
  # expanding subject variable into individual metrics
  #tidyr::pivot_wider (names_from = 2, values_from = "value") %>%
  
  # renaming metrics
  dplyr::rename_with (
    ~ case_when (
      . == "value" ~ "delta_gdp",
      TRUE ~ .
    )
  ) %>%
  
  # isolating GDP
  #dplyr::select (- c(gdp_pc, gdp_share)) %>% 
  
  # replacing n/a with NAs 
  dplyr::mutate (delta_gdp = ifelse (delta_gdp == "n/a", NA, delta_gdp)) %>%
  
  # removing phantom variable
  #dplyr::select (-`NA`) %>%
  
  # converting values to numeric
  dplyr::mutate_at (3, as.numeric) %>%
  
  # removing AFG, LBN, SYR, UKR, VEN because they are incomplete
  dplyr::filter (!ISO %in% unique (.$ISO [is.na (.$delta_gdp)])) %>%
  
  # aggregating into continents
  dplyr::mutate (continent = countrycode (ISO, "iso3c", "continent")) %>%
  dplyr::select (-ISO) %>%
  dplyr::group_by (continent, year) %>%
  dplyr::summarise (delta_gdp = mean (delta_gdp, na.rm = T)) %>%
  tidyr::drop_na () %>%
  
  # widening to have year+metric
  tidyr::pivot_wider (names_from = 2, values_from = 3) %>%
  
  # creating change variables
  dplyr::mutate (delta_gdp_21 = ((`2021` - `2020`) / `2020`) * 100,
                 delta_gdp_22 = ((`2022` - `2021`) / `2021`) * 100,
                 delta_gdp_23 = ((`2023` - `2022`) / `2022`) * 100,
                 delta_gdp_24 = ((`2024` - `2023`) / `2023`) * 100,
                 delta_gdp_25 = ((`2025` - `2024`) / `2024`) * 100,
                 delta_gdp_26 = ((`2026` - `2025`) / `2025`) * 100,
                 delta_gdp_27 = ((`2027` - `2026`) / `2026`) * 100,
                 delta_gdp_28 = ((`2028` - `2027`) / `2027`) * 100)

## Cleaning WDPro

wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, Inf))

# clean
wdp_clean = wdp %>%
  
  # country-year headcounts and spending per spending group and age
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # recoding spendng groups into CC and not
  dplyr::mutate (daily_spending = ifelse (daily_spending == "[12,Inf)", "CC", "NCC")) %>%
  
  # aggregate 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>%
  
  # keeping only consumer class
  dplyr::filter (daily_spending == "CC") %>%
  dplyr::select (-daily_spending) %>%
  
  # years of interest
  dplyr::filter (year %in% c(2020:2028)) %>%
  
  # continents
  dplyr::mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  dplyr::select (-ccode) %>%
  dplyr::group_by (continent, year) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>%
  
  # widening for calculating delta vars
  pivot_wider (names_from = 2, values_from = c(3:4)) %>%
  
  # calculating delta vars (pop)
  dplyr::mutate (delta_cc_hc_21 = ((pop_2021 - pop_2020) / pop_2020) * 100,
                 delta_cc_hc_22 = ((pop_2022 - pop_2021) / pop_2021) * 100,
                 delta_cc_hc_23 = ((pop_2023 - pop_2022) / pop_2022) * 100,
                 delta_cc_hc_24 = ((pop_2024 - pop_2023) / pop_2023) * 100,
                 delta_cc_hc_25 = ((pop_2025 - pop_2024) / pop_2024) * 100,
                 delta_cc_hc_26 = ((pop_2026 - pop_2025) / pop_2025) * 100,
                 delta_cc_hc_27 = ((pop_2027 - pop_2026) / pop_2026) * 100,
                 delta_cc_hc_28 = ((pop_2028 - pop_2027) / pop_2027) * 100) %>%
  
  # calculating delta vars (exp)
  dplyr::mutate (delta_cc_spend_21 = ((exp_2021 - exp_2020) / exp_2020) * 100,
                 delta_cc_spend_22 = ((exp_2022 - exp_2021) / exp_2021) * 100,
                 delta_cc_spend_23 = ((exp_2023 - exp_2022) / exp_2022) * 100,
                 delta_cc_spend_24 = ((exp_2024 - exp_2023) / exp_2023) * 100,
                 delta_cc_spend_25 = ((exp_2025 - exp_2024) / exp_2024) * 100,
                 delta_cc_spend_26 = ((exp_2026 - exp_2025) / exp_2025) * 100,
                 delta_cc_spend_27 = ((exp_2027 - exp_2026) / exp_2026) * 100,
                 delta_cc_spend_28 = ((exp_2028 - exp_2027) / exp_2027) * 100)

columns_to_remove1 <- grep ("pop_", names (wdp_clean))
columns_to_remove2 <- grep ("exp_", names (wdp_clean))

wdp_clean = wdp_clean %>% select (-c(columns_to_remove1, columns_to_remove2))

## Chapter 3: line chart -------------------------------------------------------

## dataframe for line chart
wdp_clean_line_hc = wdp_clean %>%
  
  # keeping only delta vars
  dplyr::select (-c(2:19)) %>%
  
  # keeping only headcount vars
  dplyr::select (1:9) %>%
  
  # elongating
  pivot_longer (2:9, names_to = "year", values_to = "headcounts") %>%
  
  # changing year var
  dplyr::mutate (year = ifelse (year == "delta_cc_hc_21", 2021,
                                ifelse (year == "delta_cc_hc_22", 2022,
                                        ifelse (year == "delta_cc_hc_23", 2023,
                                                ifelse (year == "delta_cc_hc_24", 2024,
                                                        ifelse (year == "delta_cc_hc_25", 2025,
                                                                ifelse (year == "delta_cc_hc_26", 2026,
                                                                        ifelse (year == "delta_cc_hc_27", 2027,
                                                                                ifelse (year == "delta_cc_hc_28", 2028, NA)))))))))

## dataframe for line chart (spend)
wdp_clean_line_spend = wdp_clean %>%
  
  # keeping only delta vars
  dplyr::select (-c(2:19)) %>%
  
  # keeping only headcount vars
  dplyr::select (10:17) %>%
  
  # elongating
  pivot_longer (2:9, names_to = "year", values_to = "spending") %>%
  
  # changing year var
  dplyr::mutate (year = ifelse (year == "delta_cc_spend_21", 2021,
                                ifelse (year == "delta_cc_spend_22", 2022,
                                        ifelse (year == "delta_cc_spend_23", 2023,
                                                ifelse (year == "delta_cc_spend_24", 2024,
                                                        ifelse (year == "delta_cc_spend_25", 2025,
                                                                ifelse (year == "delta_cc_spend_26", 2026,
                                                                        ifelse (year == "delta_cc_spend_27", 2027,
                                                                                ifelse (year == "delta_cc_spend_28", 2028, NA)))))))))

## joining data frames 
wco_linechart_data = weo_clean %>%
  
  # numeric year
  mutate (year = as.numeric (year)) %>%
  
  # merge
  left_join (wdp_clean_line_hc, by = c("continent", "year")) %>%
  left_join (wdp_clean_line_spend, by = c("continent", "year")) %>%
  
  # elongating for line groups
  pivot_longer (3:5, names_to = "group", values_to = "value") %>%
  
  # renaming 
  mutate (group = ifelse (group == "delta_gdp", "imf_gdp", group))

## adding a World section to contnents
world = wco_linechart_data %>% 
  
  dplyr::group_by (year, group) %>%
  dplyr::summarise (value = mean (value, na.rm = T)) %>%
  dplyr::mutate (continent = "World") %>%
  dplyr::relocate (continent, .before = year)

## joining to original df 
wco_linechart_data = rbind (wco_linechart_data, world)

## buildng plot
wco_linechart = ggplot (data = wco_linechart_data, 
                        aes (x = year, y = value, group = group, colour = group)) + 
  geom_line () + 
  facet_wrap (~continent) + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl2() + 
  labs (title = "WEO GDP growth vs WDPro Consumer Class growth", 
        subtitle = "Continental trends")
  

## Chapter 4: fastest growng age group -----------------------------------------

wdp_clean_ages = wdp %>%
  
  # country-year headcounts and spending per spending group and age
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # recoding age groups into 10-year scale
  dplyr::mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)"), '[00,10)', 
                                     ifelse (age_group %in% c("[10,15)","[15,20)"), '[10,20)', 
                                             ifelse (age_group %in% c("[20,25)","[25,30)"), '[20,30)', 
                                                     ifelse (age_group %in% c("[30,35)","[35,40)"), '[30,40)', 
                                                             ifelse (age_group %in% c("[40,45)","[45,50)"), '[40,50)', 
                                                                     ifelse (age_group %in% c("[50,55)","[55,60)"), '[50,60)', 
                                                                             ifelse (age_group %in% c("[60,65)","[65,70)"), '[60,70)', 
                                                                                     ifelse (age_group %in% c("[70,75)","[75,INF)"), '[70,INF)', age_group))))))))) %>%
  
  # aggregating into new groups
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>%
  
  # recoding spendng groups into CC and not
  dplyr::mutate (daily_spending = ifelse (daily_spending == "[12,Inf)", "CC", "NCC")) %>%
  
  # aggregate 
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>%
  
  # keeping only consumer class
  dplyr::filter (daily_spending == "CC") %>%
  dplyr::select (-daily_spending) %>%
  
  # years of interest
  dplyr::filter (year %in% c(2020:2030)) %>%
  
  # aggregatng over age group
  dplyr::group_by (year, age_group) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>%
  
  # computing delta
  dplyr::group_by (age_group) %>%
  dplyr::mutate (pop_lag = Hmisc::Lag (pop, 1),
                 exp_lag = Hmisc::Lag (exp, 1),
                 delta_pop = ((pop - pop_lag) / pop_lag) * 100,
                 delta_exp = ((exp - exp_lag) / exp_lag) * 100) %>% 
  dplyr::ungroup() %>%
  
  # removing NAs
  tidyr::drop_na() %>%
  
  # keeping only delta vars
  dplyr::select (year, age_group, delta_pop, delta_exp) %>%
  
  # elongating for line plots to be categorsed into spend and headcount growth
  pivot_longer (3:4, names_to = "group", values_to = "value") %>%
  
  # renaming group variable 
  dplyr::mutate (group = ifelse (group == "delta_exp", "Spending", "Headcounts"))
  
## buildng plot
wco_ageplot = ggplot (data = wdp_clean_ages, 
                        aes (x = factor(year), y = value, group = age_group, colour = age_group)) + 
  geom_line () + 
  facet_wrap (~group, scales = "free", nrow = 1, ncol = 2) + 
  worlddataverse::theme_wdl() +
  viridis::scale_color_viridis (discrete = T, option = "F") +
  labs (title = "Which age group is growing the fastest?", 
        subtitle = "2021-2028")
wco_ageplot



## Chapter 5: poor vs consumer class line chart --------------------------------

## headcounts
wdp_parallel_hc = wdp %>%
  
  # yearly headcounts and spending per spending group
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # renaming spending groups for legend
  dplyr::mutate (daily_spending = ifelse (daily_spending == "[0,12)", "poor & vulnerable", "consumer class")) %>%
  
  # changing scale
  dplyr::mutate (pop = pop / 10^9) %>%
  
  # creating line chart
  ggplot (aes (x = year, y = pop, group = daily_spending, col = daily_spending)) + 
  geom_line (linewidth = 2) + 
  worlddataverse::theme_wdl () + 
  labs (title = "Comparing growth by spending group",
        x = "\nYear",
        y = "Headcounts, in Billions") + 
  theme (axis.text.x = element_text (angle = 360),
         legend.position = "bottom")

wdp_parallel_hc

## spending
wdp_parallel_exp = wdp %>%
  
  # yearly headcounts and spending per spending group
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # renaming spending groups for legend
  dplyr::mutate (daily_spending = ifelse (daily_spending == "[0,12)", "poor & vulnerable", "consumer class")) %>%
  
  # changing scale
  dplyr::mutate (exp = exp / 10^12) %>%
  
  # creating line chart
  ggplot (aes (x = year, y = exp, group = daily_spending, col = daily_spending)) + 
  geom_line (linewidth = 2) + 
  worlddataverse::theme_wdl () + 
  labs (title = "Comparing growth by spending group",
        x = "\nYear",
        y = "Spending, in Trillions USD 2017 PPP") + 
  theme (axis.text.x = element_text (angle = 360),
         legend.position = "bottom")

wdp_parallel_exp



## Chapter 6: CC growth in spending vs WEO GDP growth --------------------------

## WEO data
weo_groups = readxl::read_xls ("~/Desktop/WEO_Data_3.xls") %>%
  
  # elongating
  pivot_longer (2:9, names_to = "year", values_to = "delta_gdp") %>%
  
  # renaming for ease
  dplyr::mutate (`Country Group Name` = ifelse (`Country Group Name` == "Advanced economies",
                                                "advanced", 
                                                "emerging")) %>%
  
  # rename country variable
  rename (ccode = `Country Group Name`) %>%
  
  # converting year var to numeric
  dplyr::mutate (year = as.numeric (year))

## country groups
advanced = countrycode (c("Andorra", "Australia", "Austria", "Belgium", "Canada", "Croatia", 
                          "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
                          "Germany", "Greece", "Hong Kong SAR", "Iceland", "Ireland", "Israel", 
                          "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", 
                          "Macao SAR", "Malta", "The Netherlands", "New Zealand", "Norway", 
                          "Portugal", "Puerto Rico", "San Marino", "Singapore", "Slovak Republic", 
                          "Slovenia", "Spain", "Sweden", "Switzerland", "Taiwan Province of China", 
                          "United Kingdom", "United States"), 
                        "country.name", 
                        "iso3c")

emerging = countrycode (c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda", 
                          "Argentina", "Armenia", "Aruba", "Azerbaijan", "The Bahamas", "Bahrain", 
                          "Bangladesh", "Barbados", "Belarus", "Belize", "Benin", "Bhutan", "Bolivia", 
                          "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei Darussalam", "Bulgaria", 
                          "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", 
                          "Central African Republic", "Chad", "Chile", "China", "Colombia", 
                          "Comoros", "Democratic Republic of the Congo", "Republic of Congo", 
                          "Costa Rica", "Côte d'Ivoire", "Djibouti", "Dominica", "Dominican Republic", 
                          "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini", 
                          "Ethiopia", "Fiji", "Gabon", "The Gambia", "Georgia", "Ghana", "Grenada", 
                          "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", 
                          "Hungary", "India", "Indonesia", "Iran", "Iraq", "Jamaica", "Jordan", 
                          "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyz Republic", 
                          "Lao P.D.R.", "Lebanon", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
                          "Malaysia", "Maldives", "Mali", "Marshall Islands", "Mauritania", "Mauritius", 
                          "Mexico", "Micronesia", "Moldova", "Mongolia", "Montenegro", "Morocco", "Mozambique", 
                          "Myanmar", "Namibia", "Nauru", "Nepal", "Nicaragua", "Niger", "Nigeria", 
                          "North Macedonia", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", 
                          "Paraguay", "Peru", "Philippines", "Poland", "Qatar", "Romania", "Russia", 
                          "Rwanda", "Samoa", "São Tomé and Príncipe", "Saudi Arabia", "Senegal", "Serbia", 
                          "Seychelles", "Sierra Leone", "Solomon Islands", "Somalia", "South Africa", 
                          "South Sudan", "Sri Lanka", "St. Kitts and Nevis", "St. Lucia", 
                          "St. Vincent and the Grenadines", "Sudan", "Suriname", "Syria", "Tajikistan", 
                          "Tanzania", "Thailand", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", 
                          "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", 
                          "United Arab Emirates", "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", 
                          "Vietnam", "West Bank and Gaza", "Yemen", "Zambia", "Zimbabwe"), 
                        "country.name", 
                        "iso3c")


## reformatting WDPro data to match WEO data 
wdp_groups = wdp %>% ungroup () %>%
  
  # country-year headcounts and spending per spending group 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # creating country groups
  dplyr::mutate (ccode = ifelse (ccode %in% advanced, "advanced", "emerging")) %>%
  
  # re-aggregating
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>%
  
  ungroup () %>%
  
  # keeping only consumer class spending
  dplyr::filter (daily_spending == "[12,Inf)") %>%
  dplyr::select (-daily_spending) %>%
  
  # filtering to relevant years
  dplyr::filter (year %in% c(2020:2028)) %>%
  
  # computing delta
  dplyr::group_by (ccode) %>%
  dplyr::mutate (pop_lag = lag (pop)) %>%
  dplyr::mutate (delta_cc_pop = ((pop - pop_lag) / pop_lag) * 100) %>%
  
  # keeping only delta var
  dplyr::select (ccode, year, delta_cc_pop) %>%
  
  # keeping only relevant years
  dplyr::filter (year %in% c(2021:2028))

## creating comparison chart
adv_emer_compare = wdp_groups %>% 
  
  # executing join of WDPro and WEO dataframes
  left_join (weo_groups, by = c("ccode", "year")) %>%
  
  # elongating for line chart 
  pivot_longer (3:4, names_to = "variable", values_to = "delta") %>%
  
  # renaming variable for legend
  dplyr::mutate (variable = ifelse (variable == "delta_cc_pop", 
                                    "Consumer Class \nHeadcounts Growth (WDPro)", 
                                    "GDP Growth \n(WEO 2023)")) %>% ungroup () %>%
  
  # creating chart
  ggplot (aes (x = year, y = delta, col = ccode, linetype = variable)) + 
  geom_line (linewidth = 1) + 
  worlddataverse::theme_wdl () + 
  labs (title = "Comparing Consumer Class Headcounts Growth with GDP Growth",
        x = "\nYear",
        y = "Percentage Change") + 
  theme (axis.text.x = element_text (angle = 360),
         legend.position = "bottom")
  
adv_emer_compare
  
  
  
## Chapter 7: Treemaps of CC headcounts / spending share -----------------------

## package
install.packages ("treemapify")
library (treemapify)

## country groups
rest_of_asia = countrycode (c("Afghanistan", "Armenia", "Azerbaijan", 
                              "Bahrain", "Bangladesh", "Bhutan", "Brunei", 
                              "Cambodia", "Cyprus", "Georgia", "Indonesia", 
                              "Iran", "Iraq", "Israel", "Japan", "Jordan", 
                              "Kazakhstan", "Kuwait",  "Kyrgyzstan", "Laos", 
                              "Lebanon", "Malaysia", 
                              "Maldives", "Mongolia", "Myanmar", "Nepal", 
                              "North Korea", "Oman", "Pakistan", "Palestine", 
                              "Philippines", "Qatar", "Russia", "Saudi Arabia", 
                              "Singapore", "South Korea", "Sri Lanka", "Syria", 
                              "Taiwan", "Tajikistan", "Thailand", "Timor-Leste", 
                              "Turkey", "Turkmenistan", "United Arab Emirates (UAE)", 
                              "Uzbekistan", "Vietnam", "Yemen"), 
                            "country.name", 
                            "iso3c")

africa = countrycode (c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
                        "Cabo Verde", "Cameroon", "Central African Republic", "Chad", 
                        "Comoros", "Congo, Democratic Republic of the", "Congo, Republic of the", 
                        "Cote d'Ivoire", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", 
                        "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                        "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", 
                        "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", 
                        "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", 
                        "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", 
                        "Zambia", "Zimbabwe"), 
                      "country.name", 
                      "iso3c")

latam = countrycode (c("Antigua and Barbuda", "Argentina", "Aruba", "The Bahamas", "Barbados", "Belize", 
                       "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominica", "Dominican Republic", 
                       "Ecuador", "El Salvador", "Grenada", "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", 
                       "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "St. Kitts and Nevis", "St. Lucia", 
                       "St. Vincent and the Grenadines", "Suriname", "Trinidad and Tobago", "Uruguay", "Venezuela"), 
                     "country.name", 
                     "iso3c")




## creating plot (headcount)
treemap_hc = wdp %>%
  
  # country-year headcounts and spending per spending group and age
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # creating country groupings
  dplyr::mutate (ccode = ifelse (ccode == "IND", "IND", 
                                 ifelse (ccode == "CHN", "CHN", 
                                         ifelse (ccode %in% rest_of_asia, "Rest of Asia",
                                                 ifelse (ccode %in% africa, "Africa", 
                                                         ifelse (ccode %in% latam, "LATAM", "Rest of World"
                                                 )))))) %>%
  
  # regrouping
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>% ungroup () %>%
   
  # filter for 2023 and for consumer class
  filter (year == 2023) %>%
  filter (daily_spending == "[12,Inf)") %>%
  
  # computing shares
  dplyr::mutate (pop_share = pop / sum (pop)) %>%
  
  # creating label
  dplyr::mutate (label = paste0 (round (pop / 10^6, 1), " ", "M", " ", "(", scales::percent(pop_share, 2), ")")) %>%
  
  # changing levels of ccode
  dplyr::mutate (ccode = factor (ccode, 
                                 levels = c("IND", "CHN", "Rest of Asia", "Africa", "LATAM", "Rest of World"))) %>%
  
  # creating plot
  ggplot (aes (area = pop_share, fill = ccode, label = label)) +
  geom_treemap() + 
  worlddataverse::theme_wdl () + 
  worlddataverse::scale_fill_wdl() + 
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme (legend.position = "bottom") + 
  labs (title = "Visualising shares of Consumer Class (Headcounts)")

treemap_hc

## creating plot (spending)
treemap_exp = wdp %>%
  
  # country-year headcounts and spending per spending group and age
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # creating country groupings
  dplyr::mutate (ccode = ifelse (ccode == "IND", "IND", 
                                 ifelse (ccode == "CHN", "CHN", 
                                         ifelse (ccode %in% rest_of_asia, "Rest of Asia",
                                                 ifelse (ccode %in% africa, "Africa", 
                                                         ifelse (ccode %in% latam, "LATAM", "Rest of World"
                                                         )))))) %>%
  
  # regrouping
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>% ungroup () %>%
  
  # filter for 2023 and for consumer class
  filter (year == 2023) %>%
  filter (daily_spending == "[12,Inf)") %>%
  
  # computing shares
  dplyr::mutate (exp_share = exp / sum (exp)) %>%
  
  # creating label
  dplyr::mutate (label = paste0 (round (exp / 10^12, 1), " ", "T", " ", "(", scales::percent(exp_share, 2), ")")) %>%
  
  # changing levels of ccode
  dplyr::mutate (ccode = factor (ccode, 
                                 levels = c("IND", "CHN", "Rest of Asia", "Africa", "LATAM", "Rest of World"))) %>%
  
  # creating plot
  ggplot (aes (area = exp_share, fill = ccode, label = label)) +
  geom_treemap() + 
  worlddataverse::theme_wdl () + 
  worlddataverse::scale_fill_wdl() + 
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme (legend.position = "bottom") + 
  labs (title = "Visualising shares of Consumer Class (Spending)")

treemap_exp


## Chapter 8: Treemaps of CClass spendng groups breakdown  ---------------------

## raw data 
wdp2 = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120,  Inf))

## building treemap
treemap_headcounts_groups = wdp2 %>%
  
  # country-year headcounts and spending per spending group and age
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # filter years
  filter (year %in% 2023:2024) %>%
  
  # filter out non CC
  filter (!daily_spending == "[0,12)") %>%
  
  # lagging forward
  dplyr::group_by (daily_spending) %>%
  mutate (pop_lag = lag (pop)) %>% ungroup () %>%
  
  # computing delta
  mutate (delta_hc = pop - pop_lag) %>%
  
  # removing NAs
  filter (year == 2024) %>%
  
  # computing share
  mutate (delta_share = delta_hc / sum (delta_hc, na.rm = T)) %>%
  
  # adding label
  mutate (label = paste0 ("+", " ", round (delta_hc / 10^6, 1), " ", "M", " ", "(", scales::percent(delta_share, 1), ")")) %>%
  
  # changing levels
  mutate (daily_spending = factor (daily_spending, levels = c("[12,40)", "[40,80)", "[80,120)", "[120,Inf)"))) %>%
  
  # bulilding chart
  ggplot (aes (area = delta_share, fill = daily_spending, label = label)) +
  geom_treemap() + 
  worlddataverse::theme_wdl () + 
  worlddataverse::scale_fill_wdl() + 
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme (legend.position = "bottom") + 
  labs (title = "113M New Consumers Breakdown into CC Spendng Group")

treemap_headcounts_groups

treemap_spending_groups = wdp2 %>%
  
  # country-year spending and spending per spending group and age
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # filter years
  filter (year %in% 2023:2024) %>%
  
  # filter out non CC
  filter (!daily_spending == "[0,12)") %>%
  
  # lagging forward
  dplyr::group_by (daily_spending) %>%
  mutate (exp_lag = lag (exp)) %>% ungroup () %>%
  
  # computing delta
  mutate (delta_exp = exp - exp_lag) %>%
  
  # removing NAs
  filter (year == 2024) %>%
  
  # computing share
  mutate (delta_share = delta_exp / sum (delta_exp, na.rm = T)) %>%
  
  # adding label
  mutate (label = paste0 ("+", " ", round (delta_exp / 10^9, 1), " ", "B", " ", "(", scales::percent(delta_share, 1), ")")) %>%
  
  # changing levels
  mutate (daily_spending = factor (daily_spending, levels = c("[12,40)", "[40,80)", "[80,120)", "[120,Inf)"))) %>%
  
  # bulilding chart
  ggplot (aes (area = delta_share, fill = daily_spending, label = label)) +
  geom_treemap() + 
  worlddataverse::theme_wdl () + 
  worlddataverse::scale_fill_wdl() + 
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) +
  theme (legend.position = "bottom") + 
  labs (title = "$2.3T New Consumers Spending Breakdown into CC Spending Group")

treemap_spending_groups

## Chapter 9: excel data on 23-24 delta per treemap classifications ------------

deltas_hc = wdp %>%
  
  # country-year headcounts and spending per spending group and age
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # creating country groupings
  dplyr::mutate (ccode = ifelse (ccode == "IND", "IND", 
                                 ifelse (ccode == "CHN", "CHN", 
                                         ifelse (ccode %in% rest_of_asia, "Rest of Asia",
                                                 ifelse (ccode %in% africa, "Africa", 
                                                         ifelse (ccode %in% latam, "LATAM", "Rest of World"
                                                         )))))) %>%
  
  # regrouping
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>% ungroup () %>%
  
  # filter for 2023 and for consumer class
  filter (year %in% 2023:2024) %>%
  filter (daily_spending == "[12,Inf)") %>%
  
  # removing daily spending var
  dplyr::select (-daily_spending) %>%
  
  # lagging forward
  dplyr::group_by (ccode) %>%
  mutate (pop_lag = lag (pop), 
          exp_lag = lag (exp)) %>% ungroup () %>%
  
  # computing delta
  mutate (delta_hc = pop - pop_lag,
          delta_exp = exp - exp_lag) %>%
  
  # removing NAs
  filter (year == 2024) %>%
  
  # keeping only relevant variables
  dplyr::select (ccode, year, delta_hc, delta_exp)

## exportiing
write.csv (deltas_hc, "slide13_deltas.csv", row.names = F)
  
  
  
## below the radar fast movers -------------------------------------------------

## loading data
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))

## cleaning into 12-120 spending group
wdp_mc <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 120, Inf))

## cleaning data 
asia_fastmovers = wdp_mc %>%
  
  # grouping into ccode, year and spending group
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # keeping only consumer class
  mutate (daily_spending = ifelse (daily_spending == "[0,12)", "noCC", "CC")) %>%
  
  # aggregating into new groups
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>%
  
  # isolating 2023 and 2024
  filter (year %in% 2023:2024) %>%
  
  # isolating countries of interest
  filter (ccode %in% c("IDN", "BGD", "VNM", "PAK", "PHL")) %>%
  
  # widening to compute change
  pivot_wider (names_from = "year", values_from = c("hc", "exp")) %>%
  
  # keeping only CC
  filter (daily_spending == "CC") %>%
  dplyr::select (-daily_spending) %>%
  
  # computing deltas
  mutate (delta_hc = (hc_2024 - hc_2023) / 10^6, 
          delta_exp = (exp_2024 - exp_2023) / 10^9,
          exp_2023 = exp_2023 / 10^9)
  
  
  
  
  
  
  
  

  
  
  
  















