
## extracting WCO data =========================================================

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, Hmisc, leaflet, geojsonio, broom)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')

## loading in WDPro latest version

# loading raw file
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))

# cleaning into spending groups
wdp <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, Inf))

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

## extracting data (regions)
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


## extracting data (countries)
deltas_hc_nat = wdp %>%
  
  # country-year headcounts and spending per spending group and age
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # creating country groupings
  #dplyr::mutate (ccode = ifelse (ccode == "IND", "IND", 
                                 #ifelse (ccode == "CHN", "CHN", 
                                         #ifelse (ccode %in% rest_of_asia, "Rest of Asia",
                                                 #ifelse (ccode %in% africa, "Africa", 
                                                         #ifelse (ccode %in% latam, "LATAM", "Rest of World"
                                                         #)))))) %>%
  
  # regrouping
  #dplyr::group_by (ccode, year, daily_spending) %>%
  #dplyr::summarise (pop = sum (pop, na.rm = T),
                    #exp = sum (exp, na.rm = T)) %>% ungroup () %>%
  
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
  dplyr::select (ccode, year, delta_hc, delta_exp) %>%
  
  # remove india and china
  filter (!ccode %in% c("IND", "CHN")) %>%
  
  # slice top 12
  arrange (desc (delta_hc)) %>%
  slice_head (n = 12)

write.csv (deltas_hc, "deltas_hc.csv", row.names = F)
write.csv (deltas_hc_nat, "deltas_hc_countries.csv", row.names = F)




