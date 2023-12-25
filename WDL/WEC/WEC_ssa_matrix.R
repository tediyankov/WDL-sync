
## South Africa Emissions vs Income matrix

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode)

## loading paths
base_path <- worlddataverse::get_wdl_path()

## building function to load WEC data
load_wec_dat = function (){
  
  require (worlddataverse)
  
  # loading paths
  base_path <- worlddataverse::get_wdl_path()
  if (is.na (base_path)) {base_path = file.path("/Users",
                                                "teddyyankov",
                                                "Library",
                                                "CloudStorage",
                                                "GoogleDrive-teodor.yankov@worlddata.io",
                                                "Shared drives", 
                                                "DATA_WDL")}
  
  # load binary (Rda) file
  load (file.path (base_path,
                   "world_emissions_clock",
                   "01_data",
                   "WEC_data_binary_20230425.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  .GlobalEnv$base_path = base_path
  
}

# loading WEC data
load_wec_dat()


## sub-saharan africa object
ssa <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", 
         "Cape Verde", "Central African Republic", "Chad", "Comoros", "Congo (Brazzaville)", 
         "Congo (Democratic Republic)", "Côte d'Ivoire", "Djibouti", "Equatorial Guinea", 
         "Eritrea", "Ethiopia", "Gabon", "The Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
         "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", 
         "Mauritius", "Mozambique", "Namibia", "Niger", "Nigeria", "Réunion", "Rwanda", 
         "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", 
         "South Africa", "Sudan", "Swaziland", "Tanzania", "Togo", "Uganda", 
         "Western Sahara", "Zambia", "Zimbabwe")

ssa_iso3c <- countrycode (ssa, "country.name", "iso3c")

## income class objects
low_income <- c("Afghanistan", "Guinea-Bissau", "Somalia", "Burkina Faso", "Korea, Dem. People's Rep", 
                "South Sudan", "Burundi", "Liberia", "Sudan", "Central African Republic", 
                "Madagascar", "Syrian Arab Republic", "Chad", "Malawi", "Togo", "Congo, Dem. Rep", 
                "Mali", "Uganda", "Eritrea", "Mozambique", "Yemen, Rep.", "Ethiopia", "Niger", 
                "Zambia", "Gambia, The", "Rwanda", "Guinea", "Sierra Leone")

low_income_iso3c <- countrycode (low_income, "country.name", "iso3c")

mid_income <- c("Angola", "India", "Philippines", "Algeria", "Indonesia", "Samoa", 
                "Bangladesh", "Iran, Islamic Rep", "São Tomé and Principe", "Benin", 
                "Kenya", "Senegal", "Bhutan", "Kiribati", "Solomon Islands", "Bolivia", 
                "Kyrgyz Republic", "Sri Lanka", "Cabo Verde", "Lao PDR", "Tanzania", "Cambodia", 
                "Lebanon", "Tajikistan", "Cameroon", "Lesotho", "Timor-Leste", "Comoros", 
                "Mauritania", "Tunisia", "Congo, Rep.", "Micronesia, Fed. Sts.", "Ukraine", 
                "Côte d'Ivoire", "Mongolia", "Uzbekistan", "Djibouti", "Morocco", "Vanuatu", 
                "Egypt, Arab Rep.", "Myanmar", "Vietnam", "El Salvador", "Nepal", "West Bank and Gaza", 
                "Eswatini", "Nicaragua", "Zimbabwe", "Ghana", "Nigeria", "Haiti", "Pakistan", 
                "Honduras", "Papua New Guinea")

mid_income_iso3c <- countrycode (mid_income, "country.name", "iso3c")

highmid_income <- c("Andorra", "Greece", "Poland", "Antigua and Barbuda", "Greenland", "Portugal", 
                 "Aruba", "Guam", "Puerto Rico", "Australia", "Hong Kong SAR, China", "Qatar", 
                 "Austria", "Hungary", "Romania", "Bahamas, The", "Iceland", "San Marino", "Bahrain", 
                 "Ireland", "Saudi Arabia", "Barbados", "Isle of Man", "Seychelles", "Belgium", "Israel", 
                 "Singapore", "Bermuda", "Italy", "Sint Maarten (Dutch part)", "British Virgin Islands", 
                 "Japan", "Slovak Republic", "Brunei Darussalam", "Korea, Rep.", "Slovenia", "Canada", 
                 "Kuwait", "Spain", "Cayman Islands", "Latvia", "St. Kitts and Nevis", "Channel Islands", 
                 "Liechtenstein", "St. Martin (French part)", "Chile", "Lithuania", "Sweden", "Croatia", 
                 "Luxembourg", "Switzerland", "Curaçao", "Macao SAR, China", "Taiwan, China", "Cyprus", 
                 "Malta", "Trinidad and Tobago", "Czech Republic", "Monaco", "Turks and Caicos Islands", 
                 "Denmark", "Nauru", "United Arab Emirates", "Estonia", "Netherlands", "United Kingdom", 
                 "Faroe Islands", "New Caledonia", "United States", "Finland", "New Zealand", "Uruguay", 
                 "France", "Northern Mariana Islands", "Virgin Islands (U.S.)", "French Polynesia", "Norway", 
                 "Germany", "Oman", "Gibraltar", "Panama", "Albania", "Fiji", "Namibia", "American Samoa", 
                 "Gabon", "North Macedonia", "Argentina", "Georgia", "Palau", "Armenia", 
                 "Grenada", "Paraguay", "Azerbaijan", "Guatemala", "Peru", "Belarus", 
                 "Guyana", "Russian Federation", "Belize", "Iraq", "Serbia", "Bosnia and Herzegovina", 
                 "Jamaica", "South Africa", "Botswana", "Jordan", "St. Lucia", "Brazil", 
                 "Kazakhstan", "St. Vincent and the Grenadines", "Bulgaria", "Kosovo", "Suriname", 
                 "China", "Libya", "Thailand", "Colombia", "Malaysia", "Tonga", "Costa Rica", "Maldives",
                 "Turkey", "Cuba", "Marshall Islands", "Turkmenistan", "Dominica", "Mauritius", "Tuvalu", 
                 "Dominican Republic", "Mexico", "Equatorial Guinea", "Moldova", "Ecuador", "Montenegro")

highmid_income_iso3c <- countrycode (highmid_income, "country.name", "iso3c")

high_income <- c("Andorra", "Greece", "Poland", "Antigua and Barbuda", "Greenland", 
                 "Portugal", "Aruba", "Guam", "Puerto Rico", "Australia", "Hong Kong SAR, China", 
                 "Qatar", "Austria", "Hungary", "Romania", "Bahamas, The", "Iceland", "San Marino", 
                 "Bahrain", "Ireland", "Saudi Arabia", "Barbados", "Isle of Man", "Seychelles", "Belgium",
                 "Israel", "Singapore", "Bermuda", "Italy", "Sint Maarten (Dutch part)", "British Virgin Islands", 
                 "Japan", "Slovak Republic", "Brunei Darussalam", "Korea, Rep.", 
                 "Slovenia", "Canada", "Kuwait", "Spain", "Cayman Islands", "Latvia", 
                 "St. Kitts and Nevis", "Channel Islands", "Liechtenstein", "St. Martin (French part)", 
                 "Chile", "Lithuania", "Sweden", "Croatia", "Luxembourg", "Switzerland", 
                 "Curaçao", "Macao SAR, China", "Taiwan, China", "Cyprus", "Malta", 
                 "Trinidad and Tobago", "Czech Republic", "Monaco", "Turks and Caicos Islands", 
                 "Denmark", "Nauru", "United Arab Emirates", "Estonia", "Netherlands", "United Kingdom", 
                 "Faroe Islands", "New Caledonia", "United States", "Finland", "New Zealand", "Uruguay", 
                 "France", "Northern Mariana Islands", "Virgin Islands (U.S.)", "French Polynesia", 
                 "Norway", "Germany", "Oman", "Gibraltar", "Panama")

high_income_iso3c <- countrycode (high_income, "country.name", "iso3c")


## cleaning data
wec_dat_ssa = wec_dat %>%
  
  # subsetting for SSA
  filter (iso3c %in% ssa_iso3c) %>%
  
  # aggregating 
  dplyr::group_by (iso3c, year, pop, gdp) %>%
  dplyr::summarise (base = sum (base, na.rm = T)) %>%
  
  # obtaining per capita emissions
  mutate (em_pc = base / pop) %>%
  
  # creating categories
  #mutate (category = ifelse (em_pc < 5, "low emissions", 
                             #ifelse (em_pc >= 5 & em_pc <= 10, "middle emissions", 
                                     #ifelse (em_pc > 10, "high emissions", NA 
                                     #)))) %>%
  
  # keeping only 2023
  filter (year == 2023) %>%
  
  # adding income category var
  mutate (income = ifelse (iso3c %in% low_income_iso3c, "low income", 
                           ifelse (iso3c %in% mid_income_iso3c, "mid income", 
                                   ifelse (iso3c %in% highmid_income_iso3c, "high-middle income", NA
                                   )))) %>%
  
  # computing GDP per capita
  mutate (gdp_pc = gdp / pop)

## creating totals dataframe 
wec_dat_ssa_tot = wec_dat_ssa %>%
  dplyr::group_by (iso3c, year, pop, gdp) %>%
  dplyr::summarise (base = sum (base, na.rm = T)) %>%
  mutate (em_pc = base / pop) %>%
  filter (year == 2023) %>%
  mutate (income = ifelse (iso3c %in% low_income_iso3c, "low income", 
                           ifelse (iso3c %in% mid_income_iso3c, "mid income", 
                                   ifelse (iso3c %in% highmid_income_iso3c, "high-middle income", NA
                                   )))) %>%
  mutate (gdp_pc = gdp / pop) %>%
  mutate (sector = "Total") %>%
  relocate (sector, .before = pop)
  

## joining dfs
wec_dat_ssa = rbind (wec_dat_ssa, wec_dat_ssa_tot)

## building facet plot 
plot1 = ggplot (data = wec_dat_ssa, 
                aes (x = em_pc, y = gdp_pc, col = income)) + 
  geom_point () + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl() + 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    trans='log') +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log') + 
  labs (title = "GDP per capita vs Emissions per capita: Sub-Saharan Africa", 
        subtitle = "All sectors, data from 2023",
        x = "Emissions per capita (log)",
        y = "GDP per capta (log)") + 
  facet_wrap (~sector, scales = "free") + 
  theme(legend.position = "bottom")
plot1

plot2 = ggplot (data = wec_dat_ssa %>% 
                  filter (sector == "Total") %>%
                  mutate (income = ifelse (income == "mid income", "low-middle income", income)) %>%
                  mutate (income = factor (income, levels = c("low income", "low-middle income", "high-middle income"))), 
                aes (x = em_pc, y = gdp_pc, col = income)) + 
  geom_point () + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl() +
  #geom_vline (xintercept = 5) + 
  #geom_vline (xintercept = 10) + 
  #annotate ("label", x = 2.5, y = max (wec_dat_ssa$gdp_pc) + 10000, label = "Low \nEmissions") +
  #annotate ("label", x = 7.1, y = max (wec_dat_ssa$gdp_pc) + 10000, label = "Middle \nEmissions") +
  #annotate ("label", x = 20, y = max (wec_dat_ssa$gdp_pc) + 10000, label = "High \nEmissions") +
  #scale_y_continuous(
   # labels = scales::number_format(accuracy = 0.01),
   # trans='log') +
  #scale_x_continuous(
    #labels = scales::number_format(accuracy = 0.01), 
    #trans='log') + 
  labs (x = "\nEmissions per capita, T (log)",
        y = "\nGDP per capita, USD 2017 PPP (log)",
        title = "GDP per capita vs Emissions per capita: \nSub-Saharan Africa",
        subtitle = "All sectors, 2023") + 
  theme (axis.title = element_text (size = 12))
  
plot2


df <- data.frame(x=c(2, 5, 6, 7, 9, 13, 14, 16, 18),
                 y=c(1400, 1700, 2300, 2500, 2800, 2900, 3400, 3900, 11000))

#create scatterplot with log scale on y-axis
ggplot(df, aes(x=x, y=y)) +
  geom_point() + 
  scale_y_continuous (trans='log10')


## troubleshooting =============================================================

## how many people in SSA? 

ssa_dat = wec_dat %>%
  
  # isolating SSA
  filter (iso3c %in% ssa_iso3c) %>%
  
  # isolating year
  filter (year == 2023) %>%
  
  # isolating population
  dplyr::group_by (iso3c, year) %>%
  dplyr::summarise (pop = mean (pop, na.rm = T))

## trying to build df with just income
ssa_dat_income = ssa_dat %>%
  
  # adding income variable
  mutate (income = ifelse (iso3c %in% low_income_iso3c, "low income", 
                           ifelse (iso3c %in% mid_income_iso3c, "mid income", 
                                   ifelse (iso3c %in% highmid_income_iso3c, "high-middle income", NA
                                   )))) %>%
  
  # summing population over income group
  dplyr::group_by (income) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T))

## creating second matrix
wec_dat_ssa = wec_dat_ssa %>% filter (sector == "Total") %>% mutate (iso3c = countrycode (iso3c, "iso3c", "country.name"))

## isolating countries
low_income_low_emissions <- wec_dat_ssa$iso3c [wec_dat_ssa$income == "low income" & wec_dat_ssa$em_pc < 5]
lowmid_income_low_emissions <- wec_dat_ssa$iso3c [wec_dat_ssa$income == "mid income" & wec_dat_ssa$em_pc < 5]
highmid_income_low_emissions <-wec_dat_ssa$iso3c [wec_dat_ssa$income == "high-middle income" & wec_dat_ssa$em_pc < 5]

low_income_mid_emissions <- wec_dat_ssa$iso3c [wec_dat_ssa$income == "low income" & wec_dat_ssa$em_pc >= 5 & wec_dat_ssa$em_pc <= 10]
lowmid_income_mid_emissions <- wec_dat_ssa$iso3c [wec_dat_ssa$income == "mid income" & wec_dat_ssa$em_pc >= 5 & wec_dat_ssa$em_pc <= 10]
highmid_income_mid_emissions <- wec_dat_ssa$iso3c [wec_dat_ssa$income == "high-middle income" & wec_dat_ssa$em_pc >= 5 & wec_dat_ssa$em_pc <= 10]

low_income_high_emissions <- wec_dat_ssa$iso3c [wec_dat_ssa$income == "low income" & wec_dat_ssa$em_pc > 10]
lowmid_income_high_emissions <- wec_dat_ssa$iso3c [wec_dat_ssa$income == "mid income" & wec_dat_ssa$em_pc > 10]
highmid_income_high_emissions <- wec_dat_ssa$iso3c [wec_dat_ssa$income == "high-middle income" & wec_dat_ssa$em_pc > 10]


## MISC ------------------------------------------------------------------------

wec_dat_midincome_lowemissions = wec_dat %>%
  
  # aggregating 
  dplyr::group_by (iso3c, year, pop, gdp) %>%
  dplyr::summarise (base = sum (base, na.rm = T)) %>%
  
  # obtaining per capita emissions
  mutate (em_pc = base / pop) %>%
  
  # creating categories
  #mutate (category = ifelse (em_pc < 5, "low emissions", 
  #ifelse (em_pc >= 5 & em_pc <= 10, "middle emissions", 
  #ifelse (em_pc > 10, "high emissions", NA 
  #)))) %>%
  
  # keeping only 2023
  filter (year == 2023) %>%
  
  # adding income category var
  mutate (income = ifelse (iso3c %in% low_income_iso3c, "low income", "not-low income")) %>%
  
  # computing GDP per capita
  mutate (gdp_pc = gdp / pop) %>%
  
  # high income, low emissions
  filter (income == "low income") %>%
  filter (em_pc >= 5 & em_pc <= 10) %>%
  
  # mutate into country names
  mutate (country = countrycode (iso3c, "iso3c", "country.name"))

wec_dat1 = wec_dat %>%
  filter (year == 2022) %>%
  dplyr::group_by (iso3c, year, gas, pop, gdp) %>%
  dplyr::summarise (base = sum (base, na.rm = T)) %>%
  dplyr::group_by (gas) %>%
  dplyr::summarise (base = sum (base, na.rm = T))







