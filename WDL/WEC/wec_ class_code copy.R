
## WEC: emissions x income classification dataframe ============================

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

## income class objects
low_income = c("Afghanistan", "Korea, Dem. People's Rep", "South Sudan", "Burkina Faso", 
               "Liberia", "Sudan", "Burundi", "Madagascar", "Syrian Arab Republic", 
               "Central African Republic", "Malawi", "Togo", "Chad", "Mali", "Uganda", 
               "Congo, Dem. Rep", "Mozambique", "Yemen, Rep.", "Eritrea", "Niger", 
               "Ethiopia", "Rwanda", "Gambia, The", "Sierra Leone", "Guinea-Bissau", "Somalia")

low_income_iso3c <- countrycode (low_income, "country.name", "iso3c")

mid_income = c("Angola", "Jordan", "Philippines", "Algeria", "India", "Samoa", 
               "Bangladesh", "Iran, Islamic Rep", "São Tomé and Principe", "Benin", 
               "Kenya", "Senegal", "Bhutan", "Kiribati", "Solomon Islands", "Bolivia", 
               "Kyrgyz Republic", "Sri Lanka", "Cabo Verde", "Lao PDR", "Tanzania", 
               "Cambodia", "Lebanon", "Tajikistan", "Cameroon", "Lesotho", "Timor-Leste", 
               "Comoros", "Mauritania", "Tunisia", "Congo, Rep.", "Micronesia, Fed. Sts.", 
               "Ukraine", "Côte d'Ivoire", "Mongolia", "Uzbekistan", "Djibouti", "Morocco", 
               "Vanuatu", "Egypt, Arab Rep.", "Myanmar", "Vietnam", "Eswatini", "Nepal", 
               "Zambia", "Ghana", "Nicaragua", "Zimbabwe", "Guinea", "Nigeria", "Haiti", 
               "Pakistan", "Honduras", "Papua New Guinea", "Albania", "Fiji", "North Macedonia", 
               "Argentina", "Gabon", "Palau", "Armenia", "Georgia", "Paraguay", "Azerbaijan", 
               "Grenada", "Peru", "Belarus", "Guatemala", "Russian Federation", "Belize", 
               "Indonesia", "Serbia", "Bosnia and Herzegovina", "Iraq", "South Africa", 
               "Botswana", "Jamaica", "St. Lucia", "Brazil", "Kazakhstan", 
               "St. Vincent and the Grenadines", "Bulgaria", "Kosovo", "Suriname",
               "China", "Libya", "Thailand", "Colombia", "Malaysia", "Tonga", "Costa Rica", 
               "Maldives", "Turkey", "Cuba", "Marshall Islands", "Turkmenistan", "Dominica", 
               "Mauritius", "Tuvalu", "Dominican Republic", "Mexico", "West Bank and Gaza", 
               "El Salvador", "Moldova", "Equatorial Guinea", "Montenegro", "Ecuador", "Namibia")

mid_income_iso3c <- countrycode (mid_income, "country.name", "iso3c")

high_income = c("American Samoa", "Germany", "Oman", "Andorra", "Gibraltar", "Panama", 
                "Antigua and Barbuda", "Greece", "Poland", "Aruba", "Greenland", "Portugal", 
                "Australia", "Guam", "Puerto Rico", "Austria", "Hong Kong SAR, China", "Qatar", 
                "Bahamas, The", "Hungary", "Romania", "Bahrain", "Iceland", "San Marino", 
                "Barbados", "Ireland", "Saudi Arabia", "Belgium", "Isle of Man", "Seychelles", 
                "Bermuda", "Israel", "Singapore", "British Virgin Islands", "Italy", 
                "Sint Maarten (Dutch part)", "Brunei Darussalam", "Japan", "Slovak Republic", 
                "Canada", "Korea, Rep.", "Slovenia", "Cayman Islands", "Kuwait", "Spain", 
                "Channel Islands", "Latvia", "St. Kitts and Nevis", "Chile", "Liechtenstein", 
                "St. Martin (French part)", "Croatia", "Lithuania", "Sweden", "Curaçao", 
                "Luxembourg", "Switzerland", "Cyprus", "Macao SAR, China", "Taiwan, China", 
                "Czech Republic", "Malta", "Trinidad and Tobago", "Denmark", "Monaco", 
                "Turks and Caicos Islands", "Estonia", "Nauru", "United Arab Emirates", 
                "Faroe Islands", "Netherlands", "United Kingdom", "Finland", "New Caledonia", 
                "United States", "France", "New Zealand", "Uruguay", "French Polynesia", 
                "Northern Mariana Islands", "Virgin Islands (U.S.)", "Guyana", "Norway")

high_income_iso3c <- countrycode (high_income, "country.name", "iso3c")


## building classification table
wec_classification = wec_dat %>%
  
  # general aggregation
  dplyr::group_by (iso3c, year, pop) %>%
  dplyr::summarise (emissions = sum (base, na.rm = T)) %>%
  
  # computing emissions per capita
  mutate (em_pc = emissions / pop) %>%
  
  # converting to country name
  #mutate (country = countrycode (iso3c, "iso3c", "country.name")) %>%
  
  # creating income classifier variable
  mutate (income_class = ifelse (iso3c %in% low_income_iso3c, "low income", 
                           ifelse (iso3c %in% mid_income_iso3c, "middle income", 
                                   ifelse (iso3c %in% high_income_iso3c, "high income", "unclassified"
                                   )))) %>%
  
  # adding emissions classifier variable 
  mutate (emissions_class = ifelse (em_pc < 5, "low emissions", 
                                    ifelse (em_pc >= 5 & em_pc <= 10, "middle emissions", 
                                            ifelse (em_pc > 10, "high emissions", NA 
                                            ))))

wec_classification_ndc = wec_dat %>%
  
  # general aggregation
  dplyr::group_by (iso3c, year, pop) %>%
  dplyr::summarise (emissions = sum (ndc, na.rm = T)) %>%
  
  # computing emissions per capita
  mutate (em_pc = emissions / pop) %>%
  
  # converting to country name
  #mutate (country = countrycode (iso3c, "iso3c", "country.name")) %>%
  
  # creating income classifier variable
  mutate (income_class = ifelse (iso3c %in% low_income_iso3c, "low income", 
                                 ifelse (iso3c %in% mid_income_iso3c, "middle income", 
                                         ifelse (iso3c %in% high_income_iso3c, "high income", "unclassified"
                                         )))) %>%
  
  # adding emissions classifier variable 
  mutate (emissions_class = ifelse (em_pc < 5, "low emissions", 
                                    ifelse (em_pc >= 5 & em_pc <= 10, "middle emissions", 
                                            ifelse (em_pc > 10, "high emissions", NA 
                                            ))))

wec_classification_1.5 = wec_dat %>%
  
  # general aggregation
  dplyr::group_by (iso3c, year, pop) %>%
  dplyr::summarise (emissions = sum (o_1p5c, na.rm = T)) %>%
  
  # computing emissions per capita
  mutate (em_pc = emissions / pop) %>%
  
  # converting to country name
  #mutate (country = countrycode (iso3c, "iso3c", "country.name")) %>%
  
  # creating income classifier variable
  mutate (income_class = ifelse (iso3c %in% low_income_iso3c, "low income", 
                                 ifelse (iso3c %in% mid_income_iso3c, "middle income", 
                                         ifelse (iso3c %in% high_income_iso3c, "high income", "unclassified"
                                         )))) %>%
  
  # adding emissions classifier variable 
  mutate (emissions_class = ifelse (em_pc < 5, "low emissions", 
                                    ifelse (em_pc >= 5 & em_pc <= 10, "middle emissions", 
                                            ifelse (em_pc > 10, "high emissions", NA 
                                            ))))

## exporting
write.csv (wec_classification, "wec_classification.csv", row.names = F)
write.csv (wec_classification_ndc, "wec_classification_ndc.csv", row.names = F)
write.csv (wec_classification_1.5, "wec_classification_1.5.csv", row.names = F)

gdp_sectordat <- read.csv (file.path (base_path, "product_categories", "trade_model", "01_input_data", "itpde", "ITPD_E_R02.csv"))












