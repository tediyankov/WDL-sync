
## code for data viz for white paper (author: Teddy) ===========================

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, ggrepel)

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

## building chart for bullet point 1  ------------------------------------------

## adding continent var to wec_classsification
wec_classification$continent <- countrycode (wec_classification$iso3c, "iso3c", "continent")

## adding GNI per capita

# cleaning input data 
API_NY_GNP_MKTP_PP_CD_DS2_en_excel_v2_5728903 <- read_excel("API_NY.GNP.MKTP.PP.CD_DS2_en_excel_v2_5728903.xls")

# making the third row the header row
names (API_NY_GNP_MKTP_PP_CD_DS2_en_excel_v2_5728903) <- API_NY_GNP_MKTP_PP_CD_DS2_en_excel_v2_5728903 [3,]
API_NY_GNP_MKTP_PP_CD_DS2_en_excel_v2_5728903 <- API_NY_GNP_MKTP_PP_CD_DS2_en_excel_v2_5728903 [-c(1:3),]

# reformatting
gni = API_NY_GNP_MKTP_PP_CD_DS2_en_excel_v2_5728903 %>%
  
  # selecting relevant vars
  dplyr::select (`Country Code`, starts_with ("20")) %>%
  
  # renaming country var
  rename (ccode = `Country Code`) %>%
  
  # Convert columns to long format
  pivot_longer (cols = -ccode, names_to = "Year", values_to = "GNI") %>%
  
  # Convert Year to numeric
  mutate (Year = as.numeric (Year)) %>%
  
  # Group by Country and filter for the most recent Year
  dplyr::group_by (ccode) %>%
  filter (Year == max (Year, na.rm = TRUE)) %>%
  
  # Select the relevant columns
  select (-Year) %>%
  
  # Rename the resulting column
  rename (Most_Recent_GNI = GNI) %>%
  
  # Ungroup the data
  ungroup()

# identifying NAs
gni_nas = gni %>% 
  filter (is.na (Most_Recent_GNI)) %>% 
  mutate (country = countrycode (ccode, "iso3c", "country.name")) %>%
  drop_na (country)

# filling the NAs
gni_nas$Most_Recent_GNI <- c(4433.41, 
                             67688.24,
                             NA, 
                             NA,
                             8784.58,
                             NA, 
                             3640.38, 
                             3796.66, 
                             5164.76, 
                             NA, 
                             NA, 
                             NA, 
                             NA, 
                             NA, 
                             74957.31, 
                             NA, 
                             48702.05,
                             NA, 
                             NA, 
                             NA, 
                             NA, 
                             280.47, 
                             NA, 
                             NA, 
                             2060.33, 
                             11604.30, 
                             1509.33, 
                             NA, 
                             94843.83, 
                             758.73, 
                             493557.72, 
                             NA, 
                             NA, 
                             NA) * 10^6

gni_nas = gni_nas %>% drop_na (Most_Recent_GNI)

# final df with GNI values filled as much as possible
gni_new = rbind (gni %>% drop_na(), gni_nas %>% dplyr::select (-country)) %>% mutate (year = 2023) %>% rename (iso3c = ccode)

# joining w/ wec_classification
wec_classification_wgni = wec_classification %>% 
  left_join (gni_new, by = c("iso3c", "year")) %>% 
  filter (year == 2023) %>%
  drop_na (Most_Recent_GNI) %>%
  
  # converting GNI to numeric
  mutate (Most_Recent_GNI = as.numeric (Most_Recent_GNI)) %>%
  
  # computing per capita
  mutate (gni_pc = Most_Recent_GNI / pop)
  

## building scatter plot w/ continent
ggplot (data = wec_classification_wgni, 
        aes (x = em_pc, 
             y = gni_pc, 
             colour = continent)) + 
  
  # plotting points
  geom_point () + 
  
  # plotting thresholds
  geom_hline (yintercept = c(1085, 13205)) +
  geom_vline (xintercept = c(5, 10)) + 
  
  #scale_x_continuous (trans = "log") + 
  #scale_y_continuous (trans = "log") + 
  
  # adding labels 
  #geom_label_repel(aes (label = iso3c),
                   #box.padding   = 0.35, 
                   #point.padding = 0.5,
                   #segment.color = 'grey50') +
  
  # adding title and labels 
  labs (title = "Emissions-Income matrix (colour-coded by Continent)",
        subtitle = "Plotting the world in 2023", 
        y = "\nGNI per capita, USD (Atlas Method)",
        x = "\nEmissions per capita, T") + 
  
  # adjusting scales
  #ylim (50000,75000) + 
  #xlim (-20, 50) +
  
  # adding theme
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl()

## building scatter plot w/ population

quants <- quantile (wec_classification_wgni$pop, probs = seq(0, 1, 1/4))
wec_classification_wgni$quant  <- with (wec_classification_wgni, factor (ifelse (pop < quants[1], 0, 
                                                                                 ifelse (pop < quants[2], 1, 
                                                                                         ifelse (pop < quants[3], 2, 
                                                                                                 ifelse (pop < quants[4], 3, 
                                                                                                         ifelse (pop < quants[5], 4, 5 
                                                                                                         )))))))

ggplot (data = wec_classification_wgni, 
        aes (x = em_pc, 
             y = gni_pc, 
             colour = quant)) + 
  
  # plotting points
  geom_point () + 
  
  # plotting thresholds
  geom_hline (yintercept = c(1085, 13205)) +
  geom_vline (xintercept = c(5, 10)) + 
  
  #scale_x_continuous (trans = "log") + 
  #scale_y_continuous (trans = "log") + 
  
  # adding labels 
  #geom_label_repel(aes (label = iso3c),
  #box.padding   = 0.35, 
  #point.padding = 0.5,
  #segment.color = 'grey50') +
  
  # adding title and labels 
labs (title = "Emissions-Income matrix (colour-coded by Population)",
      subtitle = "Plotting the world in 2023", 
      y = "\nGNI per capita, USD (Atlas Method)",
      x = "\nEmissions per capita, T") + 
  
  # adjusting scales
  #ylim (50000,75000) + 
  #xlim (-20, 50) +
  
  # adding theme
  worlddataverse::theme_wdl() +
  scale_colour_manual(values = c("#FAAE7B", "#CC8B79", "#9F6976", "#714674", "#432371"), 
                      labels = c("0 - 104k", "104k - 2.9m", "2.9m - 9.5m", "9.5m - 34.3m", "34.3m - 1.4b"))


## building chart for bullet point 2  ------------------------------------------

wec_classification_wgni = wec_classification %>% 
  
  left_join (gni_new, by = c("iso3c", "year")) %>% 
  
  drop_na (Most_Recent_GNI) %>%
  
  # converting GNI to numeric
  mutate (Most_Recent_GNI = as.numeric (Most_Recent_GNI)) %>%
  
  # computing per capita
  mutate (gni_pc = Most_Recent_GNI / pop)

## scatter plot w labels
ggplot (data = wec_classification_wgni %>% 
          filter (income_class == "high income" & emissions_class == "low emissions" & year == 2023), 
        aes (x = em_pc, 
             y = gni_pc)) + 
  
  # plotting points
  geom_point () + 
  
  # plotting thresholds
  #geom_hline (yintercept = c(1085, 13205)) +
  #geom_vline (xintercept = c(5, 10)) + 
  
  #scale_x_continuous (trans = "log") + 
  #scale_y_continuous (trans = "log") + 
  
  # adding labels 
  geom_label_repel(aes (label = iso3c),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  
  # adding title and labels 
labs (title = "Emissions-Income matrix (colour-coded by Continent)",
      subtitle = "Plotting the world in 2023", 
      y = "\nGNI per capita, USD (Atlas Method)",
      x = "\nEmissions per capita, T") + 
  
  # adjusting scales
  #ylim (50000,75000) + 
  #xlim (-20, 50) +
  
  # adding theme
  worlddataverse::theme_wdl()






