
## WEC ADB slides ==============================================================

## chapter 1: prelims ----------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, viridis, stringr, ggthemes, 
                ggalt, maps, rgeos, maptools, grid, rworldmap, cowplot,
                gridExtra,ggmap, ggrepel)


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

## Chapter 2: emissions-income world map ---------------------------------------

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
  dplyr::group_by (iso3c, year, pop, gdp) %>%
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

## note: we are using wec_classification from wec_class_code.R
wec_classification_base = wec_classification %>%
  
  # keeping relevant variables
  dplyr::select (iso3c, year, income_class, emissions_class) %>%
  
  # keeping only 2023
  filter (year == 2023)



## building map

# drawing blank world map
worldmap <- map_data (map = "world")

# changing iso3c variable to country names
wec_classification_base = wec_classification %>%
  
  # keeping relevant variables
  dplyr::select (iso3c, year, income_class, emissions_class) %>%
  
  # keeping only 2023
  filter (year == 2023) %>%
  
  # filter out unclassified
  filter (!income_class == "unclassified") %>%
  
  # changing iso3c to country names
  # mutate (iso3c = countrycode (iso3c, "iso3c", "country.name")) %>%
  
  # creating one classifier variable
  unite ("income_emissions", 3:4, sep = "_") %>%
  
  # keeping only relevant vars
  dplyr::select (-year) %>%
  
  # dropping NAs
  drop_na () %>% ungroup () %>% dplyr::select (-year) %>%
  
  # changing levels of classification variable
  mutate (income_emissions = factor (income_emissions, levels = c("low income_low emissions",
                                                                  "low income_middle emissions",
                                                                  "low income_high emissions",
                                                                  "middle income_low emissions",
                                                                  "middle income_middle emissions",
                                                                  "middle income_high emissions",
                                                                  "high income_low emissions",
                                                                  "high income_middle emissions",
                                                                  "high income_high emissions",
                                                                  "unclassified_low emissions",
                                                                  "unclassified_middle emissions")))

## creatng spatial polygon data frame 
world_map <- joinCountryData2Map (wec_classification_base, joinCode = "ISO3", nameJoinColumn = "iso3c")

## building map 
rworldmap::mapCountryData (world_map, 
                           nameColumnToPlot="income_emissions",
                           catMethod ='categorical',
                           colourPalette = c("#75FA4C",
                                             "#FAD749", 
                                             "#EB5149", 
                                             "#5DC93B", 
                                             "#C8AD39", 
                                             "#BD413A", 
                                             "#3C8825", 
                                             "#C59635",
                                             "#802C28",
                                             "black", 
                                             "white"),
                           mapTitle = "Bivariate Choropleth Map: Income and Emissions Typologies",
                           addLegend = F)




## Chapter 3: Scatter plot -----------------------------------------------------

## subsetting df
wec_sasia_em_inc = wec_classification %>%
  
  # subsetting for 2023 and S Asia countries
  filter (year == 2023) %>%
  filter (iso3c %in% c("BGD", "BTN", "IND", "LKA", "MDV", "NPL")) %>%
  
  # adding info on GNI
  mutate (gni_pc = ifelse (iso3c == "BGD", 2820, 
                           ifelse (iso3c == "BTN", 3040, 
                                   ifelse (iso3c == "IND", 2380, 
                                           ifelse (iso3c == "LKA", 3610, 
                                                   ifelse (iso3c == "MDV", 11030, 
                                                           ifelse (iso3c == "NPL", 1340, NA 
                                                           ))))))) %>%
  
  # adding country labels
  mutate (country = countrycode (iso3c, "iso3c", "country.name"))

## creating plot
ggplot (data = wec_sasia_em_inc, 
        aes (x = em_pc, 
             y = gni_pc)) + 
  
  # adjusting scales
  ylim (0,15000) + 
  xlim (0, 15) +
  
  # plotting points
  geom_point () + 
  
  # plotting thresholds
  geom_hline (yintercept = c(1085, 13205)) +
  geom_vline (xintercept = c(5, 10)) + 
  
  # adding labels 
  geom_label_repel(aes(label = country),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  
  # adding title and labels 
  labs (title = "Emissions-Income matrix",
        subtitle = "Plotting South Asia in 2023", 
        y = "\nGNI per capita, USD (Atlas Method)",
        x = "\nEmissions per capita, T") + 
  
  # adding theme
  worlddataverse::theme_wdl()
  







## Chapter 4: adding Asia, South Asia and India to the prosperea bars ----------

## sectoral rankings
# function
fun_sect_int_double <- function (year) {
  
  year = year
  
  # OECD countries list
  OECD = c("Austria", "Belgium", "Czech Republic", "Denmark", "Estonia",
           "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
           "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
           "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic",
           "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom",
           "Canada", "Chile", "Colombia", "Mexico", "Costa Rica",
           "United States", "Australia", "Japan", "Korea", "New Zealand",
           "Israel", "Turkey")
  
  # converting to ISO3 country codes
  OECD_ISO3 = countrycode (sourcevar = OECD,
                           origin = "country.name",
                           destination = "iso3c",
                           warn = TRUE,
                           nomatch = NA)
  
  # sub-setting round 1
  wec_dat_oecd = wec_dat %>%
    filter (iso3c %in% OECD_ISO3)
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base, na.rm = TRUE)) %>%
    drop_na() %>%
    unite ("ID", iso3c:sector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_int = emissions / gdp) %>%
    mutate (gdp_pc = gdp / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = 30000
  
  # setting pop threshold
  wec_dat_oecd_2$pop_threshold = 5000000
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2 %>%
    filter (pop >= pop_threshold) %>%
    filter (gdp_pc >= gdp_threshold)
  
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_int)],by = sector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_int)],by = sector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_int)],by = sector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "emissions", "pop")) %>%
    mutate (em_pc = emissions / pop) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (sector) %>%
    dplyr::select ("year", "sector", "iso3c", "country", "em_pc") %>%
    mutate (rank = rep(c(1,2,3), times = 5)) %>%
    relocate (rank, .before = iso3c)
  
}
dat_sect <- fun_sect_int_double (2023)

# needed elements to get Spain slice for Energy sector

# OECD countries list
OECD = c("Austria", "Belgium", "Czech Republic", "Denmark", "Estonia",
         "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
         "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
         "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic",
         "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom",
         "Canada", "Chile", "Colombia", "Mexico", "Costa Rica",
         "United States", "Australia", "Japan", "Korea", "New Zealand",
         "Israel", "Turkey")

# converting to ISO3 country codes
OECD_ISO3 = countrycode (sourcevar = OECD,
                         origin = "country.name",
                         destination = "iso3c",
                         warn = TRUE,
                         nomatch = NA)

# sub-setting round 1
wec_dat_oecd = wec_dat %>%
  filter (iso3c %in% OECD_ISO3)

wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == 2022,]

# sub-setting round 2
wec_dat_oecd_2 = wec_dat_oecd %>%
  dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
  group_by (year, iso3c, sector, pop, gdp) %>%
  summarise (emissions = sum(base, na.rm = TRUE)) %>%
  drop_na() %>%
  unite ("ID", iso3c:sector, sep = "_", remove = F, na.rm = F) %>%
  mutate (em_int = emissions / gdp) %>%
  mutate (gdp_pc = gdp / pop)

# setting GDP threshold
wec_dat_oecd_2$gdp_threshold = 30000

# setting pop threshold
wec_dat_oecd_2$pop_threshold = 5000000

# first minimum emissions
wec_dat_oecd_3 = wec_dat_oecd_2 %>%
  filter (pop >= pop_threshold) %>%
  filter (gdp_pc >= gdp_threshold) %>%
  filter (sector == "Energy") %>%
  dplyr::select (c("year", "iso3c", "sector", "emissions", "pop")) %>%
  mutate (em_pc = emissions / pop) %>%
  # adding country names
  mutate (country = countrycode (sourcevar = iso3c,
                                 origin = "iso3c",
                                 destination = "country.name",
                                 warn = TRUE,
                                 nomatch = NA)) %>%
  relocate (country, .before = sector)

pangea = dat_sect %>%
  filter (country %in% c("South Korea", "Sweden", "Switzerland", "United Kingdom", "Netherlands")) %>%
  slice (1,2,3,6,8) %>%
  dplyr::select(-rank)

sum (pangea$em_pc) # 3.1

## bar chart
# world avg data input
world_avg = wec_dat %>%
  filter (year == 2023) %>%
  group_by (year, iso3c, sector) %>%
  summarise (base = sum (base, na.rm = T),
             pop = mean (pop, na.rm = T)) %>%
  ungroup () %>%
  group_by (year, sector) %>%
  summarise (base = sum (base, na.rm = T),
             pop = sum (pop, na.rm = T)) %>%
  mutate (pop = replace (pop, sector == "Transport", pop [sector == "Transport"] / 2),
          em_pc = base / pop) %>%
  ungroup () %>%
  dplyr::select (sector, em_pc)

# OECD avg data input
fun_OECD = function (year) {
  
  year = year
  
  # OECD countries list
  OECD = c("Austria", "Belgium", "Czech Republic", "Denmark", "Estonia",
           "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
           "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
           "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic",
           "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom",
           "Canada", "Chile", "Colombia", "Mexico", "Costa Rica",
           "United States", "Australia", "Japan", "Korea", "New Zealand",
           "Israel", "Turkey")
  
  # converting to ISO3 country codes
  OECD_ISO3 = countrycode (sourcevar = OECD,
                           origin = "country.name",
                           destination = "iso3c",
                           warn = TRUE,
                           nomatch = NA)
  
  # sub-setting round 1
  wec_dat_oecd = wec_dat %>%
    filter (iso3c %in% OECD_ISO3)
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  oecd = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    mutate (em_pc = emissions / pop) %>%
    group_by (year, sector) %>%
    summarise (em_pc = mean (em_pc, na.rm = T))
  
}
OECD_avg <- fun_OECD (2023) %>% ungroup %>% dplyr::select (-year)

## EU avg data input
fun_EU = function (year) {
  
  year = year
  
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
  
  wec_dat_eu = wec_dat %>%
    filter (iso3c %in% EU_ISO3)
  
  wec_dat_eu = wec_dat_eu [wec_dat_eu$year == year, ]
  
  eu = wec_dat_eu %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base, na.rm = T)) %>%
    mutate (em_pc = emissions / pop) %>%
    group_by (year, sector) %>%
    summarise (em_pc = weighted.mean (em_pc, pop, na.rm = T))
  
}

EU_avg <- fun_EU (2023) %>% ungroup %>% dplyr::select (-year)

## sum value (should get 8.2 ideally)
sum (EU_avg$em_pc)

# North America data input
fun_NA = function (year) {
  
  year = year
  
  north_america = c("United States")
  north_america_ISO3 = countrycode (sourcevar = north_america,
                                    origin = "country.name",
                                    destination = "iso3c",
                                    warn = TRUE,
                                    nomatch = NA)
  wec_dat_na = wec_dat %>%
    filter (iso3c %in% north_america_ISO3)
  
  wec_dat_na = wec_dat_na [wec_dat_na$year == year, ]
  
  na = wec_dat_na %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base, na.rm = T)) %>%
    mutate (em_pc = emissions / pop) %>%
    group_by (year, sector) %>%
    summarise (em_pc = mean (em_pc, na.rm = T))
  
}
NA_avg <- fun_NA (2023) %>% ungroup %>% dplyr::select (-year)

# Pangea data input
pangea_avg = pangea %>% dplyr::select (sector, em_pc)

# Saudi Arabia 
fun_SA = function (year) {
  
  year = year
  
  saudi = c("Saudi Arabia")
  saudi_ISO3 = countrycode (sourcevar = saudi,
                            origin = "country.name",
                            destination = "iso3c",
                            warn = TRUE,
                            nomatch = NA)
  wec_dat_sa = wec_dat %>%
    filter (iso3c %in% saudi_ISO3)
  
  wec_dat_sa = wec_dat_sa [wec_dat_sa$year == year, ]
  
  sa = wec_dat_sa %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base, na.rm = T)) %>%
    mutate (em_pc = emissions / pop) %>%
    group_by (year, sector) %>%
    summarise (em_pc = mean (em_pc, na.rm = T))
  
}
SA_avg <- fun_SA (2023) %>% ungroup %>% dplyr::select (-year)

# Australia
fun_AUS = function (year) {
  
  year = year
  
  aus = c("Australia")
  aus_ISO3 = countrycode (sourcevar = aus,
                          origin = "country.name",
                          destination = "iso3c",
                          warn = TRUE,
                          nomatch = NA)
  wec_dat_AUS = wec_dat %>%
    filter (iso3c %in% aus_ISO3)
  
  wec_dat_AUS = wec_dat_AUS [wec_dat_AUS$year == year, ]
  
  AUS = wec_dat_AUS %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base, na.rm = T)) %>%
    mutate (em_pc = emissions / pop) %>%
    group_by (year, sector) %>%
    summarise (em_pc = mean (em_pc, na.rm = T))
  
}
AUS_avg <- fun_AUS (2023) %>% ungroup %>% dplyr::select (-year)

# unified input data frame
bar_input = rbind (world_avg, EU_avg, OECD_avg, NA_avg, AUS_avg, SA_avg, pangea_avg)
bar_input$case = rep (c("World", "EU", "OECD", "USA", "Australia", "Saudi Arabia", "Pangea"), each = 5)
bar_input$case = factor (bar_input$case,
                         levels = c("World", "EU", "OECD", "USA", "Australia", "Saudi Arabia", "Pangea"))

## adding Prosperea NDC
fun_sect_int_double_2 <- function (year) {
  
  year = year
  
  # OECD countries list
  OECD = c("Austria", "Belgium", "Czech Republic", "Denmark", "Estonia",
           "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
           "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
           "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic",
           "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom",
           "Canada", "Chile", "Colombia", "Mexico", "Costa Rica",
           "United States", "Australia", "Japan", "Korea", "New Zealand",
           "Israel", "Turkey")
  
  # converting to ISO3 country codes
  OECD_ISO3 = countrycode (sourcevar = OECD,
                           origin = "country.name",
                           destination = "iso3c",
                           warn = TRUE,
                           nomatch = NA)
  
  # sub-setting round 1
  wec_dat_oecd = wec_dat %>%
    filter (iso3c %in% OECD_ISO3)
  
  wec_dat_oecd = wec_dat_oecd [wec_dat_oecd$year == year,]
  
  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(ndc, na.rm = TRUE)) %>%
    drop_na() %>%
    unite ("ID", iso3c:sector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_int = emissions / gdp) %>%
    mutate (gdp_pc = gdp / pop)
  
  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = 30000
  
  # setting pop threshold
  wec_dat_oecd_2$pop_threshold = 5000000
  
  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2 %>%
    filter (pop >= pop_threshold) %>%
    filter (gdp_pc >= gdp_threshold)
  
  highWlowE_1 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_int)],by = sector]
  
  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_1$ID))
  highWlowE_2 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_int)],by = sector]
  
  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWlowE_2$ID))
  highWlowE_3 = setDT(wec_dat_oecd_5)[,.SD[which.min(em_int)],by = sector]
  
  # binding into 1 data frame containing rankings per sub-sector
  highWlowE = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "emissions", "pop")) %>%
    mutate (em_pc = emissions / pop) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (sector) %>%
    dplyr::select ("year", "sector", "iso3c", "country", "em_pc") %>%
    mutate (rank = rep(c(1,2,3), times = 5)) %>%
    relocate (rank, .before = iso3c)
  
}
dat_sect2 <- fun_sect_int_double_2 (2030)

pangea2 = dat_sect2 %>%
  filter (country %in% c("South Korea", "Sweden", "Switzerland", "United Kingdom", "Netherlands")) %>%
  slice (1,2,3,6,8) %>%
  dplyr::select(-rank)

sum (pangea2$em_pc) # 2.2

## adding Asia
# Australia
fun_ASIA = function (year) {
  
  year = year
  
  wec_dat_ASIA = wec_dat %>%
    mutate (continent = countrycode (iso3c, "iso3c", "continent")) %>%
    filter (continent == "Asia")
  
  wec_dat_ASIA = wec_dat_ASIA [wec_dat_ASIA$year == year, ]
  
  ASIA = wec_dat_ASIA %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base, na.rm = T)) %>%
    mutate (em_pc = emissions / pop) %>%
    group_by (year, sector) %>%
    summarise (em_pc = mean (em_pc, na.rm = T))
  
}

ASIA_avg <- fun_ASIA (2023) %>% ungroup %>% dplyr::select (-year)

## adding India
fun_IND = function (year) {
  
  year = year
  
  india = "IND"
  
  wec_dat_IND = wec_dat %>%
    filter (iso3c %in% india)
  
  wec_dat_IND = wec_dat_IND [wec_dat_IND$year == year, ]
  
  AUS = wec_dat_IND %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base, na.rm = T)) %>%
    mutate (em_pc = emissions / pop) %>%
    group_by (year, sector) %>%
    summarise (em_pc = mean (em_pc, na.rm = T))
  
}
IND_avg <- fun_IND (2023) %>% ungroup %>% dplyr::select (-year)

## adding South Asia
fun_SASIA = function (year) {
  
  year = year
  
  sasia = c("BGD", "BTN", "IND", "LKA", "MDV", "NPL")
  
  wec_dat_sasia = wec_dat %>%
    filter (iso3c %in% sasia)
  
  wec_dat_sasia = wec_dat_sasia [wec_dat_sasia$year == year, ]
  
  AUS = wec_dat_sasia %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base, na.rm = T)) %>%
    mutate (em_pc = emissions / pop) %>%
    group_by (year, sector) %>%
    summarise (em_pc = mean (em_pc, na.rm = T))
  
}
SASIA_avg <- fun_SASIA (2023) %>% ungroup %>% dplyr::select (-year)

pangea2avg = pangea2 %>% dplyr::select (sector, em_pc)

bar_input = rbind (world_avg, EU_avg, OECD_avg, NA_avg, AUS_avg, SA_avg, ASIA_avg, SASIA_avg, IND_avg, pangea_avg, pangea2avg)
bar_input$case = rep (c("World", "EU", "OECD", "USA", "Australia", "Saudi Arabia",
                        "Asia", "South Asia", "India","Prosperea (BAU)", "Prosperea\n(NDC - 2030)"), each = 5)
bar_input$case = factor (bar_input$case,
                         levels = c("World", "EU", "Asia", "OECD", "USA", "Australia", "Saudi Arabia", 
                                     "India","Prosperea (BAU)", "Prosperea\n(NDC - 2030)", "South Asia"))

bar_plot = ggplot (bar_input, aes (fill = sector, y = em_pc, x = case)) +
  geom_bar (position = "stack", stat = "identity") +
  theme_wdl() +
  worlddataverse::scale_fill_wdl2 () +
  labs (title = "Emissions per capita: Pangea vs World, EU, OECD",
        subtitle = "Averages, Business as Usual (BAU) Scenario, 2023 figures",
        x = "Case",
        y = "Emissions per capita, T pc py") + 
  theme (axis.text.x = element_text (angle = 360))

bar_plot




## Chapter 5: pie charts -------------------------------------------------------

## pie chart per country, w/ sector shares

# cleaning data 
pie_sasia_percountry_dat = wec_dat %>%
  
  # filter for South Asia countries and 2023
  filter (iso3c %in% c("BGD", "BTN", "IND", "LKA", "MDV", "NPL") & year == 2023) %>%
  
  # aggregate into year-country-sector per row
  dplyr::group_by (year, iso3c, sector, pop) %>%
  dplyr::summarise (emissions = sum (base, na.rm = T)) %>%
  
  # adding emissions per capita
  mutate (em_pc = emissions / pop) %>%
  
  # adding totals
  dplyr::group_by (year, iso3c) %>%
  mutate (em_pc_sum = sum (em_pc, na.rm = T)) %>% ungroup () %>%
  
  # scaling em_pc 
  mutate (em_pc = round (em_pc, 2))

# creating chart
ggplot (data = pie_sasia_percountry_dat %>% mutate (emissions = emissions / 10^6), 
        aes (x = year,
             y = emissions,
             fill = sector)) + 
  geom_bar (stat = "identity", position = "stack") + 
  theme (axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.x = element_blank()) +
  facet_wrap (~iso3c, scales = "free", ncol = 6, nrow = 1) + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl2() + 
  theme (axis.text.x = element_blank()) + 
  labs (title = "South Asian Countries' Sectoral Emissions Profile",
        y = "Emissions, MT")
  
# creating chart
ggplot (data = pie_sasia_percountry_dat %>% mutate (emissions = emissions / 10^6), 
        aes (x = year,
             y = emissions,
             fill = iso3c)) + 
  geom_bar (stat = "identity", position = "stack") + 
  theme (axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.x = element_blank()) +
  facet_wrap (~sector, scales = "free", ncol = 6, nrow = 1) + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl() + 
  theme (axis.text.x = element_blank()) + 
  labs (title = "Breaking down South Asia sectoral emissions by country",
        y = "Emissions, MT")

# creating chart V3
ggplot (data = pie_sasia_percountry_dat %>% mutate (emissions = emissions / 10^6), 
        aes (x = reorder (iso3c, -em_pc_sum),
             y = em_pc,
             fill = sector)) + 
  geom_bar (stat = "identity", position = "stack") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl2() + 
  labs (title = "Breaking down South Asian country emissions by sector",
        y = "\nEmissions per capita, T",
        x = "\nCountry") + 
  theme (axis.text.x = element_text (angle = 360))





## Chapter 6: country-specific subsector breakdown -----------------------------

## creating df with sector and subsector breakdown per SAsia country (per capita)
sasia_subsec = wec_dat %>%
  
  # filter for South Asia countries and 2023
  filter (iso3c %in% c("BGD", "BTN", "IND", "LKA", "MDV", "NPL") & year == 2023) %>%
  
  # aggregate into year-country-sector per row
  dplyr::group_by (year, iso3c, sector, subsector, pop) %>%
  dplyr::summarise (emissions = sum (base, na.rm = T)) %>%
  
  # adding emissions per capita
  mutate (em_pc = round (emissions / pop, 2))

ggplot (data = sasia_subsec, 
        aes (x = factor (year),
             y = em_pc, 
             fill = sector)) + 
  geom_bar (stat = "identity", 
            position = "stack", 
            col = "black") + 
  theme_minimal () + 
  worlddataverse::scale_fill_wdl2() + 
  facet_wrap (~iso3c, scales = "free", nrow = 1, ncol = 6)

## Chapter 7: quantifying India gaps -------------------------------------------

wec_india = wec_dat %>%
  
  # filtering for India in 2030
  filter (iso3c == "IND" & year == 2030) %>%
  
  # summing emissions
  dplyr::group_by (year, iso3c) %>%
  dplyr::summarise (base = sum (base, na.rm = T) / 10^9,
                    ndc = sum (ndc, na.rm = T) / 10^9,
                    o_1p5c = sum (o_1p5c, na.rm = T) / 10^9)




















