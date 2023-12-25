
## WEC: emissions-income bivariate choropleth map: global visual ===============

## chapter 1: prelims ----------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, viridis, stringr, ggthemes, ggalt, maps, rgeos, maptools, grid, rworldmap, cowplot,gridExtra,ggmap)


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

## note: we are using wec_classification from wec_class_code.R
wec_classification_base = wec_classification %>%
  
  # keeping relevant variables
  dplyr::select (iso3c, year, income_class, emissions_class) %>%
  
  # keeping only 2023
  filter (year == 2023)

## chapter 2: colour grid ------------------------------------------------------

# Add some labels
d <- expand.grid (income = 1:3, emissions = 1:3)
d <- merge (d, data.frame (income = 1:3, xlabel = c("Low Income", "Middle Income","High Income")), by="income")
d <- merge (d, data.frame (emissions = 1:3, ylabel = c("Low Emissions", "Middle Emissions","High Emissions")), by="emissions")

g.legend <-
  ggplot (d, aes(income, emissions, 
                 fill=atan(emissions/income), 
                 alpha = income + emissions, 
                 label = paste0 (xlabel,"\n",ylabel))) +
  geom_tile ()+
  geom_text (alpha=1)+
  scale_fill_viridis()+
  theme_void()+
  theme (legend.position = "none",
        panel.background = element_blank(),
        plot.margin = margin (t=10, b=10, l=10))+
  labs (title="A bivariate color scheme (Viridis)", x="Income", y="Emissions")+
  theme (axis.title = element_text (color="black")) +
  
  # Draw some arrows:
  geom_segment (aes(x=1, xend = 3 , y=0, yend = 0), linewidth=1.5,
               arrow = arrow (length = unit (0.6,"cm"))) +
  geom_segment (aes(x=0, xend = 0 , y=1, yend = 3), linewidth=1.5,
               arrow = arrow (length = unit (0.6,"cm"))) + 
  theme (text = element_text (size = 18))

g.legend

## chapter 3: building map -----------------------------------------------------

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

# adding missing countries
class_missing = data.frame (iso3c = c("SSD", "PRK", "VEN"), 
                            income_emissions = c("low income_low emissions", "low income_low emissions", "middle income_middle emissions"))
wec_classification_base <- rbind (wec_classification_base, class_missing)


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









