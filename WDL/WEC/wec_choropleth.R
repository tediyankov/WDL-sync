
## WEC: emissions-income bivariate choropleth map: global visual ===============

## chapter 1: prelims ----------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, viridis, stringr, ggthemes, ggalt, maps, rgeos, maptools, grid, rworldmap, cowplot,gridExtra)

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

## note: we are using wec_classification from wec_class_code.R
wec_classification_base = wec_classification %>%
  
  # keeping relevant variables
  dplyr::select (iso3c, year, income_class, emissions_class) %>%
  
  # keeping only 2023
  filter (year == 2023)

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
                                            )))) %>%
  
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
                                            )))) %>%
  
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
  theme (axis.title = element_text (color="black"))+
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

## creatng spatial polygon data frame 
world_map <- joinCountryData2Map (wec_classification_base, joinCode = "ISO3", nameJoinColumn = "iso3c")

## building map 
rworldmap::mapCountryData (world_map, 
                                  nameColumnToPlot="income_emissions",
                                  catMethod='categorical',
                                  colourPalette = c("#00FF00",
                                                    "#00CD00", 
                                                    "#008B00", 
                                                    "#FFD700", 
                                                    "#CDAD00", 
                                                    "#CD950C", 
                                                    "#FF4040", 
                                                    "#CD3333",
                                                    "#8B2323",
                                                    "black", 
                                                    "white"),
                                  mapTitle = "Bivariate Choropleth Map: Income and Emissions Typologies",
                                  addLegend = F)


## alternative scenario maps

## creatng spatial polygon data frame 
world_map <- joinCountryData2Map (wec_classification_ndc, joinCode = "ISO3", nameJoinColumn = "iso3c")

## building map 
rworldmap::mapCountryData (world_map, 
                           nameColumnToPlot="income_emissions",
                           catMethod='categorical',
                           colourPalette = c("#00FF00",
                                             "#00CD00", 
                                             "#008B00", 
                                             "#FFD700", 
                                             "#CDAD00", 
                                             "#CD950C", 
                                             "#FF4040", 
                                             "#CD3333",
                                             "#8B2323",
                                             "black", 
                                             "white"),
                           mapTitle = "Bivariate Choropleth Map: Income and Emissions Typologies",
                           addLegend = F)

## creatng spatial polygon data frame 
world_map <- joinCountryData2Map (wec_classification_1.5, joinCode = "ISO3", nameJoinColumn = "iso3c")

## building map 
rworldmap::mapCountryData (world_map, 
                           nameColumnToPlot="income_emissions",
                           catMethod='categorical',
                           colourPalette = c("#00FF00",
                                             "#00CD00", 
                                             "#008B00", 
                                             "#FFD700", 
                                             "#CDAD00", 
                                             "#CD950C", 
                                             "#FF4040", 
                                             "#CD3333",
                                             "#8B2323",
                                             "black", 
                                             "white"),
                           mapTitle = "Bivariate Choropleth Map: Income and Emissions Typologies",
                           addLegend = F)











