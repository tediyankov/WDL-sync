
## World inequality map ========================================================

## clean environment
rm (list = ls())

## loading packages
pacman::p_load (worlddataverse, tidyverse, countrycode, viridis, stringr, ggthemes, ggalt, maps, rgeos, maptools, grid, rworldmap, cowplot,gridExtra)

## loading in raw data 
gini_rawdat = read.csv ("~/Desktop/economic-inequality-gini-index.csv")

## obtaining the last available year for each country 
gini_dat = gini_rawdat %>%
  
  # filtering for no NAs in the country code column
  mutate (Code = ifelse (Code == "" | Code == " ", NA, Code)) %>%
  filter (!is.na(Code)) %>%
  
  # finding the latest available year
  dplyr::group_by (Code) %>%
  arrange (Year) %>%
  slice (tail (row_number(), 1)) %>% ungroup () %>%
  
  # keeping final vars
  dplyr::select (Code, Year, Gini.coefficient)

## creating baseline map 
worldmap <- map_data (map = "world")

## creatng spatial polygon data frame 
world_map_gini <- joinCountryData2Map (gini_dat, joinCode = "ISO3", nameJoinColumn = "Code")

rworldmap::mapCountryData (world_map_gini, 
                           nameColumnToPlot = "Gini.coefficient",
                           mapTitle = "Mapping Income Inequality",
                           catMethod = "pretty",
                           addLegend = T)


















