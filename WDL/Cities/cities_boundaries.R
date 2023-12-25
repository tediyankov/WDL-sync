
## Cities model: obtaining city boundaries =====================================

## preliminaries ---------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (reshape2,
                tidyverse,
                worlddataverse,
                zoo,
                data.table,
                haven,
                Hmisc,
                readr,
                raster, 
                sf)

## loading paths
base_path <- worlddataverse::get_wdl_path ()
city_path <- file.path (base_path, "wdp_city_spending")

## loading in raw tiff file
drc <- raster (file.path (city_path, "Urban_Area_Footprints/COD.tif"))
drc_df <- as.data.frame (drc, xy = TRUE)

## loading city names file
city_names <- read_csv (file.path (city_path, "City_Spending_As_A_Share_Of_Country_Total.csv")) %>%
  
  # this file is useful! Has the long-lat and spending share per city per country. 
  # we're aiming to create new spending shares for the cities
  
  # keeping only relevant vars
  dplyr::select (citycode, countrycode, name) %>%
  
  # filtering for DRC
  filter (countrycode == "COD")

## example viz -----------------------------------------------------------------

# showing the whole thing (the non-red ones are the city rectangles)
ggplot () +
  geom_raster (drc_df, mapping = aes(x = x, y = y, fill = as.character(COD))) +
  coord_equal()

# showing Kinshasa (here you will see how this essentially maps out a very pixelated Kinshasa)
ggplot () +
  geom_raster (filter (drc_df, COD == 662), mapping = aes (x = x, y = y, fill = as.character(COD))) +
  coord_equal ()

## testing spatial join (w/ st_join) -------------------------------------------

cities_locations = read_csv (file.path (city_path, "City_Spending_As_A_Share_Of_Country_Total.csv")) %>%
  
  # keeping only relevant vars
  dplyr::select (citycode, countrycode, name, lat, lon) %>%
  
  # filtering for DRC
  filter (countrycode == "COD")

# converting drc_df to sf (spatial poygon data frame)
drc_sf <- st_as_sf (drc_df, coords = c("x", "y"))
  
# converting city coordinates df to sf (spatial poygon data frame)
cities_sf <- st_as_sf (cities_locations, coords = c("lon", "lat"))

# set tolerange distance
tolerance_distance <- 100

# Create a buffer around each city coordinate
cities_buffered <- st_buffer (cities_sf, dist = tolerance_distance)

# performing spatial join
drc_wcities = st_join (cities_sf, drc_sf, join = st_within)

drc_wcities <- cities_buffered %>%
  st_join (drc_sf, join = st_within)


## testing spatial join (w/ st_intersect) --------------------------------------

cities_locations = read_csv (file.path (city_path, "City_Spending_As_A_Share_Of_Country_Total.csv")) %>%
  
  # keeping only relevant vars
  dplyr::select (citycode, countrycode, name, lat, lon) %>%
  
  # filtering for DRC
  filter (countrycode == "COD")

# converting drc_df to sf (spatial poygon data frame)
drc_sf <- st_as_sf (drc_df, coords = c("x", "y"))

# converting city coordinates df to sf (spatial poygon data frame)
cities_sf <- st_as_sf (cities_locations, coords = c("lat", "lon"))

# set tolerange distance
tolerance_distance <- 100

# Create a buffer around each city coordinate
cities_buffered <- st_buffer (cities_sf, dist = tolerance_distance)

# Perform the spatial join with tolerance using st_intersection
drc_with_city <- st_intersection (cities_buffered, drc_sf)



