
## Colgate DRC code ============================================================

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, Hmisc, leaflet, geojsonio, broom, viridis)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')

## loading in WDPro latest version

# loading raw file
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))

# first cleaning of WDP dataset
wdp <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 8, Inf))

## Age: DRC CC hc/exp / agegroup bar charts ------------------------------------

## Headcounts
age_drc_age_bar_hc = wdp %>%
  
  # country-year hc per age group and spending group
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # isolating consumer class
  filter (daily_spending == "[8,Inf)") %>%
  
  # recoding age groups into 10-year scale
  dplyr::mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)"), '[00,10)', 
                                     ifelse (age_group %in% c("[10,15)","[15,20)"), '[10,20)', 
                                             ifelse (age_group %in% c("[20,25)","[25,30)"), '[20,30)', 
                                                     ifelse (age_group %in% c("[30,35)","[35,40)"), '[30,40)', 
                                                             ifelse (age_group %in% c("[40,45)","[45,50)"), '[40,50)', 
                                                                     ifelse (age_group %in% c("[50,55)","[55,60)"), '[50,60)', 
                                                                             ifelse (age_group %in% c("[60,65)","[65,70)"), '[60,70)', 
                                                                                     ifelse (age_group %in% c("[70,75)","[75,INF)"), '[70,INF)', age_group))))))))) %>%
  
  # aggregating into new age groups
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>%
  
  # isolating DRC
  filter (ccode == "COD") %>%
  
  # Isolating 2023 and 2030
  filter (year %in% c(2023, 2030)) %>%
  
  # widening for bar plot 
  pivot_wider (names_from = "year", values_from = "pop") %>%
  
  ## creating plot foundation
  ggplot (aes (x = age_group)) + 
  
  # creating bars
  geom_bar (aes (y = `2030`), width = 0.8, stat = "identity", position = "identity", fill = "#6883BC") +
  geom_bar (aes (y = `2023`), width = 0.4, stat = "identity", position = "identity", fill = "#8A307F") + 
  
  # theme
  worlddataverse::theme_wdl() + 
  theme (axis.ticks.x = element_blank(),
         axis.text.x = element_text (angle = 360)) +
  
  # title 
  labs (title = "Age Breakdown of DRC Consumers: comparing headcounts 2023 and 2030",
        y = "CC headcounts")
  
age_drc_age_bar_hc

## Spending
age_drc_age_bar_exp = wdp %>%
  
  # country-year hc per age group and spending group
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # isolating consumer class
  filter (daily_spending == "[8,Inf)") %>%
  
  # recoding age groups into 10-year scale
  dplyr::mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)"), '[00,10)', 
                                     ifelse (age_group %in% c("[10,15)","[15,20)"), '[10,20)', 
                                             ifelse (age_group %in% c("[20,25)","[25,30)"), '[20,30)', 
                                                     ifelse (age_group %in% c("[30,35)","[35,40)"), '[30,40)', 
                                                             ifelse (age_group %in% c("[40,45)","[45,50)"), '[40,50)', 
                                                                     ifelse (age_group %in% c("[50,55)","[55,60)"), '[50,60)', 
                                                                             ifelse (age_group %in% c("[60,65)","[65,70)"), '[60,70)', 
                                                                                     ifelse (age_group %in% c("[70,75)","[75,INF)"), '[70,INF)', age_group))))))))) %>%
  
  # aggregating into new age groups
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # isolating DRC
  filter (ccode == "COD") %>%
  
  # Isolating 2023 and 2030
  filter (year %in% c(2023, 2030)) %>%
  
  # scaling spending to billions
  mutate (exp = exp / 10^9) %>%
  
  # widening for bar plot 
  pivot_wider (names_from = "year", values_from = "exp") %>%
  
  # creating plot foundation
  ggplot (aes (x = age_group)) + 
  
  # creating bars
  geom_bar (aes (y = `2030`), width = 0.8, stat = "identity", position = "identity", fill = "#6883BC") +
  geom_bar (aes (y = `2023`), width = 0.4, stat = "identity", position = "identity", fill = "#8A307F") + 
  
  # theme
  worlddataverse::theme_wdl() + 
  theme (axis.ticks.x = element_blank(),
         axis.text.x = element_text (angle = 360)) +
  
  # title 
  labs (title = "Age Breakdown of DRC Consumers: comparing spending 2023 and 2030",
        y = "CC spending (Billions $ 2017 PPP)")

age_drc_age_bar_exp

## Age: DRC + key countries CC hc/exp / agegroup stacked bar charts ------------

## Headcounts
age_all_stackedbar_hc = wdp %>%
  
  # country-year hc per age group and spending group
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # isolating consumer class
  filter (daily_spending == "[8,Inf)") %>%
  
  # new age groups
  dplyr::mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)", "[10,15)","[15,20)", "[20,25)","[25,30)", "[30,35)"), "0_35",
                                     ifelse (age_group %in% c("[35,40)", "[40,45)","[45,50)", "[50,55)","[55,60)", "[60,65)"), "35_65",
                                             ifelse (age_group %in% c("[65,70)", "[70,75)","[75,INF)"), "65_INF",
                                             )))) %>%
  
  # aggregating into new age groups 
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>%
  
  # isolating DRC and key comparison countries
  filter (ccode %in% c("COD", "KEN", "ETH", "AGO", "NGA")) %>%
  
  # Isolating 2023 and 2030
  filter (year %in% c(2023, 2030)) %>%
  
  # segmentation via shares
  dplyr::group_by (ccode, year) %>%
  mutate (pop = pop / sum (pop, na.rm = T)) %>% ungroup () %>%
  
  # widening for bar plot 
  #pivot_wider (names_from = "year", values_from = "pop") %>%
  
  # creating plot
  ggplot (aes (x = factor (year), y = pop, fill = age_group)) + 
  
  # creating bars
  geom_bar (stat = "identity", color="white") +
  
  # facetting
  facet_wrap (~ccode, nrow = 1, ncol = 5) + 
  
  # theme
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl2() +
  theme (axis.text.x = element_text (angle = 360)) + 
  #scale_fill_manual (values = c("#610303", "#890f0d", "#ad4012", "#d17016", "#c28c1d", "#b3a723", "#289e8e", "#212020")) + 
  
  # title
  labs (title = "$8+ segmentation by age group: comparing across \ncountries of interest",
        x = "Year", 
        y = "$8+ Headcounts (share / age group)")
  

age_all_stackedbar_hc

## Spending
age_all_stackedbar_exp = wdp %>%
  
  # country-year hc per age group and spending group
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # isolating consumer class
  filter (daily_spending == "[8,Inf)") %>%
  
  # recoding age groups into 10-year scale
  dplyr::mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)", "[10,15)","[15,20)", "[20,25)","[25,30)", "[30,35)"), "0_35",
                                     ifelse (age_group %in% c("[35,40)", "[40,45)","[45,50)", "[50,55)","[55,60)", "[60,65)"), "35_65",
                                             ifelse (age_group %in% c("[65,70)", "[70,75)","[75,INF)"), "65_INF",
                                             )))) %>%
  
  # aggregating into new age groups
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # isolating DRC and key comparison countries
  filter (ccode %in% c("COD", "KEN", "ETH", "AGO", "NGA")) %>%
  
  # Isolating 2023 and 2030
  filter (year %in% c(2023, 2030)) %>%
  
  # segmentation via shares
  dplyr::group_by (ccode, year) %>%
  mutate (exp = exp / sum (exp, na.rm = T)) %>% ungroup () %>%
  
  # widening for bar plot 
  #pivot_wider (names_from = "year", values_from = "pop") %>%
  
  # creating plot
  ggplot (aes (x = factor (year), y = exp, fill = age_group)) + 
  
  # creating bars
  geom_bar (stat = "identity", color="white") +
  
  # facetting
  facet_wrap (~ccode, nrow = 1, ncol = 5) + 
  
  # theme
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl2() +
  theme (axis.text.x = element_text (angle = 360)) + 
  #scale_fill_manual (values = c("#610303", "#890f0d", "#ad4012", "#d17016", "#c28c1d", "#b3a723", "#289e8e", "#212020")) + 
  
  # title
  labs (title = "$8+ segmentation by age group: comparing across \ncountries of interest (spending)",
        x = "Year", 
        y = "$8+ Spending (share / age group)")


age_all_stackedbar_exp


## DRC cities ------------------------------------------------------------------

## loading in raw cities data
africa_cities_raw <- read_csv ("~/Desktop/africa_cities_v2.csv")

## total population of DRC cities df
drc_cities_pop <- data.frame (year = 2023, 
                              name = c("Kinshasa", "Mbuji-Mayi", "Lubumbashi", 
                                       "Kananga", "Kisangani", "Bukavu", "Tshikapa",
                                       "Bunia", "Goma", "Uvira", "Likasi", "Kikwit", 
                                       "Kabinda", "Kolwezi", "Mbandaka", "Matadi"),
                              pop = c(16404060, 2905109, 2828109, 1672811, 1432538,
                                      1252857, 1079228, 811978, 745591, 692407, 632730, 
                                      572100, 570294, 530897, 498182, 435342))

## isolating DRC
drc_cities_original = africa_cities_raw %>% filter (ccode == "COD")
bri_cities_original = africa_cities_raw %>% filter (ccode == "BDI")

## finding missing cities 
setdiff (unique (drc_cities_pop$name), unique (drc_cities$name))

## creating df with all cities (including gaps)

# creating objects with unique values
cities <- as.factor (unique (drc_cities_pop$name))
spend_group <- as.factor (unique (drc_cities_original$spend_group))
ccode <- "COD"
year <- c(2023, 2030)

# creatinf df
drc_cities = expand.grid (ccode = ccode, 
                          year = year,
                          name = cities, 
                          spend_group = spend_group) %>%
  
  # joining with drc_cities_pop to get total population
  left_join (drc_cities_pop, by = c("year", "name")) %>%
  
  # joining with drc_cities_original to fill in values we already have
  left_join (drc_cities_original, by = c("ccode", "year", "name", "spend_group")) %>%
  
  # for now, keepng only 2023
  filter (year %in% c(2023, 2030)) %>%
  
  # widening for ease
  pivot_wider (names_from = "year", values_from = c(5:7))

## modelling the missing data (using regression)

# adding total hc and exp for COD for the relevant spending groups
wdp2 <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))
wdp2_clean = wdp2 %>%
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc_totaldrc = sum (hc.pdf.m, na.rm = T),
                    exp_totaldrc = sum (exp.pdf.m, na.rm = T)) %>%
  filter (year %in% c(2023,2030) & ccode == "COD") %>%
  rename (spend_group = daily_spending)
drc_cities = drc_cities %>% left_join (wdp2_clean, by = c("ccode", "year", "spend_group"))

# adding total COD population and city population share
wdp2_clean2 = wdp2 %>%
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop_totaldrc = sum (hc.pdf.m, na.rm = T)) %>%
  filter (year %in% c(2023,2030) & ccode == "COD")
drc_cities = drc_cities %>% left_join (wdp2_clean2, by = c("ccode", "year"))

# splitting data into complete and missing
train <- drc_cities %>% filter (!is.na (hc) & !is.na (exp))
test <- drc_cities %>% filter (is.na (hc) & is.na (exp))

# training regression model
m1_hc <- lm (hc ~ pop, data = train)
m1_exp <- lm (exp ~ pop, data = train)

# filling in missing data
test$hc <- predict (m1_hc, newdata = test)
test$exp <- predict (m1_exp, newdata = test)

# joining into an NA-free data frame
drc_cities_reg_impute = rbind (train, test)


## WCO-style summary of 8+ -----------------------------------------------------

## load rraw WDPro data 
wdp3 <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 8, 12, Inf))

## cleaning 
drc_8plus <- wdp3 %>%
  
  # grouping into country-years per daily spending 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # filtering for 2022-2024 and for DRC onl7
  filter (year %in% c(2022:2024) & ccode == "COD") %>%
  
  # recoding spending groups to allow for 8+
  mutate (daily_spending = ifelse (daily_spending == "[0,8)", "8_minus", '8_plus')) %>%
  
  # reaggregating into new groups
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>%
  
  # keeping onyl 8+ 
  filter (daily_spending == '8_plus') %>%
  
  # widening
  pivot_wider (names_from = "year", values_from = c(4:5)) %>%
  
  # computing deltas 
  mutate (delta_hc_23 = (hc_2023 - hc_2022) / 10^6,
          delta_exp_23 = (exp_2023 - exp_2022) / 10^9,
          delta_hc_24 = (hc_2024 - hc_2023) / 10^6,
          delta_exp_24 = (exp_2024 - exp_2023) / 10^9,
          delta_exp_23_pct = ((exp_2023 - exp_2022) / exp_2022) * 100,
          delta_exp_24_pct = ((exp_2024 - exp_2023) / exp_2023) * 100) %>%
  
  # keeping only relevant vars
  dplyr::select (ccode, delta_hc_23, delta_exp_23, delta_hc_24, delta_exp_24, delta_exp_23_pct, delta_exp_24_pct)


## new cities charts -----------------------------------------------------------

## loading data 
city_distribution_pop_shares2 <- readRDS ("~/Desktop/city_distribution_pop_shares.rds")
city_distribution_pop_shares <- readRDS (file.path (base_path, 
                                                    "wdp_city_spending", 
                                                    "wdpro_combination", 
                                                    "city_distribution_un_pop_shares.rds"))

### 2023 -----------------------------------------------------------------------

## isolating DRC and year of interest
drc_cities_2023 = city_distribution_pop_shares %>% 
  
  # isolating DRC in 2023
  filter (ccode == "COD" & year %in% c(2023)) %>%
  
  # relocating
  relocate (name, .after = "ccode") %>%
  
  # removing irrelevant vars
  dplyr::select (year, name, pop, spending_data_kernel)

## creating new var for 0-8
cities = unique (drc_cities_2023$name)
drc_cities_2023$spend_0_8 = NA

for (i in 1:16) {
  
  drc_cities_2023$spend_0_8 [drc_cities_2023$name == cities[i]] <- drc_cities_2023 [[4]][[i]][11,3]
  
}

## calculating 8+ 
drc_cities_2023 = drc_cities_2023 %>% mutate (spend_8_plus = pop - spend_0_8)

### 2022 -----------------------------------------------------------------------

## isolating DRC and year of interest
drc_cities_2022 = city_distribution_pop_shares %>% 
  
  # isolating DRC in 2022
  filter (ccode == "COD" & year %in% c(2022)) %>%
  
  # relocating
  relocate (name, .after = "ccode") %>%
  
  # removing irrelevant vars
  dplyr::select (year, name, pop, spending_data_kernel)

## creating new var for 0-8
cities = unique (drc_cities_2022$name)
drc_cities_2022$spend_0_8 = NA

for (i in 1:16) {
  
  drc_cities_2022$spend_0_8 [drc_cities_2022$name == cities[i]] <- drc_cities_2022 [[4]][[i]][11,3]
  
}

## calculating 8+ 
drc_cities_2022 = drc_cities_2022 %>% mutate (spend_8_plus = pop - spend_0_8)

### 2024 -----------------------------------------------------------------------
 
## isolating DRC and year of interest
drc_cities_2024 = city_distribution_pop_shares %>% 
  
  # isolating DRC in 2024
  filter (ccode == "COD" & year %in% c(2024)) %>%
  
  # relocating
  relocate (name, .after = "ccode") %>%
  
  # removing irrelevant vars
  dplyr::select (year, name, pop, spending_data_kernel)

## creating new var for 0-8
cities = unique (drc_cities_2024$name)
drc_cities_2024$spend_0_8 = NA

for (i in 1:16) {
  
  drc_cities_2024$spend_0_8 [drc_cities_2024$name == cities[i]] <- drc_cities_2024 [[4]][[i]][11,3]
  
}

## calculating 8+ 
drc_cities_2024 = drc_cities_2024 %>% mutate (spend_8_plus = pop - spend_0_8)

### 2030 -----------------------------------------------------------------------

## isolating DRC and year of interest
drc_cities_2030 = city_distribution_pop_shares %>% 
  
  # isolating DRC in 2030
  filter (ccode == "COD" & year %in% c(2030)) %>%
  
  # relocating
  relocate (name, .after = "ccode") %>%
  
  # removing irrelevant vars
  dplyr::select (year, name, pop, spending_data_kernel)

## creating new var for 0-8
cities = unique (drc_cities_2030$name)
drc_cities_2030$spend_0_8 = NA

for (i in 1:16) {
  
  drc_cities_2030$spend_0_8 [drc_cities_2030$name == cities[i]] <- drc_cities_2030 [[4]][[i]][11,3]
  
}

## calculating 8+ 
drc_cities_2030 = drc_cities_2030 %>% mutate (spend_8_plus = pop - spend_0_8)

## building map visual

# cleaning dfs with 8+ hc
drc_cities_2022_2 = drc_cities_2022 %>% dplyr::select (name, spend_8_plus) %>% rename (spend_8_plus_2022 = spend_8_plus)
drc_cities_2023_2 = drc_cities_2023 %>% dplyr::select (name, spend_8_plus) %>% rename (spend_8_plus_2023 = spend_8_plus)
drc_cities_2024_2 = drc_cities_2024 %>% dplyr::select (name, spend_8_plus) %>% rename (spend_8_plus_2024 = spend_8_plus)
drc_cities_2030_2 = drc_cities_2030 %>% dplyr::select (name, spend_8_plus) %>% rename (spend_8_plus_2030 = spend_8_plus)

# Create a data frame with the city information
cities <- data.frame (
  
  name = c("Bukavu", "Bunia", "Goma", "Kabinda", "Kananga", "Kikwit", "Kinshasa", "Kisangani",
           "Kolwezi", "Likasi", "Lubumbashi", "Matadi", "Mbandaka", "Mbuji-Mayi", "Tshikapa", "Uvira"),
  pop = c(1252857, 811978, 745591, 570294, 1672811, 572100, 16404060, 1432538,
                 530897, 632730, 2828109, 435342, 498182, 2905109, 1079228, 692407),
  lat = c(-2.5083, 1.5667, -1.6792, -6.1379, -5.8962, -5.041, -4.3276, 0.5167, -10.7148,
          -10.9814, -11.6609, -5.8177, 0.0487, -6.15, -6.4162, -3.3953),
  lon = c(28.8608, 30.25, 29.2228, 24.4818, 22.4166, 18.8162, 15.3136, 25.2, 25.4667,
          26.7333, 27.4794, 13.4717, 18.2603, 23.6, 20.8, 29.1378)
) %>%
  
  # merging with 2022, 2023, 2024 and 2030 dfs
  left_join (drc_cities_2022_2, by = "name") %>%
  left_join (drc_cities_2023_2, by = "name") %>%
  left_join (drc_cities_2024_2, by = "name") %>%
  left_join (drc_cities_2030_2, by = "name") %>%
  
  # computing deltas
  mutate (delta_2024 = spend_8_plus_2024 - spend_8_plus_2023, 
          delta_2030 = spend_8_plus_2030 - spend_8_plus_2023)

# Create a leaflet map
map <- leaflet() %>%
  setView (lng = 25, lat = 0, zoom = 5)  # Set the initial view of the map

# Add the outline of DRC
map <- map %>% addTiles() %>%
  addPolygons(
    lng = c(12.19, 30.8489, 30.8489, 12.19, 12.19),
    lat = c(-13.455, -13.455, 5.377, 5.377, -13.455),
    fillColor = "transparent",
    color = "blue"
  )

# Calculate the size of dots proportional to the city population
dot_size <- sqrt(cities$delta_2024 / max(cities$delta_2024)) * 30

# Add the cities as dots on the map
map <- map %>% addCircleMarkers(
  lng = cities$lon,
  lat = cities$lat,
  radius = dot_size,
  color = "red",
  fillOpacity = 0.8
)

# Display the map
map

# Create a leaflet map centered on the Democratic Republic of the Congo (DRC)
map <- leaflet() %>%
  setView(lng = 23.6, lat = -2.5, zoom = 6)

# Add tiles to the map
map <- map %>%
  addTiles("Stamen.TonerLite") %>%
  addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE))

# Calculate the size of dots proportional to the city population
dot_size <- sqrt(cities$delta_2030 / max(cities$delta_2030)) * 20

# Add city dots with popups to the map
for (i in 1:nrow(cities)) {
  city <- cities[i, ]
  map <- map %>%
    addCircleMarkers(
      lng = city$lon, lat = city$lat,
      radius = dot_size[i],
      color = "red",
      fillOpacity = 0.8,
      label = paste0(city$name),
      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "bottom")
    )
}
# Save the map as an HTML file
#saveWidget(map, "drc_cities_map.html", selfcontained = TRUE)
# Display the map
map


## doing the cities stuff but with spending ------------------------------------

### 2023 -----------------------------------------------------------------------

## isolating DRC and year of interest
drc_cities_2023 = city_distribution_pop_shares %>% 
  
  # isolating DRC in 2023
  filter (ccode == "COD" & year %in% c(2023)) %>%
  
  # relocating
  relocate (name, .after = "ccode") %>%
  
  # removing irrelevant vars
  dplyr::select (year, name, pop, spending_data_kernel)

## creating new var for 0-8
cities = unique (drc_cities_2023$name)

for (i in 1:16) {
  
  drc_cities_2023$spend_8_plus [drc_cities_2023$name == cities[i]] <- drc_cities_2023 [[4]][[i]][164,4] - drc_cities_2023 [[4]][[i]][11,4] 
  
}


### 2022 -----------------------------------------------------------------------

## isolating DRC and year of interest
drc_cities_2022 = city_distribution_pop_shares %>% 
  
  # isolating DRC in 2023
  filter (ccode == "COD" & year %in% c(2022)) %>%
  
  # relocating
  relocate (name, .after = "ccode") %>%
  
  # removing irrelevant vars
  dplyr::select (year, name, pop, spending_data_kernel)

## creating new var for 0-8
cities = unique (drc_cities_2022$name)

for (i in 1:16) {
  
  drc_cities_2022$spend_8_plus [drc_cities_2022$name == cities[i]] <- drc_cities_2022 [[4]][[i]][164,4] - drc_cities_2022 [[4]][[i]][11,4] 
  
}

### 2024 -----------------------------------------------------------------------

## isolating DRC and year of interest
drc_cities_2024 = city_distribution_pop_shares %>% 
  
  # isolating DRC in 2023
  filter (ccode == "COD" & year %in% c(2024)) %>%
  
  # relocating
  relocate (name, .after = "ccode") %>%
  
  # removing irrelevant vars
  dplyr::select (year, name, pop, spending_data_kernel)

## creating new var for 0-8
cities = unique (drc_cities_2024$name)

for (i in 1:16) {
  
  drc_cities_2024$spend_8_plus [drc_cities_2024$name == cities[i]] <- drc_cities_2024 [[4]][[i]][164,4] - drc_cities_2024 [[4]][[i]][11,4] 
  
}

### 2030 -----------------------------------------------------------------------

## isolating DRC and year of interest
drc_cities_2030 = city_distribution_pop_shares %>% 
  
  # isolating DRC in 2023
  filter (ccode == "COD" & year %in% c(2030)) %>%
  
  # relocating
  relocate (name, .after = "ccode") %>%
  
  # removing irrelevant vars
  dplyr::select (year, name, pop, spending_data_kernel)

## creating new var for 0-8
cities = unique (drc_cities_2030$name)

for (i in 1:16) {
  
  drc_cities_2030$spend_8_plus [drc_cities_2030$name == cities[i]] <- drc_cities_2030 [[4]][[i]][164,4] - drc_cities_2030 [[4]][[i]][11,4] 
  
}

## map visuals -----------------------------------------------------------------

# cleaning dfs with 8+ hc
drc_cities_2022_2 = drc_cities_2022 %>% dplyr::select (name, spend_8_plus) %>% rename (spend_8_plus_2022 = spend_8_plus)
drc_cities_2023_2 = drc_cities_2023 %>% dplyr::select (name, spend_8_plus) %>% rename (spend_8_plus_2023 = spend_8_plus)
drc_cities_2024_2 = drc_cities_2024 %>% dplyr::select (name, spend_8_plus) %>% rename (spend_8_plus_2024 = spend_8_plus)
drc_cities_2030_2 = drc_cities_2030 %>% dplyr::select (name, spend_8_plus) %>% rename (spend_8_plus_2030 = spend_8_plus)

# Create a data frame with the city information
cities <- data.frame (
  
  name = c("Bukavu", "Bunia", "Goma", "Kabinda", "Kananga", "Kikwit", "Kinshasa", "Kisangani",
           "Kolwezi", "Likasi", "Lubumbashi", "Matadi", "Mbandaka", "Mbuji-Mayi", "Tshikapa", "Uvira"),
  pop = c(1252857, 811978, 745591, 570294, 1672811, 572100, 16404060, 1432538,
          530897, 632730, 2828109, 435342, 498182, 2905109, 1079228, 692407),
  lat = c(-2.5083, 1.5667, -1.6792, -6.1379, -5.8962, -5.041, -4.3276, 0.5167, -10.7148,
          -10.9814, -11.6609, -5.8177, 0.0487, -6.15, -6.4162, -3.3953),
  lon = c(28.8608, 30.25, 29.2228, 24.4818, 22.4166, 18.8162, 15.3136, 25.2, 25.4667,
          26.7333, 27.4794, 13.4717, 18.2603, 23.6, 20.8, 29.1378)
) %>%
  
  # merging with 2022, 2023, 2024 and 2030 dfs
  left_join (drc_cities_2022_2, by = "name") %>%
  left_join (drc_cities_2023_2, by = "name") %>%
  left_join (drc_cities_2024_2, by = "name") %>%
  left_join (drc_cities_2030_2, by = "name") %>%
  
  # computing deltas
  mutate (delta_2024 = spend_8_plus_2024 - spend_8_plus_2023, 
          delta_2030 = spend_8_plus_2030 - spend_8_plus_2023)


# Create a leaflet map centered on the Democratic Republic of the Congo (DRC)
map <- leaflet() %>%
  setView(lng = 23.6, lat = -2.5, zoom = 6)

# Add tiles to the map
map <- map %>%
  addTiles("Stamen.TonerLite") %>%
  addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE))

# Calculate the size of dots proportional to the city population
dot_size <- sqrt(cities$delta_2030 / max(cities$delta_2030)) * 20

# Add city dots with popups to the map
for (i in 1:nrow(cities)) {
  city <- cities[i, ]
  map <- map %>%
    addCircleMarkers(
      lng = city$lon, lat = city$lat,
      radius = dot_size[i],
      color = "red",
      fillOpacity = 0.8,
      label = paste0(city$name),
      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "bottom")
    )
}
# Save the map as an HTML file
#saveWidget(map, "drc_cities_map.html", selfcontained = TRUE)
# Display the map
map

## re-do of some slides ========================================================

## pie chart: 0-2, 2-4, 4-6, 6-8, 8-10, 8-12, 12+ ------------------------------

## loading in raw WDP data
wdp_drc <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0,2,4,6,8,10,12, Inf))

## Headcounts pie chart

hc_pie_drc_2023 <- wdp_drc %>%
  
  # group into ccode-year-spending group
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # isolating DRC in 2023
  filter (ccode == "COD" & year %in% 2023) %>%
  
  # computing shares
  mutate (hc_share = hc / sum (hc, na.rm = T),
          exp_share = exp / sum (exp, na.rm = T)) %>%
  
  # creating labels
  mutate (hc_label = paste0 (round (hc / 10^6, 1), " ", "M"),
          exp_label = paste0 (round (exp / 10^9, 1), " ", "B")) %>%
  
  # reordering the levels of daily_spending
  mutate (daily_spending = factor (daily_spending, levels = c("[0,2)", "[2,4)", "[4,6)", "[6,8)", "[8,10)", "[10,12)", "[12,Inf)"))) %>%
  
  # creating plot 
  ggplot (aes (x = factor (year), y = hc_share, fill = daily_spending)) + 
  geom_bar (stat = "identity", position = "stack", border = "white") + 
  geom_label (aes (x = factor (year), y = hc_share, label = hc_label, group = daily_spending),
             position = position_stack (vjust = 0.5),
             col = "white", 
             label.padding = unit (1, "lines")) +
  worlddataverse::scale_fill_wdl () + 
  coord_polar ("y") + 
  theme_void ()

hc_pie_drc_2023

## Spending pie chart 

exp_pie_drc_2023 <- wdp_drc %>%
  
  # group into ccode-year-spending group
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # isolating DRC in 2023
  filter (ccode == "COD" & year %in% 2023) %>%
  
  # computing shares
  mutate (hc_share = hc / sum (hc, na.rm = T),
          exp_share = exp / sum (exp, na.rm = T)) %>%
  
  # creating labels
  mutate (hc_label = paste0 (round (hc / 10^6, 1), " ", "M"),
          exp_label = paste0 (round (exp / 10^9, 1), " ", "B")) %>%
  
  # reordering the levels of daily_spending
  mutate (daily_spending = factor (daily_spending, levels = c("[0,2)", "[2,4)", "[4,6)", "[6,8)", "[8,10)", "[10,12)", "[12,Inf)"))) %>%
  
  # creating plot 
  ggplot (aes (x = factor (year), y = exp_share, fill = daily_spending)) + 
  geom_bar (stat = "identity", position = "stack") + 
  geom_label (aes (x = factor (year), y = exp_share, label = exp_label, group = daily_spending),
              position = position_stack (vjust = 0.5),
              col = "white", 
              label.padding = unit (1, "lines")) +
  worlddataverse::scale_fill_wdl () + 
  coord_polar ("y") + 
  theme_void () + 
  theme (legend.position = "bottom")

exp_pie_drc_2023


## map using total spending ----------------------------------------------------

## isolating DRC and year of interest (2023)
drc_cities_2023 = city_distribution_pop_shares %>% 
  
  # isolating DRC in 2023
  filter (ccode == "COD" & year %in% c(2023)) %>%
  
  # relocating
  relocate (name, .after = "ccode") %>%
  
  # removing irrelevant vars
  dplyr::select (year, name, pop, spending_data_kernel)

## creating new var for 0-8
cities = unique (drc_cities_2023$name)
drc_cities_2023$spend_total = NA

for (i in 1:16) {
  
  drc_cities_2023$spend_total [drc_cities_2023$name == cities[i]] <- drc_cities_2023 [[4]][[i]][164,4]
  drc_cities_2023$spend_cc [drc_cities_2023$name == cities[i]] <- drc_cities_2023 [[4]][[i]][164,4] - drc_cities_2023 [[4]][[i]][15,4]
  
}

## isolating DRC and year of interest (2030)
drc_cities_2030 = city_distribution_pop_shares %>% 
  
  # isolating DRC in 2030
  filter (ccode == "COD" & year %in% c(2030)) %>%
  
  # relocating
  relocate (name, .after = "ccode") %>%
  
  # removing irrelevant vars
  dplyr::select (year, name, pop, spending_data_kernel)

## creating new var for 0-8
cities = unique (drc_cities_2030$name)
drc_cities_2030$spend_total = NA

for (i in 1:16) {
  
  drc_cities_2030$spend_total [drc_cities_2030$name == cities[i]] <- drc_cities_2030 [[4]][[i]][164,4]
  drc_cities_2030$spend_cc [drc_cities_2030$name == cities[i]] <- drc_cities_2030 [[4]][[i]][164,4] - drc_cities_2030 [[4]][[i]][15,4]
  
}

# cleaning map input dfs
drc_cities_2023_2 = drc_cities_2023 %>% 
  dplyr::select (name, spend_total, spend_cc) %>% 
  rename (spend_total_2023 = spend_total, 
          spend_cc_2023 = spend_cc)

drc_cities_2030_2 = drc_cities_2030 %>% 
  dplyr::select (name, spend_total, spend_cc) %>% 
  rename (spend_total_2030 = spend_total, 
          spend_cc_2030 = spend_cc)

# Create a data frame with the city information
cities <- data.frame (
  
  name = c("Bukavu", "Bunia", "Goma", "Kabinda", "Kananga", "Kikwit", "Kinshasa", "Kisangani",
           "Kolwezi", "Likasi", "Lubumbashi", "Matadi", "Mbandaka", "Mbuji-Mayi", "Tshikapa", "Uvira"),
  pop = c(1252857, 811978, 745591, 570294, 1672811, 572100, 16404060, 1432538,
          530897, 632730, 2828109, 435342, 498182, 2905109, 1079228, 692407),
  lat = c(-2.5083, 1.5667, -1.6792, -6.1379, -5.8962, -5.041, -4.3276, 0.5167, -10.7148,
          -10.9814, -11.6609, -5.8177, 0.0487, -6.15, -6.4162, -3.3953),
  lon = c(28.8608, 30.25, 29.2228, 24.4818, 22.4166, 18.8162, 15.3136, 25.2, 25.4667,
          26.7333, 27.4794, 13.4717, 18.2603, 23.6, 20.8, 29.1378)
) %>%
  
  # merging with 2022, 2023, 2024 and 2030 dfs
  left_join (drc_cities_2030_2, by = "name") %>%
  left_join (drc_cities_2023_2, by = "name") %>%
  
  # computing deltas
  mutate (delta_2030_total = spend_total_2030 - spend_total_2023,
          delta_2030_cc = spend_cc_2030 - spend_cc_2023,
          delta_2030_total_pct = ((spend_total_2030 - spend_total_2023) / spend_total_2023) * 100)

## building 2023 totals map  ---------------------------------------------------
map_total_2023 <- leaflet() %>%
  setView(lng = 23.6, lat = -2.5, zoom = 6)

# Add tiles to the map
map_total_2023 <- map_total_2023 %>%
  addTiles("Stamen.TonerLite") %>%
  addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE))

# Calculate the size of dots proportional to the city population
dot_size <- sqrt(cities$spend_total_2023 / max(cities$spend_total_2023)) * 20

# Add city dots with popups to the map
for (i in 1:nrow(cities)) {
  city <- cities[i, ]
  map_total_2023 <- map_total_2023 %>%
    addCircleMarkers(
      lng = city$lon, lat = city$lat,
      radius = dot_size[i],
      color = "red",
      fillOpacity = 0.8,
      label = paste0(city$name),
      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "bottom")
    )
}

# Display the map
map_total_2023


## building 2030 totals map ----------------------------------------------------
map_total_2030 <- leaflet() %>%
  setView(lng = 23.6, lat = -2.5, zoom = 6)

# Add tiles to the map
map_total_2030 <- map_total_2030 %>%
  addTiles("Stamen.TonerLite") %>%
  addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE))

# Calculate the size of dots proportional to the city population
dot_size <- sqrt(cities$spend_total_2030 / max(cities$spend_total_2030)) * 20

# Add city dots with popups to the map
for (i in 1:nrow(cities)) {
  city <- cities[i, ]
  map_total_2030 <- map_total_2030 %>%
    addCircleMarkers(
      lng = city$lon, lat = city$lat,
      radius = dot_size[i],
      color = "red",
      fillOpacity = 0.8,
      label = paste0(city$name),
      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "bottom")
    )
}

# Display the map
map_total_2030



## building 2023 ccs map  ---------------------------------------------------
map_cc_2023 <- leaflet() %>%
  setView(lng = 23.6, lat = -2.5, zoom = 6)

# Add tiles to the map
map_cc_2023 <- map_cc_2023 %>%
  addTiles("Stamen.TonerLite") %>%
  addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE))

# Calculate the size of dots proportional to the city population
dot_size <- sqrt(cities$spend_cc_2023 / max(cities$spend_cc_2023)) * 20

# Add city dots with popups to the map
for (i in 1:nrow(cities)) {
  city <- cities[i, ]
  map_cc_2023 <- map_cc_2023 %>%
    addCircleMarkers(
      lng = city$lon, lat = city$lat,
      radius = dot_size[i],
      color = "red",
      fillOpacity = 0.8,
      label = paste0(city$name),
      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "bottom")
    )
}

# Display the map
map_cc_2023


## building 2030 ccs map ----------------------------------------------------
map_cc_2030 <- leaflet() %>%
  setView(lng = 23.6, lat = -2.5, zoom = 6)

# Add tiles to the map
map_cc_2030 <- map_cc_2030 %>%
  addTiles("Stamen.TonerLite") %>%
  addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE))

# Calculate the size of dots proportional to the city population
dot_size <- sqrt(cities$spend_cc_2030 / max(cities$spend_cc_2030)) * 20

# Add city dots with popups to the map
for (i in 1:nrow(cities)) {
  city <- cities[i, ]
  map_cc_2030 <- map_cc_2030 %>%
    addCircleMarkers(
      lng = city$lon, lat = city$lat,
      radius = dot_size[i],
      color = "red",
      fillOpacity = 0.8,
      label = paste0(city$name),
      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "bottom")
    )
}

# Display the map
map_cc_2030

## median age ------------------------------------------------------------------

## input data
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))
wdp3 <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 8, 12, Inf))

drc <- wdp3 %>% 
  filter (ccode == "COD" & year == 2023) %>%
  mutate (LowerBound = as.numeric(sub("\\[([^,]+).*", "\\1", age_group)),
         UpperBound = as.numeric(sub(".*?,([^\\)]+).*", "\\1", age_group))) %>%
  dplyr::group_by (ccode, year, age_group, LowerBound, ) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T))

# Calculate median age
median_age <- drc %>%
  dplyr::group_by (ccode, year) %>%
  dplyr::summarize(WeightedMedianAge = sum((LowerBound + UpperBound) / 2 * pop) / sum (pop))



## new age bar charts ----------------------------------------------------------

## Headcounts
age_drc_age_bar_hc_totalpop = wdp3 %>%
  
  # country-year hc per age group and spending group
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # recoding age groups into 10-year scale
  dplyr::mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)", "[10,15)"), "[0,15)",
                                     ifelse (age_group %in% c("[15,20)", "[20,25)","[25,30)"), "[15,35)",
                                             ifelse (age_group %in% c("[35,40)", "[40,45)","[45,50)", "[50,55)","[55,60)", "[60,65)"), "[35,65)", '[65,Inf)'
                                             )))) %>%
  
  # aggregating into new age groups
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>%
  
  # isolating DRC
  filter (ccode == "COD") %>%
  
  # Isolating 2023 and 2030
  filter (year %in% c(2023, 2030)) %>%
  
  # widening for bar plot 
  pivot_wider (names_from = "year", values_from = "pop") %>%
  
  # scaling pop to millions
  mutate (`2030` = `2030` / 10^6,
          `2023` = `2023` / 10^6) %>%
  
  ## creating plot foundation
  ggplot (aes (x = age_group)) + 
  
  # creating bars
  geom_bar (aes (y = `2030`), width = 0.8, stat = "identity", position = "identity", fill = "#6883BC") +
  geom_bar (aes (y = `2023`), width = 0.8, stat = "identity", position = "identity", fill = "#8A307F") + 
  
  # theme
  worlddataverse::theme_wdl() + 
  theme (axis.ticks.x = element_blank(),
         axis.text.x = element_text (angle = 360)) +
  
  # title 
  labs (title = "Age Breakdown of DRC Population: comparing headcounts 2023 and 2030",
        y = "Total Population (Millions)")

age_drc_age_bar_hc_totalpop

## Spending
age_drc_age_bar_exp_totalpop = wdp3 %>%
  
  # country-year hc per age group and spending group
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # recoding age groups into 10-year scale
  dplyr::mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)", "[10,15)"), "[0,15)",
                                     ifelse (age_group %in% c("[15,20)", "[20,25)","[25,30)"), "[15,35)",
                                             ifelse (age_group %in% c("[35,40)", "[40,45)","[45,50)", "[50,55)","[55,60)", "[60,65)"), "[35,65)", '[65,Inf)'
                                             )))) %>%
  
  # aggregating into new age groups
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # isolating DRC
  filter (ccode == "COD") %>%
  
  # Isolating 2023 and 2030
  filter (year %in% c(2023, 2030)) %>%
  
  # widening for bar plot 
  pivot_wider (names_from = "year", values_from = "exp") %>%
  
  # scaling pop to Billions
  mutate (`2030` = `2030` / 10^9,
          `2023` = `2023` / 10^9) %>%
  
  ## creating plot foundation
  ggplot (aes (x = age_group)) + 
  
  # creating bars
  geom_bar (aes (y = `2030`), width = 0.8, stat = "identity", position = "identity", fill = "#6883BC") +
  geom_bar (aes (y = `2023`), width = 0.8, stat = "identity", position = "identity", fill = "#8A307F") + 
  
  # theme
  worlddataverse::theme_wdl() + 
  theme (axis.ticks.x = element_blank(),
         axis.text.x = element_text (angle = 360)) +
  
  # title 
  labs (title = "Age Breakdown of DRC Spending: comparing spending 2023 and 2030",
        y = "Total spending (Billions)")

age_drc_age_bar_exp_totalpop

## Headcounts (8+)
age_drc_age_bar_hc = wdp3 %>%
  
  # country-year hc per age group and spending group
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # new spending groups
  dplyr::mutate (daily_spending = ifelse (daily_spending == "[0,8)", "[0,8)", "[8,Inf)")) %>%
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>%
  
  # isolating consumer class
  filter (daily_spending == "[8,Inf)") %>%
  
  # recoding age groups into 10-year scale
  dplyr::mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)", "[10,15)"), "[0,15)",
                                     ifelse (age_group %in% c("[15,20)", "[20,25)","[25,30)"), "[15,35)",
                                             ifelse (age_group %in% c("[35,40)", "[40,45)","[45,50)", "[50,55)","[55,60)", "[60,65)"), "[35,65)", '[65,Inf)'
                                             )))) %>%
  
  # aggregating into new age groups
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>%
  
  # isolating DRC
  filter (ccode == "COD") %>%
  
  # Isolating 2023 and 2030
  filter (year %in% c(2023, 2030)) %>%
  
  # widening for bar plot 
  pivot_wider (names_from = "year", values_from = "pop") %>%
  
  ## creating plot foundation
  ggplot (aes (x = age_group)) + 
  
  # creating bars
  geom_bar (aes (y = `2030`), width = 0.8, stat = "identity", position = "identity", fill = "#6883BC") +
  geom_bar (aes (y = `2023`), width = 0.8, stat = "identity", position = "identity", fill = "#8A307F") + 
  
  # theme
  worlddataverse::theme_wdl() + 
  theme (axis.ticks.x = element_blank(),
         axis.text.x = element_text (angle = 360)) +
  
  # title 
  labs (title = "Age Breakdown of DRC Consumers: comparing headcounts 2023 and 2030",
        y = "CC headcounts")

age_drc_age_bar_hc

## Spending (8+)
age_drc_age_bar_exp = wdp3 %>%
  
  # country-year hc per age group and spending group
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # new spending groups
  dplyr::mutate (daily_spending = ifelse (daily_spending == "[0,8)", "[0,8)", "[8,Inf)")) %>%
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # isolating consumer class
  filter (daily_spending == "[8,Inf)") %>%
  
  # recoding age groups into 10-year scale
  dplyr::mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)", "[10,15)"), "[0,15)",
                                     ifelse (age_group %in% c("[15,20)", "[20,25)","[25,30)"), "[15,35)",
                                             ifelse (age_group %in% c("[35,40)", "[40,45)","[45,50)", "[50,55)","[55,60)", "[60,65)"), "[35,65)", '[65,Inf)'
                                             )))) %>%
  
  # aggregating into new age groups
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # isolating DRC
  filter (ccode == "COD") %>%
  
  # Isolating 2023 and 2030
  filter (year %in% c(2023, 2030)) %>%
  
  # widening for bar plot 
  pivot_wider (names_from = "year", values_from = "exp") %>%
  
  ## creating plot foundation
  ggplot (aes (x = age_group)) + 
  
  # creating bars
  geom_bar (aes (y = `2030`), width = 0.8, stat = "identity", position = "identity", fill = "#6883BC") +
  geom_bar (aes (y = `2023`), width = 0.8, stat = "identity", position = "identity", fill = "#8A307F") + 
  
  # theme
  worlddataverse::theme_wdl() + 
  theme (axis.ticks.x = element_blank(),
         axis.text.x = element_text (angle = 360)) +
  
  # title 
  labs (title = "Age Breakdown of DRC Consumers: comparing headcounts 2023 and 2030",
        y = "Spending")

age_drc_age_bar_exp

## Cities -------------------------------------------------------------------------------------------------------------------------

## cities increase in total spending as %

drc_cities_pct = city_distribution_pop_shares %>%
  filter (ccode == "COD" & year %in% c(2023, 2030)) %>%
  dplyr::group_by (name, year) %>%
  dplyr::summarise (headcount = sum (headcount, na.rm = T),
                    spending = sum (spending, na.rm = T)) %>%
  pivot_wider (names_from = "year", values_from = 3:4) %>%
  mutate (spend_delta = spending_2030 - spending_2023, 
          spend_delta_pct = ((spending_2030 - spending_2023) / spending_2023)*100) 

## cities 8+ people
drc_cities_8plus = city_distribution_pop_shares %>%
  filter (ccode == "COD" & year == 2023) %>%
  filter (spending_threshold %in% c(8:999)) %>%
  dplyr::group_by (name, year) %>%
  dplyr::summarise (headcount = sum (headcount, na.rm = T),
                    spending = sum (spending, na.rm = T)) %>%
  left_join (drc_cities_pop, by = c("year", "name")) %>% ungroup () %>%
  mutate (pct = scales::percent (spending / sum (spending, na.rm = T),1))

kinshasa = city_distribution_pop_shares %>%
  filter (name == "Kinshasa") %>%
  dplyr::group_by (name, year) %>%
  dplyr::summarise (headcount = sum (headcount, na.rm = T),
                    spending = sum (spending, na.rm = T)) %>%
  mutate (spending = spending / 10^9) %>%
  ggplot (aes (x = factor (year), y = spending)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() +
  labs (y = "Spending (billions USD)",
        x = "Year")
kinshasa
 
## Pop x Pop-Density Map ----------------------------------------------------------------------------------------------------------------

## pop density data 
popdensity_dat = read.csv ("~/Desktop/ADM2_pop.csv") %>%
  
  # isolating variables of interest
  dplyr::select (shapeName, gpw_v4_rev11_density.2020.mean, gpw_v4_rev11_count.2020.sum) %>%
  
  # renaming 
  rename_with (
    ~ case_when (
      . == "shapeName" ~ "name",
      . == "gpw_v4_rev11_density.2020.mean" ~ "popdensity",
      . == "gpw_v4_rev11_count.2020.sum" ~ "poptotal",
      TRUE ~ .
    )
  )
  
## city center data 
library(sf)
drc_spdf2 <- st_read ("COD_CITIES_20180906H.shp")
drc_spdf2_plot <- ggplot(drc_spdf2) +
  geom_sf() + 
  theme_void ()

## colour coding the cities based on population x population density

# adding population x population density parameters
drc_spdf2_withpop = drc_spdf2 %>% 
  
  # joining with popdensity_dat
  left_join (popdensity_dat, by = "name") %>%
  
  # remove NAs from popdensity_dat
  filter (!is.na (popdensity)) %>%
  filter (!is.na (poptotal))

# creating quantiles for categorisation 
quantiles_popdensity <- quantile (drc_spdf2_withpop$popdensity, probs = c(0, 0.33, 0.66, 1))
quantiles_poptotal <- quantile (drc_spdf2_withpop$poptotal , probs = c(0, 0.33, 0.66, 1))

# creating new variables for levels
drc_spdf2_withpop$popdensity_level <- cut (drc_spdf2_withpop$popdensity,
                                           breaks = quantiles_popdensity,
                                           labels = c("low_density", "middle_density", "high_density"),
                                           include.lowest = TRUE)

drc_spdf2_withpop$poptotal_level <- cut (drc_spdf2_withpop$poptotal,
                                           breaks = quantiles_poptotal,
                                           labels = c("low_population", "middle_population", "high_population"),
                                           include.lowest = TRUE)

# create a unified classification variable
drc_spdf2_withpop_wclass = drc_spdf2_withpop %>% 
  
  # adding single classifier variable
  unite ("density_pop", popdensity_level:poptotal_level, sep = "_") %>%
  
  # changing levels 
  mutate (density_pop = factor (density_pop, levels = c("low_density_low_population",
                                                        "low_density_middle_population",
                                                        "middle_density_low_population",
                                                        "middle_density_middle_population",
                                                        "middle_density_high_population",
                                                        "high_density_low_population", 
                                                        "high_density_middle_population", 
                                                        "high_density_high_population"))) %>%
  
  # add a colouring variable
  mutate (colour = ifelse (density_pop == "low_density_low_population", "#ECF4F4",
                           ifelse (density_pop == "low_density_middle_population", "#DFF2C8",
                                   ifelse (density_pop == "middle_density_low_population", "#C9C5D9",
                                           ifelse (density_pop == "middle_density_middle_population", "#C9C5D9",
                                                   ifelse (density_pop == "middle_density_high_population", "#95C5C3",
                                                           ifelse (density_pop == "high_density_low_population", "#7ACF91",
                                                                   ifelse (density_pop == "high_density_middle_population", "#A188A8",
                                                                           ifelse (density_pop == "high_density_high_population", "#3F908D", "black"
                                                                           )))))))))

# creating colour gradient
d <- expand.grid (density = 1:3, pop = 1:3)
d <- merge (d, data.frame (density = 1:3, xlabel = c("Low Density", "Middle Density","High Density")), by="density")
d <- merge (d, data.frame (pop = 1:3, ylabel = c("Low Population", "Middle Population","High Population")), by="pop")

g.legend <-
  ggplot (d, aes(density, pop, 
                 fill=atan(pop/density), 
                 alpha = density + pop, 
                 label = paste0 (xlabel,"\n",ylabel))) +
  geom_tile ()+
  geom_text (alpha=1)+
  scale_fill_viridis()+
  theme_void()+
  theme (legend.position = "none",
         panel.background = element_blank(),
         plot.margin = margin (t=10, b=10, l=10))+
  labs (title="A bivariate color scheme (Viridis)", x="Density", y="Population")+
  theme (axis.title = element_text (color="black"))+
  # Draw some arrows:
  geom_segment (aes(x=1, xend = 3 , y=0, yend = 0), linewidth=1.5,
                arrow = arrow (length = unit (0.6,"cm"))) +
  geom_segment (aes(x=0, xend = 0 , y=1, yend = 3), linewidth=1.5,
                arrow = arrow (length = unit (0.6,"cm")))

g.legend

# re-creating plot with colours
drc_spdf2_plot2 <- ggplot (drc_spdf2_withpop_wclass) +
  geom_sf (aes(color = density_pop)) + 
  #scale_color_gradient (low = "blue", high = "red", name = "Population Density") +
  #scale_size_continuous (range = c(1, 10), name = "Total Population") +
  theme_void () + 
  scale_color_manual (values = c("#ECF4F4",
                                 "#DFF2C8", 
                                 "#C9C5D9", 
                                 "#95C5C3", 
                                 "#7ACF91", 
                                 "#A188A8", 
                                 "#6B85A9",
                                 "#3F908D")) + 
  theme (legend.position = "none")
  
drc_spdf2_plot2

# Create a leaflet map centered on the Democratic Republic of the Congo (DRC)
map <- leaflet() %>%
  setView(lng = 23.6, lat = -2.5, zoom = 6)

# Add tiles to the map
map <- map %>%
  addTiles("Stamen.TonerLite") %>%
  addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE))

# Add city dots with popups to the map
for (i in 1:nrow(drc_spdf2_withpop_wclass)) {
  city <- drc_spdf2_withpop_wclass[i, ]
  map <- map %>%
    addCircleMarkers(
      lng = city$lon, lat = city$lat,
      color = city$colour,
      fillOpacity = 0.8,
      label = paste0(city$name),
      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "bottom")
    )
}
# Save the map as an HTML file
#saveWidget(map, "drc_cities_map.html", selfcontained = TRUE)
# Display the map
map

## Cities rescaling using new urban share --------------------------------------

## applying scaling factor to scale up by 28%
city_distribution_pop_shares_rescaled = city_distribution_pop_shares %>%
  
  # filtering for DRC
  filter (ccode == "COD") %>%
  
  # rescaling 
  mutate (headcount = round (headcount * 1.28, 0),
          spending = spending * 1.28)

## recreating charts

wdp_drc = wdp %>%
  
  # aggregating
  dplyr::group_by (year, ccode) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # filtering for DRC and 8+
  filter (ccode == 'COD')
  #filter (daily_spending == "[8,Inf)")

drc_cities_8plus = city_distribution_pop_shares_rescaled %>%
  filter (ccode == "COD" & year == 2023) %>%
  filter (spending_threshold %in% c(8:999)) %>%
  dplyr::group_by (name, year) %>%
  dplyr::summarise (headcount = sum (headcount, na.rm = T),
                    spending = sum (spending, na.rm = T)) %>%
  left_join (drc_cities_pop, by = c("year", "name")) %>% ungroup () %>%
  mutate (pct = scales::percent (spending / 4691764672, 1))


drc_cities_pct = city_distribution_pop_shares_rescaled %>%
  filter (ccode == "COD" & year %in% c(2023, 2030)) %>%
  dplyr::group_by (name, year) %>%
  dplyr::summarise (headcount = sum (headcount, na.rm = T),
                    spending = sum (spending, na.rm = T)) %>%
  pivot_wider (names_from = "year", values_from = 3:4) %>%
  mutate (spend_delta = spending_2030 - spending_2023, 
          spend_delta_pct = ((spending_2030 - spending_2023) / spending_2023)*100,
          spending_sharetotal = scales::percent (spending_2023 / 81944869943,1))






