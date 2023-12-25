
## clean environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, worlddataverse, ggimage)

## set font
worlddataverse::font_wdl()

## loading data ----------------------------------------------------------------

# building function to load WEC data
load_wec_dat = function (){
  
  require (worlddataverse)
  
  # loading paths
  base_path <- worlddataverse::get_wdl_path()
  if (is.na (base_path)) {base_path = file.path("/Volumes/",
                                                "GoogleDrive-115239951252043738907",
                                                "Shared drives",
                                                "DATA_WDL")}
  
  # load binary (Rda) file
  load (file.path (base_path,
                   "world_emissions_clock",
                   "01_data",
                   "WEC_data_binary_new.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  
}

# loading WEC data
load_wec_dat()

## input data ------------------------------------------------------------------

# initial cleam
wec_clean = wec_dat %>%
  
  # computing BAU totals per country and year
  group_by (iso3c, year, sector) %>%
  summarise (emissions = sum (base, na.rm = T)) %>%
  
  # keeping 2022, 2030 and 2040
  filter (year %in% c(2023)) %>%
  
  # arrange by emissions and year
  arrange (sector, desc (emissions)) %>%
  arrange (year) %>%
  
  # filter for top 25 emitters 
  filter (iso3c %in% c("CHN","USA","IND","BRA","IDN","RUS","JPN","IRN","SAU","CAN",
                       "MEX","COD","KOR","DEU","TUR","PAK","AUS","GBR","VNM","ZAF",
                       "ARG","MYS","THA","KAZ","EGY")) %>%
  
  # scaling down the numbers
  mutate (emissions = emissions / 1000000000) %>%
  
  # adding iso2c for ggflags
  mutate (iso2c = countrycode (iso3c, "iso3c", "iso2c")) %>%
  mutate (iso2c = tolower(iso2c)) 

## bar plot --------------------------------------------------------------------

## color-coded by sector
totals = wec_clean %>%
  group_by (iso3c) %>%
  summarise (total = round (sum (emissions, na.rm = T), 2))

wec_clean = wec_clean %>% left_join (totals, by = "iso3c")
  
bar = ggplot (data = wec_clean, aes (x = reorder (iso3c, emissions), y = emissions, group = sector, fill = sector)) +
  geom_bar (stat = "identity", position = "stack") + 
  geom_flag (y = -0.5, aes (image = iso2c), by = "height") + 
  worlddataverse::scale_fill_wdl2() + 
  worlddataverse::theme_wdl () + 
  theme (axis.title.x = element_text (margin = margin(t = 0, r = 0, b = 5, l = 0)),
         axis.title.y = element_blank (),
         axis.ticks.x = element_blank (),
         axis.ticks.y = element_blank (),
         axis.text.x = element_text (angle = 360)) +
  expand_limits (y = -0.5) + 
  scale_y_continuous (breaks = seq(0,15,1)) +
  coord_flip () +
  geom_text (aes (iso3c, total + 1, label = total)) + 
  labs (title = "Top 25 Global GHG Emitters",
        subtitle = "volume of GHT in GT emitted in 2023",
        y = "\nGHG Emissions Volume, in GT")
bar

## color coded by OECD vs emerging

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

totals = wec_clean %>%
  group_by (iso3c) %>%
  summarise (total = round (sum (emissions, na.rm = T), 2))

c = wec_dat %>%
  # computing BAU totals per country and year
  group_by (iso3c, year, sector) %>%
  summarise (emissions = sum (base, na.rm = T)) %>%
  
  # keeping 2022, 2030 and 2040
  filter (year %in% c(2023)) %>%
  
  # arrange by emissions and year
  arrange (sector, desc (emissions)) %>%
  arrange (year) %>%
  
  # world totals
  group_by (year) %>%
  summarise (world_total = sum (emissions, na.rm = T)) %>%
  mutate (world_total = world_total / 1000000000)

wec_clean_oecd = wec_dat %>%
  
  # computing BAU totals per country and year
  group_by (iso3c, year) %>%
  summarise (emissions = sum (base, na.rm = T)) %>%
  
  # keeping 2022, 2030 and 2040
  filter (year %in% c(2023)) %>%
  
  # arrange by emissions and year
  arrange (desc (emissions)) %>%
  arrange (year) %>%
  
  # filter for top 25 emitters 
  filter (iso3c %in% c("CHN","USA","IND","BRA","IDN","RUS","JPN","IRN","SAU","CAN",
                       "MEX","COD","KOR","DEU","TUR","PAK","AUS","GBR","VNM","ZAF",
                       "ARG","MYS","THA","KAZ","EGY")) %>%
  
  # scaling down the numbers
  mutate (emissions = emissions / 1000000000) %>%
  
  # adding iso2c for ggflags
  mutate (iso2c = countrycode (iso3c, "iso3c", "iso2c")) %>%
  mutate (iso2c = tolower(iso2c)) %>%
  
  # adding oecd or not variable 
  mutate (Market = ifelse (iso3c %in% OECD_ISO3, "OECD", "Emerging"),
          Market = as.factor (Market)) %>%
  
  # totals
  left_join (totals, by = "iso3c") %>%
  left_join (c, by = "year") %>%
  
  # adding shares
  mutate (share = emissions / world_total,
          pct_share = scales::percent(share))
  


bar_2 = ggplot (data = wec_clean_oecd, aes (x = reorder (iso3c, emissions), y = emissions, fill = Market)) +
  geom_bar (stat = "identity", position = "stack") + 
  geom_flag (y = -0.5, aes (image = iso2c), by = "height") + 
  worlddataverse::theme_wdl () + 
  theme (axis.title.x = element_text (margin = margin(t = 0, r = 0, b = 5, l = 0)),
         axis.title.y = element_blank (),
         axis.ticks.x = element_blank (),
         axis.ticks.y = element_blank (),
         axis.text.x = element_text (angle = 360)) +
  expand_limits (y = -0.5) + 
  scale_y_continuous (breaks = seq(0,15,1)) +
  coord_flip () +
  geom_text (aes (iso3c, total + 1, label = total)) + 
  labs (title = "Top 25 Global GHG Emitters",
        subtitle = "volume of GHT in GT emitted in 2023",
        y = "\nGHG Emissions Volume, in GT")
bar_2


## rank plot -------------------------------------------------------------------

## data input 

# initial cleam
wec_clean = wec_dat %>%
  
  # computing BAU totals per country and year
  group_by (iso3c, year) %>%
  summarise (emissions = sum (base, na.rm = T)) %>%
  
  # keeping 2022, 2030 and 2040
  filter (year %in% c(2022, 2050)) %>%
  
  # arrange by emissions and year
  arrange (desc (emissions)) %>%
  arrange (year) %>%
  
  # filter for top 25 emitters 
  filter (iso3c %in% c("CHN","USA","IND","BRA","IDN","RUS","JPN","IRN","SAU","CAN",
                       "MEX","COD","KOR","DEU","TUR","PAK","AUS","GBR","VNM","ZAF",
                       "ARG","MYS","THA","KAZ","EGY")) %>%
  
  # scaling down the numbers
  mutate (emissions = emissions / 1000000000) %>%
  
  # adding iso2c for ggflags
  mutate (iso2c = countrycode (iso3c, "iso3c", "iso2c")) %>%
  mutate (iso2c = tolower(iso2c)) %>%
  
  # compute ranking variable
  group_by (year) %>%
  mutate (rank = rank (-emissions, ties.method = "first")) 

rank22dfcc = wec_clean %>% ungroup() %>% filter (year == 2022) %>% dplyr::select (iso3c, rank)
rank50dfcc = wec_clean %>% ungroup() %>% filter (year == 2050) %>% dplyr::select (iso3c, rank)
rankdfcc <- merge (rank22dfcc, rank50dfcc, by = "iso3c", suffixes = c("_22", "_50"))
rankdfcc <- rankdfcc %>%
  mutate (country = countrycode (iso3c, origin = "iso3c", destination = "country.name")) %>%
  mutate (rank_diff = rank_50 - rank_22) %>%
  mutate (color_diff = case_when (
    rank_diff == 0 ~ "#0F1290",
    rank_diff > 0 ~ "#00A046",
    rank_diff < 0 ~ "#AA0E2F")) %>%
  left_join (wec_clean %>% 
               dplyr::select (iso3c, year, emissions) %>%
               pivot_wider (names_from = "year", values_from = "emissions"), by = "iso3c") %>%
  rename_with (
    ~ case_when (
      . == "2022" ~ "emissions_gt_2022",
      . == "2050" ~ "emissions_gt_2050",
      TRUE ~ .
    )
  ) %>%
  dplyr::select (iso3c, rank_diff, color_diff)

datcc <- merge (wec_clean, rankdfcc, by = "iso3c") %>%
  mutate (iso2c = tolower (countrycode (iso3c, "iso3c", "iso2c"))) %>%
  mutate (country = countrycode (iso3c, "iso3c", "country.name")) 

## plot theme 
my_theme <- function() {
  
  # Colors
  color.background = "white"
  color.text = "#22211d"
  
  # Begin construction of chart
  theme_minimal(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background,
                                          color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background,
                                          color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    
    # Format the legend
    theme(legend.position = "none") +
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=18, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold",
                                          vjust=1.25)) +
    theme(axis.text.x      = element_text(size=18, vjust=0.5, hjust=0.5,
                                          color = color.text)) +
    theme(axis.text.y      = element_blank()) +
    theme(axis.ticks.y     = element_blank()) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = margin(t = 0.5, r = 2, b = 0, l = 2, "cm"))
}

## plot
ggplot (datcc, aes (x = as.factor (year), y = rank, group = iso3c)) +
  geom_line (aes(color = color_diff), size = 1) +
  geom_point (aes(color = color_diff), size = 4) +
  scale_y_reverse(breaks = 1:30) +
  coord_cartesian(ylim = c(29.2, 1), clip = "off") +
  scale_x_discrete(position = "top") +
  theme(legend.position = 'none') +
  geom_text(data = datcc %>% filter(year == 2022),
            aes(label = country, x = 0.75) , hjust = .5,
            fontface = "bold", color = "black", size = 4) +
  geom_text(data = datcc %>% filter(year == 2050),
            aes(label = country, x = 2.25) , hjust = 0.5,
            fontface = "bold", color = "black", size = 4) +
  geom_flag(data = datcc %>% filter(year == 2022),
            aes(x = 0.5, y = rank, image = iso2c)) +
  geom_flag(data = datcc %>% filter(year == 2050),
            aes(x = 2.5, y = rank, image = iso2c)) +
  geom_text(data = datcc %>% filter(year == 2050),
            aes(x = 2.59, y = rank, label = rank_diff, 
                color = color_diff, fontface = "bold", size = 4)) +
  geom_text(data = datcc %>% filter(year == 2022),
            aes (x = 0.3, y = rank, label = round (emissions, 2),
                 fontface = "bold", color = "black", size = 4)) + 
  geom_text(data = datcc %>% filter(year == 2050),
            aes (x = 2.7, y = rank, label = round (emissions, 2),
                 fontface = "bold", color = "black", size = 4)) +
  #geom_text(aes(x = 2.55, y = -1, label = "Change \nin rank", size = 4)) +
  geom_text(aes(x = 2.71, y = -1, label = "Emissions \nGT", size = 4)) +
  geom_text(aes(x = 0.3, y = -1, label = "Emissions \nGT", size = 4)) +
  labs(title = "Top 25 emitters: change in rank from 2022 to 2050",
       x = 'Year') +
  scale_color_manual (values = c("#00A046", "#0F1290", "#AA0E2F", "black")) +                                                          
  my_theme() +
  theme(axis.title.y = element_blank(),
        text = element_text(size = 50),
        plot.title = element_text(hjust = 0.5))

write.csv (wec_clean, "wec_top25.csv", row.names = T)


## donut plot for energy -------------------------------------------------------

## input data
wec_clean_donut = wec_dat %>%
  
  # filter for 2023
  filter (year == 2023) %>%
  
  # group by year, country, sectoe and compute totals for emissions and population
  group_by (year, iso3c, sector) %>%
  summarise (base = sum (base, na.rm = T),
             pop = mean (pop, na.rm = T)) %>%
  ungroup () %>%
  
  # group by year and sector and compute emissions and population totals
  group_by (year, sector) %>%
  summarise (base = sum (base, na.rm = T),
             pop = sum (pop, na.rm = T)) %>%
  
  # compute per capita emissions per sector
  mutate (pop = replace (pop, sector == "Transport", pop [sector == "Transport"] / 2),
          em_pc = base / pop) %>%
  ungroup () %>%
  dplyr::select (sector, em_pc) %>%
  
  # compute percentages 
  mutate (fraction = em_pc / sum (em_pc)) %>% 
  
  # compute cumulative percentages 
  mutate (ymax = cumsum (fraction)) %>% 
  
  # compute bottom of each rectangle
  mutate (ymin = c(0, head (ymax, n = -1))) %>%
  
  # compute central label position
  mutate (labelPosition = c(0.1049087, 0.23, 0.46, 0.74, 0.93)) %>%
  
  # compute labels 
  mutate (label = c ("Agriculture: \n1.5 T", "Building: \n0.4 T", "Energy: \n2.6 T", "Industry: \n1.7 T", "Transport: \n1.1 T")) %>%
  
  # changing levels of sector
  mutate (sector = factor (sector, levels = c("Agriculture", "Building", "Energy", "Industry", "Transport")))

## constructing donut

wec_donut = ggplot (wec_clean_donut, aes (ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = sector)) + 
  geom_rect () + 
  geom_text (x = 2, aes (y = labelPosition, label = label, color = sector), size = 5) + 
  worlddataverse::scale_fill_wdl2() + 
  worlddataverse::scale_color_wdl2() +
  coord_polar (theta = "y") + 
  xlim (c(-1, 4)) +
  worlddataverse::theme_wdl() + 
  theme_void () + 
  theme (legend.position = "none")
wec_donut


## energy bar chart ------------------------------------------------------------

## input data 
wec_clean_energybar = wec_clean_donut_country %>%
  
  # filtering for relevant countries and sector
  filter (iso3c %in% c("DEU", "ESP", "CHN", "MNG", "CHE")) %>%
  filter (sector == "Energy") %>%
  
  # rounding figures
  mutate (em_pc = round (em_pc, 1)) %>%
  
  # adding iso2
  mutate (iso2c = tolower (countrycode (iso3c, "iso3c", "iso2c")))

## constructing plot
wec_clean_energybar_plot = ggplot (data = wec_clean_energybar, aes (x = reorder (iso3c, em_pc), y = em_pc)) + 
  geom_bar (stat = "identity") + 
  geom_flag (y = -0.5, aes (image = iso2c), by = "height") + 
  worlddataverse::scale_fill_wdl2() + 
  worlddataverse::theme_wdl() + 
  theme (axis.ticks.x = element_blank (),
         axis.text.x = element_blank ()) +
  ylim (-1, 10) + 
  labs (x = "Country",
        y = "Emissions per capita in 2023, T")






  
  
  
  
  
  
  
  
  







