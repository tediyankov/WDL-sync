
## empty environment
rm (list = ls())

library (data.table)

## loading data ----------------------------------------------------------------

# building function to load WEC data
load_wec_dat = function (){
  
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
                   "WEC_data_binary_20230329.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  
}

# loading WEC data
load_wec_dat()

## sectoral rankings -----------------------------------------------------------

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
  dplyr::select(-rank) %>%
  mutate (em_pc = ifelse (sector == "Energy",
                          wec_dat_oecd_3$em_pc [wec_dat_oecd_3$iso3c == "ESP"],
                          em_pc)) %>%
  mutate (country = ifelse (sector == "Energy",
                            "Spain",
                            country))

sum (pangea$em_pc) # 4.656288


## bar chart -------------------------------------------------------------------

# world avg data input
world_avg = wec_dat %>%
  filter (year == 2022) %>%
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
  
  oecd = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    mutate (em_pc = emissions / pop) %>%
    group_by (year, sector) %>%
    summarise (em_pc = mean (em_pc, na.rm = T))
  
}
OECD_avg <- fun_OECD (2022) %>% ungroup %>% dplyr::select (-year)

# EU avg data input
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
    summarise (emissions = sum(base)) %>%
    mutate (em_pc = emissions / pop) %>%
    group_by (year, sector) %>%
    summarise (em_pc = mean (em_pc, na.rm = T))
  
}
EU_avg <- fun_EU (2022) %>% ungroup %>% dplyr::select (-year)

# North America data input
fun_NA = function (year) {
  
  year = year
  
  north_america = c("United States", "Canada")
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
NA_avg <- fun_NA (2022) %>% ungroup %>% dplyr::select (-year)

# Pangea data input
pangea_avg = pangea %>% dplyr::select (sector, em_pc)

# unified input data frame
bar_input = rbind (world_avg, EU_avg, OECD_avg, NA_avg, pangea_avg)
bar_input$case = rep (c("World", "EU", "OECD", "North America", "Pangea"), each = 5)
bar_input$case = factor (bar_input$case,
                         levels = c("World", "EU", "OECD", "North America", "Pangea"))

# creating the plot

pacman::p_load(tidyverse,ggthemes,showtext,png,grid,cowplot,hrbrthemes,magick)

pal_worlddataverse <- c("#0dcdc0", "#264653", "#e9c46a", "#f4a261", "#e76f51")

# get fonts
font_add_google("Work Sans", "worksans")
showtext_auto()

# get logo for branding

# for iMac
img <- readPNG(source = file.path("/Volumes/",
                                  "GoogleDrive-115239951252043738907",
                                  "Shared drives",
                                  "DATA_WDL",
                                  "worlddataverse",
                                  "wdl_logo.png"))

# for laptop
img <- readPNG(source = "/Volumes/GoogleDrive/Shared drives/DATA_WDL/worlddataverse/wdl_logo.png")

scale_fill_worlddataverse <- function(){
  structure(list(
    scale_fill_manual(values=pal_worlddataverse)
  ))
}

scale_colour_discrete_worlddataverse <- function (){
  structure(list(
    scale_color_manual(values = pal_worlddataverse)
  ))
}

theme_wdl <- function (base_size = 8, base_font = NA){
  theme_ipsum(base_size=8,base_family="worksans") +
    theme (
      text = element_text (colour = "#000245", face = "plain"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "#000245", margin = margin(0,0,8,0)),
      axis.title = element_text(size = 14, face = "italic", colour = "#202020"),
      axis.text.x = element_text (angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F5F5F5"),
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F5F5F5"),
      legend.text = element_text (size = 10, colour = "#000245", face = "plain"),
      legend.title = element_text (colour = "#000245", face = "bold")
    )
}

svg ("pangea_vs_world.svg")

bar_plot = ggplot (bar_input, aes (fill = sector, y = em_pc, x = case)) +
  geom_bar (position = "stack", stat = "identity") +
  theme_wdl() +
  scale_fill_worlddataverse() +
  labs (title = "Emissions per capita: Pangea vs World, EU, OECD",
        subtitle = "Base scenario, 2022",
        x = "Case",
        y = "Emissions per capita, T pc py")

bar_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

dev.off()





