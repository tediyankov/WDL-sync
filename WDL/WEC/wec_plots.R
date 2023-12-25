
## preliminaries ---------------------------------------------------------------

# function to load WEC data 

load_wec_dat = function (){
  
  require (worlddataverse, tidyverse, countrycode)
  
  # route to Google Drive
  base_path <- worlddataverse::get_wdl_path()
  
  # for German users
  get_drive_path <- function(){
    dplyr::case_when(
      dir.exists("G:/Geteilte Ablagen") ~ file.path("G:",
                                                    "Geteilte Ablagen",
                                                    "DATA_WDL"),
      dir.exists("G:/Shared Drives") ~ file.path("G:",
                                                 "Shared Drives",
                                                 "DATA_WDL"),
      dir.exists("/Volumes/GoogleDrive/Geteilte Ablagen") ~
        file.path("/Volumes",
                  "GoogleDrive",
                  "Geteilte Ablagen",
                  "DATA_WDL"))
  }
  
  if(is.na(base_path)) {
    base_path = get_drive_path()
  }
  
  # load binary (Rda) file
  load (file.path (base_path, 
                   "world_emissions_clock", 
                   "01_data", 
                   "WEC_data_binary_15072022_new.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # clean environment
  rm (list = c("IIASA_data_consolidated",
               "IIASA_subsector_shares",
               "IIASA_WDL_data_consolidated",
               "IIASA_WDL_data_probabilities",
               "WDL_IIASA_data_consolidated",
               "WDL_IIASA_data_consolidated_ind_essd",
               "WDL_IIASA_data_probabilities"
  )
  )
  
  .GlobalEnv$wec_dat = wec_dat
  
}

# loading WEC data
load_wec_dat()

## plot aesthetics -------------------------------------------------------------

# aesthetics

# packages
pacman::p_load(tibble,tidyverse,ggplot2,ggthemes,plyr,reshape2,grid,lubridate,stringr,showtext,png,grid,cowplot,hrbrthemes,magick)

# general sample of theme colours
pal_worlddataverse1 <- c("#613FC2","#000245","#A08CDA","#D848C4","#FCDC00","#FEF1D3","#00A046","#D6D6D6","#F5F5F5")

# generate purple gradient colors
pal_worlddataverse2 <- c("#191919","#120D25","#202020","#362870","#483595","#5B44BB","#7B68C7","#9C8DD5","#BDB3E3","#DED9F1")

# get fonts
font_add_google("Work Sans", "worksans")
showtext_auto()

# get logo for branding
img <- readPNG(source = "/Volumes/GoogleDrive/Shared drives/DATA_WDL/worlddataverse/wdl_logo.png")

# add to override default colour palette of dot plots with a WDL one (applicable when factor() is used in the aes())
scale_colour_discrete_worlddataverse <- function (){
  structure(list(
    scale_color_manual(values = pal_worlddataverse1)
  ))
}

theme_wdl <- function (base_size = 8, base_font = NA){
  theme_ipsum(base_size=8,base_family="worksans") + 
    theme (
      text = element_text (colour = "#000245", face = "plain"), 
      plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "#000245", margin = margin(0,0,8,0)), 
      axis.title = element_text(size = 14, face = "italic", colour = "#202020"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F5F5F5"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F5F5F5"),
      legend.text = element_text (size = 10, colour = "#000245", face = "plain"),
      legend.title = element_text (colour = "#000245", face = "bold")
    )
}

## general plot GDP per capita vs emissions per capita -------------------------

detach("package:plyr", unload = TRUE)

# preprocessing data

wec_dat_totals = wec_dat %>%
  select (-c("h_cpol", "o_1p5c", "ndc")) %>%
  add_column (total_emissions = NA) %>%
  group_by (iso3c, year, sector, subsector) %>%
  summarise (total_emissions = sum (base))

wec_dat_info = wec_dat %>%
  ungroup() %>%
  select (c(iso3c, year, pop, gdp)) %>%
  distinct()

wec_dat_totals = wec_dat_totals %>%
  filter (year == 2020)  %>%
  left_join (wec_dat_info, by = c("iso3c", "year"))
    
wec_dat_totals$gdp.pc <- wec_dat_totals$gdp / wec_dat_totals$pop
wec_dat_totals$emissions.pc <- wec_dat_totals$total_emissions / wec_dat_totals$pop

wec_dat_totals = wec_dat_totals %>% filter (emissions.pc > 0)

# getting the R^2 values

r2_general <- ddply (wec_dat_totals,.(sector),
                     function(x) summary(lm(x$emissions.pc ~ x$gdp.pc))$r.squared)
names(r2_general) <- c("sector","r2")

# making the plot 

general_plot = ggplot (data = wec_dat_totals, aes (gdp.pc, emissions.pc, color = factor(sector))) +
  geom_point() + 
  geom_smooth (method = "lm", fill = NA) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "All sectors, data from 2020", x = "GDP per capita", y = "Emissions per capita") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() +
  scale_colour_discrete_worlddataverse() + 
  geom_text(data = r2_general,
            aes(color = sector, label = paste("R^2: ", r2, sep="")),
            parse=T, x=100000, y=c(65,55,45,35,25), 
            show_guide=F,
            family = "worksans")

general_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

## Agriculture plot GDP per capita vs emissions per capita ---------------------

# preprocessing data

wec_dat_totals_agri = wec_dat_totals %>% filter (sector == "Agriculture")

# making the plot 

agri_plot = ggplot (data = wec_dat_totals_agri, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Agriculture sector, data from 2020", x = "GDP per capita", y = "Emissions per capita") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() 
  
agri_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

## Building plot GDP per capita vs emissions per capita ---------------------

# preprocessing data

wec_dat_totals_build = wec_dat_totals %>% filter (sector == "Building")

# making the plot 

build_plot = ggplot (data = wec_dat_totals_build, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Building sector, data from 2020", x = "GDP per capita", y = "Emissions per capita") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() 

build_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

## Energy plot GDP per capita vs emissions per capita ---------------------

# preprocessing data

wec_dat_totals_energy = wec_dat_totals %>% filter (sector == "Energy")

# making the plot 

energy_plot = ggplot (data = wec_dat_totals_energy, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Energy sector, data from 2020", x = "GDP per capita", y = "Emissions per capita") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() 

energy_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

## Industry plot GDP per capita vs emissions per capita ---------------------

# preprocessing data

wec_dat_totals_ind = wec_dat_totals %>% filter (sector == "Industry")

# making the plot 

ind_plot = ggplot (data = wec_dat_totals_ind, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Industry sector, data from 2020", x = "GDP per capita", y = "Emissions per capita") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() 

ind_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

## Transport plot GDP per capita vs emissions per capita -----------------------

# preprocessing data

wec_dat_totals_trans = wec_dat_totals %>% filter (sector == "Transport")

# making the plot 

trans_plot = ggplot (data = wec_dat_totals_trans, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Transport sector, data from 2020", x = "GDP per capita", y = "Emissions per capita") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() 

trans_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

## plots w/ log scales on both -------------------------------------------------

## general plot without sector subsets

all_plot = ggplot (data = wec_dat_totals, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "all sectors, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita (log scale)\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() + 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    trans='log') +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

all_plot
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

## general plot w/ sector subsets

all_plot_sectors = ggplot (data = wec_dat_totals, aes (gdp.pc, emissions.pc, colour = factor(sector))) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm",
               formula = y ~ x + I(x^2), 
               size = 1,
               fill = NA) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "all sectors, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita (log scale)\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    trans='log') +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

all_plot_sectors
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

scale_color_continuous_worlddataverse() + 
  scale_fill_worlddataverse() +


## agriculture 

agri_plot_logboth = ggplot (data = wec_dat_totals_agri, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Agriculture, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita (log scale)\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() + 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    trans='log') +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

agri_plot_logboth
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

# building

build_plot_logboth = ggplot (data = wec_dat_totals_build, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Building, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita (log scale)\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() + 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    trans='log') +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

build_plot_logboth
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

# industry

ind_plot_logboth = ggplot (data = wec_dat_totals_ind, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Industry, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita (log scale)\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() + 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    trans='log') +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

ind_plot_logboth
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

# energy 

energy_plot_logboth = ggplot (data = wec_dat_totals_energy, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Energy, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita (log scale)\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() + 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    trans='log') +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

energy_plot_logboth
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

# energy 

trans_plot_logboth = ggplot (data = wec_dat_totals_trans, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Transport, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita (log scale)\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() + 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    trans='log') +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

trans_plot_logboth
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))


## plots w/ log scales on GDP only ---------------------------------------------

## general plot without sector subsets

all_plot_log_gdp = ggplot (data = wec_dat_totals, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "all sectors, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() + 
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

all_plot_log_gdp
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

## general plot w/ sector subsets

all_plot_sectors_log_gdp = ggplot (data = wec_dat_totals, aes (gdp.pc, emissions.pc, colour = factor(sector))) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm",
               formula = y ~ x + I(x^2), 
               size = 1,
               fill = NA) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "all sectors, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

all_plot_sectors_log_gdp
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

## agriculture 

agri_plot_log_gdp = ggplot (data = wec_dat_totals_agri, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Agriculture, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

agri_plot_log_gdp
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

# building

build_plot_log_gdp = ggplot (data = wec_dat_totals_build, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Building, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() + 
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

build_plot_log_gdp
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

# industry

ind_plot_log_gdp = ggplot (data = wec_dat_totals_ind, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Industry, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() + 
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

ind_plot_log_gdp
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

# energy 

energy_plot_log_gdp = ggplot (data = wec_dat_totals_energy, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Energy, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() + 
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

energy_plot_log_gdp
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

# transport 

trans_plot_log_gdp = ggplot (data = wec_dat_totals_trans, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "Transport, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

trans_plot_log_gdp
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

## plot without sector subset --------------------------------------------------

# data 
wec_dat_totals_nosectors = wec_dat %>%
  select (-c("h_cpol", "o_1p5c", "ndc")) %>%
  add_column (total_emissions = NA) %>%
  group_by (iso3c, year) %>%
  summarise (total_emissions = sum (base))

wec_dat_info = wec_dat %>%
  ungroup() %>%
  select (c(iso3c, year, pop, gdp)) %>%
  distinct()

wec_dat_totals_nosectors = wec_dat_totals_nosectors %>%
  filter (year == 2020)  %>%
  left_join (wec_dat_info, by = c("iso3c", "year"))

wec_dat_totals_nosectors$gdp.pc <- wec_dat_totals_nosectors$gdp / wec_dat_totals_nosectors$pop
wec_dat_totals_nosectors$emissions.pc <- wec_dat_totals_nosectors$total_emissions / wec_dat_totals_nosectors$pop

wec_dat_totals_nosectors = wec_dat_totals_nosectors %>% filter (emissions.pc > 0)


# general plot w/ log scale on both emissions and GDP

all_plot_nosectors_logboth = ggplot (data = wec_dat_totals_nosectors, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "All sectors, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita (log scale)\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    trans='log') +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log') + 

all_plot_nosectors_logboth
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))


# general plot w/ log scale only on GDP

all_plot_nosectors_log_gdp = ggplot (data = wec_dat_totals_nosectors, aes (gdp.pc, emissions.pc)) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm", 
               formula = y ~ x + I(x^2), size = 1,
               color = pal_worlddataverse1[1], fill = pal_worlddataverse2[9]) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "All sectors, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_colour_discrete_worlddataverse() +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log') 
  
all_plot_nosectors_log_gdp
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))

















