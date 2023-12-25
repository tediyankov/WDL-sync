
## Plot aesthetics -------------------------------------------------------------

# aesthetics

# packages
pacman::p_load(tibble,tidyverse,ggplot2,ggthemes,plyr,reshape2,grid,lubridate,stringr,showtext,png,grid,cowplot,hrbrthemes,magick)

# general sample of theme colours
pal_worlddataverse1 <- c("#613FC2","#000245","#A08CDA","#D848C4","#FCDC00","#FEF1D3","#00A046","#D6D6D6","#F5F5F5", "")

# generate purple gradient colors
pal_worlddataverse2 <- c("#003F5C","#2F4B7C","#665191","#A05195","#D45087","#F95D6A","#FF7C43","#FFA600","#191919","#120D25","#202020","#362870","#483595","#5B44BB","#7B68C7","#9C8DD5","#BDB3E3","#DED9F1")

# generate 3rd random colour scheme
pal_worlddataverse3 <- c("#613FC2","#A08CDA","#FCDC00","#00A046","#F5F5F5")

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

# add to override default "fill" colour palette with a WDL one (use for bar plots)
scale_fill_worlddataverse <- function(){
  structure(list(
    scale_fill_manual(values=pal_worlddataverse_4)
  ))
}

# add to override default colour palette of dot plots with a WDL one (applicable when factor() is used in the aes())
scale_colour_discrete_worlddataverse <- function (){
  structure(list(
    scale_color_manual(values = pal_worlddataverse_4)
  ))
}

# overrides default blue gradient (when one is needed) with a WDL purple gradient. Add to any graph that uses gradient scales (e.g. geom_point or geom_raster).
scale_color_continuous_worlddataverse <- function(){
  structure(list(
    scale_fill_gradientn(colours = pal_worlddataverse2)
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



## line chart (three ranks) ----------------------------------------------------

# reformatting data

input_data = em_pc_totals_future %>%
  mutate (rank = c(1,2,3)) %>%
  relocate (rank, .before = 1) %>%
  pivot_longer (2:30, names_to = "year", values_to = "total_em_pc")

# chart

input_data_1 = input_data [input_data$rank == 1,]

line_plot = ggplot (input_data, aes (x = year, y = total_em_pc, group = rank, color = factor (rank))) +
  geom_line () +
  labs (title = "High Wealth Low Emissions: \nchanges in emissions per capita over time",
        subtitle = "2022 - 2050",
        x = "year",
        y = "total yearly emissions per capita (all sectors), tonnes") +
  theme_ipsum(base_size=8,base_family="worksans") +
  theme_wdl() +
  scale_colour_discrete_worlddataverse()

line_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))



## line chart (best case, worst case, average) ---------------------------------
# best case scenario data ----
best_case = input_data [input_data$rank == 1,] %>%
  dplyr::select (-c(rank)) %>%
  rename_with (
    ~ case_when (
      . == "total_em_pc" ~ "best_case_em_pc",
      TRUE ~ .
    )
  )


# OECD average function ----

highWavgE_fun <- function (year){

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

  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)

  highWavgE = wec_dat_oecd_2 %>%
    ungroup() %>%
    group_by (year, sector, subsector) %>%
    summarise (em_pc = mean (em_pc)) %>%
    ungroup() %>%
    group_by (year) %>%
    summarise (em_pc = sum (em_pc))

  .GlobalEnv$highWavgE = highWavgE

}

values = c(2022:2050)
datalist = vector("list", length = length(unique(values)))

for (i in values) {

  datalist [[i]] <- highWavgE_fun (i)

}

highWavgE_future = do.call(rbind, datalist)


## worst case function: scenario 1 ----

highWhighE_fun_1 <- function (year) {

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

  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)

  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2
  highWhighE_1 = setDT(wec_dat_oecd_3)[,.SD[which.max(em_pc)],by = subsector]

  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWhighE_1$ID))
  highWhighE_2 = setDT(wec_dat_oecd_4)[,.SD[which.max(em_pc)],by = subsector]

  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWhighE_2$ID))
  highWhighE_3 = setDT(wec_dat_oecd_5)[,.SD[which.max(em_pc)],by = subsector]

  # binding into 1 data frame containing rankings per sub-sector
  highWhighE = rbind (highWhighE_1, highWhighE_2, highWhighE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 25)) %>%
    relocate (rank, .before = iso3c)

  # saving to environment
  .GlobalEnv$highWhighE = highWhighE

}

values = c(2022:2050)
datalist = vector("list", length = length(unique(values)))

for (i in values) {

  datalist [[i]] <- highWhighE_fun_1 (i)

}

highWhighE_future_1 = do.call(rbind, datalist)

# getting totals from highWhighE_future

values = unique (highWhighE_future_1$year)
n = length (values)
datalist = vector("list", length = n)

for (i in 1:n) {

  # subsetting highWhighE_future
  highWhighE_future_year = highWhighE_future_1 [highWhighE_future_1$year == values[i],]

  # creating df to store per capita totals
  em_pc_totals = data.frame (matrix (NA, nrow = 3, ncol = 1))

  # saving totals to em_pc_totals
  em_pc_totals[1,1] = sum (highWhighE_future_year$em_pc [highWhighE_future_year$rank == 1])
  em_pc_totals[2,1] = sum (highWhighE_future_year$em_pc [highWhighE_future_year$rank == 2])
  em_pc_totals[3,1] = sum (highWhighE_future_year$em_pc [highWhighE_future_year$rank == 3])

  datalist [[i]] <- em_pc_totals

}

em_pc_totals_worst_case_future_1 = do.call(cbind, datalist)
colnames (em_pc_totals_worst_case_future_1) = values

## worst case function: scenario 2 ----

highWhighE_fun_2 <- function (year) {

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

  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)

  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = mean (wec_dat_oecd_2$gdp)

  ## for all sectors except for energy_oil

  wec_dat_oecd_2_nooil = wec_dat_oecd_2 %>% filter (!(subsector == "energy_oil"))

  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2_nooil %>% filter (gdp > gdp_threshold)
  highWhighE_1 = setDT(wec_dat_oecd_3)[,.SD[which.max(em_pc)],by = subsector]

  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWhighE_1$ID))
  highWhighE_2 = setDT(wec_dat_oecd_4)[,.SD[which.max(em_pc)],by = subsector]

  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWhighE_2$ID))
  highWhighE_3 = setDT(wec_dat_oecd_5)[,.SD[which.max(em_pc)],by = subsector]

  # binding into 1 data frame containing rankings per sub-sector
  highWhighE_nooil = rbind (highWhighE_1, highWhighE_2, highWhighE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 24)) %>%
    relocate (rank, .before = iso3c)

  ## for energy_oil
  wec_dat_oecd_2_oil = wec_dat_oecd_2 %>% filter (subsector == "energy_oil")

  # first minimum emissions
  wec_dat_oecd_3_oil = wec_dat_oecd_2_oil
  highWhighE_1_oil = setDT(wec_dat_oecd_3_oil)[,.SD[which.max(em_pc)],by = subsector]

  # second minimum emissions
  wec_dat_oecd_4_oil = wec_dat_oecd_3_oil %>% filter (!(ID %in% highWhighE_1_oil$ID))
  highWhighE_2_oil = setDT(wec_dat_oecd_4_oil)[,.SD[which.max(em_pc)],by = subsector]

  # third minimum emissions
  wec_dat_oecd_5_oil = wec_dat_oecd_4_oil %>% filter (!(ID %in% highWhighE_2_oil$ID))
  highWhighE_3_oil = setDT(wec_dat_oecd_5_oil)[,.SD[which.max(em_pc)],by = subsector]

  highWhighE_oil = rbind (highWhighE_1_oil, highWhighE_2_oil, highWhighE_3_oil) %>%
    dplyr::select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 1)) %>%
    relocate (rank, .before = iso3c)

  highWhighE = rbind (highWhighE_nooil, highWhighE_oil) %>% arrange (sector)

  # saving to environment
  .GlobalEnv$highWhighE = highWhighE

}

values = c(2022:2050)
datalist = vector("list", length = length(unique(values)))

for (i in values) {

  datalist [[i]] <- highWhighE_fun_2 (i)

}

highWhighE_future_2 = do.call(rbind, datalist)

# getting totals from highWhighE_future

values = unique (highWhighE_future_2$year)
n = length (values)
datalist = vector("list", length = n)

for (i in 1:n) {

  # subsetting highWhighE_future
  highWhighE_future_year = highWhighE_future_2 [highWhighE_future_2$year == values[i],]

  # creating df to store per capita totals
  em_pc_totals = data.frame (matrix (NA, nrow = 3, ncol = 1))

  # saving totals to em_pc_totals
  em_pc_totals[1,1] = sum (highWhighE_future_year$em_pc [highWhighE_future_year$rank == 1])
  em_pc_totals[2,1] = sum (highWhighE_future_year$em_pc [highWhighE_future_year$rank == 2])
  em_pc_totals[3,1] = sum (highWhighE_future_year$em_pc [highWhighE_future_year$rank == 3])

  datalist [[i]] <- em_pc_totals

}

em_pc_totals_worst_case_future_2 = do.call(cbind, datalist)
colnames (em_pc_totals_worst_case_future_2) = values

## worst case function: scenario 3 ----

highWhighE_fun_3 <- function (year) {

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

  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)

  # setting GDP threshold
  wec_dat_oecd_2$gdp_threshold = mean (wec_dat_oecd_2$gdp)

  ## for all sectors except for energy_oil

  wec_dat_oecd_2_nooil = wec_dat_oecd_2 %>% filter (!(subsector == "energy_oil"))

  # first minimum emissions
  wec_dat_oecd_3 = wec_dat_oecd_2_nooil %>% filter (gdp < gdp_threshold)
  highWhighE_1 = setDT(wec_dat_oecd_3)[,.SD[which.max(em_pc)],by = subsector]

  # second minimum emissions
  wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWhighE_1$ID))
  highWhighE_2 = setDT(wec_dat_oecd_4)[,.SD[which.max(em_pc)],by = subsector]

  # third minimum emissions
  wec_dat_oecd_5 = wec_dat_oecd_4 %>% filter (!(ID %in% highWhighE_2$ID))
  highWhighE_3 = setDT(wec_dat_oecd_5)[,.SD[which.max(em_pc)],by = subsector]

  # binding into 1 data frame containing rankings per sub-sector
  highWhighE_nooil = rbind (highWhighE_1, highWhighE_2, highWhighE_3) %>%
    dplyr::select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 24)) %>%
    relocate (rank, .before = iso3c)

  ## for energy_oil
  wec_dat_oecd_2_oil = wec_dat_oecd_2 %>% filter (subsector == "energy_oil")

  # first minimum emissions
  wec_dat_oecd_3_oil = wec_dat_oecd_2_oil
  highWhighE_1_oil = setDT(wec_dat_oecd_3_oil)[,.SD[which.max(em_pc)],by = subsector]

  # second minimum emissions
  wec_dat_oecd_4_oil = wec_dat_oecd_3_oil %>% filter (!(ID %in% highWhighE_1_oil$ID))
  highWhighE_2_oil = setDT(wec_dat_oecd_4_oil)[,.SD[which.max(em_pc)],by = subsector]

  # third minimum emissions
  wec_dat_oecd_5_oil = wec_dat_oecd_4_oil %>% filter (!(ID %in% highWhighE_2_oil$ID))
  highWhighE_3_oil = setDT(wec_dat_oecd_5_oil)[,.SD[which.max(em_pc)],by = subsector]

  highWhighE_oil = rbind (highWhighE_1_oil, highWhighE_2_oil, highWhighE_3_oil) %>%
    dplyr::select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
    # adding country names
    mutate (country = countrycode (sourcevar = iso3c,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE,
                                   nomatch = NA)) %>%
    relocate (country, .before = sector) %>%
    arrange (subsector) %>%
    relocate (year, sector, subsector, iso3c, country, em_pc) %>%
    mutate (rank = rep(c(1,2,3), times = 1)) %>%
    relocate (rank, .before = iso3c)

  highWhighE = rbind (highWhighE_nooil, highWhighE_oil) %>% arrange (sector)

  # saving to environment
  .GlobalEnv$highWhighE = highWhighE

}

values = c(2022:2050)
datalist = vector("list", length = length(unique(values)))

for (i in values) {

  datalist [[i]] <- highWhighE_fun_3 (i)

}

highWhighE_future_3 = do.call(rbind, datalist)

# getting totals from highWhighE_future

values = unique (highWhighE_future_3$year)
n = length (values)
datalist = vector("list", length = n)

for (i in 1:n) {

  # subsetting highWhighE_future
  highWhighE_future_year = highWhighE_future_3 [highWhighE_future_3$year == values[i],]

  # creating df to store per capita totals
  em_pc_totals = data.frame (matrix (NA, nrow = 3, ncol = 1))

  # saving totals to em_pc_totals
  em_pc_totals[1,1] = sum (highWhighE_future_year$em_pc [highWhighE_future_year$rank == 1])
  em_pc_totals[2,1] = sum (highWhighE_future_year$em_pc [highWhighE_future_year$rank == 2])
  em_pc_totals[3,1] = sum (highWhighE_future_year$em_pc [highWhighE_future_year$rank == 3])

  datalist [[i]] <- em_pc_totals

}

em_pc_totals_worst_case_future_3 = do.call(cbind, datalist)
colnames (em_pc_totals_worst_case_future_3) = values


## binding into one df for worst case ----

worst_case = rbind (em_pc_totals_worst_case_future_1[1,],
                    em_pc_totals_worst_case_future_2[1,],
                    em_pc_totals_worst_case_future_3[1,]) %>%
  mutate (scenario = c(1,2,3)) %>%
  relocate (scenario, .before = 1) %>%
  pivot_longer (2:30, names_to = "year", values_to = "worst_case_em_pc") %>%
  pivot_wider (names_from = "scenario", values_from = "worst_case_em_pc", names_sep = "_") %>%
  rename_with (
    ~ case_when (
      . == "1" ~ "worst_case_em_pc_1",
      . == "2" ~ "worst_case_em_pc_2",
      . == "3" ~ "worst_case_em_pc_3",
      TRUE ~ .
    )
  ) %>%
  mutate (year = as.numeric (year))

## inner_join into one input df

input_cases = best_case %>%
  mutate (year = as.numeric (year)) %>%
  inner_join (highWavgE_future, by = "year") %>%
  inner_join (worst_case, by = "year") %>%
  rename_with (
    ~ case_when (
      . == "best_case_em_pc" ~ "average_em_pc",
      . == "best_case_em_pc" ~ "best",
      . == "average_em_pc" ~ "avg",
      . == "worst_case_em_pc_1" ~ "worst1",
      . == "worst_case_em_pc_2" ~ "worst2",
      . == "worst_case_em_pc_3" ~ "worst3",
      TRUE ~ .
    )
  )


## creating plot ----

input_cases_2 = input_cases %>%
  pivot_longer (2:6, names_to = "case", values_to = "em_pc") 

line_plot_cases = ggplot (input_cases_2, aes (x = year, y = em_pc, group = case, color = factor (case))) +
  geom_line () +
  labs (title = "High Wealth Low Emissions: \nchanges in emissions per capita over time",
        subtitle = "Varied by cases (best case, average case and worst case, 2022 - 2050",
        x = "year",
        y = "total yearly emissions per capita \n(all sectors), tonnes") +
  theme_ipsum(base_size=8,base_family="worksans") +
  theme_wdl() +
  scale_colour_discrete_worlddataverse() +
  theme (legend.position = "none") +
  geom_label(aes(label = case),
             data = input_cases_2 %>% filter(year == 2050),
             nudge_x = 0.35,
             size = 4)

line_plot_cases
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

input_cases_3 = input_cases_2 %>%
  filter (case %in% c("best", "average_em_pc", "worst2"))

input_cases_3$case <- ifelse (input_cases_3$case == "average_em_pc", "avg", 
                              ifelse (input_cases_3$case == "worst2", "worst", 
                                      input_cases_3$case))


line_plot_cases_2 = ggplot (input_cases_3, aes (x = year, y = em_pc, group = case, color = factor (case))) +
  geom_line () +
  labs (title = "High Wealth Low Emissions: \nchanges in emissions per capita over time",
        subtitle = "Varied by cases (best case, average case and worst case, 2022 - 2050",
        x = "year",
        y = "total yearly emissions per capita \n(all sectors), tonnes") +
  theme_ipsum(base_size=8,base_family="worksans") +
  theme_wdl() +
  scale_colour_discrete_worlddataverse() +
  theme (legend.position = "none") +
  geom_label(aes(label = case),
             data = input_cases_3 %>% filter(year == 2050),
             nudge_x = 0.35,
             size = 4)

line_plot_cases_2
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

line_plot_cases_3 = ggplot (input_cases_3 %>% filter (case == "best"), aes (x = year, y = em_pc)) +
  geom_line () +
  labs (title = "High Wealth Low Emissions: \nchanges in emissions per capita over time",
        subtitle = "Best case, 2022 - 2050",
        x = "year",
        y = "total yearly emissions per capita \n(all sectors), tonnes") +
  theme_ipsum(base_size=8,base_family="worksans") +
  theme_wdl() +
  scale_colour_discrete_worlddataverse() +
  theme (legend.position = "none")

line_plot_cases_3
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

# scale_y_continuous(
 # labels = scales::number_format(accuracy = 0.01),
 # trans='log') +


## visualizing top 10 countries across time -------------------------------------

# prelims
pacman::p_load (ggbump)

# data

bump_data_fun = function (year) {

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

  # sub-setting round 2
  wec_dat_oecd_2 = wec_dat_oecd %>%
    dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
    group_by (year, iso3c, sector, subsector, pop, gdp) %>%
    summarise (emissions = sum(base)) %>%
    drop_na() %>%
    unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
    mutate (em_pc = emissions / pop)

  bump_data = wec_dat_oecd_2 %>%
    ungroup () %>%
    group_by (year, iso3c, sector) %>%
    summarise (em_pc = sum (em_pc))

  .GlobalEnv$bump_data = bump_data

}

# test
bump_data_22 <- bump_data_fun (2022)

values = c(2022:2050)
n = length (values)
datalist = vector("list", length = n)

for (i in values) {

  datalist [[i]] <- bump_data_fun (i)

}

bump_data_exp = do.call(rbind, datalist)

# Agriculture bump chart

bump_data_exp_agri <- bump_data_exp [bump_data_exp$sector == "Agriculture",] %>%
  arrange (year, em_pc) %>%
  group_by (year) %>%
  mutate (rank = rank (em_pc, ties.method = "first")) %>%
  dplyr::select (iso3c, year, rank) %>%
  rename_with (
    ~ case_when (
      . == "iso3c" ~ "country",
      TRUE ~ .
    )
  ) %>%
  filter (rank <= 10) %>%
  mutate (countrycode = countrycode (sourcevar = country,
                                     origin = "iso3c",
                                     destination = "iso2c",
                                     warn = TRUE,
                                     nomatch = NA)) %>%
  mutate (countrycode = tolower(countrycode))
  
bump_data_exp_agri_plot = ggplot (bump_data_exp_agri, aes (x = year, y = rank, color = country, fill = country)) +
  geom_bump(aes (smooth = 10), size = 1.5, lineend = "round") + 
  geom_flag(data = bump_data_exp_agri %>% filter(year == 2022), 
            aes(x = 2022, country = countrycode),
            size = 8,
            color = "black") +
  geom_flag(data = bump_data_exp_agri %>% filter(year == 2050), 
            aes(x = 2050, country = countrycode),
            size = 8,
            color = "black") +
  scale_y_reverse (breaks = 1:100) +
  scale_x_continuous(breaks = bump_data_exp_agri$year %>% unique()) +
  theme_wdl() + 
  theme (legend.position = "none") +
  scale_colour_discrete_worlddataverse() + 
  labs (title = "Ranking of OECD countries by \nlowest emissions per capita (Agriculture)",
        subtitle = "2022 - 2050",
        x = "year",
        y = "rank")

bump_data_exp_agri_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))
  
# Building Plot

bump_data_exp_build <- bump_data_exp [bump_data_exp$sector == "Building",] %>%
  arrange (year, em_pc) %>%
  group_by (year) %>%
  mutate (rank = rank (em_pc, ties.method = "first")) %>%
  dplyr::select (iso3c, year, rank) %>%
  rename_with (
    ~ case_when (
      . == "iso3c" ~ "country",
      TRUE ~ .
    )
  ) %>%
  filter (rank <= 10) %>%
  mutate (countrycode = countrycode (sourcevar = country,
                                     origin = "iso3c",
                                     destination = "iso2c",
                                     warn = TRUE,
                                     nomatch = NA)) %>%
  mutate (countrycode = tolower(countrycode))

bump_data_exp_build_plot = ggplot (bump_data_exp_build, aes (x = year, y = rank, color = country, fill = country)) +
  geom_bump(aes (smooth = 10), size = 1.5, lineend = "round") + 
  geom_flag(data = bump_data_exp_build %>% filter(year == 2022), 
            aes(x = 2022, country = countrycode),
            size = 8,
            color = "black") +
  geom_flag(data = bump_data_exp_build %>% filter(year == 2050), 
            aes(x = 2050, country = countrycode),
            size = 8,
            color = "black") +
  scale_y_reverse (breaks = 1:100) +
  scale_x_continuous(breaks = bump_data_exp_build$year %>% unique()) +
  theme_wdl() + 
  theme (legend.position = "none") +
  scale_colour_discrete_worlddataverse() + 
  labs (title = "Ranking of OECD countries by \nlowest emissions per capita (Building)",
        subtitle = "2022 - 2050",
        x = "year",
        y = "rank")

bump_data_exp_build_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

# Energy Plot

bump_data_exp_energy <- bump_data_exp [bump_data_exp$sector == "Energy",] %>%
  arrange (year, em_pc) %>%
  group_by (year) %>%
  mutate (rank = rank (em_pc, ties.method = "first")) %>%
  dplyr::select (iso3c, year, rank) %>%
  rename_with (
    ~ case_when (
      . == "iso3c" ~ "country",
      TRUE ~ .
    )
  ) %>%
  filter (rank <= 10) %>%
  mutate (countrycode = countrycode (sourcevar = country,
                                     origin = "iso3c",
                                     destination = "iso2c",
                                     warn = TRUE,
                                     nomatch = NA)) %>%
  mutate (countrycode = tolower(countrycode))

bump_data_exp_energy_plot = ggplot (bump_data_exp_energy, aes (x = year, y = rank, color = country, fill = country)) +
  geom_bump(aes (smooth = 10), size = 1.5, lineend = "round") + 
  geom_flag(data = bump_data_exp_energy %>% filter(year == 2022), 
            aes(x = 2022, country = countrycode),
            size = 8,
            color = "black") +
  geom_flag(data = bump_data_exp_energy %>% filter(year == 2050), 
            aes(x = 2050, country = countrycode),
            size = 8,
            color = "black") +
  scale_y_reverse (breaks = 1:100) +
  scale_x_continuous(breaks = bump_data_exp_energy$year %>% unique()) +
  theme_wdl() + 
  theme (legend.position = "none") +
  scale_colour_discrete_worlddataverse() + 
  labs (title = "Ranking of OECD countries by \nlowest emissions per capita (Energy)",
        subtitle = "2022 - 2050",
        x = "year",
        y = "rank")

bump_data_exp_energy_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

# Industry plot

bump_data_exp_industry <- bump_data_exp [bump_data_exp$sector == "Industry",] %>%
  arrange (year, em_pc) %>%
  group_by (year) %>%
  mutate (rank = rank (em_pc, ties.method = "first")) %>%
  dplyr::select (iso3c, year, rank) %>%
  rename_with (
    ~ case_when (
      . == "iso3c" ~ "country",
      TRUE ~ .
    )
  ) %>%
  filter (rank <= 10) %>%
  mutate (countrycode = countrycode (sourcevar = country,
                                     origin = "iso3c",
                                     destination = "iso2c",
                                     warn = TRUE,
                                     nomatch = NA)) %>%
  mutate (countrycode = tolower(countrycode))

bump_data_exp_industry_plot = ggplot (bump_data_exp_industry, aes (x = year, y = rank, color = country, fill = country)) +
  geom_bump(aes (smooth = 10), size = 1.5, lineend = "round") + 
  geom_flag(data = bump_data_exp_industry %>% filter(year == 2022), 
            aes(x = 2022, country = countrycode),
            size = 8,
            color = "black") +
  geom_flag(data = bump_data_exp_industry %>% filter(year == 2050), 
            aes(x = 2050, country = countrycode),
            size = 8,
            color = "black") +
  scale_y_reverse (breaks = 1:100) +
  scale_x_continuous(breaks = bump_data_exp_industry$year %>% unique()) +
  theme_wdl() + 
  theme (legend.position = "none") +
  scale_colour_discrete_worlddataverse() + 
  labs (title = "Ranking of OECD countries by \nlowest emissions per capita (Industry)",
        subtitle = "2022 - 2050",
        x = "year",
        y = "rank")

bump_data_exp_industry_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

# Transport plot

bump_data_exp_trans <- bump_data_exp [bump_data_exp$sector == "Transport",] %>%
  arrange (year, em_pc) %>%
  group_by (year) %>%
  mutate (rank = rank (em_pc, ties.method = "first")) %>%
  dplyr::select (iso3c, year, rank) %>%
  rename_with (
    ~ case_when (
      . == "iso3c" ~ "country",
      TRUE ~ .
    )
  ) %>%
  filter (rank <= 10) %>%
  mutate (countrycode = countrycode (sourcevar = country,
                                     origin = "iso3c",
                                     destination = "iso2c",
                                     warn = TRUE,
                                     nomatch = NA)) %>%
  mutate (countrycode = tolower(countrycode))

bump_data_exp_trans_plot = ggplot (bump_data_exp_trans, aes (x = year, y = rank, color = country, fill = country)) +
  geom_bump(aes (smooth = 10), size = 1.5, lineend = "round") + 
  geom_flag(data = bump_data_exp_trans %>% filter(year == 2022), 
            aes(x = 2022, country = countrycode),
            size = 8,
            color = "black") +
  geom_flag(data = bump_data_exp_trans %>% filter(year == 2050), 
            aes(x = 2050, country = countrycode),
            size = 8,
            color = "black") +
  scale_y_reverse (breaks = 1:100) +
  scale_x_continuous(breaks = bump_data_exp_trans$year %>% unique()) +
  theme_wdl() + 
  theme (legend.position = "none") +
  scale_colour_discrete_worlddataverse() + 
  labs (title = "Ranking of OECD countries by \nlowest emissions per capita (Transport)",
        subtitle = "2022 - 2050",
        x = "year",
        y = "rank")

bump_data_exp_trans_plot
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))




bump_data_exp_transport <- bump_data_exp [bump_data_exp$sector == "Transport",]








## nested pie chart -----

devtools::install_github("yogevherz/plotme")
library (plotme)

pie_dat = highWlowE_2022 %>%
  filter (rank == 1) %>%
  filter (!(subsector == "LULUCF")) %>%
  mutate (pct = em_pc / sum (em_pc)) %>%
  mutate (pct = pct * 100) %>%
  mutate (cum_pct = cumsum (pct)) %>%
  rename (cum_pct = ymax) %>%
  mutate (ymin = ymax - pct) %>%
  dplyr::select (-c("rank", "year", "iso3c", "country", "em_pc"))

sunburst = pie_dat %>% 
  rename (pct = n) %>%
  count_to_sunburst()



















library (Hmisc)

pie_dat = highWlowE_2022 %>%
  filter (rank == 1) %>%
  filter (!(subsector == "LULUCF")) %>%
  mutate (pct = em_pc / sum (em_pc)) %>%
  mutate (pct = pct * 100) %>%
  mutate (cum_pct = cumsum (pct)) %>%
  rename (cum_pct = ymax) %>%
  mutate (ymin = ymax - pct) %>%
  dplyr::select (-c("rank", "year", "iso3c", "country", "em_pc"))

sunburst = pie_dat %>% 
  rename (pct = n) %>%
  count_to_sunburst()

sunburst
  
pacman::p_load (webr)
library (webr)

colfunc <- colorRampPalette(c("#003F5C", "#DED9F1"))
pal_worlddataverse_4 <- colfunc (29)

pie = ggplot (pie_dat) + 
  geom_rect (aes (fill = subsector, ymin = ymin, ymax = ymax, xmax = 4, xmin = 3)) + 
  geom_rect (aes (fill = sector, ymin = ymin, ymax = ymax, xmax = 3, xmin = 0)) +
  xlim(c(0, 4)) +
  coord_polar(theta="y")
pie

pacman::p_load (ggiraphExtra, moonBook)
library (ggiraphExtra, moonBook)

pie_dat2 = pie_dat %>% dplyr::select (1:3)
piedonut = ggPieDonut(data = pie_dat2, 
                      mapping = aes (pies = sector, 
                                     donuts = subsector
                                     )
                      )
piedonut

piedonut_2 = ggPieDonut (pie_dat2, aes (pies = sector, 
                                        donuts = subsector))

pie_dat_3 = highWlowE_2022 %>%
  filter (rank == 1) %>%
  filter (!(subsector == "LULUCF")) %>%
  dplyr::select (-c("rank", "year", "iso3c", "country"))

write.csv (pie_dat_3, "pie_dat.csv")

piedonut = ggPieDonut(data = pie_dat_3, 
                      mapping = aes (pies = sector, 
                                     donuts = subsector, 
                                     count = em_pc
                      )
)
piedonut

sunburst = pie_dat %>% 
  rename (pct = n) %>%
  count_to_sunburst()

sunburst

sunburst_2 = pie_dat_3 %>% 
  rename (em_pc = n) %>%
  count_to_sunburst(fill_by_n = TRUE, color = subsector, colors = pal_worlddataverse_4)

sunburst_2


subcolours = function(.dta, main, mainCol){
  
  tmp_dta = cbind(.dta,1,'col')
  
  tmp1 = unique(.dta[[main]])
  
  for (i in 1:length(tmp1)){
    tmp_dta$"col"[.dta[[main]] == tmp1[i]] = mainCol[i]
  }
  
  u <- unlist(by(tmp_dta$"1",tmp_dta[[main]],cumsum))
  n <- dim(.dta)[1]
  
  subcol=rep(rgb(0,0,0),n);
  
  for(i in 1:n){
    t1 = col2rgb(tmp_dta$col[i])/256
    subcol[i]=rgb(t1[1],t1[2],t1[3],1/(1+u[i]))
  }
  
  return(subcol);
}

mainCol <- pal_worlddataverse3
subCol <- setNames(subcolors(donnees, "marquage", mainCol), donnees$anticorps)




