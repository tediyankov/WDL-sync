
## preliminaries ---------------------------------------------------------------

# packages
pacman::p_load(tidyverse,
               countrycode,
               data.table,
               worlddataverse)

# paths
base_path <- worlddataverse::get_wdl_path()
if (is.na (base_path)) {base_path = file.path("/Volumes/",
                                              "GoogleDrive-115239951252043738907",
                                              "Shared drives",
                                              "DATA_WDL")}

input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data")
output_path <- file.path (base_path, "IOM", "2nd_paper", "output")

##  loading + preprocessing data

# flows pred by edu_age breakdown + filtered for countries of interest
flows <- read.csv (file.path (output_path, "output_average_predictions_OECD.RData")) %>%
  filter (dest %in% c("AUS","BEL","CAN","CHE","CZE","DEU","FRA","HUN","IRL","ISR","NLD","POL","USA"))

# flows pred by edu_age breakdown but summed by destination
flows_sums <- read.csv (file.path (output_path, "flows_edu_age_sums.csv")) %>%
  filter (dest %in% c("AUS","BEL","CAN","CHE","CZE","DEU","FRA","HUN","IRL","ISR","NLD","POL","USA")) %>%
  dplyr::select (2:3, 11) %>%
  rename_with (
    ~ case_when (
      . == "dest" ~ "ccode",
      TRUE ~ .
    )
  ) %>%
  relocate (ccode, year, flow_total)

# fertility 2000 - 2021
fertility_present <- read.csv (file.path (input_path, "fertility_present.csv")) %>%
  dplyr::select (6, 11:12) %>%
  rename_with (
    ~ case_when (
      . == "ISO3.Alpha.code" ~ "ISO3",
      . == "Year" ~ "year",
      . == "Total.Fertility.Rate..live.births.per.woman." ~ "fertility",
      TRUE ~ .
    )
  ) %>%
  mutate (ccode = countrycode (sourcevar = ISO3,
                               origin = "iso3c",
                               destination = "iso3c",
                               warn = TRUE,
                               nomatch = NA)) %>%
  filter (!(is.na (ccode)) & year %in% 2000:2019) %>%
  dplyr::select (ccode, year, fertility) %>%
  mutate (fertility = as.numeric (fertility),
          year = 5*floor(year/5)) %>%
  group_by (ccode, year) %>%
  summarise (fertility_avg = mean(fertility, na.rm = T), .groups = "keep")
  
# fertility 2022 - 2030
fertility_future <- read.csv (file.path (input_path, "fertility_future.csv")) %>%
  dplyr::select (6, 11:12) %>%
  rename_with (
    ~ case_when (
      . == "ISO3.Alpha.code" ~ "ISO3",
      . == "Year" ~ "year",
      . == "Total.Fertility.Rate..live.births.per.woman." ~ "fertility",
      TRUE ~ .
    )
  ) %>%
  mutate (ccode = countrycode (sourcevar = ISO3,
                               origin = "iso3c",
                               destination = "iso3c",
                               warn = TRUE,
                               nomatch = NA)) %>%
  filter (!(is.na (ccode)) & year <= 2030) %>%
  dplyr::select (ccode, year, fertility) %>%
  mutate (fertility = as.numeric (fertility),
          year = 5*floor(year/5)) %>%
  group_by (ccode, year) %>%
  summarise (fertility_avg = mean(fertility, na.rm = T), .groups = "keep")

# fertility 2000 - 2030 + subset for countries of interest
fertility = rbind (fertility_present, fertility_future) %>%
  arrange (ccode, year) %>%
  filter (ccode %in% c("AUS","BEL","CAN","CHE","CZE","DEU","FRA","HUN","IRL","ISR","NLD","POL","USA"))

# population
mpro <- readRDS(file.path(base_path, "./Mpro_PC/2021_July/02_output_data/X9_3_mc_remerge_mixed_2021-09-06.RDS"))
pop = mpro$macro.data %>% 
  dplyr::select (1:3) %>% 
  filter (ccode %in% c("AUS","BEL","CAN","CHE","CZE","DEU","FRA","HUN","IRL","ISR","NLD","POL","USA"))

# combining all into one data frame
data = list (pop, fertility, flows_sums) %>% 
  reduce (inner_join, by = c("ccode", "year")) %>% 
  arrange (ccode) %>%
  transform (pop_nomig = pop - flow_total) %>%
  pivot_longer (c(3,6), names_to = "migYN", values_to = "pop")

## data visualization

# 1st iteration: combined bar-line plot

## IDEA: graph showing two lines: one with the population with migration, one without, and then one line of fertility rate (highlohght integral between two lines to show th ewidening gap)

AUS_test <- ggplot (data = data [ccode = "AUS",],
                    aes (x = year, 
                         y = pop,
                         group = migYN,
                         fill = factor (migYN))) + 
  geom_bar (position = "stack", stat = "identity") + 
  geom_line (aes (x = year, y = fertility_avg))
AUS_test

AUS_test2 <- ggplot (data = data [ccode = "AUS",],
                     aes (x = year, 
                          y = fertility_avg)) + 
  geom_line()
AUS_test2


## line chart for DEU: line of pop growth vs pop growth w/ no migration
## don't use WDP population 

