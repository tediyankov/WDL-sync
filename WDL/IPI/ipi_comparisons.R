
## IPI 2.0: comparing to IPI 1.0 ===============================================

## Chapter 1: preliminaries ----------------------------------------------------

## clean environment
rm (list = ls())

## packages

# general packages
pacman::p_load (worlddataverse, tidyverse, countrycode, data.table, ggthemes, png, grid,cowplot,hrbrthemes,magick, ggrepel,ggbump,gt,janitor, ggimage)

# VDem package 
devtools::install_github("vdeminstitute/vdemdata")
library (vdemdata)

## system font 
worlddataverse::font_wdl()

## paths
base_path <- worlddataverse::get_wdl_path()
input_path <- file.path (base_path, "IPI", "phase_2", "Data", "input")
output_path <- file.path (base_path, "IPI", "phase_2", "Data", "output")

## raw data
setwd (input_path)
files = list.files (pattern = "*.csv")
for (i in 1:length(files)) assign(files[i], read.csv (files[i]))
rm (files)

## Chapter 2: clean data -------------------------------------------------------

# gsma
gsma = GSMA_data.csv %>%
  dplyr::select (-c(2,3,5)) %>%
  rename_with (
    ~ case_when (
      . == "ISO.Code" ~ "ccode",
      . == "Year" ~ "year",
      . == "Mobile.download.speeds" ~ "download_speed",
      . == "Mobile.upload.speeds" ~ "upload_speed",
      . == "Mobile.latencies" ~ "latency",
      . == "International.Internet.bandwidth.per.user" ~ "bandwidth",
      . == "X2G.Coverage" ~ "2G",
      . == "X3G.Coverage" ~ "3G",
      . == "X4G.Coverage" ~ "4G",
      . == "Inequality" ~ "inequality",
      TRUE ~ .
    )
  )

# itu
itu = ITU_data.csv %>%
  dplyr::select (-c(2:15)) %>%
  pivot_longer (2:10, names_to = "year", values_to = "price_2.5GB") %>%
  mutate_at ("year", str_replace, "X", "") %>%
  transform (year = as.numeric (year)) %>%
  rename_with (
    ~ case_when (
      . == "IsoCode" ~ "ccode",
      . == "X2.5GB_price" ~ "2.5GB_price",
      TRUE ~ .
    )
  )

# wb_electricity
wb_electricity =
  WB_electricity_access.csv [, colnames (WB_electricity_access.csv)
                             %like% paste (c(1990:2021, "Country.Code"),
                                           collapse="|")] %>%
  pivot_longer (2:33, names_to = "year", values_to = "electricity_share") %>%
  mutate_at ("year", str_replace, "X", "") %>%
  transform (year = as.numeric (year)) %>%
  rename_with (
    ~ case_when (
      . == "Country.Code" ~ "ccode",
      TRUE ~ .
    )
  )

# wb_polstab
wb_polstab_raw = WB_politicalstability.csv %>% dplyr::select (-1)
wb_polstab = wb_polstab_raw [, c(TRUE,
                                 subset(wb_polstab_raw[,-1],
                                        wb_polstab_raw$X.1 == "Code") == "Estimate")] %>%
  slice (-1) %>%
  pivot_longer (2:24, names_to = "year", values_to = "polstab") %>%
  mutate_at ("year", str_replace, "X", "") %>%
  transform (year = as.numeric (year)) %>%
  rename_with (
    ~ case_when (
      . == "X.1" ~ "ccode",
      TRUE ~ .
    )
  ) %>%
  mutate (polstab = ifelse (polstab == "#N/A",
                            NA,
                            polstab))

# wb_serv
wb_serv = WB_secureservers.csv [, colnames (WB_secureservers.csv)
                                %like% paste (c(2010:2020, "Country.Code"),
                                              collapse="|")] %>%
  pivot_longer (2:12, names_to = "year", values_to = "servers_per_1m") %>%
  mutate_at ("year", str_replace, "X", "") %>%
  transform (year = as.numeric (year)) %>%
  rename_with (
    ~ case_when (
      . == "Country.Code" ~ "ccode",
      TRUE ~ .
    )
  )

# weo_infl
weo_infl = WEO_inflation.csv %>%
  dplyr::select (-c(4, 25)) %>%
  pivot_longer (4:23, names_to = "year", values_to = "infl") %>%
  mutate_at ("year", str_replace, "X", "") %>%
  transform (year = as.numeric (year)) %>%
  mutate (Subject.Descriptor = ifelse (Subject.Descriptor == "Inflation, average consumer prices", "infl_avgconsp",
                                       ifelse (Subject.Descriptor == "Inflation, end of period consumer prices", "infl_endconsp",
                                               Subject.Descriptor)),
          Units = ifelse (Units == "Index", "index",
                          ifelse (Units == "Percent change", "percent",
                                  Units))) %>%
  unite ("descriptor", Subject.Descriptor:Units, sep = "_", remove = F, na.rm = F) %>%
  dplyr::select (-c("Subject.Descriptor", "Units")) %>%
  pivot_wider (names_from = "descriptor", values_from = "infl") %>%
  dplyr::select (-7) %>%
  rename_with (
    ~ case_when (
      . == "Country" ~ "ccode",
      TRUE ~ .
    )
  ) %>%
  mutate (ccode = countrycode(sourcevar = ccode,
                              origin = "country.name",
                              destination = "iso3c",
                              warn = TRUE,
                              nomatch = NA)) %>%
  filter (!(is.na(ccode))) %>%
  mutate (across (starts_with ('infl'), ~ifelse (.x == "n/a", NA, .x)))

# SSP
ssp = ssp2_urbanization_2020.csv

# WDP
gdp <- readRDS (file.path (base_path,"Mpro_2.0", "01_Data", "R_2022_10_11", "2022_11_02_new_pov_2017ppp", "02_intermediary_data", "03_008_macro_imputed.rds")) %>%
  dplyr::select (ccode, year, GDP.PC)

# V_Dem 

metrics <- c('v2x_libdem','v2x_polyarchy', "v2mecenefm", "v2mecenefi", "v2mecenefibin") # liberal democracy metric, electoral democracy metric, Government censorship effort, Internet censorship metric, internet binary
id.vars<-c('country_name', 'COWcode','histname' ,'codingstart_contemp', 'codingend_contemp','year')

vdem <- vdemdata::vdem %>%
  # isolating variables of interest
  dplyr::select (all_of (id.vars), all_of (metrics)) %>%
  # isolating years of interest
  filter (year %in% 2014:2021) %>%
  # reformatting countries
  mutate (ccode = countrycode (country_name, "country.name", "iso3c")) %>%
  # removing countries that did not get match to ISO3 codes
  filter (!(is.na (ccode))) %>%
  # keeping only ccode, year and the vars of interest
  dplyr::select (ccode, year, metrics) %>%
  # rename metrics
  rename_with (
    ~ case_when (
      . == "v2x_libdem" ~ "lib_democ",
      . == "v2x_polyarchy" ~ "elect_democ",
      . == "v2mecenefm" ~ "govt_censorship",
      . == "v2mecenefi" ~ "internet_censorship",
      . == "v2mecenefibin" ~ "internet_binary",
      TRUE ~ .
    )
  ) %>%
  # arranging alphabetically by ccode
  arrange (ccode)

## Chapter 3: merge data -------------------------------------------------------

## staggered join approach
ipi_input = itu %>%
  inner_join (gsma, by = c("ccode", "year")) %>%
  left_join (wb_electricity, by = c("ccode", "year")) %>%
  left_join (wb_polstab, by = c("ccode", "year")) %>%
  left_join (wb_serv, by = c("ccode", "year")) %>%
  left_join (weo_infl, by = c("ccode", "year")) %>%
  left_join (ssp, by = c("ccode", "year")) %>%
  left_join (gdp, by = c("ccode", "year")) %>%
  left_join (vdem, by = c("ccode", "year"))

nas_staggered <- as.data.frame (apply (ipi_input, 2, function(col) sum(is.na(col))/length(col)))

## reducing input data to relevant variables
ipi_input_tmp = ipi_input %>%
  
  # keeping country, year, price, download speed and upload speed
  dplyr::select (ccode, year, price_2.5GB, download_speed, upload_speed) %>%
  
  # adding continent variable
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  relocate (continent, .after = ccode) %>%
  
  # adding iso2c ccode variable for ggflags
  mutate (flags = countrycode (ccode, "iso3c", "iso2c")) %>%
  mutate (flags = tolower(flags)) %>%
  relocate (flags, .after = ccode) %>%
  
  # adding full word country variable
  mutate (country = countrycode (ccode, "iso3c", "country.name")) %>%
  relocate (country, .before = ccode) %>%
  
  # filtering to relevant years
  filter (year %in% c(2019:2021))

## Chapter 4: investigating patterns in ranks of price and quality data --------

## bump chart data: internet price
price_bump_dat = ipi_input_tmp %>%
  
  # keeping relevant variables
  dplyr::select (country, ccode, flags, continent, year, price_2.5GB) %>%
  
  # removing NAs
  drop_na () %>%
  
  # computing ranks
  dplyr::select (country, ccode, flags, continent, year, price_2.5GB) %>%
  group_by (year) %>%
  mutate (rank = rank (-price_2.5GB, ties.method = "min")) %>%
  filter (rank %in% 1:10) %>%
  arrange (year, rank) %>%
  
  # keeping relevant vars
  dplyr::select (year, country, ccode, flags, rank, price_2.5GB) %>%
  
  mutate (flag = tolower (countrycode (country, "country.name", "iso2c")))
  
## bump chart data: download speed
download_bump_dat = ipi_input_tmp %>%
  
  # keeping relevant variables
  dplyr::select (country, ccode, flags, continent, year, download_speed) %>%
  
  # removing NAs
  drop_na () %>%
  
  # computing ranks
  dplyr::select (country, ccode, flags, continent, year, download_speed) %>%
  group_by (year) %>%
  mutate (rank = rank (-download_speed, ties.method = "max")) %>%
  filter (rank %in% 1:10) %>%
  arrange (year, rank) %>%
  
  # keeping relevant vars
  dplyr::select (year, country, ccode, flags, rank, download_speed)

## bump chart data: upload speed
upload_bump_dat = ipi_input_tmp %>%
  
  # keeping relevant variables
  dplyr::select (country, ccode, flags, continent, year, upload_speed) %>%
  
  # removing NAs
  drop_na () %>%
  
  # computing ranks
  dplyr::select (country, ccode, flags, continent, year, upload_speed) %>%
  group_by (year) %>%
  mutate (rank = rank (-upload_speed, ties.method = "min")) %>%
  filter (rank %in% 1:10) %>%
  arrange (year, rank) %>%
  
  # keeping relevant vars
  dplyr::select (year, country, ccode, flags, rank, upload_speed)

## bump charts

library (ggimage)

price_bump = ggplot (price_bump_dat, aes (x = year, y = rank, color = ccode, fill = ccode)) +
  geom_bump (aes (smooth = 10), size = 1.5, lineend = "round") + 
  geom_flag (data = price_bump_dat, 
            aes(x = 2019, country = flag),
            size = 8,
            color = "black") +
  geom_flag (data = price_bump_dat, 
            aes(x = 2020, country = flag),
            size = 8,
            color = "black") +
  geom_flag (data = price_bump_dat, 
            aes(x = 2021, country = flag),
            size = 8,
            color = "black") +
  scale_y_reverse (breaks = 1:100) +
  scale_x_continuous (breaks = price_bump_dat$year %>% unique()) +
  worlddataverse::theme_wdl () + 
  worlddataverse::scale_color_wdl () +
  theme (legend.position = "none") +
  labs (title = "Top 10 countries by internet price (2.5GB)",
        subtitle = "Changes in rank through time: 2019-2021",
        x = "year",
        y = "rank")
price_bump




## Chapter 5: investigating patterns in price and quality data -----------------

## line chart

ipi_input_tmp = ipi_input_tmp %>% mutate (year = as.factor (year))

plot1 = ggplot (data = ipi_input_tmp, aes (x = year, y = price_2.5GB, col = country)) +
  geom_line () + 
  worlddataverse::theme_wdl() +
  theme (legend.position = "none") + 
  facet_wrap (~continent, scales = "free")
plot1

## bar chart approach
changes_bar_dat = ipi_input_tmp %>%
  dplyr::select (ccode, continent, year, price_2.5GB, download_speed, upload_speed) %>%
  pivot_wider (names_from = year, values_from = 4:6) %>%
  mutate (delta_price = ((price_2.5GB_2021 - price_2.5GB_2019) / price_2.5GB_2019) * 100,
          delta_download = ((download_speed_2021 - download_speed_2019) / download_speed_2019) * 100, 
          delta_upload = ((upload_speed_2021 - upload_speed_2019) / upload_speed_2019) * 100) %>%
  dplyr::select (ccode, continent, delta_price, delta_download, delta_upload)

## creating charts

price_bar = ggplot (changes_bar_dat, aes (x = ccode, y = delta_price)) + 
  geom_bar (stat = "identity") +
  worlddataverse::theme_wdl() + 
  theme (axis.text.x = element_text (size = 6)) + 
  labs (title = "Change in Price of Internet (2GB) per country",
        subtitle = "From 2019 to 2021",
        x = "country", 
        y = "percent change in price") + 
  facet_wrap (~continent, scales = "free")
price_bar

download_bar = ggplot (changes_bar_dat, aes (x = ccode, y = delta_download)) + 
  geom_bar (stat = "identity") +
  worlddataverse::theme_wdl() + 
  theme (axis.text.x = element_text (size = 6)) + 
  labs (title = "Change in download speed per country",
        subtitle = "From 2019 to 2021",
        x = "country", 
        y = "percent change in download speed") + 
  facet_wrap (~continent, scales = "free")
download_bar

upload_bar = ggplot (changes_bar_dat, aes (x = ccode, y = delta_upload)) + 
  geom_bar (stat = "identity") +
  worlddataverse::theme_wdl() + 
  theme (axis.text.x = element_text (size = 6)) + 
  labs (title = "Change in upload speed per country",
        subtitle = "From 2019 to 2021",
        x = "country", 
        y = "percent change in upload speed") + 
  facet_wrap (~continent, scales = "free")
upload_bar

## reformatting rankings table
price_ranks = price_bump_dat %>%
  dplyr::select (year, rank, country) %>%
  pivot_wider (names_from = year, values_from = country)

download_bump_dat = ipi_input_tmp %>%
  
  # keeping relevant variables
  dplyr::select (country, ccode, flags, continent, year, download_speed) %>%
  
  # removing NAs
  drop_na () %>%
  
  # computing ranks
  dplyr::select (country, ccode, flags, continent, year, download_speed) %>%
  group_by (year) %>%
  mutate (rank = rank (-download_speed, ties.method = "min")) %>%
  filter (rank %in% 1:10) %>%
  arrange (year, rank) %>%
  
  # keeping relevant vars
  dplyr::select (year, country, ccode, flags, rank, download_speed)


download_ranks = download_bump_dat %>%
  dplyr::select (year, rank, country) %>%
  pivot_wider (names_from = year, values_from = country)

upload_ranks = upload_bump_dat %>%
  dplyr::select (year, rank, country) %>%
  pivot_wider (names_from = year, values_from = country)
  
# inverse price rankings
price_ranks_inverse = ipi_input_tmp %>%
  
  # keeping relevant variables
  dplyr::select (country, ccode, flags, continent, year, price_2.5GB) %>%
  
  # removing NAs
  drop_na () %>%
  
  # computing ranks
  dplyr::select (country, ccode, flags, continent, year, price_2.5GB) %>%
  group_by (year) %>%
  mutate (rank = rank (price_2.5GB, ties.method = "min")) %>%
  filter (rank %in% 1:10) %>%
  arrange (year, rank) %>%
  
  # keeping relevant vars
  dplyr::select (year, country, ccode, flags, rank, price_2.5GB) %>%
  dplyr::select (year, rank, country) %>%
  pivot_wider (names_from = year, values_from = country)
  











  
  
  




