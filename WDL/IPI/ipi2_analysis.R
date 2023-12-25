
## IPI 2.0 DATA ANALYSIS =======================================================


# aim: breakdown interent poverty change to find which region drove the biggest changes

## Prologue --------------------------------------------------------------------

## clean environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, worlddataverse)

## set font
worlddataverse::font_wdl()

## set paths 
base_path <- base_path <- worlddataverse::get_wdl_path()
ipi_1_path <- file.path (base_path, "IPI", "phase_1", "Output")
ipi_2_path <- file.path (base_path, "IPI", "phase_2", "Data", "output")

## loading data
ipi_2_prices <- read.csv (file.path (ipi_2_path, "ipi_2_internet_prices.csv"))
ip_1_prices <- read.csv (file.path (ipi_1_path, "internet_prices_2021-12.csv"))
ipi_1_2_numpoor <- read.csv (file.path (ipi_2_path, "ipi_2_ranking_comparison_2022_2023.csv"))
ipi_2_numpoor <- read.csv (file.path (ipi_2_path, "internet_poverty_until_2027.csv"))

## Chapter 1: internet poor (IPI1.0 2022 -> IPI2.0 2023) -----------------------

## clean data 
clean_ipi_1_2_numpoor <- ipi_1_2_numpoor %>%
  
  # isolating relevant columns
  dplyr::select (ipi_1_country_name, ipi1_internet_poor, ipi_2_internet_poor) %>%
  
  # rename vars
  rename_with (
    ~ case_when (
      . == "ipi_1_country_name" ~ "country",
      . == "ipi1_internet_poor" ~ "ipi_1_numpoor_2022",
      . == "ipi_2_internet_poor" ~ "ipi_2_numpoor_2023",
      TRUE ~ .
    )
  ) %>%
  
  # computing delta
  mutate (delta_abs = ipi_2_numpoor_2023 - ipi_1_numpoor_2022,
          delta_pct = ((ipi_2_numpoor_2023 - ipi_1_numpoor_2022) / ipi_1_numpoor_2022) * 100) %>%
  
  # country identifiers
  mutate (ccode = countrycode (country, "country.name", "iso3c"),
          continent = countrycode (country, "country.name", "continent")) %>%
  
  # final arrange
  dplyr::select (ccode, continent, ipi_1_numpoor_2022, ipi_2_numpoor_2023, delta_abs, delta_pct)

## building charts

# absolute delta
plot1 <- ggplot (data = clean_ipi_1_2_numpoor, 
                 aes (x = ccode, y = delta_abs)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  facet_wrap (~continent, scales = "free") + 
  labs (title = "change in number of poor, IPI 1.0 2022 to IPI 2.0 2023",
        x = "country", 
        y = "absolute change in internet poor")
plot1

# percentage delta
plot2 <- ggplot (data = clean_ipi_1_2_numpoor, 
                 aes (x = ccode, y = delta_pct)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  facet_wrap (~continent, scales = "free") + 
  labs (title = "change in number of poor, IPI 1.0 2022 to IPI 2.0 2023",
        x = "country", 
        y = "percentage change in internet poor")
plot2


## Chapter 2: internet poor (IPI2.0 2022 -> IPI2.0 2023) -----------------------

## Chapter 3: internet prices (IPI1.0 2022 -> IPI2.0 2023) ---------------------

## getting internet prices from IPI1.0 2022
clean_ipi_1_2_prices <- ip_1_prices %>%
  
  # isolating relevant vars 
  dplyr::select (ccode, year, predicted_price) %>%
  
  # merging with 2.0
  left_join (ipi_2_prices %>% dplyr::select (-quantity_adjusted_price), by = c("ccode", "year")) %>%
  
  # rename vars
  rename_with (
    ~ case_when (
      . == "predicted_price" ~ "ipi_1_price_2022",
      . == "quantity_adjusted_price.1" ~ "ipi_2_price_2023",
      TRUE ~ .
    )
  ) %>%
  
  # computing delta
  mutate (delta_abs = ipi_2_price_2023 - ipi_1_price_2022,
          delta_pct = ((ipi_2_price_2023 - ipi_1_price_2022) / ipi_1_price_2022) * 100) %>%
  
  # country identifiers
  mutate (continent = countrycode (ccode, "iso3c", "continent"))
  
## building charts

# absolute change in prices
plot3 = ggplot (data = clean_ipi_1_2_prices,
                aes (x = ccode, y = delta_abs)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  facet_wrap (~continent, scales = "free") + 
  labs (title = "change in quantity adjusted price, IPI 1.0 2022 to IPI 2.0 2023",
        subtitle = "absolute delta",
        x = "country", 
        y = "absolute change in internet price")

plot3

# percentage change in prices
plot4 = ggplot (data = clean_ipi_1_2_prices,
                aes (x = ccode, y = delta_pct)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  facet_wrap (~continent, scales = "free") + 
  labs (title = "change in quantity adjusted price, IPI 1.0 2022 to IPI 2.0 2023",
        subtitle = "percentage delta",
        x = "country", 
        y = "percentage change in internet price")

plot4

## Chapter 4: internet prices (IPI2.0 2022 -> IPI2.0 2023) ---------------------

## Chapter 5: others -----------------------------------------------------------

# continental zoom-in
clean_ipi_1_2_numpoor_2 = clean_ipi_1_2_numpoor %>%
  
  # keep relevant vars
  dplyr::select (ccode, continent, ipi_2_numpoor_2023) %>%
  
  # aggregating 
  group_by (continent) %>%
  mutate (total = sum (ipi_2_numpoor_2023, na.rm = T)) %>%
  ungroup () %>%
  
  # calcualting shares for the pie
  mutate (share = ipi_2_numpoor_2023 / total) %>%
  
  ggplot (aes (x = "", y = share, fill = ccode)) + 
  geom_col (color = "black") + 
  coord_polar (theta = "y") + 
  facet_wrap (~continent) +
  worlddataverse::theme_wdl() + 
  #theme_void () + 
  theme (legend.position = "none")
  

clean_ipi_1_2_numpoor_2








