
## DHS Ethipia =================================================================

## Chapter 1: preliminaries ----------------------------------------------------

# clean environment
rm (list = ls())

# packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools)

# system font 
worlddataverse::font_wdl()

# loading paths
base_path <- worlddataverse::get_wdl_path()
input_path <- file.path (base_path, "DHS_Data", "Ethiopia", "2019 - DHS VIII")
output_path <- file.path (base_path, "DHS_Data", "Ethiopia")

# loading data 
library (haven)
eth_hh = read_dta (file.path (input_path, "ETHR81DT", "ETHR81FL.DTA"))
eth_ind = read_dta (file.path (input_path, "ETIR81DT", "ETIR81FL.DTA"))

## Chapter 2: data ----------------------------------------------------

# input data first clean
eth_hh_clean = eth_hh %>%
  
  # isolating household-ID, household number, number of residents per hh and wealth index and urban/rural status
  dplyr::select (hhid, hv009, hv271, hv025) %>%
  
  # renaming variables
  rename_with (
    ~ case_when (
      . == "hhid" ~ "ID",
      . == "hv009" ~ "hh_members",
      . == "hv271" ~ "wealth_index",
      . == "hv025" ~ "urban",
      TRUE ~ .
    )
  ) %>%
  
  # converting wealth_index and urban to numeric variables
  mutate (wealth_index = as.numeric (wealth_index),
          urban = as.numeric (urban))

# creating percentile values
eth_hh_clean_percentile = as.numeric (quantile (eth_hh_clean$wealth_index, probs = seq (0, 1, 0.01)))

# creating vectors for urban count of HHs in each percentile
eth_hh_perc_list_urban <- c()
values_urban <- length (eth_hh_clean_percentile)

for (i in 1:values_urban) {
  eth_hh_perc_list_urban [i] <- sum (eth_hh_clean$wealth_index [eth_hh_clean$urban == 1] <= eth_hh_clean_percentile [i])
  }

# creating vectors for rural count of HHs in each percentile
eth_hh_perc_list_rural <- c()
values_rural <- length (eth_hh_clean_percentile)

for (i in 1:values_rural) {
  eth_hh_perc_list_rural [i] <- sum (eth_hh_clean$wealth_index [eth_hh_clean$urban == 2] <= eth_hh_clean_percentile [i])
}

# uniting into one df
eth_hh_dat <- data.frame (percentile = seq (0, 100, 1),
                          urban = eth_hh_perc_list_urban, 
                          rural = eth_hh_perc_list_rural) %>%
  pivot_longer (2:3, names_to = "residence", values_to = "hh_num")

## Chapter 3: plot -------------------------------------------------------------

eth_hh_bar1 = ggplot (data = eth_hh_dat, aes (x = percentile, y = hh_num, fill = residence, label = hh_num)) + 
  geom_bar (position = "stack", stat = "identity") + 
  #geom_text (size = 3, position = position_stack (vjust = 0.5), fontface = "bold") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl() + 
  labs (title = "Ethiopia: Total number of households by Wealth Index Percentile (2019)", 
        subtitle = "Visualising the urban / rural divide", 
        x = "Wealth Index Percentile", 
        y = "Number of households") + 
  theme (axis.text.x = element_text (angle = 0, margin = margin (t = 0, r = 0, b = 20, l = 0)),
         axis.text.y = element_text (margin = margin (t = 0, r = 20, b = 0, l = 20))) + 
  guides (fill = guide_legend (title = "Residence Type"))
eth_hh_bar1

















