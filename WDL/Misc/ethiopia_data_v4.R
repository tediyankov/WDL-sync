
## DHS Ethipia =================================================================

## Chapter 1: preliminaries ----------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
dhs_path <- file.path (base_path, "DHS_Data")

## loading data 
library (haven)

# Ethiopia
eth_hh = read_dta (file.path (dhs_path, "Ethiopia", "2019 - DHS VIII", "ETHR81DT", "ETHR81FL.DTA"))

# Rwanda
rwa_hh = read_dta (file.path (dhs_path, "Rwanda", "2019-20 - DHS VIII", "RW_2019-20_DHS_12152022_1110_185225", "RWHR81DT", "RWHR81FL.DTA"))

# Burundi
bur_hh = read_dta (file.path (dhs_path, "Burundi", "2016-17 - DHS VII", "BU_2016-17_DHS_12152022_1110_185225", "BUHR71DT", "BUHR71FL.DTA"))

# DRC
drc_hh = read_dta (file.path (dhs_path, "DRC", "2013-14 - DHS VI", "CD_2013-14_DHS_12152022_1339_185225", "CDHR61DT", "CDHR61FL.DTA"))

# Kenya
ken_hh = read_dta (file.path (dhs_path, "Kenya", "2014 - DHS VII", "KE_2014_DHS_12152022_1342_185225", "KEHR72DT", "KEHR72FL.DTA"))

# Tanzania
tzn_hh = read_dta (file.path (dhs_path, "Tanzania", "2015-16 - DHS VII", "TZ_2015-16_DHS_12152022_1344_185225", "TZHR7BDT", "TZHR7BFL.DTA"))

# Uganda
uga_hh = read_dta (file.path (dhs_path, "Uganda", "2016 - DHS VII", "UG_2016_DHS_12152022_1346_185225", "UGHR7BDT", "UGHR7BFL.DTA"))

## Chapter 2: household data ---------------------------------------------------

# input data first clean
eth_hh_clean = eth_hh %>%
  
  # isolating household-ID, household number, number of residents per hh and wealth index and urban/rural status
  dplyr::select (hhid, hv002, hv009, hv271, hv025, hv005) %>%
  
  # renaming variables
  rename_with (
    ~ case_when (
      . == "hhid" ~ "ID",
      . == "hv009" ~ "hh_members",
      . == "hv271" ~ "wealth_index",
      . == "hv025" ~ "urban",
      . == "hv005" ~ "weight",
      . == "hv002" ~ "hh_num",
      TRUE ~ .
    )
  ) %>%
  
  # converting wealth_index and urban to numeric variables and fixing weights var 
  mutate (wealth_index = as.numeric (wealth_index),
          urban = as.numeric (urban),
          weight = weight / 1000000)

# compute percentiles
eth_hh_clean_percentile = Hmisc::wtd.quantile (eth_hh_clean$wealth_index, 
                                               weights = eth_hh_clean$weight * eth_hh_clean$hh_members, 
                                               probs = seq (0, 1, 0.01))

# create df with percentiles and corresponding values
eth_hh_clean_percentile = data.frame (cbind (names (eth_hh_clean_percentile), eth_hh_clean_percentile))
names (eth_hh_clean_percentile) <- c("percentile", "value")
eth_hh_clean_percentile$value <- as.numeric (eth_hh_clean_percentile$value)

# match data with percentiles
eth_hh_dat = eth_hh_clean %>% mutate (wgt_hh_members = weight * hh_members)
eth_hh_dat$percentile <- eth_hh_clean_percentile$percentile [findInterval (eth_hh_dat$wealth_index, eth_hh_clean_percentile$value, rightmost.closed = TRUE)+1]

# compute weight sums per urban/rural and percentile
eth_hh_dat = eth_hh_dat %>% drop_na () %>%
  
  # recoding urban with word categories
  mutate (urban = ifelse (urban == 1, "Urban", 
                          ifelse (urban == 2, "Rural", NA))) %>%
  
  # computing percentile weight totals
  group_by (percentile, urban) %>%
  summarise (wgt_pctl = sum (wgt_hh_members, na.rm = T)) %>%
  
  # removing % sign
  mutate (percentile = gsub ("%", "", percentile),
          percentile = as.numeric (percentile))

## Chapter 3: household bar chart ----------------------------------------------

# bar chart 
eth_hh_barplot_1 <- ggplot (data = eth_hh_dat, aes (x = percentile, y = wgt_pctl, fill = urban)) + 
  geom_bar (position = "stack", stat = "identity") + 
  #geom_text (size = 3, position = position_stack (vjust = 0.5), fontface = "bold") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl() + 
  labs (title = "Ethiopia: Total population by Wealth Index Percentile (2019)", 
        subtitle = "Visualising the urban / rural divide", 
        x = "Wealth Index Percentile (weighted)", 
        y = "Population (weighted)") + 
  theme (axis.text.x = element_text (angle = 0, margin = margin (t = 0, r = 0, b = 20, l = 0)),
         axis.text.y = element_text (margin = margin (t = 0, r = 20, b = 0, l = 20))) + 
  guides (fill = guide_legend (title = "Residence Type")) + 
  geom_vline (aes (xintercept = 95.92391), linewidth = 0.3, colour = "black") + 
  geom_vline (aes (xintercept = 99.81884), linewidth = 0.3, colour = "black") +
  geom_vline (aes (xintercept = 99.8231), linewidth = 0.3, colour = "black") +
  geom_vline (aes (xintercept = 99.82736), linewidth = 0.3, colour = "black")
  
eth_hh_barplot_1

## Chapter 4: matching to WDP spending groups ----------------------------------

# creating df with rural urban shares
eth_hh_dat_shares = eth_hh_dat %>%
  pivot_wider (names_from = urban, values_from = wgt_pctl) %>%
  mutate (urban_share = Urban / (Urban + Rural),
          rural_share = Rural / (Urban + Rural)) %>%
  dplyr::select (percentile, urban_share, rural_share) %>%
  #pivot_longer (2:3, names_to = "category", values_to = "share")

# graphing shares
shares_plot = ggplot (data = eth_hh_dat_shares, aes (x = percentile, y = share, groups = category, col = category)) + 
  geom_point () + 
  geom_smooth (method = "loess")
shares_plot

# local regression models
model_urban <- loess (urban_share ~ percentile, data = eth_hh_dat_shares, span = 0.10)
model_rural <- loess (rural_share ~ percentile, data = eth_hh_dat_shares, span = 0.10)

# creating df with predicted values for specified ranges
out = data.frame (percentile = c(eth_hh_dat_shares$percentile, 95.92391, 99.81884, 99.8231, 99.82736)) %>%
  arrange (percentile)
out$urban <- predict (model_urban, newdata = out)
out$rural <- predict (model_rural, newdata = out)

# final df with urban / rural shares per spending group
final = data.frame (group = c("$<12", "$12-40", "$40-80", "$80-120", "$>120"))
final$urban_share <- NA
final$rural_share <- NA

# computing urban shares per spending group
final$urban_share [final$group == "$<12"] = sum (out$urban [1:96]) / 96
final$urban_share [final$group == "$12-40"] = sum (out$urban [97:101]) / 5
final$urban_share [final$group == "$40-80"] = sum (out$urban [101:102]) / 2
final$urban_share [final$group == "$80-120"] = sum (out$urban [102:103]) / 2
final$urban_share [final$group == "$>120"] = sum (out$urban [103:104]) / 2

# computing rural shares per spending group
final$rural_share [final$group == "$<12"] = sum (out$rural [1:96]) / 96
final$rural_share [final$group == "$12-40"] = sum (out$rural [97:101]) / 5
final$rural_share [final$group == "$40-80"] = sum (out$rural [101:102]) / 2
final$rural_share [final$group == "$80-120"] = sum (out$rural [102:103]) / 2
final$rural_share [final$group == "$>120"] = sum (out$rural [103:104]) / 2

final$sum = final$urban_share + final$rural_share








