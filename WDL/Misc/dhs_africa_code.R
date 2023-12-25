
## DHS African Countries =======================================================

## Preface: 

# The naming conventions for the DHS files is quite odd - when you get a country file.
# Each country will have a number of folders, each starting with a country abbreviation (e.g. for Ethiopia it's ET...)
# The household survey data is in the folder which has "HR" following the country abbreviation (e.g. ETHR...)
# The folder names will also contain some numbers (e.g. for 2019 survey data it's 81DT, so for Ethiopia the household file will be ETHR81DT)
# The individual survey data is similar, but instead of HR it's IR. I use the household data however, as it has sample weights. 
# In the folder, the .DTA file contains the data and the .MAP file contains the list of variables and their names and value ranges, kind of like a codebook. 
# The following variables are the most relevant (the rest you can find their name in the .MAP file): 
# "hhid" = ID of respondent individual or household
# "hv271" = wealth index factor score (the raw wealth index score)
# "hv025" = binary classification of whether respondent household is urban or rural. 
# "hv005" = household sample weight
# "hv002" = number of residents in a household (I use this and the household sample weights to derive individual level data from the household data)



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
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2022_10_11','2022_10_12_second_2017ppp','03_outputs')

## loading data 
library (haven)

# Ethiopia 2019
eth_hh = read_dta (file.path (dhs_path, "Ethiopia", "2019 - DHS VIII", "ETHR81DT", "ETHR81FL.DTA"))

# Rwanda 2019-20
rwa_hh = read_dta (file.path (dhs_path, "Rwanda", "2019-20 - DHS VIII", "RW_2019-20_DHS_12152022_1110_185225", "RWHR81DT", "RWHR81FL.DTA"))

# Burundi 2016-17
bur_hh = read_dta (file.path (dhs_path, "Burundi", "2016-17 - DHS VII", "BU_2016-17_DHS_12152022_1110_185225", "BUHR71DT", "BUHR71FL.DTA"))

# DRC 2013-14
drc_hh = read_dta (file.path (dhs_path, "DRC", "2013-14 - DHS VI", "CD_2013-14_DHS_12152022_1339_185225", "CDHR61DT", "CDHR61FL.DTA"))

# Kenya 2014
ken_hh = read_dta (file.path (dhs_path, "Kenya", "2014 - DHS VII", "KE_2014_DHS_12152022_1342_185225", "KEHR72DT", "KEHR72FL.DTA"))

# Tanzania 2015-16
tzn_hh = read_dta (file.path (dhs_path, "Tanzania", "2015-16 - DHS VII", "TZ_2015-16_DHS_12152022_1344_185225", "TZHR7BDT", "TZHR7BFL.DTA"))

# Uganda 2016
uga_hh = read_dta (file.path (dhs_path, "Uganda", "2016 - DHS VII", "UG_2016_DHS_12152022_1346_185225", "UGHR7BDT", "UGHR7BFL.DTA"))



## Chapter 2: cleaning raw DHS files -------------------------------------------

## creating function to clean raw data files
dhs_datclean = function (data) {
  
  require (Hmisc)
  require (tidyverse)
  
  raw_dat = data
  
  ## initial clean
  dat_hh_clean = raw_dat %>%
    
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
  
  ## compute percentiles
  dat_hh_clean_percentile = Hmisc::wtd.quantile (dat_hh_clean$wealth_index, 
                                                 weights = dat_hh_clean$weight * dat_hh_clean$hh_members, 
                                                 probs = seq (0, 1, 0.01))
  
  ## create df with percentiles and corresponding values
  dat_hh_clean_percentile = data.frame (cbind (names (dat_hh_clean_percentile), dat_hh_clean_percentile))
  names (dat_hh_clean_percentile) <- c("percentile", "value")
  dat_hh_clean_percentile$value <- as.numeric (dat_hh_clean_percentile$value)
  
  ## match data with percentiles 
  dat = dat_hh_clean %>% mutate (wgt_hh_members = weight * hh_members)
  dat$percentile <- dat_hh_clean_percentile$percentile [findInterval (dat$wealth_index, 
                                                                      dat_hh_clean_percentile$value, 
                                                                      rightmost.closed = TRUE)+1]
  
  ## compute weight sums per urban/rural and percentile
  dat = dat %>% drop_na () %>%
    
    # recoding urban with word categories
    mutate (urban = ifelse (urban == 1, "Urban", 
                            ifelse (urban == 2, "Rural", NA))) %>%
    
    # computing percentile weight totals
    group_by (percentile, urban) %>%
    summarise (wgt_pctl = sum (wgt_hh_members, na.rm = T)) %>%
    
    # removing % sign
    mutate (percentile = gsub ("%", "", percentile),
            percentile = as.numeric (percentile))
  
  .GlobalEnv$dat = dat
  
}

## applying cleaning function to country files
eth_hh_clean = dhs_datclean (eth_hh)
rwa_hh_clean = dhs_datclean (rwa_hh)
bur_hh_clean = dhs_datclean (bur_hh)
drc_hh_clean = dhs_datclean (drc_hh)

# additional transformations to fix a problematic percentile
ken_hh_clean = dhs_datclean (ken_hh) %>%
  filter (!(percentile == 0)) %>%
  mutate (wgt_pctl = ifelse (urban == "Rural" & percentile == 99, 1, wgt_pctl))

tzn_hh_clean = dhs_datclean (tzn_hh)
uga_hh_clean = dhs_datclean (uga_hh)



## Chapter 3: computing shares -------------------------------------------------

## Getting WDP data 

# loading raw file
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_10_11_2017ppp_1_USD.rds"))

# sorting into desired spending groups
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 2, 5, 8, 12, 40, 80, 120, Inf))

# cleaning data
wdp_clean = wdp %>%
  
  # country-year headcounts per spending group
  group_by (ccode, year, daily_spending) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # adding a temporary country-year variable
  unite ("ccode_year", c(ccode, year), sep = "_", remove = F, na.rm = F) %>%
  
  # filtering for relevant country-years
  filter (ccode_year %in% c("ETH_2019", "RWA_2019", "RWA_2020", "BDI_2016", "BDI_2017", "COD_2013", "COD_2014", "KEN_2014", "TZA_2015", "TZA_2016", "UGA_2016")) %>%
  dplyr::select (-ccode_year) %>%
  
  # adding total population variable
  group_by (ccode, year) %>%
  mutate (total = sum (pop, na.rm = T),
          share = (pop / total)*100) %>%
  
  # arrange into order by ccode, year and spending group 
  arrange (ccode, year, factor (daily_spending, levels = c("[0,2)", "[2,5)", "[5,8)", "[8,12)", "[12,40)", "[40,80)", "[80,120)", "[120,Inf)"))) %>%
  
  # making daily_spending a factor variable with a set order
  mutate (daily_spending = factor (daily_spending, levels = c("[0,2)", "[2,5)", "[5,8)", "[8,12)", "[12,40)", "[40,80)", "[80,120)", "[120,Inf)"))) %>%
  
  # for countries with a year boundary, computing average of both years' shares
  group_by (ccode, daily_spending) %>%
  summarise (share = mean (share, na.rm = T)) %>%
  
  # obtaining percentile location of each spending group using cumsum
  ungroup () %>%
  group_by (ccode) %>%
  mutate (cs = cumsum (share)) %>% ungroup () %>%
  
  # removing the 100th percentile
  filter (!(daily_spending == "[120,Inf)"))

## creating function to generate shares
dhs_shares = function (data, country) {
  
  data = data
  country = country
  
  # creating df with rural urban shares
  dat_shares = data %>%
    pivot_wider (names_from = urban, values_from = wgt_pctl) %>%
    mutate (urban_share = Urban / (Urban + Rural),
            rural_share = Rural / (Urban + Rural)) %>%
    dplyr::select (percentile, urban_share, rural_share)
  
  # LOESS (local) regression models 
  model_urban <- loess (urban_share ~ percentile, data = dat_shares, span = 0.10)
  model_rural <- loess (rural_share ~ percentile, data = dat_shares, span = 0.10)
  
  # creating df with predicted values for specified ranges
  out = data.frame (percentile = c(dat_shares$percentile, wdp_clean$cs [wdp_clean$ccode == country])) %>% 
    arrange (percentile) %>%
    mutate (ccode = country) %>%
    relocate (ccode, .before = percentile)
  out$urban <- predict (model_urban, newdata = out)
  out$rural <- predict (model_rural, newdata = out)
  
  .GlobalEnv$out = out
  
}

## applying function to cleaned country files
eth_shares = dhs_shares (eth_hh_clean, "ETH")
rwa_shares = dhs_shares (rwa_hh_clean, "RWA")
bur_shares = dhs_shares (bur_hh_clean, "BDI")

# DRC irregularity: according to survey, 100% of people past the 93rd percentile live in urban areas
drc_shares = dhs_shares (drc_hh_clean, "COD") %>%
  mutate (urban = ifelse (is.na (urban), 1, urban),
          rural = ifelse (is.na (rural), 0, rural))

# Kenya irregularity: data is weird due to household weights, and according to survey, the percentiles between 99 and 100 ar 100% urban 
ken_shares = dhs_shares (ken_hh_clean, "KEN")
ken_shares$urban [104:107] <- 1
ken_shares$rural [104:107] <- 0
ken_shares$urban [1] <- 0
ken_shares$rural [1] <- 1

tzn_shares = dhs_shares (tzn_hh_clean, "TZA")
uga_shares = dhs_shares (uga_hh_clean, "UGA")

## final df with urban / rural shares per spending group

spending = as.factor (c("$0-2", "$2-5", "$5-8", "$8-12", "$12-40", "$40-80", "$80-120", "$>120"))
countries = as.factor (c("ETH", "RWA", "BDI", "COD", "KEN", "TZA", "UGA"))
final = expand.grid (ccode = countries, spending = spending) %>% arrange (ccode)
final$urban_share <- NA
final$rural_share <- NA

final = final %>%
  
  # all the massive mutate code essentially takes the inter-percentile averages for each spending group
  # Ethiopia
  mutate (urban_share = ifelse (ccode == "ETH" & spending == "$0-2", sum (eth_shares$urban [1:10] / 10),
                                ifelse (ccode == "ETH" & spending == "$2-5", sum (eth_shares$urban [11:60] / 50),
                                        ifelse (ccode == "ETH" & spending == "$5-8", sum (eth_shares$urban [61:90] / 30),
                                                ifelse (ccode == "ETH" & spending == "$8-12", sum (eth_shares$urban [91:99] / 9),
                                                        ifelse (ccode == "ETH" & spending == "$12-40", sum (eth_shares$urban [100:104] / 5),
                                                                ifelse (ccode == "ETH" & spending == "$40-80", sum (eth_shares$urban [105] / 1),
                                                                        ifelse (ccode == "ETH" & spending == "$80-120", sum (eth_shares$urban [106] / 1),
                                                                                ifelse (ccode == "ETH" & spending == "$>120", sum (eth_shares$urban [107] / 1),
                                                                                        urban_share))))))))) %>%
  
  mutate (rural_share = ifelse (ccode == "ETH" & spending == "$0-2", sum (eth_shares$rural [1:10] / 10),
                                ifelse (ccode == "ETH" & spending == "$2-5", sum (eth_shares$rural [11:60] / 50),
                                        ifelse (ccode == "ETH" & spending == "$5-8", sum (eth_shares$rural [61:90] / 30),
                                                ifelse (ccode == "ETH" & spending == "$8-12", sum (eth_shares$rural [91:99] / 9),
                                                        ifelse (ccode == "ETH" & spending == "$12-40", sum (eth_shares$rural [100:104] / 5),
                                                                ifelse (ccode == "ETH" & spending == "$40-80", sum (eth_shares$rural [105] / 1),
                                                                        ifelse (ccode == "ETH" & spending == "$80-120", sum (eth_shares$rural [106] / 1),
                                                                                ifelse (ccode == "ETH" & spending == "$>120", sum (eth_shares$rural [107] / 1),
                                                                                        rural_share))))))))) %>%
 
   # Rwanda
  mutate (urban_share = ifelse (ccode == "RWA" & spending == "$0-2", sum (rwa_shares$urban [1:41] / 41),
                                ifelse (ccode == "RWA" & spending == "$2-5", sum (rwa_shares$urban [42:79] / 38),
                                        ifelse (ccode == "RWA" & spending == "$5-8", sum (rwa_shares$urban [80:90] / 11),
                                                ifelse (ccode == "RWA" & spending == "$8-12", sum (rwa_shares$urban [91:97] / 7),
                                                        ifelse (ccode == "RWA" & spending == "$12-40", sum (rwa_shares$urban [98:103] / 6),
                                                                ifelse (ccode == "RWA" & spending == "$40-80", sum (rwa_shares$urban [104:105] / 2),
                                                                        ifelse (ccode == "RWA" & spending == "$80-120", sum (rwa_shares$urban [106] / 1),
                                                                                ifelse (ccode == "RWA" & spending == "$>120", sum (rwa_shares$urban [107] / 1),
                                                                                        urban_share))))))))) %>%
  
  mutate (rural_share = ifelse (ccode == "RWA" & spending == "$0-2", sum (rwa_shares$rural [1:41] / 41),
                                ifelse (ccode == "RWA" & spending == "$2-5", sum (rwa_shares$rural [42:79] / 38),
                                        ifelse (ccode == "RWA" & spending == "$5-8", sum (rwa_shares$rural [80:90] / 11),
                                                ifelse (ccode == "RWA" & spending == "$8-12", sum (rwa_shares$rural [91:97] / 7),
                                                        ifelse (ccode == "RWA" & spending == "$12-40", sum (rwa_shares$rural [98:103] / 6),
                                                                ifelse (ccode == "RWA" & spending == "$40-80", sum (rwa_shares$rural [104:105] / 2),
                                                                        ifelse (ccode == "RWA" & spending == "$80-120", sum (rwa_shares$rural [106] / 1),
                                                                                ifelse (ccode == "RWA" & spending == "$>120", sum (rwa_shares$rural [107] / 1),
                                                                                        rural_share))))))))) %>%
  
  # Burundi
  mutate (urban_share = ifelse (ccode == "BDI" & spending == "$0-2", sum (bur_shares$urban [1:77] / 77),
                                ifelse (ccode == "BDI" & spending == "$2-5", sum (bur_shares$urban [78:100] / 23),
                                        ifelse (ccode == "BDI" & spending == "$5-8", sum (bur_shares$urban [101:102] / 2),
                                                ifelse (ccode == "BDI" & spending == "$8-12", sum (bur_shares$urban [103] / 1),
                                                        ifelse (ccode == "BDI" & spending == "$12-40", sum (bur_shares$urban [104] / 1),
                                                                ifelse (ccode == "BDI" & spending == "$40-80", sum (bur_shares$urban [105] / 1),
                                                                        ifelse (ccode == "BDI" & spending == "$80-120", sum (bur_shares$urban [106] / 1),
                                                                                ifelse (ccode == "BDI" & spending == "$>120", sum (bur_shares$urban [107] / 1),
                                                                                        urban_share))))))))) %>%
  
  mutate (rural_share = ifelse (ccode == "BDI" & spending == "$0-2", sum (bur_shares$rural [1:77] / 77),
                                ifelse (ccode == "BDI" & spending == "$2-5", sum (bur_shares$rural [78:100] / 23),
                                        ifelse (ccode == "BDI" & spending == "$5-8", sum (bur_shares$rural [101:102] / 2),
                                                ifelse (ccode == "BDI" & spending == "$8-12", sum (bur_shares$rural [103] / 1),
                                                        ifelse (ccode == "BDI" & spending == "$12-40", sum (bur_shares$rural [104] / 1),
                                                                ifelse (ccode == "BDI" & spending == "$40-80", sum (bur_shares$rural [105] / 1),
                                                                        ifelse (ccode == "BDI" & spending == "$80-120", sum (bur_shares$rural [106] / 1),
                                                                                ifelse (ccode == "BDI" & spending == "$>120", sum (bur_shares$rural [107] / 1),
                                                                                        rural_share))))))))) %>%
  
  # DRC
  mutate (urban_share = ifelse (ccode == "COD" & spending == "$0-2", sum (drc_shares$urban [1:65] / 65),
                                ifelse (ccode == "COD" & spending == "$2-5", sum (drc_shares$urban [66:96] / 31),
                                        ifelse (ccode == "COD" & spending == "$5-8", sum (drc_shares$urban [97:101] / 5),
                                                ifelse (ccode == "COD" & spending == "$8-12", sum (drc_shares$urban [102:103] / 2),
                                                        ifelse (ccode == "COD" & spending == "$12-40", sum (drc_shares$urban [104] / 1),
                                                                ifelse (ccode == "COD" & spending == "$40-80", sum (drc_shares$urban [105] / 1),
                                                                        ifelse (ccode == "COD" & spending == "$80-120", sum (drc_shares$urban [106] / 1),
                                                                                ifelse (ccode == "COD" & spending == "$>120", sum (drc_shares$urban [107] / 1),
                                                                                        urban_share))))))))) %>%
  
  mutate (rural_share = ifelse (ccode == "COD" & spending == "$0-2", sum (drc_shares$rural [1:65] / 65),
                                ifelse (ccode == "COD" & spending == "$2-5", sum (drc_shares$rural [66:96] / 31),
                                        ifelse (ccode == "COD" & spending == "$5-8", sum (drc_shares$rural [97:101] / 5),
                                                ifelse (ccode == "COD" & spending == "$8-12", sum (drc_shares$rural [102:103] / 2),
                                                        ifelse (ccode == "COD" & spending == "$12-40", sum (drc_shares$rural [104] / 1),
                                                                ifelse (ccode == "COD" & spending == "$40-80", sum (drc_shares$rural [105] / 1),
                                                                        ifelse (ccode == "COD" & spending == "$80-120", sum (drc_shares$rural [106] / 1),
                                                                                ifelse (ccode == "COD" & spending == "$>120", sum (drc_shares$rural [107] / 1),
                                                                                        rural_share))))))))) %>%
  
  # Kenya
  mutate (urban_share = ifelse (ccode == "KEN" & spending == "$0-2", sum (ken_shares$urban [1:13] / 13),
                                ifelse (ccode == "KEN" & spending == "$2-5", sum (ken_shares$urban [14:46] / 33),
                                        ifelse (ccode == "KEN" & spending == "$5-8", sum (ken_shares$urban [47:68] / 22),
                                                ifelse (ccode == "KEN" & spending == "$8-12", sum (ken_shares$urban [69:83] / 15),
                                                        ifelse (ccode == "KEN" & spending == "$12-40", sum (ken_shares$urban [84:102] / 19),
                                                                ifelse (ccode == "KEN" & spending == "$40-80", sum (ken_shares$urban [103:105] / 3),
                                                                        ifelse (ccode == "KEN" & spending == "$80-120", sum (ken_shares$urban [106] / 1),
                                                                                ifelse (ccode == "KEN" & spending == "$>120", sum (ken_shares$urban [107] / 1),
                                                                                        urban_share))))))))) %>%
  
  mutate (rural_share = ifelse (ccode == "KEN" & spending == "$0-2", sum (ken_shares$rural [1:13] / 13),
                                ifelse (ccode == "KEN" & spending == "$2-5", sum (ken_shares$rural [14:46] / 33),
                                        ifelse (ccode == "KEN" & spending == "$5-8", sum (ken_shares$rural [47:68] / 22),
                                                ifelse (ccode == "KEN" & spending == "$8-12", sum (ken_shares$rural [69:83] / 15),
                                                        ifelse (ccode == "KEN" & spending == "$12-40", sum (ken_shares$rural [84:102] / 19),
                                                                ifelse (ccode == "KEN" & spending == "$40-80", sum (ken_shares$rural [103:105] / 3),
                                                                        ifelse (ccode == "KEN" & spending == "$80-120", sum (ken_shares$rural [106] / 1),
                                                                                ifelse (ccode == "KEN" & spending == "$>120", sum (ken_shares$rural [107] / 1),
                                                                                        rural_share))))))))) %>%
  
  # Tanzania
  mutate (urban_share = ifelse (ccode == "TZA" & spending == "$0-2", sum (tzn_shares$urban [1:32] / 32),
                                ifelse (ccode == "TZA" & spending == "$2-5", sum (tzn_shares$urban [33:76] / 44),
                                        ifelse (ccode == "TZA" & spending == "$5-8", sum (tzn_shares$urban [77:91] / 15),
                                                ifelse (ccode == "TZA" & spending == "$8-12", sum (tzn_shares$urban [92:98] / 7),
                                                        ifelse (ccode == "TZA" & spending == "$12-40", sum (tzn_shares$urban [99:104] / 6),
                                                                ifelse (ccode == "TZA" & spending == "$40-80", sum (tzn_shares$urban [105] / 1),
                                                                        ifelse (ccode == "TZA" & spending == "$80-120", sum (tzn_shares$urban [106] / 1),
                                                                                ifelse (ccode == "TZA" & spending == "$>120", sum (tzn_shares$urban [107] / 1),
                                                                                        urban_share))))))))) %>%
  
  mutate (rural_share = ifelse (ccode == "TZA" & spending == "$0-2", sum (tzn_shares$rural [1:32] / 32),
                                ifelse (ccode == "TZA" & spending == "$2-5", sum (tzn_shares$rural [33:76] / 44),
                                        ifelse (ccode == "TZA" & spending == "$5-8", sum (tzn_shares$rural [77:91] / 15),
                                                ifelse (ccode == "TZA" & spending == "$8-12", sum (tzn_shares$rural [92:98] / 7),
                                                        ifelse (ccode == "TZA" & spending == "$12-40", sum (tzn_shares$rural [99:104] / 6),
                                                                ifelse (ccode == "TZA" & spending == "$40-80", sum (tzn_shares$rural [105] / 1),
                                                                        ifelse (ccode == "TZA" & spending == "$80-120", sum (tzn_shares$rural [106] / 1),
                                                                                ifelse (ccode == "TZA" & spending == "$>120", sum (tzn_shares$rural [107] / 1),
                                                                                        rural_share))))))))) %>%
  
  # Uganda
  mutate (urban_share = ifelse (ccode == "UGA" & spending == "$0-2", sum (uga_shares$urban [1:36] / 36),
                                ifelse (ccode == "UGA" & spending == "$2-5", sum (uga_shares$urban [37:78] / 42),
                                        ifelse (ccode == "UGA" & spending == "$5-8", sum (uga_shares$urban [79:91] / 13),
                                                ifelse (ccode == "UGA" & spending == "$8-12", sum (uga_shares$urban [92:98] / 7),
                                                        ifelse (ccode == "UGA" & spending == "$12-40", sum (uga_shares$urban [99:104] / 6),
                                                                ifelse (ccode == "UGA" & spending == "$40-80", sum (uga_shares$urban [105] / 1),
                                                                        ifelse (ccode == "UGA" & spending == "$80-120", sum (uga_shares$urban [106] / 1),
                                                                                ifelse (ccode == "UGA" & spending == "$>120", sum (uga_shares$urban [107] / 1),
                                                                                        urban_share))))))))) %>%
  
  mutate (rural_share = ifelse (ccode == "UGA" & spending == "$0-2", sum (uga_shares$rural [1:36] / 36),
                                ifelse (ccode == "UGA" & spending == "$2-5", sum (uga_shares$rural [37:78] / 42),
                                        ifelse (ccode == "UGA" & spending == "$5-8", sum (uga_shares$rural [79:91] / 13),
                                                ifelse (ccode == "UGA" & spending == "$8-12", sum (uga_shares$rural [92:98] / 7),
                                                        ifelse (ccode == "UGA" & spending == "$12-40", sum (uga_shares$rural [99:104] / 6),
                                                                ifelse (ccode == "UGA" & spending == "$40-80", sum (uga_shares$rural [105] / 1),
                                                                        ifelse (ccode == "UGA" & spending == "$80-120", sum (uga_shares$rural [106] / 1),
                                                                                ifelse (ccode == "UGA" & spending == "$>120", sum (uga_shares$rural [107] / 1),
                                                                                        rural_share))))))))) %>%
  
  # adding sum variable to check
  mutate (sum = urban_share + rural_share) %>%
  
  # converting ccode variable into country names 
  mutate (country = countrycode (ccode, "iso3c", "country.name")) %>%
  dplyr::select (country, spending, urban_share, rural_share, sum) %>%
  mutate (country = ifelse (country == "Congo - Kinshasa", "DRC", country))



## Chapter 4: data visualisation -----------------------------------------------

## creating function to present percentile distribution
dhs_graph = function (data) {
  
  dat = data
  
  dat_hh_barplot <- ggplot (data = dat, aes (x = percentile, y = wgt_pctl, fill = urban)) + 
    geom_bar (position = "stack", stat = "identity") + 
    #geom_text (size = 3, position = position_stack (vjust = 0.5), fontface = "bold") + 
    worlddataverse::theme_wdl() + 
    worlddataverse::scale_color_wdl() + 
    labs (title = "Total population by Wealth Index Percentile", 
          subtitle = "Visualising the urban / rural divide", 
          x = "Wealth Index Percentile (weighted)", 
          y = "Population (weighted)") + 
    theme (axis.text.x = element_text (angle = 0, margin = margin (t = 0, r = 0, b = 20, l = 0)),
           axis.text.y = element_text (margin = margin (t = 0, r = 20, b = 0, l = 20))) + 
    guides (fill = guide_legend (title = "Residence Type"))
  
  .GlobalEnv$dat_hh_barplot = dat_hh_barplot
  
}

## applying function to clean country files to obtain individual charts
eth_hh_plot = dhs_graph (eth_hh_clean)
rwa_hh_plot = dhs_graph (rwa_hh_clean)
bur_hh_plot = dhs_graph (bur_hh_clean)
drc_hh_plot = dhs_graph (drc_hh_clean)
ken_hh_plot = dhs_graph (ken_hh_clean)
tzn_hh_plot = dhs_graph (tzn_hh_clean)
uga_hh_plot = dhs_graph (uga_hh_clean)

## building grid plot

# joining all into one dataframe
dat_hh <- list (eth_hh_clean, rwa_hh_clean, bur_hh_clean, drc_hh_clean, ken_hh_clean, tzn_hh_clean, uga_hh_clean) %>%
  reduce (inner_join, by = c("percentile", "urban")) %>%
  rename_with (
    ~ case_when (
      . == "wgt_pctl.x" ~ "Ethiopia",
      . == "wgt_pctl.y" ~ "Rwanda",
      . == "wgt_pctl.x.x" ~ "Burundi",
      . == "wgt_pctl.y.y" ~ "DRC",
      . == "wgt_pctl.x.x.x" ~ "Kenya",
      . == "wgt_pctl.y.y.y" ~ "Tanzania",
      . == "wgt_pctl" ~ "Uganda",
      TRUE ~ .
    )
  ) %>%
  pivot_longer (3:9, names_to = "Country", values_to = "wgt_pct")

# creating plot
shares_gridplot <- ggplot (dat_hh, aes (x = percentile, y = wgt_pct, fill = urban)) + 
  geom_bar (position = "stack", stat = "identity") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl() + 
  labs (title = "Total population by Wealth Index Percentile", 
        subtitle = "Visualising the urban / rural divide", 
        x = "Wealth Index Percentile (weighted)", 
        y = "Population (weighted)") + 
  # theme (axis.text.x = element_text (angle = 0, margin = margin (t = 0, r = 0, b = 20, l = 0)),
         #axis.text.y = element_text (margin = margin (t = 0, r = 20, b = 0, l = 20))) + 
  guides (fill = guide_legend (title = "Residence Type")) + 
  facet_wrap (~Country, scale = "free")
shares_gridplot

## Chapter 5: conclusions ------------------------------------------------------

write.csv (final, "dhs_africa_urban_rural_shares.csv")




























