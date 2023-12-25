
## chapter 1: preliminaries ----------------------------------------------------

# clean environment
rm (list = ls())

# packages
pacman::p_load (worlddataverse, tidyverse, countrycode, zoo)

# loading paths
base_path <- worlddataverse::get_wdl_path()
if (is.na (base_path)) {base_path = file.path("/Volumes/",
                                              "GoogleDrive-115239951252043738907",
                                              "Shared drives",
                                              "DATA_WDL")}

input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data", "production_function", "OECD_DIOC_rawdata_IOM")
output_path <- file.path (base_path, "IOM", "2nd_paper", "output")

## chapter 2: interpolating ----------------------------------------------------

# interpolating 2015 and 2020
# subset 
DIOC_2000_20_1 = DIOC_2000_10 [,1:12]

# making sure 2015:2030 are blank  
DIOC_2000_20_1 [DIOC_2000_20_1$year %in% c(2015,2020,2025,2030), 6:12] = NA

values = unique (DIOC_2000_20_1$orig_dest)

datalist = vector("list", length = length(unique(values)))

for (i in values) {
  
  data1 = DIOC_2000_20_1 [DIOC_2000_20_1$orig_dest == i,] %>%
    
    mutate (pri_15_34_lagged = Lag (pri_15_34, -1)) %>%
    mutate (pri_15_34_growth = (pri_15_34_lagged - pri_15_34)/pri_15_34) %>%
    mutate (pri_15_34_growth = Lag (pri_15_34_growth, 1)) %>%
    
    mutate (sec_15_34_lagged = Lag (sec_15_34, -1)) %>%
    mutate (sec_15_34_growth = (sec_15_34_lagged - sec_15_34)/sec_15_34) %>%
    mutate (sec_15_34_growth = Lag (sec_15_34_growth, 1)) %>%
    
    mutate (ter_15_34_lagged = Lag (ter_15_34, -1)) %>%
    mutate (ter_15_34_growth = (ter_15_34_lagged - ter_15_34)/ter_15_34) %>%
    mutate (ter_15_34_growth = Lag (ter_15_34_growth, 1)) %>%
    
    mutate (pri_35_64_lagged = Lag (pri_35_64, -1)) %>%
    mutate (pri_35_64_growth = (pri_35_64_lagged - pri_35_64)/pri_35_64) %>%
    mutate (pri_35_64_growth = Lag (pri_35_64_growth, 1)) %>%
    
    mutate (sec_35_64_lagged = Lag (sec_35_64, -1)) %>%
    mutate (sec_35_64_growth = (sec_35_64_lagged - sec_35_64)/sec_35_64) %>%
    mutate (sec_35_64_growth = Lag (sec_35_64_growth, 1)) %>%
    
    mutate (ter_35_64_lagged = Lag (ter_35_64, -1)) %>%
    mutate (ter_35_64_growth = (ter_35_64_lagged - ter_35_64)/ter_35_64) %>%
    mutate (ter_35_64_growth = Lag (ter_35_64_growth, 1)) %>%
    
    mutate (others_lagged = Lag (others, -1)) %>%
    mutate (others_growth = (others_lagged - others)/others) %>%
    mutate (others_growth = Lag (others_growth, 1))
  
  pri_15_34_growth_avg = mean (data1$pri_15_34_growth, na.rm = T)
  sec_15_34_growth_avg = mean (data1$sec_15_34_growth, na.rm = T)
  ter_15_34_growth_avg = mean (data1$ter_15_34_growth, na.rm = T)
  pri_35_64_growth_avg = mean (data1$pri_35_64_growth, na.rm = T)
  sec_35_64_growth_avg = mean (data1$sec_35_64_growth, na.rm = T)
  ter_35_64_growth_avg = mean (data1$ter_35_64_growth, na.rm = T)
  others_growth_avg = mean (data1$others_growth, na.rm = T)
  
  data1$pri_15_34 [data1$year == 2015] <- data1$pri_15_34 [data1$year == 2010] * (1 + pri_15_34_growth_avg)
  data1$sec_15_34 [data1$year == 2015] <- data1$sec_15_34 [data1$year == 2010] * (1 + sec_15_34_growth_avg)
  data1$ter_15_34 [data1$year == 2015] <- data1$ter_15_34 [data1$year == 2010] * (1 + ter_15_34_growth_avg)
  data1$pri_35_64 [data1$year == 2015] <- data1$pri_35_64 [data1$year == 2010] * (1 + pri_35_64_growth_avg)
  data1$sec_35_64 [data1$year == 2015] <- data1$sec_35_64 [data1$year == 2010] * (1 + sec_35_64_growth_avg)
  data1$ter_35_64 [data1$year == 2015] <- data1$ter_35_64 [data1$year == 2010] * (1 + ter_35_64_growth_avg)
  data1$others [data1$year == 2015] <- data1$others [data1$year == 2010] * (1 + others_growth_avg)
  
  data1$pri_15_34 [data1$year == 2020] <- data1$pri_15_34 [data1$year == 2010] * (1 + pri_15_34_growth_avg)^2
  data1$sec_15_34 [data1$year == 2020] <- data1$sec_15_34 [data1$year == 2010] * (1 + sec_15_34_growth_avg)^2
  data1$ter_15_34 [data1$year == 2020] <- data1$ter_15_34 [data1$year == 2010] * (1 + ter_15_34_growth_avg)^2
  data1$pri_35_64 [data1$year == 2020] <- data1$pri_35_64 [data1$year == 2010] * (1 + pri_35_64_growth_avg)^2
  data1$sec_35_64 [data1$year == 2020] <- data1$sec_35_64 [data1$year == 2010] * (1 + sec_35_64_growth_avg)^2
  data1$ter_35_64 [data1$year == 2020] <- data1$ter_35_64 [data1$year == 2010] * (1 + ter_35_64_growth_avg)^2
  data1$others [data1$year == 2020] <- data1$others [data1$year == 2010] * (1 + others_growth_avg)
  
  data1$pri_15_34 [data1$year == 2025] <- mean (data1$pri_15_34 [data1$year %in% c(2000,2005,2010,2015,2020)], na.rm = T)
  data1$sec_15_34 [data1$year == 2025] <- mean (data1$sec_15_34 [data1$year %in% c(2000,2005,2010,2015,2020)], na.rm = T)
  data1$ter_15_34 [data1$year == 2025] <- mean (data1$ter_15_34 [data1$year %in% c(2000,2005,2010,2015,2020)], na.rm = T)
  data1$pri_35_64 [data1$year == 2025] <- mean (data1$pri_35_64 [data1$year %in% c(2000,2005,2010,2015,2020)], na.rm = T)
  data1$sec_35_64 [data1$year == 2025] <- mean (data1$sec_35_64 [data1$year %in% c(2000,2005,2010,2015,2020)], na.rm = T)
  data1$ter_35_64 [data1$year == 2025] <- mean (data1$ter_35_64 [data1$year %in% c(2000,2005,2010,2015,2020)], na.rm = T)
  data1$others [data1$year == 2025] <- mean (data1$others [data1$year %in% c(2000,2005,2010,2015,2020)], na.rm = T)
  
  data1$pri_15_34 [data1$year == 2030] = data1$pri_15_34 [data1$year == 2025]
  data1$sec_15_34 [data1$year == 2030] = data1$sec_15_34 [data1$year == 2025]
  data1$ter_15_34 [data1$year == 2030] = data1$ter_15_34 [data1$year == 2025]
  data1$pri_35_64 [data1$year == 2030] = data1$pri_35_64 [data1$year == 2025]
  data1$sec_35_64 [data1$year == 2030] = data1$sec_35_64 [data1$year == 2025]
  data1$ter_35_64 [data1$year == 2030] = data1$ter_35_64 [data1$year == 2025]
  data1$others [data1$year == 2030] = data1$others [data1$year == 2025]
  
  data1 = data1 %>% dplyr::select (1:12)
  
  datalist[[i]] <- data1
  
}

big_data = do.call(rbind, datalist)

DIOC_2000_30 = big_data 
rm (df_1, df_2, df_3, data1, datalist)

# fixing Inf problem in "others" column

DIOC_2000_30$others = ifelse (is.na(DIOC_2000_30$others), "NA_NA",
                              ifelse (DIOC_2000_30$others == "Inf", NA, DIOC_2000_30$others))

values = unique (DIOC_2000_30$orig_dest)

for (i in values) {
  
  DIOC_2000_30$others [DIOC_2000_30$orig_dest == i] <- 
    ifelse (is.na (DIOC_2000_30$others [DIOC_2000_30$orig_dest == i]),
            zoo::na.locf(DIOC_2000_30$others [DIOC_2000_30$orig_dest == i]),
            ifelse (DIOC_2000_30$others [DIOC_2000_30$orig_dest == i] == "NA_NA",
                    NA, DIOC_2000_30$others [DIOC_2000_30$orig_dest == i]))
           
}

# checking total shares
DIOC_2000_30$others = as.numeric (DIOC_2000_30$others)
DIOC_2000_30$total_shares <- rowSums (DIOC_2000_30 [,6:12], na.rm = T)
DIOC_2000_30 [,6:12] = DIOC_2000_30 [,6:12] / DIOC_2000_30 [,13]
DIOC_2000_30$total_shares_2 = rowSums (DIOC_2000_30[,6:12], na.rm = T)
DIOC_2000_30 [is.na(DIOC_2000_30)] <- 0
DIOC_2000_30_shares = DIOC_2000_30 %>% 
  dplyr::select (-c("total_shares")) %>%
  rename (total_shares_2 = total_shares)


## FOR ALTERVATIVE VERSION WHERE WE USE THE LAST AVAILABLE GROWTH RATE

# creating df with shares until 2030

DIOC_2025_empty <- DIOC_2000_20_shares %>%
  filter (year == 2020) %>%
  distinct (orig_dest, .keep_all = T) %>%
  mutate (year = 2025) %>%
  relocate (year, .before = orig_dest) %>%
  dplyr::select (-13)

DIOC_2030_empty <- DIOC_2000_20_shares %>%
  filter (year == 2020) %>%
  distinct (orig_dest, .keep_all = T) %>%
  mutate (year = 2030) %>%
  relocate (year, .before = orig_dest) %>%
  dplyr::select (-13)

DIOC_2025_2030_empty = rbind (DIOC_2025_empty, DIOC_2030_empty)
DIOC_2025_2030_empty$total_shares = rowSums (DIOC_2025_2030_empty [,6:12], na.rm = T)

DIOC_2000_2030_1 = rbind (DIOC_2000_20_shares, DIOC_2025_2030_empty)


