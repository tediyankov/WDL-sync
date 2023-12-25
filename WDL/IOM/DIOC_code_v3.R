
## chapter 1: preliminaries ----------------------------------------------------

## loading data 

DIOC_load = function () {
  
  # packages
  pacman::p_load (worlddataverse, tidyverse, countrycode)
  require ("worlddataverse", "tidyverse", "countrycode")
  
  # base path to Google Drive (including alternative route)
  base_path <- worlddataverse::get_wdl_path()
  
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
  
  # input and output paths + save to Global Environment
  input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data", "production_function", "OECD_DIOC_rawdata_IOM")
  output_path <- file.path (base_path, "IOM", "2nd_paper", "output")
  
  .GlobalEnv$base_path = base_path
  .GlobalEnv$input_path = input_path
  .GlobalEnv$output_path = output_path
  
  # load raw data
  DIOC_2000_raw = read.csv (file.path(input_path, "2000_01 release", "FILE_A_1_T.csv"))
  DIOC_2005_raw = read.csv (file.path(input_path, "2005_06 release", "T1.2_2005.CSV"))
  DIOC_2010_raw = read.csv (file.path(input_path, "2010_11 release", "DIOC_2010_11_File_A_quater_REV.csv"))
  
  # save raw data files to Global Environment
  .GlobalEnv$DIOC_2000_raw = DIOC_2000_raw
  .GlobalEnv$DIOC_2005_raw = DIOC_2005_raw
  .GlobalEnv$DIOC_2010_raw = DIOC_2010_raw
  
}

## execute
DIOC_load()

## chapter 2: cleaning raw data ------------------------------------------------

## 2.1 - DIOC 2000_01 release --------------------------------------------------

DIOC_2000_clean = function () {
  
  # cleaning long data
  DIOC_2000_1 = DIOC_2000_raw %>% 
    select(-c("edu_cen", "age_lfs", "reg_oecd")) %>%
    rename_with (
      ~ case_when (
        . == "country" ~ "dest",
        . == "coub" ~ "orig",
        . == "edu_lfs" ~ "edu_lvl",
        . == "number" ~ "migStock",
        . == "age_cen" ~ "age",
        . == "reg_regions" ~ "region_birth",
        TRUE ~ .
      )
    ) %>%
    relocate (orig, .before = dest) %>%
    unite ("orig_dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    filter (fborn == 1) %>%
    select (-c("fborn")) %>%
    mutate (year = 2000) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock)) %>%
    mutate (edu_lvl = ifelse (edu_lvl == 1, "pri",
                              ifelse (edu_lvl == 2, "sec",
                                      ifelse (edu_lvl == 3, "ter", "unknown")
                              )
    )
    ) %>%
    mutate (age = ifelse (age == 1, "15_19",
                          ifelse (age == 2, "20_24",
                                  ifelse (age == 3, "25_29",
                                          ifelse (age == 4, "30_34",
                                                  ifelse (age == 5, "35_39",
                                                          ifelse (age == 6, "40_44",
                                                                  ifelse (age == 7, "45_49",
                                                                          ifelse (age == 8, "50_54",
                                                                                  ifelse (age == 9, "55_59",
                                                                                          ifelse (age == 10, "60_64",
                                                                                                  ifelse (age == 11, "65_69", 
                                                                                                          ifelse (age == 12, "70+","unknown")
                                                                                                  )
                                                                                          )
                                                                                  )
                                                                          )
                                                                  )
                                                          )
                                                  )
                                          )
                                  )
                          )
    )
    ) %>%
    mutate (age = ifelse (age %in% c("15_19", "20_24", "25_29", "30_34"), "15_34",
                          ifelse (!(age %in% c("unknown","65_69", "70+")), "35_64", 
                                  ifelse (age %in% c("65_69","70+"), "65+", age)))) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock))
  
  # making sure orig and dest exclude non-country ISO3 codes 
  DIOC_2000_2 = DIOC_2000_1 %>%
    ungroup() %>%
    mutate (orig = countrycode(sourcevar = orig,
                               origin = "iso3c",
                               destination = "iso3c",
                               warn = TRUE, 
                               nomatch = NA)) %>%
    mutate (dest = countrycode(sourcevar = dest,
                               origin = "iso3c",
                               destination = "iso3c",
                               warn = TRUE, 
                               nomatch = NA)) %>%
    filter (!(is.na(orig))) %>%
    filter (!(is.na(dest)))
  
  # df with migStock totals w/out category dis-aggregation
  DIOC_2000_total = DIOC_2000_2 %>% 
    group_by (year, orig_dest, orig, region_birth, dest) %>%
    summarise (migStock_total = sum (migStock))
  
  # converting migStocks into percentages
  DIOC_2000_3 = DIOC_2000_2 %>% 
    left_join (DIOC_2000_total, by = c("year", "orig_dest", "orig", "region_birth", "dest")) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age, migStock_total) %>%
    summarise (migStock_percent = migStock / migStock_total) %>%
    ungroup() %>%
    select (-c("migStock_total")) %>%
    rename (migStock = migStock_percent)
  
  # converting long data into wide data 
  DIOC_2000_4 = reshape2::dcast(DIOC_2000_3,
                                year + orig_dest + orig + region_birth + dest + age ~ edu_lvl,
                                value.var = "migStock")
  
  DIOC_2000_5 = reshape (DIOC_2000_4, 
                         idvar = c("year", "orig_dest", "orig", "region_birth", "dest"),
                         timevar = "age",
                         direction = "wide") %>%
    rename_with (
      ~ case_when (
        . == "primary.15_34" ~ "pri_15_34",
        . == "primary.35_64" ~ "pri_35_64",
        . == "secondary.15_34" ~ "sec_15_34",
        . == "secondary.35_64" ~ "sec_35_64",
        . == "tertiary.15_34" ~ "ter_15_34",
        . == "tertiary.35_64" ~ "ter_35_64",
        TRUE ~ .
      )
    )
  
  DIOC_2000_5[is.na(DIOC_2000_5)] <- 0
  
  DIOC_2000_5$total_shares <- rowSums (DIOC_2000_5 [,6:21])
  
  
  
  # saving to Global Environment
  .GlobalEnv$DIOC_2000 = DIOC_2000_5
  
}

DIOC_2000_clean()

## 2.2 - DIOC 2005_06 release --------------------------------------------------

## data cleaning function

DIOC_2005_clean = function () {
  
  # cleaning long data
  DIOC_2005_1 = DIOC_2005_raw %>%
    select(-c("edu_cen", "age_lfs", "reg_oecd_2000", "reg_oecd_2010")) %>%
    rename_with (
      ~ case_when (
        . == "country" ~ "dest",
        . == "coub" ~ "orig",
        . == "edu_lfs" ~ "edu_lvl",
        . == "number" ~ "migStock",
        . == "age_cen" ~ "age",
        . == "reg_regions" ~ "region_birth",
        TRUE ~ .
      )
    ) %>%
    relocate (orig, .before = dest) %>%
    unite ("orig_dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    filter (fborn == 1) %>%
    select (-c("fborn")) %>%
    mutate (year = 2005) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock)) %>%
    mutate (edu_lvl = ifelse (edu_lvl == 1, "pri",
                              ifelse (edu_lvl == 2, "sec",
                                      ifelse (edu_lvl == 3, "ter", "unknown")
                              )
    )
    ) %>%
    mutate (age = ifelse (age == 1, "15_19",
                          ifelse (age == 2, "20_24",
                                  ifelse (age == 3, "25_29",
                                          ifelse (age == 4, "30_34",
                                                  ifelse (age == 5, "35_39",
                                                          ifelse (age == 6, "40_44",
                                                                  ifelse (age == 7, "45_49",
                                                                          ifelse (age == 8, "50_54",
                                                                                  ifelse (age == 9, "55_59",
                                                                                          ifelse (age == 10, "60_64",
                                                                                                  ifelse (age == 11, "65_69", 
                                                                                                          ifelse (age == 12, "70+","unknown")
                                                                                                  )
                                                                                          )
                                                                                  )
                                                                          )
                                                                  )
                                                          )
                                                  )
                                          )
                                  )
                          )
    )
    ) %>%
    mutate (age = ifelse (age %in% c("15_19", "20_24", "25_29", "30_34"), "15_34",
                          ifelse (!(age %in% c("unknown","65_69", "70+")), "35_64", 
                                  ifelse (age %in% c("65_69","70+"), "65+", age)))) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock))
  
  # making sure orig and dest exclude non-country ISO3 codes 
  DIOC_2005_2 = DIOC_2005_1 %>%
    ungroup () %>%
    mutate (orig = countrycode(sourcevar = orig,
                               origin = "iso3c",
                               destination = "iso3c",
                               warn = TRUE, 
                               nomatch = NA)) %>%
    mutate (dest = countrycode(sourcevar = dest,
                               origin = "iso3c",
                               destination = "iso3c",
                               warn = TRUE, 
                               nomatch = NA)) %>%
    filter (!(is.na(orig))) %>%
    filter (!(is.na(dest)))
  
  # df with migStock totals w/out category dis-aggregation
  DIOC_2005_total = DIOC_2005_2 %>% 
    group_by (year, orig_dest, orig, region_birth, dest) %>%
    summarise (migStock_total = sum (migStock))
  
  # converting migStocks into percentages
  DIOC_2005_3 = DIOC_2005_2 %>% 
    left_join (DIOC_2005_total, by = c("year", "orig_dest", "orig", "region_birth", "dest")) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age, migStock_total) %>%
    summarise (migStock_percent = migStock / migStock_total) %>%
    ungroup() %>%
    select (-c("migStock_total")) %>%
    rename (migStock = migStock_percent)
  
  # converting long data into wide data 
  DIOC_2005_4 = reshape2::dcast(DIOC_2005_3,
                                year + orig_dest + orig + region_birth + dest + age ~ edu_lvl,
                                value.var = "migStock")
  
  DIOC_2005_5 = reshape (DIOC_2005_4, 
                         idvar = c("year", "orig_dest", "orig", "region_birth", "dest"),
                         timevar = "age",
                         direction = "wide") %>%
    rename_with (
      ~ case_when (
        . == "primary.15_34" ~ "pri_15_34",
        . == "primary.35_64" ~ "pri_35_64",
        . == "secondary.15_34" ~ "sec_15_34",
        . == "secondary.35_64" ~ "sec_35_64",
        . == "tertiary.15_34" ~ "ter_15_34",
        . == "tertiary.35_64" ~ "ter_35_64",
        TRUE ~ .
      )
    )
  
  DIOC_2005_5[is.na(DIOC_2005_5)] <- 0
  
  DIOC_2005_5$total_shares <- rowSums (DIOC_2005_5 [,6:21])
  
  # saving to Global Environment
  .GlobalEnv$DIOC_2005 = DIOC_2005_5
  
}

## execute
DIOC_2005_clean()

## 2.3 - DIOC 2010_11 release --------------------------------------------------

## data cleaning function

DIOC_2010_clean = function () {
  
  # cleaning long data
  DIOC_2010_1 = DIOC_2010_raw %>%
    select(-c("edu_detailed", "birth", "source", "oecd", "oecdb")) %>%
    rename_with (
      ~ case_when (
        . == "country" ~ "dest",
        . == "coub" ~ "orig",
        . == "edu_lfs" ~ "edu_lvl",
        . == "number" ~ "migStock",
        . == "regionb" ~ "region_birth",
        TRUE ~ .
      )
    ) %>%
    relocate (orig, .before = dest) %>%
    unite ("orig_dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    filter (fborn == 1) %>%
    select (-c("fborn")) %>%
    mutate (year = 2010) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock)) %>%
    mutate (edu_lvl = ifelse (edu_lvl == 1, "pri",
                              ifelse (edu_lvl == 2, "sec",
                                      ifelse (edu_lvl == 3, "ter", "unknown")
                              )
    )
    ) %>%
    mutate (age = ifelse (age == 1, "0_14",
                          ifelse (age == 2, "15_24",
                                  ifelse (age == 3, "25_34",
                                          ifelse (age == 4, "35_44",
                                                  ifelse (age == 5, "45_54",
                                                          ifelse (age == 6, "55_64",
                                                                  ifelse (age == 7, "65+","unknown")
                                                          )
                                                  )
                                          )
                                  )
                          )
    )
    ) %>%
    mutate (age = ifelse (age %in% c("15_24", "25_34"), "15_34",
                          ifelse (!(age %in% c("unknown","0_14", "65+")), "35_64", age))) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock))
  
  # making sure orig and dest exclude non-country ISO3 codes 
  DIOC_2010_2 = DIOC_2010_1 %>%
    ungroup () %>%
    mutate (orig = countrycode(sourcevar = orig,
                               origin = "iso3c",
                               destination = "iso3c",
                               warn = TRUE, 
                               nomatch = NA)) %>%
    mutate (dest = countrycode(sourcevar = dest,
                               origin = "iso3c",
                               destination = "iso3c",
                               warn = TRUE, 
                               nomatch = NA)) %>%
    filter (!(is.na(orig))) %>%
    filter (!(is.na(dest)))
  
  # df with migStock totals w/out category dis-aggregation
  DIOC_2010_total = DIOC_2010_2 %>% 
    group_by (year, orig_dest, orig, region_birth, dest) %>%
    summarise (migStock_total = sum (migStock))
  
  # converting migStocks into percentages
  DIOC_2010_3 = DIOC_2010_2 %>% 
    left_join (DIOC_2010_total, by = c("year", "orig_dest", "orig", "region_birth", "dest")) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age, migStock_total) %>%
    summarise (migStock_percent = migStock / migStock_total) %>%
    ungroup() %>%
    select (-c("migStock_total")) %>%
    rename (migStock = migStock_percent)
  
  # converting long data into wide data 
  DIOC_2010_4 = reshape2::dcast(DIOC_2010_3,
                                year + orig_dest + orig + region_birth + dest + age ~ edu_lvl,
                                value.var = "migStock")
  
  DIOC_2010_5 = reshape (DIOC_2010_4, 
                         idvar = c("year", "orig_dest", "orig", "region_birth", "dest"),
                         timevar = "age",
                         direction = "wide") %>%
    rename_with (
      ~ case_when (
        . == "primary.15_34" ~ "pri_15_34",
        . == "primary.35_64" ~ "pri_35_64",
        . == "secondary.15_34" ~ "sec_15_34",
        . == "secondary.35_64" ~ "sec_35_64",
        . == "tertiary.15_34" ~ "ter_15_34",
        . == "tertiary.35_64" ~ "ter_35_64",
        TRUE ~ .
      )
    )
  
  DIOC_2010_5[is.na(DIOC_2010_5)] <- 0
  
  DIOC_2010_5$total_shares <- rowSums (DIOC_2010_5 [,6:25])
  
  DIOC_2010_6 = DIOC_2010_5 %>% 
    filter (!(total_shares == 0)) %>%
    
    # removing 0_14 group as it does not appear anywhere else
    select (-c(6:9))
  
  # saving to Global Environment
  .GlobalEnv$DIOC_2010 = DIOC_2010_6
  
}

## execute
DIOC_2010_clean()

## binding into one unified data frame + ensuring full time series

# function

DIOC_bind_2000_10 = function () {
  
  # binding 2000 - 2010 into one data frame
  DIOC_2000_10 = rbind (DIOC_2000, DIOC_2005, DIOC_2010) %>%
    arrange (orig_dest) %>%
    rename_with (
      ~ case_when (
        . == "pri.15_34" ~ "pri_15_34",
        . == "sec.15_34" ~ "sec_15_34",
        . == "ter.15_34" ~ "ter_15_34",
        . == "pri.35_64" ~ "pri_35_64",
        . == "sec.35_64" ~ "sec_35_64",
        . == "ter.35_64" ~ "ter_35_64",
        TRUE ~ .
      )
    )
  
  df_1 = DIOC_2000_10 %>%
    select (c("orig_dest", "orig", "region_birth", "dest")) %>%
    distinct ()
  
  df_2 = df_1[rep(seq_len(nrow(df_1)), each = 5), ]
  
  df_3 = df_2 %>%
    mutate (year = rep(c(2000,2005,2010,2015,2020), times = 5509)) %>%
    relocate (year, .before = orig_dest) %>%
    # joining back with main df 
    left_join (DIOC_2000_10, by = names(.))
  
  df_3 [is.na(df_3)] <- 0
  
  df_3$others = rowSums (df_3[,c(9,13:21)])
  
  df_4 = df_3 %>% select (-c(9,13:21)) %>% relocate (others, .before = total_shares)
  
  .GlobalEnv$DIOC_2000_10 = df_4
  
}

DIOC_bind_2000_10()

## chapter 4: interpolating w/ growth rates  -----------------------------------

# interpolating 2015 and 2020

interpol_2015_20 = function () {
  
  # subset 
  DIOC_2000_20_1 = DIOC_2000_10 
  
  # blanking 2015 and 2020 rows 
  DIOC_2000_20_1 [DIOC_2000_20_1$year %in% c(2015,2020), 6:21] = NA
  
  values = unique (DIOC_2000_20_1$orig_dest)
  
  datalist = vector("list", length = length(unique(values)))
  
  for (i in values) {
    
    data1 = DIOC_2000_20_1 [DIOC_2000_20_1$orig_dest == i,]
    data1 [data1 == 0] <- NA
    
    data1 = data1 %>%
      
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
    
    data1 = data1 %>% select (1:22)
    
    datalist[[i]] <- data1
    
  }
  
  big_data = do.call(rbind, datalist)
  
  big_data [is.na(big_data)] <- 0
  
  .GlobalEnv$DIOC_2000_20 = big_data
  
}

# execute
interpol_2015_20()

# checking total shares
DIOC_2000_20$total_shares <- rowSums (DIOC_2000_20 [,6:12])

# normalising 2015 and 2020 rows

DIOC_2000_20 [,6:12] = DIOC_2000_20 [,6:12] / DIOC_2000_20 [,13]

DIOC_2000_20$total_shares_2 = rowSums (DIOC_2000_20[,6:12])

DIOC_2000_20 [is.na(DIOC_2000_20)] <- 0

DIOC_2000_20_1 = DIOC_2000_20_1 %>%
  select (-c("total_shares")) %>%
  rename (total_shares = total_shares_2)

DIOC_2000_20_2 = rbind (DIOC_2000_20_1, DIOC_2000_20_2010) %>% arrange (orig_dest)

DIOC_2000_20_2 [is.na (DIOC_2000_20_2)] <- 0

## chapter 5: restructuring into orig-totals + applying percentage distribution to actual stocks  -----------------------------------

restructure = function () {
  
  stocks_1 = read.csv (file.path (output_path,"merged_data_2040_exp.csv")) %>% 
    select (1:5, 12) %>%
    filter (!(year == 2020))
  
  stocks_1$year = ifelse (stocks_1$year == 2019, 
                          2020, 
                          stocks_1$year)
  
  stocks_1 = stocks_1 %>%
    filter (year %in% c(2000, 2005, 2010, 2015, 2020)) %>%
    rename (orig_dest = orig.dest,
            migStock = migstock_bir_int) %>%
    select (-c("orig.dest.year"))
  
  distributions = DIOC_2000_20_2 
  
  distributions$others = rowSums (distributions [,c(9, 13:21)])
  
  distributions = distributions %>% 
    select (-c(9, 13:21)) %>% 
    relocate (others, .before = total_shares)
  
  distributions$total_shares_2 = rowSums (distributions[,6:12])
  
  distributions = distributions %>% select (-c("total_shares", "total_shares_2"))
  
  migStocks_1 = distributions %>%
    left_join (stocks_1, by = c("year", "orig_dest", "orig", "dest"))
  
  migStocks_2 = migStocks_1 %>%
    mutate (mig_pri_15_34 = round (migStock * pri_15_34),
            mig_sec_15_34 = round (migStock * sec_15_34),
            mig_ter_15_34 = round (migStock * ter_15_34),
            mig_pri_35_64 = round (migStock * pri_35_64), 
            mig_sec_35_64 = round (migStock * sec_35_64), 
            mig_ter_35_64 = round (migStock * ter_35_64),
            mig_others = round (migStock * others))
  
  migStocks_2[is.na(migStocks_2)] <- 0
  
  migStock_3 = migStocks_2 %>%
    ungroup() %>%
    select (-c(2, 6:11)) %>%
    group_by (year, dest) %>%
    summarise (mig_pri_15_34 = sum (mig_pri_15_34),
               mig_sec_15_34 = sum (mig_sec_15_34),
               mig_ter_15_34 = sum (mig_ter_15_34),
               mig_pri_35_64 = sum (mig_pri_35_64), 
               mig_sec_35_64 = sum (mig_sec_35_64), 
               mig_ter_35_64 = sum (mig_ter_35_64),
               mig_others = sum (mig_others))  %>%
    ungroup () %>%
    mutate (migStock_total = mig_pri_15_34 + mig_sec_15_34 + mig_ter_15_34 + mig_pri_35_64 + mig_sec_35_64 + mig_ter_35_64 + mig_others)
  
}

write.csv (migStock_3, file.path(output_path,"v3_migStock_aft_multiplication.csv"), row.names = FALSE)

# exporting files for later use
# distributions derived from DIOC 2000 - 2010 with 2015-2020 values filled in using normalised growth rate interpolations
write.csv (distributions, file.path(output_path,"migStock_edu_age_shares.csv"), row.names = FALSE)

v3_migStock_aft_multiplication = read.csv (file.path(output_path,"v3_migStock_aft_multiplication.csv"))







