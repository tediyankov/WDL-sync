
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
  DIOC_2015_raw = read.csv (file.path(input_path, "2015_16 release", "DIOC_2015_16_File_A_1.csv"))
  
  # save raw data files to Global Environment
  .GlobalEnv$DIOC_2000_raw = DIOC_2000_raw
  .GlobalEnv$DIOC_2005_raw = DIOC_2005_raw
  .GlobalEnv$DIOC_2010_raw = DIOC_2010_raw
  .GlobalEnv$DIOC_2015_raw = DIOC_2015_raw
  
}

## execute
DIOC_load()

## chapter 2: cleaning raw data ------------------------------------------------

## 2.1 - DIOC 2000_01 release --------------------------------------------------

## data cleaning function 

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
  
  # df with migStock totals w/out category dis-aggregation
  DIOC_2000_total = DIOC_2000_1 %>% 
    group_by (year, orig_dest, orig, region_birth, dest) %>%
    summarise (migStock_total = sum (migStock))
  
  # converting migStocks into percentages
  DIOC_2000_2 = DIOC_2000_1 %>% 
    left_join (DIOC_2000_total, by = c("year", "orig_dest", "orig", "region_birth", "dest")) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age, migStock_total) %>%
    summarise (migStock_percent = migStock / migStock_total) %>%
    ungroup() %>%
    select (-c("migStock_total")) %>%
    rename (migStock = migStock_percent)
  
  # making sure orig and dest exclude non-country ISO3 codes 
  DIOC_2000_3 = DIOC_2000_2 %>%
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
   
  # saving to Global Environment
  .GlobalEnv$DIOC_2000 = DIOC_2000_5
}

## execute

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
  
  # df with migStock totals w/out category dis-aggregation
  DIOC_2005_total = DIOC_2005_1 %>% 
    group_by (year, orig_dest, orig, region_birth, dest) %>%
    summarise (migStock_total = sum (migStock))
  
  # converting migStocks into percentages
  DIOC_2005_2 = DIOC_2005_1 %>% 
    left_join (DIOC_2005_total, by = c("year", "orig_dest", "orig", "region_birth", "dest")) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age, migStock_total) %>%
    summarise (migStock_percent = migStock / migStock_total) %>%
    ungroup() %>%
    select (-c("migStock_total")) %>%
    rename (migStock = migStock_percent)

  # making sure orig and dest exclude non-country ISO3 codes 
  DIOC_2005_3 = DIOC_2005_2 %>%
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
  
  # df with migStock totals w/out category dis-aggregation
  DIOC_2010_total = DIOC_2010_1 %>% 
    group_by (year, orig_dest, orig, region_birth, dest) %>%
    summarise (migStock_total = sum (migStock))
  
  # converting migStocks into percentages
  DIOC_2010_2 = DIOC_2010_1 %>% 
    left_join (DIOC_2010_total, by = c("year", "orig_dest", "orig", "region_birth", "dest")) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age, migStock_total) %>%
    summarise (migStock_percent = migStock / migStock_total) %>%
    ungroup() %>%
    select (-c("migStock_total")) %>%
    rename (migStock = migStock_percent)
  
  # making sure orig and dest exclude non-country ISO3 codes 
  DIOC_2010_3 = DIOC_2010_2 %>%
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
  
  # saving to Global Environment
  .GlobalEnv$DIOC_2010 = DIOC_2010_5
  
}

## execute
DIOC_2010_clean()

## 2.4 - DIOC 2015_16 release --------------------------------------------------

## data cleaning function

DIOC_2015_clean = function () {
  
  # cleaning long data
  DIOC_2015_1 = DIOC_2015_raw %>%
    mutate (year = 2015) %>%
    select (c(year, regionb, country, national, sex, age_lfs, edu_lfs, fborn, number)) %>%
    rename_with (
      ~ case_when (
        . == "country" ~ "dest",
        . == "edu_lfs" ~ "edu_lvl",
        . == "number" ~ "migStock",
        . == "regionb" ~ "region_birth",
        . == "age_lfs" ~ "age",
        TRUE ~ .
      ) 
    ) %>%
    filter (fborn == 1) %>%
    select (-c("fborn")) %>%
    group_by (year, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock)) %>%
    mutate (edu_lvl = ifelse (edu_lvl == 1, "primary", 
                              ifelse (edu_lvl == 2, "secondary", 
                                      ifelse (edu_lvl == 3, "tertiary","unknown")
                              )
    )
    ) %>%
    mutate (age = ifelse (age == 0, "0_14",
                          ifelse (age == 1, "15_34",
                                  ifelse (age == 2, "35_64",
                                          ifelse (age == 3, "65+", "unknown")
                                  )
                          )
    )
    ) %>%
    group_by (year, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock))
  
  # df with migStock totals w/out category dis-aggregation
  DIOC_2015_total = DIOC_2015_1 %>% 
    group_by (year, region_birth, dest) %>%
    summarise (migStock_total = sum (migStock))
  
  # converting migStocks into percentages
  DIOC_2015_2 = DIOC_2015_1 %>% 
    left_join (DIOC_2015_total, by = c("year", "region_birth", "dest")) %>%
    group_by (year, region_birth, dest, edu_lvl, age, migStock_total) %>%
    summarise (migStock_percent = migStock / migStock_total) %>%
    ungroup() %>%
    select (-c("migStock_total")) %>%
    rename (migStock = migStock_percent)
  
  # converting long data into wide data 
  DIOC_2015_3 = reshape2::dcast(DIOC_2015_2,
                                year + region_birth + dest + age ~ edu_lvl,
                                value.var = "migStock")
  
  DIOC_2015_4 = reshape (DIOC_2015_3, 
                         idvar = c("year", "region_birth", "dest"),
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
  
  # saving to Global Environment
  .GlobalEnv$DIOC_2015 = DIOC_2015_4
  
}

# execute 
DIOC_2015_clean()

## chapter 3: bringing everything together  ------------------------------------

## aligning variables (keeping age groups 15_34, 35_64, unknown)

# function
var_align = function () {
  
  DIOC_2000 = DIOC_2000 %>% select (-c("pri.65+", "sec.65+", "ter.65+", "unknown.65+"))
  DIOC_2005 = DIOC_2005 %>% select (-c("pri.65+", "sec.65+", "ter.65+", "unknown.65+"))
  DIOC_2010 = DIOC_2010 %>% select (-c("pri.0_14", "sec.0_14", "ter.0_14", "unknown.0_14", "pri.65+", "sec.65+", "ter.65+", "unknown.65+"))
  DIOC_2015 = DIOC_2015 %>% select (-c("primary.0_14", "secondary.0_14", "tertiary.0_14", "unknown.0_14", "primary.65+", "secondary.65+", "tertiary.65+", "unknown.65+"))
  
  # saving to Global Environment
  .GlobalEnv$DIOC_2000 = DIOC_2000
  .GlobalEnv$DIOC_2005 = DIOC_2005
  .GlobalEnv$DIOC_2010 = DIOC_2010
  .GlobalEnv$DIOC_2015 = DIOC_2015
  
}

# execute 
var_align()

## binding into one unified data frame + filling gaps

# function

DIOC_bind_2000_15 = function () {
  
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
  
  # df for 2015 (for later joining)
  DIOC_2015_forjoin = DIOC_2000_10 %>%
    select (orig_dest, orig, region_birth, dest) %>%
    distinct() %>%
    mutate (year = 2015) %>%
    relocate (year, .before = orig_dest) %>%
    left_join (DIOC_2015, by = c("year", "region_birth", "dest"))
  
  # converting totals to averages for regions
  
  DIOC_2015_forjoin_copy = DIOC_2015_forjoin %>%
    rename_with (
      ~ case_when (
        . == "primary.unknown" ~ "pri.unknown",
        . == "secondary.unknown" ~ "sec.unknown",
        . == "tertiary.unknown" ~ "ter.unknown",
        TRUE ~ .
      )
    )
  
  DIOC_2015_forjoin_SCAC = DIOC_2015_forjoin_copy %>% 
    filter (region_birth == "SCAC")
  DIOC_2015_forjoin_SCAC [,6:11] = DIOC_2015_forjoin_SCAC [,6:11] / 33
  
  DIOC_2015_forjoin_ASIA = DIOC_2015_forjoin_copy %>% 
    filter (region_birth == "ASIA")
  DIOC_2015_forjoin_ASIA [,6:11] = DIOC_2015_forjoin_ASIA [,6:11] / 48
  
  DIOC_2015_forjoin_AFRI = DIOC_2015_forjoin_copy %>% 
    filter (region_birth == "AFRI")
  DIOC_2015_forjoin_AFRI [,6:11] = DIOC_2015_forjoin_AFRI [,6:11] / 54
  
  DIOC_2015_forjoin_EURO = DIOC_2015_forjoin_copy %>% 
    filter (region_birth == "EURO")
  DIOC_2015_forjoin_EURO [,6:11] = DIOC_2015_forjoin_EURO [,6:11] / 44
  
  DIOC_2015_forjoin_OCEA = DIOC_2015_forjoin_copy %>% 
    filter (region_birth == "OCEA")
  DIOC_2015_forjoin_OCEA [,6:11] = DIOC_2015_forjoin_OCEA [,6:11] / 14
  
  DIOC_2015_forjoin_NOAM = DIOC_2015_forjoin_copy %>% 
    filter (region_birth == "NOAM")
  DIOC_2015_forjoin_NOAM [,6:11] = DIOC_2015_forjoin_NOAM [,6:11] / 23
  
  DIOC_2015_forjoin_copy = rbind (DIOC_2015_forjoin_SCAC,
                                  DIOC_2015_forjoin_ASIA,
                                  DIOC_2015_forjoin_AFRI,
                                  DIOC_2015_forjoin_EURO,
                                  DIOC_2015_forjoin_OCEA, 
                                  DIOC_2015_forjoin_NOAM)
  
  # joining 2015 w/ main df
  DIOC_2000_15 = rbind (DIOC_2000_10, DIOC_2015_forjoin_copy) %>%
    arrange (orig_dest)
  
  # making sure there are full time series rows for each country pair
  df_1 = DIOC_2000_15 %>%
    select (c("orig_dest", "orig", "region_birth", "dest")) %>%
    distinct ()
  
  df_2 = df_1[rep(seq_len(nrow(df_1)), each = 5), ]
  
  df_3 = df_2 %>%
    mutate (year = rep(c(2000,2005,2010,2015,2020), times = 5546)) %>%
    relocate (year, .before = orig_dest) %>%
    # joining back with main df 
    left_join (DIOC_2000_15, by = names(.))
  
  df_3 [is.na(df_3)] <- 0
  
  .GlobalEnv$DIOC_2000_15 = df_3
    
}

# execute
DIOC_bind_2000_15()

## chapter 4: interpolating w/ growth rates  -----------------------------------

# interpolating 2015 and 2020

interpol_2015_20 = function () {
  
  # subset 
  DIOC_2000_20_2 = DIOC_2000_15 %>%
    select (-c("unknown.15_34","unknown.35_64","pri.unknown","sec.unknown","ter.unknown","unknown.unknown"))
  
  # blanking 2020 rows 
  DIOC_2000_20_2 [DIOC_2000_20_2$year %in% c(2015,2020), 6:11] = NA
  
  values = unique (DIOC_2000_20_2$orig_dest)
  
  datalist = vector("list", length = length(unique(values)))
  
  for (i in values) {
    
    data1 = DIOC_2000_20_2 [DIOC_2000_20_2$orig_dest == i,]
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
      mutate (ter_35_64_growth = Lag (ter_35_64_growth, 1))
    
    pri_15_34_growth_avg = mean (data1$pri_15_34_growth, na.rm = T)
    sec_15_34_growth_avg = mean (data1$sec_15_34_growth, na.rm = T)
    ter_15_34_growth_avg = mean (data1$ter_15_34_growth, na.rm = T)
    pri_35_64_growth_avg = mean (data1$pri_35_64_growth, na.rm = T)
    sec_35_64_growth_avg = mean (data1$sec_35_64_growth, na.rm = T)
    ter_35_64_growth_avg = mean (data1$ter_35_64_growth, na.rm = T)
    
    data1$pri_15_34 [data1$year == 2015] <- data1$pri_15_34 [data1$year == 2010] * (1 + pri_15_34_growth_avg)
    data1$sec_15_34 [data1$year == 2015] <- data1$sec_15_34 [data1$year == 2010] * (1 + sec_15_34_growth_avg)
    data1$ter_15_34 [data1$year == 2015] <- data1$ter_15_34 [data1$year == 2010] * (1 + ter_15_34_growth_avg)
    data1$pri_35_64 [data1$year == 2015] <- data1$pri_35_64 [data1$year == 2010] * (1 + pri_35_64_growth_avg)
    data1$sec_35_64 [data1$year == 2015] <- data1$sec_35_64 [data1$year == 2010] * (1 + sec_35_64_growth_avg)
    data1$ter_35_64 [data1$year == 2015] <- data1$ter_35_64 [data1$year == 2010] * (1 + ter_35_64_growth_avg)
    
    data1$pri_15_34 [data1$year == 2020] <- data1$pri_15_34 [data1$year == 2010] * (1 + pri_15_34_growth_avg)^2
    data1$sec_15_34 [data1$year == 2020] <- data1$sec_15_34 [data1$year == 2010] * (1 + sec_15_34_growth_avg)^2
    data1$ter_15_34 [data1$year == 2020] <- data1$ter_15_34 [data1$year == 2010] * (1 + ter_15_34_growth_avg)^2
    data1$pri_35_64 [data1$year == 2020] <- data1$pri_35_64 [data1$year == 2010] * (1 + pri_35_64_growth_avg)^2
    data1$sec_35_64 [data1$year == 2020] <- data1$sec_35_64 [data1$year == 2010] * (1 + sec_35_64_growth_avg)^2
    data1$ter_35_64 [data1$year == 2020] <- data1$ter_35_64 [data1$year == 2010] * (1 + ter_35_64_growth_avg)^2
    
    data1 = data1 %>% select (1:11)
    
    datalist[[i]] <- data1
    
  }
  
  big_data = do.call(rbind, datalist)
  
  big_data [is.na(big_data)] <- 0
  
  .GlobalEnv$DIOC_2000_20_2 = big_data
  
}

# execute
interpol_2015_20()

## chapter 5: merging with OECD flow data  -------------------------------------

apply_flow = function () {
  
  # load OECD flows data 
  oecd = read.csv (file.path (base_path, 'IOM', "data_input", "OECD_usethis.csv")) %>%
    filter (VAR == "B11") %>%
    select (-c("Country.of.birth.nationality", "VAR", "Variable", "GEN", "Gender", "Country", "YEA", "Flag.Codes", "Flags")) %>% 
    rename_with(
      ~ case_when(
        . == "CO2" ~ "orig",
        . == "COU" ~ "dest",
        . == "Year" ~ "year",
        . == "Value" ~ "flow",
        TRUE ~ .
      )
    ) %>% 
    unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    mutate (year = as.numeric(year))
  
  input5 = oecd %>% # putting the flows into 5-year increments
    add_column(flows.sum = NA)
  
  # keep only country pairs with full range of years (2000-2019) - otherwise the sums would not be accurate.
  input5.n = input5 %>% count (orig.dest)
  input5.withnum = merge (input5, input5.n, by = "orig.dest")
  input5 = input5.withnum [input5.withnum$n == 20,]
  rm (input5.n, input5.withnum)
  input5 = input5 %>% select (-c("n"))
  
  # aggregation
  input5 = input5 %>% 
    mutate (year = 5*floor(input5$year/5)) %>%
    group_by (orig.dest, year) %>%
    summarise (flows.sum = sum(flow)) %>%
    left_join (oecd, by = c("orig.dest", "year")) %>% 
    select (-c("flow")) %>%
    relocate (orig.dest, orig, dest, year, flows.sum)
  
  flows = input5 %>% 
    rename (flow = flows.sum, 
            orig_dest = orig.dest)
  
  # join flows data with DIOC migStock data
  DIOC_2000_20_withflows = DIOC_2000_20_2 %>% 
    left_join (flows, by = c("year", "orig_dest", "orig", "dest")) %>% 
    mutate (flows_pri_15_34 = flow * pri_15_34,
            flows_sec_15_34 = flow * sec_15_34,
            flows_ter_15_34 = flow * ter_15_34,
            flows_pri_35_64 = flow * pri_35_64,
            flows_sec_35_64 = flow * sec_35_64,
            flows_ter_35_64 = flow * ter_35_64)
  
  DIOC_2000_20_withflows [is.na(DIOC_2000_20_withflows)] <- 0
  
  DIOC_2000_20_withflows [,12:18] <- round (DIOC_2000_20_withflows [,12:18])
  
  .GlobalEnv$DIOC_2000_20_withflows = DIOC_2000_20_withflows
  
}

# execute 
apply_flow()

apply_flow_v2 = function () {
  
  # REMEMBER: add file path once merge_future_2040_v4 is exported.
  flows_1 = flows %>% 
    filter (year %in% 2000:2019) %>%
    select (-2) %>%
    add_column(flows.sum = NA) 
  
  flows_2 = flows_1 %>% 
    mutate (year = 5*floor(flows_1$year/5)) %>%
    group_by (orig.dest, orig, dest, year) %>%
    summarise (flows.sum = sum(flow, na.rm = T), .groups = "keep") %>%
    rename (flow = flows.sum) %>%
    rename (orig_dest = orig.dest)
  
  DIOC_2000_20_withflows_2 = DIOC_2000_20_2 %>% 
    left_join (flows_2, by = c("year", "orig_dest", "orig", "dest")) %>% 
    mutate (flows_pri_15_34 = flow * pri_15_34,
            flows_sec_15_34 = flow * sec_15_34,
            flows_ter_15_34 = flow * ter_15_34,
            flows_pri_35_64 = flow * pri_35_64,
            flows_sec_35_64 = flow * sec_35_64,
            flows_ter_35_64 = flow * ter_35_64)
  
  DIOC_2000_20_withflows_2 [is.na(DIOC_2000_20_withflows_2)] <- 0
  
  DIOC_2000_20_withflows_2 [,12:18] <- round (DIOC_2000_20_withflows_2 [,12:18])
  
  .GlobalEnv$DIOC_2000_20_withflows_2 = DIOC_2000_20_withflows_2
  
}

## chapter 6: conclusion -------------------------------------------------------

write.csv(DIOC_2000_20_withflows, file.path(output_path,"DIOC_2000_20_withflows.csv"), row.names = FALSE)

write.csv(DIOC_2000_20_withflows_2, file.path(output_path,"DIOC_2000_20_withflows_2.csv"), row.names = FALSE)










