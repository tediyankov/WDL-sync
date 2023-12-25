
## preliminaries ---------------------------------------------------------------

## loading data function 

DIOC_load = function () {
  
  # packages
  pacman::p_load (worlddataverse, tidyverse)
  require ("worlddataverse", "tidyverse")
  
  # base path to Google Drive
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
    base_path =get_drive_path()
  }
  
  # input and output paths
  input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data", "production_function", "OECD_DIOC_rawdata_IOM")
  output_path <- file.path (base_path, "IOM", "2nd_paper", "output")
  
  .GlobalEnv$base_path = base_path
  .GlobalEnv$input_path = input_path
  .GlobalEnv$output_path = output_path
  
  # load raw data
  DIOC_2000_raw = read.csv (file.path(input_path, "2000_01 release", "FILE_A_2_T.csv"))
  DIOC_2005_raw = read.csv (file.path(input_path, "2005_06 release", "T1.2_2005.CSV"))
  DIOC_2010_raw = read.csv (file.path(input_path, "2010_11 release", "DIOC_2010_11_File_A_quater_REV.csv"))
  DIOC_2015_raw = read.csv (file.path(input_path, "2015_16 release", "DIOC_2015_16_File_A_1.csv"))
  
  .GlobalEnv$DIOC_2000_raw = DIOC_2000_raw
  .GlobalEnv$DIOC_2005_raw = DIOC_2005_raw
  .GlobalEnv$DIOC_2010_raw = DIOC_2010_raw
  .GlobalEnv$DIOC_2015_raw_1 = DIOC_2015_raw
  
}

# loading data
DIOC_load()


## data cleaning functions

# 2000_01 release

DIOC_2000_clean = function () {
  
  # cleaning long data
  DIOC_2000 = DIOC_2000_raw %>%
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
    filter (!(is.na(age))) %>%
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
  
  # converting migStocks into percentages
  DIOC_2000_totals = DIOC_2000 %>% 
    unite ("edu_age", edu_lvl:age, sep = "_", remove = F, na.rm = F)