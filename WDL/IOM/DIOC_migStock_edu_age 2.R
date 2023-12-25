
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
  DIOC_2000_total = DIOC_2000 %>% 
    group_by (year, orig_dest, orig, region_birth, dest) %>%
    summarise (migStock_total = sum (migStock))
  
  DIOC_2000 = DIOC_2000 %>% 
    left_join (DIOC_2000_total, by = c("year", "orig_dest", "orig", "region_birth", "dest")) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age, migStock_total) %>%
    summarise (migStock_percent = migStock / migStock_total) %>%
    ungroup() %>%
    select (-c("migStock_total")) %>%
    rename (migStock = migStock_percent)
  

  # converting from long to wide format
  
  DIOC_2000_edu_age_wide = reshape2::dcast(DIOC_2000, 
                                      year + orig_dest + orig + region_birth + dest + age ~ edu_lvl,
                                      value.var = "migStock")
  
  DIOC_2000_edu_age_wider = reshape (DIOC_2000_edu_age_wide, 
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
  
  .GlobalEnv$DIOC_2000 = DIOC_2000_edu_age_wider
  
}

# execute

DIOC_2000_clean()

# 2005_06 release

DIOC_2005_clean = function () {
  
  DIOC_2005 = DIOC_2005_raw %>%
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
    # if needed to find proportions including unknowns, undo the next two lines.
    filter (!(edu_lvl == 99)) %>%
    filter (!(age == 99)) %>%
    select (-c("fborn")) %>%
    mutate (year = 2005) %>%
    relocate (year, .before = orig_dest) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock)) %>%
    mutate (edu_lvl = ifelse (edu_lvl == 1, "primary",
                              ifelse (edu_lvl == 2, "secondary", "tertiary"))) %>%
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
                                                                                                  ifelse (age == 11, "65_69", "70+")
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
                          ifelse (!(age %in% c("65_69", "70+")), "35_64", age))) %>%
    # if needed to work with ages outside the Crespo 2013 parameters, undo the next line
    filter (!(age %in% c("65_69", "70+"))) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock))
  
  # converting from long to wide format
  
  DIOC_2005_edu_age_wide = reshape2::dcast(DIOC_2005, 
                                           year + orig_dest + orig + region_birth + dest + age ~ edu_lvl,
                                           value.var = "migStock")
  
  DIOC_2005_edu_age_wider = reshape (DIOC_2005_edu_age_wide, 
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
  
  .GlobalEnv$DIOC_2005 = DIOC_2005_edu_age_wider
  
}

# execute

DIOC_2005_clean()

# 2010_11 release

DIOC_2010_clean = function () {
  
  DIOC_2010 = DIOC_2010_raw %>%
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
    # if needed to find proportions including unknowns, undo the next two lines.
    filter (!(edu_lvl == 99)) %>%
    filter (!(age == 99)) %>%
    select (-c("fborn")) %>%
    mutate (year = 2010) %>%
    relocate (year, .before = orig_dest) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock)) %>%
    mutate (edu_lvl = ifelse (edu_lvl == 1, "primary",
                              ifelse (edu_lvl == 2, "secondary", "tertiary"))) %>%
    mutate (age = ifelse (age == 1, "0_14",
                          ifelse (age == 2, "15_24",
                                  ifelse (age == 3, "25_34",
                                          ifelse (age == 4, "35_44",
                                                  ifelse (age == 5, "45_54",
                                                          ifelse (age == 6, "55_64", "65+")
                                                          )
                                                  )
                                          )
                                  )
                          )
            ) %>%
    mutate (age = ifelse (age %in% c("15_24", "25_34"), "15_34", 
                          ifelse (!(age %in% c("0_14", "65+")), "35_64", age))) %>%
    # if needed to work with ages outside the Crespo 2013 parameters, undo the next line
    filter (!(age %in% c("0_14", "65+"))) %>%
    group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock))
  
  # converting from long to wide format
  
  DIOC_2010_edu_age_wide = reshape2::dcast(DIOC_2010, 
                                           year + orig_dest + orig + region_birth + dest + age ~ edu_lvl,
                                           value.var = "migStock")
  
  DIOC_2010_edu_age_wider = reshape (DIOC_2010_edu_age_wide, 
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
  
  .GlobalEnv$DIOC_2010 = DIOC_2010_edu_age_wider
  
}

# execute 
DIOC_2010_clean()


# 2015_16 release

DIOC_2015_clean = function () {
  
  DIOC_2015 = DIOC_2015_raw_1 %>%
    mutate (year = 2015) %>%
    select (c(year, regionb, country, national, sex, age_lfs, edu_lfs, fborn, number)) %>%
    filter (fborn == 1) %>%
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
    # if needed to find proportions including unknowns, undo the next two lines.
    filter (!(edu_lvl == 99)) %>%
    filter (!(age == 99)) %>%
    select (-c("fborn")) %>%
    group_by (year, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock)) %>%
    mutate (edu_lvl = ifelse (edu_lvl == 1, "primary", 
                              ifelse (edu_lvl == 2, "secondary", "tertiary"))) %>%
    mutate (age = ifelse (age == 0, "0_14",
                          ifelse (age == 1, "15_34",
                                  ifelse (age == 2, "35_64",
                                          ifelse (age == 3, "65+", age))))) %>%
    # if needed to work with ages outside the Crespo 2013 parameters, undo the next line
    filter (age %in% c("15_34", "35_64")) %>%
    group_by (year, region_birth, dest, edu_lvl, age) %>%
    summarise (migStock = sum (migStock))
  
  # converting from long to wide format
  
  DIOC_2015_edu_age_wide = reshape2::dcast(DIOC_2015, 
                                           year + region_birth + dest + age ~ edu_lvl,
                                           value.var = "migStock")
  
  DIOC_2015_edu_age_wider = reshape (DIOC_2015_edu_age_wide, 
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
  
  .GlobalEnv$DIOC_2015 = DIOC_2015_edu_age_wider
  
}

# execute 
DIOC_2015_clean()

# binding into one dataframe
DIOC_2000_10_migStock_edu_age = rbind (DIOC_2000, DIOC_2005, DIOC_2010) %>%
  arrange (orig_dest)

# df for 2015 (for later joining)
DIOC_2015_forjoin = DIOC_2000_10_migStock_edu_age %>%
  select (orig_dest, orig, region_birth, dest) %>%
  distinct() %>%
  mutate (year = 2015) %>%
  relocate (year, .before = orig_dest) %>%
  left_join (DIOC_2015, by = c("year", "region_birth", "dest"))

# converting totals to averages for regions

DIOC_2015_forjoin_copy = DIOC_2015_forjoin

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

DIOC_2015_forjoin_copy [,6:11] = round (DIOC_2015_forjoin_copy [,6:11])

# IGNORE UNTIL LINE 450 this is having implementation issues for some reason, will fix soon.

DIOC_2015_forjoin_copy [,6:11] = if (DIOC_2015_forjoin_copy$region_birth == "SCAC") {
  DIOC_2015_forjoin_copy [,6:11] / 33
} else if (DIOC_2015_forjoin_copy$region_birth == "ASIA") {
  DIOC_2015_forjoin_copy [,6:11] / 48
} else if (DIOC_2015_forjoin_copy$region_birth == "AFRI") {
  DIOC_2015_forjoin_copy [,6:11] / 54
} else if (DIOC_2015_forjoin_copy$region_birth == "EURO") {
  DIOC_2015_forjoin_copy [,6:11] / 44
} else if (DIOC_2015_forjoin_copy$region_birth == "OCEA") {
  DIOC_2015_forjoin_copy [,6:11] / 14
} else if (DIOC_2015_forjoin_copy$region_birth == "NOAM") {
  DIOC_2015_forjoin_copy [,6:11] / 23
}

DIOC_2015_forjoin_copy [,6:11] = round (DIOC_2015_forjoin_copy [,6:11])

# joining 2015 w/ main df
DIOC_2000_15_migStock_edu_age = rbind (DIOC_2000_10_migStock_edu_age, DIOC_2015_forjoin_copy) %>%
  arrange (orig_dest)

# making sure there are full time series rows for each country pair
df = DIOC_2000_15_migStock_edu_age %>%
  select (c("orig_dest", "orig", "region_birth", "dest")) %>%
  distinct ()

## note (!!!) - do this at the start

df = df[rep(seq_len(nrow(df)), each = 4), ] 

df =  df %>%
  mutate (year = rep(c(2000,2005,2010,2015), times = 6520)) %>%
  relocate (year, .before = orig_dest) %>%
  # joining back with main df 
  left_join (DIOC_2000_15_migStock_edu_age, by = names(.)) %>%
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

write.csv (df, file.path (base_path, "IOM", "2nd_paper", "input_data", "production_function", "DIOC_2000_15_migStock_edu_age.csv"))

write.csv (DIOC_2000, "data.csv")

DIOC_2000_15_migStock_edu_age = read.csv (file.path (base_path, "IOM", "2nd_paper", "input_data", "production_function", "DIOC_2000_15_migStock_edu_age.csv"))
