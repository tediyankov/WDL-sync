
## chapter 1: preliminaries ----------------------------------------------------

# clean environment
rm (list = ls())

# packages
pacman::p_load (worlddataverse, tidyverse, countrycode)

# loading paths
base_path <- worlddataverse::get_wdl_path()
if (is.na (base_path)) {base_path = file.path("/Volumes/",
                                              "GoogleDrive-115239951252043738907",
                                              "Shared drives",
                                              "DATA_WDL")}

input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data", "production_function", "OECD_DIOC_rawdata_IOM")
output_path <- file.path (base_path, "IOM", "2nd_paper", "output")

# load the raw data
DIOC_2000_raw = read.csv (file.path(input_path, "2000_01 release", "FILE_A_1_T.csv"))

## chapter 2: cleaning ---------------------------------------------------------

# creating age_cen + age_lfs = age variable in raw data
DIOC_2000_raw$age_cen = paste0 (DIOC_2000_raw$age_cen, "_cen")
DIOC_2000_raw$age_lfs = paste0 (DIOC_2000_raw$age_lfs, "_lfs")

DIOC_2000_raw = transform (DIOC_2000_raw, age = ifelse (!(age_cen == "NA_cen"), age_cen, age_lfs))

# renaming and filtering

DIOC_2000_1 = DIOC_2000_raw %>%
  dplyr::select(-c("edu_cen", "age_cen", "age_lfs", "reg_oecd")) %>%
  rename_with (
    ~ case_when (
      . == "country" ~ "dest",
      . == "coub" ~ "orig",
      . == "edu_lfs" ~ "edu_lvl",
      . == "number" ~ "migStock",
      . == "reg_regions" ~ "region_birth",
      TRUE ~ .
    )
  ) %>%
  relocate (orig, .before = dest) %>%
  unite ("orig_dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  filter (fborn == 1) %>%
  dplyr::select (-c("fborn")) %>%
  mutate (year = 2000) %>%
  group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age) %>%
  summarise (migStock = sum (migStock)) %>%
  mutate (edu_lvl = ifelse (edu_lvl == 1, "pri",
                            ifelse (edu_lvl == 2, "sec",
                                    ifelse (edu_lvl == 3, "ter", "unknown")
                            )
  )
  ) %>%
  mutate (age = ifelse (age %in% c("1_lfs", "1_cen", "2_cen", "3_cen", "4_cen"), "15_34",
                        ifelse (age %in% c("2_lfs", "5_cen", "6_cen", "7_cen", "8_cen", "9_cen", "10_cen"), "35_64",
                                ifelse (age %in% c("3_lfs", "11_cen", "12_cen"), "65_", "unknown")))) %>%
  group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age) %>%
  summarise (migStock = sum (migStock))

# making sure orig and dest exclude non-country ISO3 codes

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

# converting migStocks into percentages

DIOC_2000_total = DIOC_2000_2 %>%
  group_by (year, orig_dest, orig, region_birth, dest) %>%
  summarise (migStock_total = sum (migStock))

DIOC_2000_3 = DIOC_2000_2 %>%
  left_join (DIOC_2000_total, by = c("year", "orig_dest", "orig", "region_birth", "dest")) %>%
  group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age, migStock_total) %>%
  summarise (migStock_percent = migStock / migStock_total) %>%
  ungroup() %>%
  dplyr::select (-c("migStock_total")) %>%
  rename (migStock_percent = migStock)

# converting long data into wide data
DIOC_2000_4 = reshape2::dcast(DIOC_2000_3,
                              year + orig_dest + orig + region_birth + dest + age ~ edu_lvl,
                              value.var = "migStock")

DIOC_2000_5 = reshape (DIOC_2000_4,
                       idvar = c("year", "orig_dest", "orig", "region_birth", "dest"),
                       timevar = "age",
                       direction = "wide",
                       sep = "_")

DIOC_2000_5$total_shares <- rowSums (DIOC_2000_5 [,6:21], na.rm = T)
DIOC_2000_5$others = rowSums (DIOC_2000_5[,c(9,13:21)], na.rm = T)

DIOC_2000_6 = DIOC_2000_5 %>% dplyr::select (-c(9,13:21)) %>% relocate (others, .before = total_shares)

# cleaning environment
DIOC_2000 = DIOC_2000_6
rm (DIOC_2000_1, DIOC_2000_2, DIOC_2000_3, DIOC_2000_4, DIOC_2000_5, DIOC_2000_6, DIOC_2000_total)
