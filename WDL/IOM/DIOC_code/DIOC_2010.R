
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
DIOC_2010_raw = read.csv (file.path(input_path, "2010_11 release", "DIOC_2010_11_File_A_quater_REV.csv"))

## chapter 2: cleaning ---------------------------------------------------------

# cleaning long data
DIOC_2010_1 = DIOC_2010_raw %>%
  dplyr::select(-c("edu_detailed", "birth", "source", "oecd", "oecdb")) %>%
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
  dplyr::select (-c("fborn")) %>%
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
                        ifelse (age %in% c(2,3), "15_34",
                                ifelse (age %in% c(4,5,6), "35_64",
                                        ifelse (age == 7, "65_", "unknown"))))) %>%
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
  dplyr::select (-c("migStock_total")) %>%
  rename (migStock_percent = migStock)

# converting long data into wide data
DIOC_2010_4 = reshape2::dcast(DIOC_2010_3,
                              year + orig_dest + orig + region_birth + dest + age ~ edu_lvl,
                              value.var = "migStock")

DIOC_2010_5 = reshape (DIOC_2010_4,
                       idvar = c("year", "orig_dest", "orig", "region_birth", "dest"),
                       timevar = "age",
                       direction = "wide",
                       sep = "_")

DIOC_2010_5$total_shares <- rowSums (DIOC_2010_5 [,6:25], na.rm = T)
DIOC_2010_5$others = rowSums (DIOC_2010_5[,c(6:9,13,17:25)], na.rm = T)

DIOC_2010_6 = DIOC_2010_5 %>% dplyr::select (-c(6:9,13,17:25)) %>% relocate (others, .before = total_shares)

# cleaning environment
DIOC_2010 = DIOC_2010_6
rm (DIOC_2010_1, DIOC_2010_2, DIOC_2010_3, DIOC_2010_4, DIOC_2010_5, DIOC_2010_6, DIOC_2010_total)
