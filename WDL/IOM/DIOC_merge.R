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

## chapter 2: binding ---------------------------------------------------------

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
  dplyr::select (c("orig_dest", "orig", "region_birth", "dest")) %>%
  distinct ()

df_2 = df_1[rep(seq_len(nrow(df_1)), each = 7), ]

df_3 = df_2 %>%
  mutate (year = rep(c(2000,2005,2010,2015,2020,2025,2030), times = 5546)) %>%
  relocate (year, .before = orig_dest) %>%
  # joining back with main df 
  left_join (DIOC_2000_10, by = names(.))

DIOC_2000_10 = df_3
