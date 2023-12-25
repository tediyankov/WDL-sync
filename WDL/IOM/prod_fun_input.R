
## preliminaries ---------------------------------------------------------------

## packages
pacman::p_load (tidyverse,countrycode,stats,Hmisc)

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

# setup paths
base_path <- worlddataverse::get_wdl_path()
if(is.na(base_path)) {
  base_path = get_drive_path()
}
 
data_path <- file.path (base_path, "IOM", "2nd_paper", "input_data", "production_function")


## employment rates by place of birth ------------------------------------------

## preprocess data

empl_POB_edu = read.csv(file.path(data_path, "RAW_employment_rates_POB_education.csv")) %>%
  select (c("Year","Country","Place.of.birth","Educational.attainment","Value")) %>%
  mutate (country = countrycode(sourcevar = Country,
                                origin = "country.name",
                                destination = "iso3c",
                                warn = TRUE, 
                                nomatch = NA)) %>%
  filter (!(is.na(country))) %>%
  rename_with(
    ~ case_when(
      . == "Year" ~ "year",
      . == "Value" ~ "employed",
      . == "Place.of.birth" ~ "p.o.b.",
      . == "Educational.attainment" ~ "edu",
      TRUE ~ .
    )
  ) %>%
  select (c("year","country","p.o.b.","edu","employed")) %>%
  drop_na()

write.csv (empl_POB_edu, file.path(data_path, "CLEAN_employment_rates_POB_education.csv"))

## immigrants by field of study ------------------------------------------------

migStock_edu = read.csv(file.path(data_path, "RAW_migStock_edu.csv")) %>%
  select (c("COUB","Country.of.birth","COU","Place.of.birth","Education.level","Field.of.study","Labour.force.status","Value")) %>%
  rename_with (
    ~ case_when (
      . == "COUB" ~ "orig",
      . == "Country.of.birth" ~ "orig_name",
      . == "Place.of.birth" ~ "place_of_birth",
      . == "COU" ~ "dest",
      . == "Value" ~ "migStock",
      . == "Labour.force.status" ~ "labour_force_status",
      . == "Field.of.study" ~ "field_of_study",
      . == "Education.level" ~ "edu_lvl",
      TRUE ~ .
    )
  ) %>%
  mutate (dest_name = countrycode(sourcevar = dest,
                                  origin = "iso3c",
                                  destination = "country.name",
                                  warn = TRUE, 
                                  nomatch = NA)) %>%
  relocate (dest_name, .before = place_of_birth) %>%
  mutate (year = 2000) %>%
  relocate (year, .before = orig) %>%
  mutate (age = "All ages") %>%
  relocate (age, .before = migStock) %>%
  mutate (sex = "Men and women") %>%
  relocate (sex, .before = migStock) %>%
  filter (!(is.na(dest_name))) %>%
  filter (!(place_of_birth == "Unknown place of birth")) %>%
  filter (!(edu_lvl == "ISCED 5/6 - Unallocated")) %>%
  drop_na()

DIOC_sex_age = read.csv (file.path(data_path, "DIOC_SEX_AGE_22082022194839567.csv")) %>%
  select (c("COUB","Country.of.birth","COU","Place.of.birth","Education.level","Age","Sex","Value")) %>%
  mutate (dest = countrycode(sourcevar = COU,
                             origin = "iso3c",
                             destination = "iso3c",
                             warn = TRUE, 
                             nomatch = NA)) %>%
  filter(!(is.na(dest))) %>%
  rename_with (
    ~ case_when (
      . == "COUB" ~ "orig",
      . == "Country.of.birth" ~ "orig_name",
      . == "Place.of.birth" ~ "place_of_birth",
      . == "Value" ~ "migStock",
      . == "Education.level" ~ "edu_lvl",
      TRUE ~ .
    )
  ) %>%
  mutate (dest_name = countrycode(sourcevar = dest,
                                  origin = "iso3c",
                                  destination = "country.name",
                                  warn = TRUE, 
                                  nomatch = NA)) %>%
  filter (Sex == "Men and women") %>% 
  filter (place_of_birth == "All places of birth") %>%
  filter (!(edu_lvl == "Unknown education")) %>%
  filter (!(edu_lvl == "All levels of education")) %>%
  filter (!(Age == "Unknown age")) %>%
  filter (!(Age == "All ages")) %>%
  filter (!(Age == "65+ years")) %>%
  mutate (year = 2000) %>% 
  select (c("year", "orig", "orig_name", "dest", "dest_name", "Age", "edu_lvl", "migStock")) %>%
  mutate (edu_lvl = ifelse (edu_lvl == "ISCED 0/1/2", "Primary", 
                            ifelse (edu_lvl == "ISCED 3/4", "Secondary", "Tertiary"))) %>%
  drop_na()
  
# reshaping the data from long to wide

require ("reshape2")

DIOC_sex_age_wide = reshape2::dcast(DIOC_sex_age, 
                                    orig + orig_name + dest + dest_name + Age ~ edu_lvl,
                                    value.var = "migStock")

DIOC_sex_age_wider = reshape (DIOC_sex_age_wide, 
                              idvar = c("orig", "orig_name", "dest", "dest_name"),
                              timevar = "Age",
                              direction = "wide")

DIOC_sex_age_wider = DIOC_sex_age_wider %>%
  rename_with (
    ~ case_when (
      . == "Primary.15-24 years" ~ "pri_15_24",
      . == "Primary.25-64 years" ~ "pri_25_64",
      . == "Secondary.15-24 years" ~ "sec_15_24",
      . == "Secondary.25-64 years" ~ "sec_25_64",
      . == "Tertiary.15-24 years" ~ "ter_15_24",
      . == "Tertiary.25-64 years" ~ "ter_25_64",
      TRUE ~ .
    )
  ) %>%
  mutate (pri_15_64 = pri_15_24 + pri_25_64) %>%
  mutate (sec_15_64 = sec_15_24 + sec_25_64) %>%
  mutate (ter_15_64 = ter_15_24 + ter_25_64) %>%
  mutate (year = 2000) %>%
  select (c(year, orig, orig_name, dest, dest_name, pri_15_24, pri_25_64, pri_15_64, 
            sec_15_24, sec_25_64, sec_15_64, ter_15_24, ter_25_64, ter_15_64))

write.csv (DIOC_sex_age_wider, file.path(data_path, "CLEAN_migStock_age_edu"))


## attempting to go more granular

DIOC_2010_age_edu = read.csv (file.path(data_path, "DIOC_2010_11_File_A_quater_REV.csv"))

DIOC_2010_age_edu_nas = DIOC_2010_age_edu %>% filter (number == 0)

DIOC_2010_age_edu_unique = DIOC_2010_age_edu %>% 
  distinct() %>%
  filter (!(edu_detailed == 99)) %>%
  filter (!(age == 99))





sample2 = DIOC_2000 %>% 
  filter (orig_dest %in% c("AFG_AUS", "AFG_TUR", "ABW_CAN"))
 
sample2_total = sample2 %>%
  group_by (year, orig_dest, orig, region_birth, dest) %>%
  summarise (migStock_total = sum (migStock)) 

sample2 = sample2 %>% 
  left_join (sample2_total, by = c("year", "orig_dest", "orig", "region_birth", "dest")) %>%
  group_by (year, orig_dest, orig, region_birth, dest, edu_lvl, age, migStock_total) %>%
  summarise (migStock_percent = migStock / migStock_total)




