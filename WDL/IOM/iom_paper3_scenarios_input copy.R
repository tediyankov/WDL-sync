
## IOM PAPER 2/3: CREATING SCENARIOS ===========================================

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, data.table)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
iom_path <- file.path (base_path, "IOM", "2nd_paper", "output")
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')

## loading in migrant flow data by education and age disaggregates
flows_edu_age_sums <- read.csv (file.path (iom_path, "flows_edu_age_sums.csv"))

## removing the X-var 
flows_edu_age_sums = flows_edu_age_sums %>% dplyr::select (-X)

## creating df with -20% -------------------------------------------------------

## creating copy of baseline 
flows_edu_age_sums_minus20 = flows_edu_age_sums

## decreasing all values by 20%
for (i in 3:10) {flows_edu_age_sums_minus20[,i] <- round (flows_edu_age_sums_minus20[,i] * 0.80, 0)}

## removing the X-var 
flows_edu_age_sums_minus20 = flows_edu_age_sums_minus20 %>% dplyr::select (-X)

## creating df with +20% -------------------------------------------------------

## creating copy of baseline 
flows_edu_age_sums_plus20 = flows_edu_age_sums

## decreasing all values by 20%
for (i in 3:10) {flows_edu_age_sums_plus20[,i] <- round (flows_edu_age_sums_plus20[,i] * 1.2, 0)}

## removing the X-var 
#flows_edu_age_sums_plus20 = flows_edu_age_sums_plus20 %>% dplyr::select (-X)

## exporting -------------------------------------------------------------------
write.csv (flows_edu_age_sums, "flows_edu_age_pred.csv", row.names = F)
write.csv (flows_edu_age_sums_minus20, "flows_edu_age_minus20pct.csv", row.names = F)
write.csv (flows_edu_age_sums_plus20, "flows_edu_age_plus20pct.csv", row.names = F)

## disaggregated home populations ----------------------------------------------

## loading in WCDE package
install.packages ("wcde")
library (wcde)

## finding relevant package code
find_indicator (x = "age")

## obtaining data 
homepop <- get_wcde (indicator = "prop",
                     country_name = countrycode (unique (flows_edu_age_sums$dest), "iso3c", "country.name"))

## cleaning data 
homepop_clean = homepop %>%
  
  # filtering years
  filter (year %in% unique (flows_edu_age_sums$year)) %>%
  
  # keep both sexes only
  filter (sex == "Both") %>%
  dplyr::select (- c(sex, scenario, country_code)) %>%
  
  # filtering to relevant age groups 
  filter (age %in% c("15--19", "20--24", "25--29", "30--34", "35--39", "40--44", 
                     "45--49", "50--54", "55--59", "60--64", "65--69", "70--74", 
                     "75--79", "80--84", "85--89", "90--94", "95--99", "100+")) %>%
  
  # re-categorising ages
  mutate (age = ifelse (age %in% c("15--19", "20--24", "25--29", "30--34"), "15_34", 
                        ifelse (age %in% c("35--39", "40--44", "45--49", "50--54", "55--59", "60--64"), "35_64", "others"
                        ))) %>%
  
  # reaggregating by taking the mean prop within the new age categories
  dplyr::group_by (name, age, education, year) %>%
  dplyr::summarise (prop = mean (prop, na.rm = T)) %>%
  
  # re-categorising education
  dplyr::mutate (education = ifelse (education == "Primary", "pri", 
                                     ifelse (education %in% c("Upper Secondary", "Lower Secondary"), "sec", 
                                             ifelse (education %in% c("Post Secondary", "Master and higher", "Bachelor"), "ter", "other")))) %>% # !! keep only post sec
  
  # reaggregating by taking the mean prop within the new education categories
  dplyr::group_by (name, age, education, year) %>%
  dplyr::summarise (prop = mean (prop, na.rm = T)) %>% # !! do sum instead of mean
  
  # widening into one country-year per row
  pivot_wider (names_from = c("education", "age"), values_from = "prop") %>%
  
  # elongating to combine all the "other" categories into one shared other category via taking the mean
  pivot_longer (3:14, names_to = "category", values_to = "prop") %>%
  
  # combine all the "other" categories into one shared other category via taking the mean
  mutate (category = ifelse (category %in% c("other_15_34", "other_35_64", "other_others", "pri_others", "sec_others", "ter_others"), "others", category)) %>%
  
  # reaggregating
  dplyr::group_by (name, year, category) %>%
  dplyr::summarise (prop = mean (prop, na.rm = T)) %>%
  
  # normalising shares so that it all adds up to 100 
  dplyr::group_by (name, year) %>%
  mutate (prop = (prop / sum (prop, na.rm = T))) %>%
  
  # re-widening
  pivot_wider (names_from = "category", values_from = "prop") %>%
  
  # compute totals column 
  mutate (total = others + pri_15_34 + pri_35_64 + sec_15_34 + sec_35_64 + ter_15_34 + ter_35_64)

## final tidy
homepop_clean2 = homepop_clean %>%
  
  # renaming 
  rename (dest = name) %>%
  
  # relocate
  relocate (dest, .after = year) %>%
  
  # recoding dest variable into ISO3 codes
  mutate (dest = countrycode (dest, "country.name", "iso3c"))

## obtaining population totals from WDP

# getting raw WDP data 
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))

# cleaning WDP data 
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, Inf))

# final clean
wdp_clean = wdp %>%
  
  # first aggregation
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # filtering for relevant countries and years
  filter (ccode %in% unique (homepop_clean2$dest)) %>%
  filter (year %in% unique (homepop_clean2$year)) %>%
  
  # renaming ccode to dest for join purposes
  rename (dest = ccode)

## combining WDP with Wittgenstein data to obtain category-specific sums

# merging data
homepop_disagg = homepop_clean2 %>%
  
  # merging with WDP data
  left_join (wdp_clean, by = c("dest", "year"))

# multiplying through
inc_cols <- colnames (homepop_disagg [,3:9])
for (i in inc_cols) {homepop_disagg [,i] = round (homepop_disagg [,i] * homepop_disagg [,11], 0)}

# double checking that the totals add up
homepop_disagg = homepop_disagg %>% mutate (total_pop = others + pri_15_34 + pri_35_64 + sec_15_34 + sec_35_64 + ter_15_34 + ter_35_64)

# isolating needed columns
homepop_disagg = homepop_disagg [,c(1:9,12)]

## computing non-migrant populations -------------------------------------------

## base df 
base = homepop_disagg %>% left_join (flows_edu_age_sums, by = c("year", "dest"))

## minusing 
base = base %>% mutate (nomig_pri_15_34 = pri_15_34 - flow_pri_15_34, 
                        nomig_pri_35_64 = pri_35_64 - flow_pri_35_64,
                        nomig_sec_15_34 = sec_15_34 - flow_sec_15_34, 
                        nomig_sec_35_64 = sec_35_64 - flow_sec_35_64,
                        nomig_ter_15_34 = ter_15_34 - flow_ter_15_34, 
                        nomig_ter_35_64 = ter_35_64 - flow_ter_35_64,
                        nomig_others = others - flow_others, 
                        nomig_total = total_pop - flow_total) %>%
  
  # keeping only final columns
  dplyr::select (year, dest, nomig_pri_15_34, nomig_pri_35_64, nomig_sec_15_34, nomig_sec_35_64, nomig_ter_15_34, nomig_ter_35_64, nomig_others, nomig_total)

## minus 20% df 
minus_20 = homepop_disagg %>% left_join (flows_edu_age_sums_minus20, by = c("year", "dest"))

## minusing 
minus_20 = minus_20 %>% mutate (nomig_pri_15_34 = pri_15_34 - flow_pri_15_34, 
                        nomig_pri_35_64 = pri_35_64 - flow_pri_35_64,
                        nomig_sec_15_34 = sec_15_34 - flow_sec_15_34, 
                        nomig_sec_35_64 = sec_35_64 - flow_sec_35_64,
                        nomig_ter_15_34 = ter_15_34 - flow_ter_15_34, 
                        nomig_ter_35_64 = ter_35_64 - flow_ter_35_64,
                        nomig_others = others - flow_others, 
                        nomig_total = total_pop - flow_total) %>%
  
  # keeping only final columns
  dplyr::select (year, dest, nomig_pri_15_34, nomig_pri_35_64, nomig_sec_15_34, nomig_sec_35_64, nomig_ter_15_34, nomig_ter_35_64, nomig_others, nomig_total)

## plus 20% df 
plus_20 = homepop_disagg %>% left_join (flows_edu_age_sums_plus20, by = c("year", "dest"))

## minusing 
plus_20 = plus_20 %>% mutate (nomig_pri_15_34 = pri_15_34 - flow_pri_15_34, 
                                nomig_pri_35_64 = pri_35_64 - flow_pri_35_64,
                                nomig_sec_15_34 = sec_15_34 - flow_sec_15_34, 
                                nomig_sec_35_64 = sec_35_64 - flow_sec_35_64,
                                nomig_ter_15_34 = ter_15_34 - flow_ter_15_34, 
                                nomig_ter_35_64 = ter_35_64 - flow_ter_35_64,
                                nomig_others = others - flow_others, 
                                nomig_total = total_pop - flow_total) %>%
  
  # keeping only final columns
  dplyr::select (year, dest, nomig_pri_15_34, nomig_pri_35_64, nomig_sec_15_34, nomig_sec_35_64, nomig_ter_15_34, nomig_ter_35_64, nomig_others, nomig_total)

## exporting -------------------------------------------------------------------
write.csv (base, file.path (iom_path, "flows_edu_age_scenarios_input", "nomig_base.csv"), row.names = F)
write.csv (minus_20, file.path (iom_path, "flows_edu_age_scenarios_input", "nomig_minus_20.csv"), row.names = F)
write.csv (plus_20, file.path (iom_path, "flows_edu_age_scenarios_input", "nomig_plus_20.csv"), row.names = F)
write.csv (homepop_disagg, file.path (iom_path, "flows_edu_age_scenarios_input", "totalpop_wmig.csv"), row.names = F)






















