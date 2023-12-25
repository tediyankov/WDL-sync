
## IOM PAPER 2/3: CREATING SCENARIOS ===========================================

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
iom_path <- file.path (base_path, "IOM", "2nd_paper", "output")

## loading in migrant flow data by education and age disaggregates
flows_edu_age_sums <- read.csv (file.path (iom_path, "flows_edu_age_sums.csv"))

## removing the X-var 
flows_edu_age_sums = flows_edu_age_sums %>% dplyr::select (-X)

## creating df with -20% -------------------------------------------------------

## creating copy of baseline 
flows_edu_age_sums_minus20 = flows_edu_age_sums

## decreasing all values by 20%
for (i in 4:11) {flows_edu_age_sums_minus20[,i] <- round (flows_edu_age_sums_minus20[,i] * 0.80, 0)}

## removing the X-var 
flows_edu_age_sums_minus20 = flows_edu_age_sums_minus20 %>% dplyr::select (-X)

## creating df with +20% -------------------------------------------------------

## creating copy of baseline 
flows_edu_age_sums_plus20 = flows_edu_age_sums

## decreasing all values by 20%
for (i in 4:11) {flows_edu_age_sums_plus20[,i] <- round (flows_edu_age_sums_plus20[,i] * 1.2, 0)}

## removing the X-var 
flows_edu_age_sums_plus20 = flows_edu_age_sums_plus20 %>% dplyr::select (-X)

## exporting -------------------------------------------------------------------
write.csv (flows_edu_age_sums, "flows_edu_age_pred.csv", row.names = F)
write.csv (flows_edu_age_sums_minus20, "flows_edu_age_minus20pct.csv", row.names = F)
write.csv (flows_edu_age_sums_plus20, "flows_edu_age_plus20pct.csv", row.names = F)


