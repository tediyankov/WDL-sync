
## THE ECONOMIST: ATHENS EVENT =================================================

## Prologue --------------------------------------------------------------------

## clean environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, worlddataverse, ggimage)

## set font
worlddataverse::font_wdl()

## set paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2022_10_11','2022_10_12_second_2017ppp','03_outputs')

## raw data load
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_10_11_2017ppp_1_USD.rds"))

## Chapter 1: cleaning WDP data ------------------------------------------------

## Chapter 2: demographic shifts -----------------------------------------------

## 2023 total population + EU27 population

## Population growth (changes in % change in world population and EU27 population)

## Show population trend by age group in Europe

## The rank shift chart with the colored lines (do with countries around Greece, which countries overtake Greece (2020 → 2030))

## Where is Greece CC coming from? (age breakdown, gender breakdown)

## GDP per capita and total GDP growth in Greece vs in EU27

## Chapter 3: spending shifts --------------------------------------------------

## How many people are entering the EU27 consumer class each second?

## Four top markets (update existing chart)

## Labor tree for Greece

## Chapter 4: digitalisation insights ------------------------------------------

## internet prices in the World

## internet prices scatter plot of EU and highlight Greece

## visualise download, upload and latency for EU27 and Greece

## Broadband access in EU and Greece, maybe highlight CEECs (use external data)















