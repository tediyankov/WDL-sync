
## cleaning beauty data ========================================================

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
beauty_path <- file.path (base_path, "product_categories", "trade_model", "03_outputs")

## loading in raw data files
old_beauty <- read_excel (file.path (beauty_path, "beauty_exp_loreal.xlsx"))
old_beautyregion <- read_excel (file.path (beauty_path, "beauty_exp_loreal.xlsx"), sheet = 2)
old_beautytotals <- read_excel (file.path (beauty_path, "beauty_exp_loreal.xlsx"), sheet = 3)
new_raw_beauty <- read_excel ("~/Desktop/Loreal_Product_Data-2023-06-28 (1).xlsx")

## cleaning data ---------------------------------------------------------------

new_beauty <- new_raw_beauty %>%
  
  # adding Country variable
  rename (ISO = Country) %>%
  mutate (Country = countrycode (ISO, "iso3c", "country.name")) %>%
  
  # keeping relevant variables
  dplyr::select ("ISO", "Country", "Year", "Region", "Population", "Expenditure Type", 
                 "Total Expenditure", "Beauty Share of Personal Care", "Personal Care", 
                 "Beauty", "Product Category", "Product Expenditure", "Product Share of Beauty") %>%

  # widening to have nominal and ppp versions of each relevant variable
  pivot_wider (names_from = "Expenditure Type", values_from = c("Total Expenditure", "Personal Care", "Beauty")) 

new_beautyregion = new_raw_beauty %>%
  
  # keeping only ppp
  filter (`Expenditure Type` == "ppp") %>%
  
  # grouping into region-years 
  dplyr::group_by (Region, Year) %>%
  dplyr::summarise (`Beauty Expenditure Total` = sum (Beauty, na.rm = T))

new_beautytotal = new_raw_beauty %>%
  
  # keeping only ppp
  filter (`Expenditure Type` == "ppp") %>%
  
  # grouping into region-years 
  dplyr::group_by (Year) %>%
  dplyr::summarise (`Beauty Expenditure Total` = sum (Beauty, na.rm = T))


write.csv (new_beautyregion, "~/Desktop/new_beautyregion.csv")
write.csv (new_beautytotal, "~/Desktop/new_beautytotal.csv")


