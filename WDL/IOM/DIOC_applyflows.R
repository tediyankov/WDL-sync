
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

## chapter 2: applying ----------------------------------------------------

# obtaining OECD stock data
stocks_1 = read.csv (file.path (output_path,"merged_data_2040_exp.csv")) %>% 
  dplyr::select (1:5, 12) %>%
  filter (!(year == 2020))

# as there is no 2020 data, we take 2019 values for 2020 instead
stocks_1$year = ifelse (stocks_1$year == 2019, 
                        2020, 
                        stocks_1$year)

# reformatting to faciliate merge
stocks_1 = stocks_1 %>%
  filter (year %in% c(2000, 2005, 2010, 2015, 2020)) %>%
  rename (migstock_bir_int = migStock, 
          orig.dest = orig_dest) %>%
  dplyr::select (-c("orig.dest.year"))

distributions = DIOC_2000_20_shares %>% dplyr::select (-13)

# merging with shares
migStocks_1 = distributions %>%
  left_join (stocks_1, by = c("year", "orig_dest", "orig", "dest"))

# obtaining and rounding estimates
migStocks_2 = migStocks_1 %>%
  mutate (mig_pri_15_24 = round (migStock * pri_15_24),
          mig_sec_15_24 = round (migStock * sec_15_24),
          mig_ter_15_24 = round (migStock * ter_15_24),
          mig_pri_25_64 = round (migStock * pri_25_64), 
          mig_sec_25_64 = round (migStock * sec_25_64), 
          mig_ter_25_64 = round (migStock * ter_25_64),
          mig_others = round (migStock * others))

migStock_3 = migStocks_2 %>%
  ungroup() %>%
  dplyr::select (-c(2, 6:11)) %>%
  group_by (year, dest) %>%
  summarise (mig_pri_15_24 = sum (mig_pri_15_24, na.rm = T),
             mig_sec_15_24 = sum (mig_sec_15_24, na.rm = T),
             mig_ter_15_24 = sum (mig_ter_15_24, na.rm = T),
             mig_pri_25_64 = sum (mig_pri_25_64, na.rm = T), 
             mig_sec_25_64 = sum (mig_sec_25_64, na.rm = T), 
             mig_ter_25_64 = sum (mig_ter_25_64, na.rm = T),
             mig_others = sum (mig_others, na.rm = T))  %>%
  ungroup () %>%
  mutate (migStock_total = mig_pri_15_24 + mig_sec_15_24 + mig_ter_15_24 + mig_pri_25_64 + mig_sec_25_64 + mig_ter_25_64 + mig_others)

write.csv (DIOC_2000_20_shares, file.path(output_path,"DIOC_2000_20_shares.csv"), row.names = FALSE)
write.csv (migStocks_2, file.path(output_path,"DIOC_2000_20_shares_and_stocks"), row.names = FALSE)
write.csv (migStock_3, file.path(output_path,"DIOC_2000_20_aggr_stocks"), row.names = FALSE)


