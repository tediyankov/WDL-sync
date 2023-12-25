
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

## chapter 2: implementing -----------------------------------------------------

## goal: expand to include 2025 and 2030, add in Jakob's predicted flows for post-2020 + replace stocks until 2020 with OECD flows

## creating df of flows to apply the shares to

# post-2020
oecd_flows_2020_ <- load("/Users/teddyyankov/Downloads/output_average_predictions_OECD.RData") # update w/ file path
oecd_flows_2020_ = output_average_predictions %>%
  rename_with (
    ~ case_when (
      . == "averaged_pred_flow" ~ "flow",
      TRUE ~ .
    ) 
  ) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  unite ("orig.dest.year", c(orig, dest, year), sep = "_", remove = F, na.rm = F) %>%
  relocate (orig.dest.year, .before = orig.dest) %>%
  add_column(flows.sum = NA) %>% 
  mutate (year = 5*floor(year/5)) %>%
  group_by (orig, dest, year) %>%
  summarise (flows.sum = sum(flow), .groups = "keep") %>%
  rename_with (
    ~ case_when (
      . == "flows.sum" ~ "flow",
      TRUE ~ .
    ) 
  ) %>%
  mutate (flow = round (flow))

# pre-2020
oecd_flows_2000_2020 <- read.csv (file.path (output_path,"merged_data_2040_exp.csv")) %>%
  filter (year %in% 2000:2019) %>%
  dplyr::select(1:6) %>%
  rename_with (
    ~ case_when (
      . == "flow_full_ts" ~ "flow",
      TRUE ~ .
    ) 
  ) %>%
  drop_na() %>%
  add_column(flows.sum = NA) %>% 
  mutate (year = 5*floor(year/5)) %>%
  group_by (orig, dest, year) %>%
  summarise (flows.sum = sum(flow), .groups = "keep") %>%
  rename_with (
    ~ case_when (
      . == "flows.sum" ~ "flow",
      TRUE ~ .
    ) 
  ) %>%
  mutate (flow = round (flow))

# bind them together
flows_2000_2030 = rbind (oecd_flows_2020_, oecd_flows_2000_2020) %>%
  unite ("orig_dest", orig:dest, sep = "_", remove = F, na.rm = F) 

## joining with shares data (using last available growth rate)

distributions = DIOC_2000_2030_1 %>% dplyr::select (-13)

DIOC_flowsexp_1 = distributions %>%
  left_join (flows_2000_2030, by = c("year", "orig_dest", "orig", "dest")) %>%
  mutate (flow_pri_15_34 = round (flow * pri_15_34),
          flow_sec_15_34 = round (flow * sec_15_34),
          flow_ter_15_34 = round (flow * ter_15_34),
          flow_pri_35_64 = round (flow * pri_35_64), 
          flow_sec_35_64 = round (flow * sec_35_64), 
          flow_ter_35_64 = round (flow * ter_35_64),
          flow_others = round (flow * others))

DIOC_flowsexp_1$total_flow = rowSums (DIOC_flowsexp_1 [,14:20], na.rm = T)
DIOC_flowsexp_1$total_shares = rowSums (DIOC_flowsexp_1 [,6:12], na.rm = T)
DIOC_flowsexp_1 = DIOC_flowsexp_1 %>% relocate (total_shares, .before = flow)


DIOC_flowsexp_2 = DIOC_flowsexp_1%>%
  ungroup() %>%
  dplyr::select (-c(2, 6:14)) %>%
  group_by (year, dest) %>%
  summarise (flow_pri_15_34 = sum (flow_pri_15_34, na.rm = T),
             flow_sec_15_34 = sum (flow_sec_15_34, na.rm = T),
             flow_ter_15_34 = sum (flow_ter_15_34, na.rm = T),
             flow_pri_35_64 = sum (flow_pri_35_64, na.rm = T), 
             flow_sec_35_64 = sum (flow_sec_35_64, na.rm = T), 
             flow_ter_35_64 = sum (flow_ter_35_64, na.rm = T),
             flow_others = sum (flow_others, na.rm = T))  %>%
  ungroup () %>%
  mutate (flow_total = flow_pri_15_34 + flow_sec_15_34 + flow_ter_15_34 + flow_pri_35_64 + flow_sec_35_64 + flow_ter_35_64 + flow_others)

write.csv (DIOC_2000_2030_1, file.path(output_path,"DIOC_2000_30_shares.csv"), row.names = FALSE)
write.csv (DIOC_flowsexp_1, file.path(output_path,"DIOC_2000_30_shares_and_flows.csv"), row.names = FALSE)
write.csv (DIOC_flowsexp_2, file.path(output_path,"DIOC_2000_30_aggr_flows.csv"), row.names = FALSE)

## joining with shares data (using average growth rate)

distributions_1_avggr = DIOC_2000_30_shares %>% dplyr::select (-13)

DIOC_flowsexp_1_avggr = distributions_1_avggr %>%
  left_join (flows_2000_2030, by = c("year", "orig_dest", "orig", "dest")) %>%
  mutate (flow_pri_15_34 = round (flow * pri_15_34),
          flow_sec_15_34 = round (flow * sec_15_34),
          flow_ter_15_34 = round (flow * ter_15_34),
          flow_pri_35_64 = round (flow * pri_35_64), 
          flow_sec_35_64 = round (flow * sec_35_64), 
          flow_ter_35_64 = round (flow * ter_35_64),
          flow_others = round (flow * others))

DIOC_flowsexp_1_avggr$total_flow = rowSums (DIOC_flowsexp_1_avggr [,14:20], na.rm = T)
DIOC_flowsexp_1_avggr$total_shares = rowSums (DIOC_flowsexp_1_avggr [,6:12], na.rm = T)
DIOC_flowsexp_1_avggr = DIOC_flowsexp_1_avggr %>% relocate (total_shares, .before = flow)


DIOC_flowsexp_2_avggr = DIOC_flowsexp_1_avggr%>%
  ungroup() %>%
  dplyr::select (-c(2, 6:14)) %>%
  group_by (year, dest) %>%
  summarise (flow_pri_15_34 = sum (flow_pri_15_34, na.rm = T),
             flow_sec_15_34 = sum (flow_sec_15_34, na.rm = T),
             flow_ter_15_34 = sum (flow_ter_15_34, na.rm = T),
             flow_pri_35_64 = sum (flow_pri_35_64, na.rm = T), 
             flow_sec_35_64 = sum (flow_sec_35_64, na.rm = T), 
             flow_ter_35_64 = sum (flow_ter_35_64, na.rm = T),
             flow_others = sum (flow_others, na.rm = T))  %>%
  ungroup () %>%
  mutate (flow_total = flow_pri_15_34 + flow_sec_15_34 + flow_ter_15_34 + flow_pri_35_64 + flow_sec_35_64 + flow_ter_35_64 + flow_others)

write.csv (DIOC_2000_30_shares, file.path(output_path,"DIOC_2000_30_shares_avggrowthrates.csv"), row.names = FALSE)
write.csv (DIOC_flowsexp_1_avggr, file.path(output_path,"DIOC_2000_30_shares_and_flows_avggrowthrates.csv"), row.names = FALSE)
write.csv (DIOC_flowsexp_2_avggr, file.path(output_path,"DIOC_2000_30_aggr_flows_avggrowthrates.csv"), row.names = FALSE)

# priority 1: change raw data: fill age_cen gaps with age_lfs [DONE]
# priority 2: implement 2025 and 2030 as avg for all dist for previous years [DONE]







