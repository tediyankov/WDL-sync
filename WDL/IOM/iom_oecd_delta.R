
## IOM: misc task --> total OECD pop change with and without mig

## paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')
output_path <- file.path (base_path, "IOM", "2nd_paper", "output")

## loading data

# WDP
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, Inf))

# Migration
flows <- read.csv (file.path (output_path, "flows_edu_age.csv")) %>%
  filter (dest %in% c("AUS","BEL","CAN","CHE","CZE","DEU","FRA","HUN","IRL","ISR","NLD","POL","USA"))

# flows pred by edu_age breakdown but summed by destination
flows_sums <- read.csv (file.path (output_path, "flows_edu_age_sums.csv")) %>%
  filter (dest %in% c("AUS","BEL","CAN","CHE","CZE","DEU","FRA","HUN","IRL","ISR","NLD","POL","USA")) %>%
  mutate (flows_15_64 = flow_total - flow_others) %>%
  dplyr::select (2:3, 12) %>%
  rename_with (
    ~ case_when (
      . == "dest" ~ "ccode",
      TRUE ~ .
    )
  ) %>%
  relocate (ccode, year, flows_15_64)

# clean WDP
wdp_clean = wdp %>%
  group_by (ccode, year, age_group) %>%
  summarise (hc.pdf.m = sum (hc.pdf.m, na.rm = T)) %>% ungroup () %>%
  filter (ccode %in% c("AUS","BEL","CAN","CHE","CZE","DEU","FRA","HUN","IRL","ISR","NLD","POL","USA") & 
            year %in% c(2000:2030)) %>%
  mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)","[10,15)"), "0_15", 
                              ifelse (age_group %in% c("[65,70)","[70,75)","[75,INF)"), "65_plus", "15_64"))) %>%
  filter (age_group == "15_64") %>%
  group_by (ccode, year) %>%
  summarise (hc.pdf.m = sum (hc.pdf.m, na.rm = T)) %>% ungroup ()

# combining into one data frame
input_data = flows_sums %>%
  left_join (wdp_clean, by = c("ccode", "year")) %>%
  arrange (ccode) %>%
  rename_with (
    ~ case_when (
      . == "hc.pdf.m" ~ "pop",
      TRUE ~ .
    )
  ) %>%
  transform (pop_nomig = pop - flows_15_64) %>%
  pivot_longer (4:5, names_to = "migYN", values_to = "pop")

## deriving delta stats
input_data_sums = input_data %>%
  
  # summing into USA, EU, others
  mutate (ccode = ifelse (ccode == "USA", "USA", 
                          ifelse (ccode %in% c("BEL", "CZE", "DEU", "FRA", "HUN", "IRL", "NLD", "POL"), "EU", "others"
                          ))) %>%
  #mutate (ccode = ifelse (!(ccode %in% c("BEL", "CZE", "DEU", "FRA", "HUN", "IRL", "NLD", "POL")), "others", ccode)) %>%
  
  # aggregate 
  dplyr::group_by (ccode, year, migYN) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>%
  
  # filtering for 2023 and 2030
  filter (year %in% c(2023, 2030))

input_data_total = input_data_sums %>% dplyr::group_by (year, migYN) %>% dplyr::summarise (pop = sum (pop, na.rm = T))

wdp_clean_total = wdp_clean %>% dplyr::group_by (year) %>% dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>% filter (year == 2023)

wdp_clean_sums = wdp_clean %>%
  
  # summing into USA, EU, others
  mutate (ccode = ifelse (ccode == "USA", "USA", 
                          ifelse (ccode %in% c("BEL", "CZE", "DEU", "FRA", "HUN", "IRL", "NLD", "POL"), "EU", "others"
                          ))) %>%
  
  #mutate (ccode = ifelse (!(ccode %in% c("BEL", "CZE", "DEU", "FRA", "HUN", "IRL", "NLD", "POL")), "others", ccode)) %>%
  
  # aggregate 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # fitlering for year 2023
  filter (year == 2023)

input_data_sums2 = input_data_sums %>% ungroup () %>%
  
  dplyr::select (ccode, migYN, pop) %>%
  rename (pop_2030 = pop) %>%
  left_join (wdp_clean_sums, by = "ccode") %>%
  rename (pop_2023 = pop) %>%
  mutate (delta_pct = ((pop_2030 - pop_2023) / pop_2023)*100,
          delta = pop_2030 - pop_2023) %>%
  dplyr::select (-year)







