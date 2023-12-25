
## preliminaries  --------------------------------------------------------------

# packages
pacman::p_load (tidyverse,countrycode)

# paths
base_path <- file.path ("/Volumes/","GoogleDrive", "Shared drives", "DATA_WDL")
input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data")
output_path <- file.path (base_path, "IOM", "2nd_paper", "output")

## executing merge -------------------------------------------------------------

# load data
flows <- read.csv (file.path (output_path, "OECD_flows.csv"))
stocks <- read.csv (file.path (output_path, "OECD_stocks.csv"))
pop_total <- read.csv (file.path (output_path, "WPP_pop_tot.csv"))
dep_ratio <- read.csv (file.path (output_path, "WPP_dep_ratio.csv"))
infant_mort <- read.csv (file.path (output_path, "WPP_infant_mort.csv"))
gdp <- read.csv (file.path (output_path, "MPRO_gdp.csv"))
geo <- read.csv (file.path (output_path, "geo.csv"))

# diverging single-country data into twin datasets for orig and dest
pop_total_orig = pop_total %>% rename (orig = country)
pop_total_dest = pop_total %>% rename (dest = country)

dep_ratio_orig = dep_ratio %>% rename (orig = country)
dep_ratio_dest = dep_ratio %>% rename (dest = country)

infant_mort_orig = infant_mort %>% rename (orig = country)
infant_mort_dest = infant_mort %>% rename (dest = country)

gdp_orig = gdp %>% rename (orig = country)
gdp_dest = gdp %>% rename (dest = country)

# for two-country datasets, keeping only commbined orig+dest variable
stocks = stocks %>% select (-c("orig", "dest"))
geo = geo %>% select (-c("orig", "dest"))

# merging
data = flows %>%
  left_join (stocks, by = c("orig.dest", "year")) %>%
  left_join (geo, by = c("orig.dest")) %>%
  left_join (pop_total_orig, by = c("orig", "year")) %>%
  left_join (pop_total_dest, by = c("dest", "year"), suffix = c(".orig", ".dest")) %>%
  left_join (dep_ratio_orig, by = c("orig", "year")) %>%
  left_join (dep_ratio_dest, by = c("dest", "year"), suffix = c(".orig", ".dest")) %>%
  left_join (infant_mort_orig, by = c("orig", "year")) %>%
  left_join (infant_mort_dest, by = c("dest", "year"), suffix = c(".orig", ".dest")) %>%
  left_join (gdp_orig, by = c("orig", "year")) %>%
  left_join (gdp_dest, by = c("dest", "year"), suffix = c(".orig", ".dest")) %>%
  select (-c("orig.dest.year.x", "orig.dest.year.y")) %>%
  arrange (orig.dest)

## interpolating NAs with in-group means

# flows
flows.na = data %>% filter(is.na(flow))
nas = unique(flows.na$orig.dest)
for (i in nas) {
  data$flow [data$orig.dest == i] = 
    ifelse (is.na (data$flow [data$orig.dest == i]),
            ave (data$flow [data$orig.dest == i], FUN = function (x) mean (x, na.rm = T)),
            data$flow [data$orig.dest == i])
}
data$flow = round(data$flow)

# migstock
migstock.na = data %>% filter(is.na(migstock))
nas = unique(migstock.na$orig.dest)
for (i in nas) {
  data$migstock [data$orig.dest == i] = 
    ifelse (is.na (data$migstock [data$orig.dest == i]),
            ave (data$migstock [data$orig.dest == i], FUN = function (x) mean (x, na.rm = T)),
            data$migstock [data$orig.dest == i])
}
data$migstock = round(data$migstock)

# adding lags
data = data %>%
  mutate (flows.lagged = Lag(flow, -1)) %>%
  mutate (flows.lagged = ifelse (year %in% c(2019),
                                 NA,
                                 flows.lagged)) %>%
  relocate (flows.lagged, .before = migstock)

# checking where there are NaNs leftover
data.sample.migstock = data %>% filter (migstock == "NaN")
data.sample.flows = data %>% filter (flow == "NaN")

# removing country-pairings which have NaN in flow data
data = data %>% filter (flow != "NaN")

# clean up environment 
rm(flows.na, migstock.na, nas)

# export data
write.csv(data, file.path(output_path,"merged_data.csv"), row.names = FALSE)



  
  
  
  