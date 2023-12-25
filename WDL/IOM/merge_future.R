
## preliminaries  --------------------------------------------------------------

# packages
pacman::p_load (tidyverse,countrycode, gridExtra)

# paths
base_path <- file.path ("/Volumes/","GoogleDrive", "Shared drives", "DATA_WDL")
input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data")
output_path <- file.path (base_path, "IOM", "2nd_paper", "output")

## preprocessing   -------------------------------------------------------------

## flows (with new cartesian product)

# load data
oecdflows = read.csv (file.path(input_path, "OECD_mig_data.csv"))
 
# preprocess
OECD_flows = oecdflows %>%
  filter (VAR == "B11") %>%
  select (-c("VAR", "Variable", "GEN", "Gender", "YEA", "Flag.Codes", "Flags")) %>%
  mutate (orig = countrycode(sourcevar = Country.of.birth.nationality,
                             origin = "country.name",
                             destination = "iso3c",
                             warn = TRUE, 
                             nomatch = NA)) %>%
  mutate (dest = countrycode(sourcevar = Country,
                             origin = "country.name",
                             destination = "iso3c",
                             warn = TRUE, 
                             nomatch = NA)) %>% 
  filter (!(is.na(orig))) %>%
  filter (!(is.na(dest))) %>%
  select (-c(1:4)) %>%
  rename_with(
    ~ case_when(
      . == "Year" ~ "year",
      . == "Value" ~ "flow",
      TRUE ~ .
    )
  ) %>%
  relocate(orig, dest, year, flow)

# first cartesian product
origfactor <- as.factor (unique(OECD_flows$orig))
destfactor <- as.factor (unique(OECD_flows$dest))
yearfactor <- as.factor (unique(OECD_flows$year))

oecd_flows <- expand.grid (orig = origfactor,
                           dest = destfactor, 
                           year = yearfactor)

# merge flows with first cartesian products
OECD.flows = oecd_flows %>% 
  mutate (orig = as.character(orig),
          dest = as.character(dest),
          year = as.character(year)) %>%
  mutate (year = as.numeric(year)) %>%
  left_join(OECD_flows, by = c("orig", "dest", "year")) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  relocate (orig.dest, orig, dest, year, flow) %>%
  # code to deal with duplicate orig.dest.year groups
  unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
  filter (!(duplicated(orig.dest.year, fromLast = T)))

# interpoloating with in-group mean
flows.na = OECD.flows %>% filter(is.na(flow))
nas = unique(flows.na$orig.dest)
for (i in nas) {
  OECD.flows$flow [OECD.flows$orig.dest == i] =
    ifelse (is.na (OECD.flows$flow [OECD.flows$orig.dest == i]),
            ave (OECD.flows$flow [OECD.flows$orig.dest == i], FUN = function (x) mean (x, na.rm = T)),
            OECD.flows$flow [OECD.flows$orig.dest == i])
}
OECD.flows$flow = round(OECD.flows$flow)

# second cartesian product

origfactor <- as.factor (unique(OECD_flows$orig))
destfactor <- as.factor (unique(OECD_flows$dest))
yearfactor <- as.factor (seq(2000, 2040, 1))

oecd_flows <- expand.grid (orig = origfactor,
                           dest = destfactor, 
                           year = yearfactor)

# merge flows (means interpolated) with second cartesian product

flows = oecd_flows %>% 
  mutate (orig = as.character(orig),
          dest = as.character(dest),
          year = as.character(year)) %>%
  mutate (year = as.numeric(year)) %>%
  left_join(OECD.flows, by = c("orig", "dest", "year")) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  relocate (orig.dest, orig, dest, year, flow) %>%
  # code to deal with duplicate orig.dest.year groups
  unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
  filter (!(duplicated(orig.dest.year, fromLast = T)))

# removing leftover unobserved country pairs

flows_2020missing = flows %>%
  filter (year %in% 2000:2019) %>%
  filter(!(is.na(flow)))

flows = flows %>%
  filter(orig.dest %in% unique(flows_2020missing$orig.dest))

rm (origfactor, destfactor, yearfactor, oecd_flows, OECD_flows, oecdflows)

## preprocessing regressor data w/ year %in% 2000:2040

# population

pop_total = read.csv (file.path(input_path, "WPP_pop_age.csv")) %>%
  select (-c(1,2,3,5,6,7,8,9,10,11,12,14:19)) %>%
  rename_with(
    ~ case_when(
      . == "ISO3_code" ~ "countrycode",
      . == "Time" ~ "year",
      TRUE ~ .
    )
  ) %>%
  mutate (country = countrycode(sourcevar = countrycode,
                                origin = "iso3c",
                                destination = "iso3c",
                                warn = TRUE, 
                                nomatch = NA)) %>%
  select (-c("countrycode")) %>%
  filter(!(is.na(country))) %>%
  filter (year %in% 2000:2040) %>%
  relocate (country, year, PopTotal) %>%
  mutate (year = as.numeric(year)) %>%
  group_by (country,year) %>%
  summarise (pop = sum(PopTotal)) %>%
  arrange(country)

# dependency ratio

pop_age <- read.csv(file.path(input_path, "WPP_pop_age.csv")) %>%
  select(c("ISO3_code", "Time", "AgeGrp", "PopTotal")) %>%
  rename_with(
    ~ case_when(
      . == "ISO3_code" ~ "countrycode",
      . == "Time" ~ "year",
      TRUE ~ .
    )
  ) %>%
  mutate (country = countrycode(sourcevar = countrycode,
                                origin = "iso3c",
                                destination = "iso3c",
                                warn = TRUE, 
                                nomatch = NA)) %>%
  select (-c("countrycode")) %>%
  filter(!(is.na(country))) %>%
  filter (year %in% 2000:2040) %>%
  relocate (country, year, AgeGrp, PopTotal)

dependents <- c("0-4", "5-9", "10-14", 
                "65-69", "70-74", "75-79", 
                "80-84", "85-89", "90-94", 
                "95-99", "100+")

dep <- subset(pop_age, pop_age$AgeGrp %in% dependents)
non_dep <- subset(pop_age, !(pop_age$AgeGrp %in% dependents))  

dep_grouped <- dep %>%
  group_by(country, year) %>% 
  dplyr::summarize(dep_total = sum(PopTotal)) %>%
  as.data.frame()

non_dep_grouped <- non_dep %>%
  group_by(country, year) %>% 
  dplyr::summarize(non_dep_total = sum(PopTotal)) %>%
  as.data.frame()

joined <- merge(dep_grouped, non_dep_grouped, by=c('country', 'year'), how='inner')

joined$dep_ratio <- joined$dep_total/joined$non_dep_total
dep_ratio <- joined[c("country", "year", "dep_ratio")]

rm (joined, non_dep_grouped, dep_grouped, non_dep, dep, dependents, pop_age)

# infant mortality

infant_mort <- read.csv(file.path(input_path, "WPP_pop_indicators.csv")) %>%
  select (c("ISO3_code", "Time", "IMR")) %>%
  rename_with(
    ~ case_when(
      . == "ISO3_code" ~ "countrycode",
      . == "Time" ~ "year",
      TRUE ~ .
    )
  ) %>%
  mutate (country = countrycode(sourcevar = countrycode,
                                origin = "iso3c",
                                destination = "iso3c",
                                warn = TRUE, 
                                nomatch = NA)) %>%
  select (-c("countrycode")) %>%
  filter(!(is.na(country))) %>%
  filter (year %in% 2000:2040) %>%
  relocate (country, year, IMR)

# GDP per capita

mpro <- readRDS(file.path(base_path, "./Mpro_PC/2021_July/02_output_data/X9_3_mc_remerge_mixed_2021-09-06.RDS"))
gdp = mpro$macro.data %>% 
  mutate (country = countrycode(sourcevar = ccode,
                                origin = "iso3c",
                                destination = "iso3c",
                                warn = TRUE, 
                                nomatch = NA)) %>%
  select (c(country, year, GDP.PC.PPP)) %>%
  relocate (country, year, GDP.PC.PPP) %>%
  mutate (GDP.PC.PPP = Lag(GDP.PC.PPP, -1)) %>%
  filter (year %in% 2000:2040) %>%
  filter (!(is.na(country))) %>% 
  rename_with(
    ~ case_when(
      . == "GDP.PC.PPP" ~ "gdp.pc",
      TRUE ~ .
    )
  )

# geo (no change in preprocessing as it's not variable by year)
geo <- read.csv (file.path (output_path, "geo.csv")) %>% 
  select (-c("orig.dest"))
  
## stocks
stocks <- read.csv (file.path (output_path, "OECD_stocks.csv")) %>%
  unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
  filter (!(duplicated(orig.dest.year, fromLast = T))) %>%
  select (-c("orig", "dest"))

# interpolating with in-group means
migstock.na = stocks %>% filter(is.na(migstock))
nas = unique(migstock.na$orig.dest)
for (i in nas) {
  stocks$migstock [stocks$orig.dest == i] = 
    ifelse (is.na (stocks$migstock [stocks$orig.dest == i]),
            ave (stocks$migstock [stocks$orig.dest == i], FUN = function (x) mean (x, na.rm = T)),
            stocks$migstock [stocks$orig.dest == i])
}
stocks$migstock = round(stocks$migstock)
  
## executing merge   -----------------------------------------------------------

# diverging single-country data into twin datasets for orig and dest

pop_total_orig = pop_total %>% rename (orig = country)
pop_total_dest = pop_total %>% rename (dest = country)

dep_ratio_orig = dep_ratio %>% rename (orig = country)
dep_ratio_dest = dep_ratio %>% rename (dest = country)

infant_mort_orig = infant_mort %>% rename (orig = country)
infant_mort_dest = infant_mort %>% rename (dest = country)

gdp_orig = gdp %>% rename (orig = country)
gdp_dest = gdp %>% rename (dest = country)

# merging 
data = flows %>%
  left_join (stocks, by = c("orig.dest", "orig.dest.year", "year")) %>%
  left_join (geo, by = c("orig", "dest")) %>%
  left_join (pop_total_orig, by = c("orig", "year")) %>%
  left_join (pop_total_dest, by = c("dest", "year"), suffix = c(".orig", ".dest")) %>%
  left_join (dep_ratio_orig, by = c("orig", "year")) %>%
  left_join (dep_ratio_dest, by = c("dest", "year"), suffix = c(".orig", ".dest")) %>%
  left_join (infant_mort_orig, by = c("orig", "year")) %>%
  left_join (infant_mort_dest, by = c("dest", "year"), suffix = c(".orig", ".dest")) %>%
  left_join (gdp_orig, by = c("orig", "year")) %>%
  left_join (gdp_dest, by = c("dest", "year"), suffix = c(".orig", ".dest")) %>%
  arrange (orig.dest) 

# adding lags
data = data %>%
  arrange (orig.dest) %>% 
  mutate (flows.lagged = Lag(flow, 1)) %>%
  mutate (flows.lagged = ifelse (year == 2000,
                                 NA,
                                 flows.lagged)) %>%
  relocate (flows.lagged, .before = migstock)

# removing unobserved country pairings
data2 = data %>%
  filter (!(is.na(flow)))

## adding extra flows variables (non-interpolated, NAs for incomplete time series) ----------------------------

## non-interpolated 

# load data
oecdflows = read.csv (file.path(input_path, "OECD_mig_data.csv"))

# preprocess
OECD_flows = oecdflows %>%
  filter (VAR == "B11") %>%
  select (-c("VAR", "Variable", "GEN", "Gender", "YEA", "Flag.Codes", "Flags")) %>%
  mutate (orig = countrycode(sourcevar = Country.of.birth.nationality,
                             origin = "country.name",
                             destination = "iso3c",
                             warn = TRUE, 
                             nomatch = NA)) %>%
  mutate (dest = countrycode(sourcevar = Country,
                             origin = "country.name",
                             destination = "iso3c",
                             warn = TRUE, 
                             nomatch = NA)) %>% 
  filter (!(is.na(orig))) %>%
  filter (!(is.na(dest))) %>%
  select (-c(1:4)) %>%
  rename_with(
    ~ case_when(
      . == "Year" ~ "year",
      . == "Value" ~ "flow",
      TRUE ~ .
    )
  ) %>%
  relocate(orig, dest, year, flow)

# first cartesian product
origfactor <- as.factor (unique(OECD_flows$orig))
destfactor <- as.factor (unique(OECD_flows$dest))
yearfactor <- as.factor (unique(OECD_flows$year))

oecd_flows <- expand.grid (orig = origfactor,
                           dest = destfactor, 
                           year = yearfactor)

# merge flows with first cartesian products
OECD.flows.nonint = oecd_flows %>% 
  mutate (orig = as.character(orig),
          dest = as.character(dest),
          year = as.character(year)) %>%
  mutate (year = as.numeric(year)) %>%
  left_join(OECD_flows, by = c("orig", "dest", "year")) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  relocate (orig.dest, orig, dest, year, flow) %>%
  # code to deal with duplicate orig.dest.year groups
  unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
  filter (!(duplicated(orig.dest.year, fromLast = T)))

# second cartesian product
origfactor <- as.factor (unique(OECD_flows$orig))
destfactor <- as.factor (unique(OECD_flows$dest))
yearfactor <- as.factor (seq(2000, 2040, 1))

oecd_flows <- expand.grid (orig = origfactor,
                           dest = destfactor, 
                           year = yearfactor)

# merge flows (means not interpolated) with second cartesian product
flows_nonint = oecd_flows %>% 
  mutate (orig = as.character(orig),
          dest = as.character(dest),
          year = as.character(year)) %>%
  mutate (year = as.numeric(year)) %>%
  left_join(OECD.flows.nonint, by = c("orig", "dest", "year")) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  relocate (orig.dest, orig, dest, year, flow) %>%
  # code to deal with duplicate orig.dest.year groups
  unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
  filter (!(duplicated(orig.dest.year, fromLast = T)))

# removing unobserved country pairings
flows_2020missing = flows_nonint %>%
  filter (year %in% 2000:2019) %>%
  filter(!(is.na(flow)))

flows_nonint = flows_nonint %>%
  filter(orig.dest %in% unique(flows_2020missing$orig.dest))
  
# extracting sample
sample = flows_nonint %>% filter (orig.dest == "AFG_JPN")

# isolate flows 
flows_nonint = flows_nonint %>% select (c("orig.dest", "year", "flow"))

# adding lags 
flows_nonint = flows_nonint %>%
  arrange (orig.dest) %>% 
  mutate (flow_nonint_lagged = Lag(flow, 1)) %>%
  mutate (flow_nonint_lagged = ifelse (year == 2000,
                                 NA,
                                 flow_nonint_lagged))

## replace partial time series w/ NA

flows_exp = flows_nonint %>%
  rename (flow_nonint = flow) %>%
  mutate (flow_full_ts = flow_nonint)

flows_exp_nas = flows_exp %>% 
  filter (year %in% 2000:2019) %>%
  filter (is.na(flow_full_ts))

flows_exp$flow_full_ts = ifelse (flows_exp$orig.dest %in% unique(flows_exp_nas$orig.dest),
                                 NA,
                                 flows_exp$flow_full_ts)

# adding lags
flows_exp = flows_exp %>%
  arrange (orig.dest) %>% 
  mutate (flow_full_ts_lagged = Lag(flow_full_ts, 1)) %>%
  mutate (flow_full_ts_lagged = ifelse (year == 2000,
                                       NA,
                                       flow_full_ts_lagged))

sample = flows_exp %>% filter (orig.dest == "AFG_JPN")

## adding migStocks by nationality and migStocks by country of birth (interpolated (means) & not interpolated) -------

# migStocks by nationality

migstock_nat <- read.csv(file.path(input_path, "OECD_mig_data.csv")) %>%
  filter (Variable == "Stock of foreign population by nationality") %>%
  select (-c("VAR", "Variable", "GEN", "Gender", "YEA", "Flag.Codes", "Flags")) %>%
  mutate (orig = countrycode(sourcevar = Country.of.birth.nationality,
                             origin = "country.name",
                             destination = "iso3c",
                             warn = TRUE, 
                             nomatch = NA)) %>%
  mutate (dest = countrycode(sourcevar = Country,
                             origin = "country.name",
                             destination = "iso3c",
                             warn = TRUE, 
                             nomatch = NA)) %>%
  filter(!(is.na(orig))) %>%
  filter(!(is.na(dest))) %>%
  select (-c(1:4)) %>%
  rename_with(
    ~ case_when(
      . == "Year" ~ "year",
      . == "Value" ~ "migstock_nat",
      TRUE ~ .
    )
  ) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  relocate (orig.dest, orig, dest, year, migstock_nat) %>%
  # code to deal with duplicate orig.dest.year groups
  unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
  filter (!(duplicated(orig.dest.year, fromLast = T))) %>%
  filter(orig.dest %in% unique(flows_2020missing$orig.dest)) %>%
  arrange(orig.dest) %>%
  # isolate stocks 
  select(c("orig.dest", "year", "migstock_nat"))

# migStocks by country of birth

migstock_bir <- read.csv(file.path(input_path, "OECD_mig_data.csv")) %>%
  filter (Variable == "Stock of foreign-born population by country of birth") %>%
  select (-c("VAR", "Variable", "GEN", "Gender", "YEA", "Flag.Codes", "Flags")) %>%
  mutate (orig = countrycode(sourcevar = Country.of.birth.nationality,
                             origin = "country.name",
                             destination = "iso3c",
                             warn = TRUE, 
                             nomatch = NA)) %>%
  mutate (dest = countrycode(sourcevar = Country,
                             origin = "country.name",
                             destination = "iso3c",
                             warn = TRUE, 
                             nomatch = NA)) %>%
  filter(!(is.na(orig))) %>%
  filter(!(is.na(dest))) %>%
  select (-c(1:4)) %>%
  rename_with(
    ~ case_when(
      . == "Year" ~ "year",
      . == "Value" ~ "migstock_bir",
      TRUE ~ .
    )
  ) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  relocate (orig.dest, orig, dest, year, migstock_bir) %>%
  # code to deal with duplicate orig.dest.year groups
  unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
  filter (!(duplicated(orig.dest.year, fromLast = T))) %>%
  filter(orig.dest %in% unique(flows_2020missing$orig.dest)) %>%
  arrange(orig.dest) %>%
  # isolate stocks 
  select(c("orig.dest", "year", "migstock_bir"))

# merge with flows_exp

data_exp = flows_exp %>%
  left_join (migstock_nat, by = c("orig.dest", "year")) %>%
  left_join (migstock_bir, by = c("orig.dest", "year")) %>%
  rename_with(
    ~ case_when(
      . == "migstock_nat" ~ "migstock_nat_nonint",
      . == "migstock_bir" ~ "migstock_bir_nonint",
      TRUE ~ .
    )
  ) %>%
  mutate (migstock_nat_int = migstock_nat_nonint) %>%
  mutate (migstock_bir_int = migstock_bir_nonint)
   
# adding interpolated version of migStock by nationality 

migstock_nat_nas = data_exp %>% 
  filter (year %in% 2000:2019) %>% 
  filter(is.na(migstock_nat_nonint))

migstock_nat_nas_list = unique (migstock_nat_nas$orig.dest)

for (i in migstock_nat_nas_list) {
  data_exp$migstock_nat_int [data_exp$orig.dest == i] = 
    ifelse (is.na(data_exp$migstock_nat_int [data_exp$orig.dest == i]),
            ave (data_exp$migstock_nat_int [data_exp$orig.dest == i], FUN = function (x) mean (x, na.rm = T)),
            data_exp$migstock_nat_int [data_exp$orig.dest == i])
}

data_exp$migstock_nat_int = round (data_exp$migstock_nat_int)

data_exp$migstock_nat_int = ifelse (data_exp$year %in% 2020:2040, 
                                       NA, 
                                       data_exp$migstock_nat_int)

# adding interpolated version of migStock by country of birth

migstock_bir_nas = data_exp %>% 
  filter (year %in% 2000:2019) %>% 
  filter(is.na(migstock_bir_nonint))

migstock_bir_nas_list = unique (migstock_bir_nas$orig.dest)

for (i in migstock_bir_nas_list) {
  data_exp$migstock_bir_int [data_exp$orig.dest == i] = 
    ifelse (is.na(data_exp$migstock_bir_int [data_exp$orig.dest == i]),
            ave (data_exp$migstock_bir_int [data_exp$orig.dest == i], FUN = function (x) mean (x, na.rm = T)),
            data_exp$migstock_bir_int [data_exp$orig.dest == i])
}

data_exp$migstock_bir_int = round (data_exp$migstock_bir_int)

data_exp$migstock_bir_int = ifelse (data_exp$year %in% 2020:2040, 
                                    NA, 
                                    data_exp$migstock_bir_int)

## merging everything back with the main data

data_new = data %>%
  select (-c("migstock")) %>%
  right_join (data_exp, by = c("orig.dest", "year")) %>%
  rename_with(
    ~ case_when(
      . == "flow" ~ "flow_int",
      . == "flows.lagged" ~ "flow_int_lagged",
      TRUE ~ .
    )
  ) %>%
  relocate (orig.dest.year, orig.dest, orig, dest, year, flow_full_ts, 
            flow_full_ts_lagged, flow_int, flow_int_lagged, flow_nonint, 
            flow_nonint_lagged,  migstock_bir_int, migstock_bir_nonint, 
            migstock_nat_int, migstock_nat_nonint, dist, contig, comlang_off, 
            pop.orig, pop.dest, IMR.orig, IMR.dest, dep_ratio.orig, 
            dep_ratio.dest, gdp.pc.orig, gdp.pc.dest)


# sample: should have years 2000:2040 for regressors and 2000:2019 for flows and stocks
sample = data_new %>% filter (orig.dest == "AFG_JPN")

# cleaning up environment
rm (i, oecd_flows, OECD_flows, oecdflows, data1, flows.na, migstock.na, nas, sample, flows, geo, OECD.flows, pop, stocks, pop_total_orig, pop_total_dest, dep_ratio_orig, dep_ratio_dest, infant_mort_orig, infant_mort_dest, gdp_orig, gdp_dest, pop_total, dep_ratio, infant_mort, gdp)

# solving geo missing orig issue.
geo_missing = unique(subset(data_new, is.na(dist), orig))
geo_missing_orig = geo_missing$orig
geo_missing_orig_names = countrycode (sourcevar = geo_missing_orig,
                                      origin = "iso3c",
                                      destination = "country.name",
                                      warn = TRUE, 
                                      nomatch = NA)
geo_missing_orig_2 = countrycode (sourcevar = geo_missing_orig_names,
                                  origin = "country.name",
                                  destination = "iso3c",
                                  warn = TRUE, 
                                  nomatch = NA)

geo = read.csv (file.path(output_path,"geo.csv")) %>%
  mutate (orig_name = countrycode (sourcevar = orig,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE, 
                                   nomatch = NA)) %>%
  filter (orig_name == "Guam")

geo_raw = read.csv (file.path(input_path, "geo_fixed.csv")) %>%
  mutate (orig_name = countrycode (sourcevar = iso_o,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   warn = TRUE, 
                                   nomatch = NA)) %>%
  filter (orig_name == "Romania")
  
# > geo_missing_orig
# [1] "COD" "GUM" "LIE" "MCO" "MNE" "PSE" "ROU" "SRB" "TLS"

# COD = ZAR [Democ. Republic of Congo] [present in CEPII]
# GUM = [Guam] [not present in CEPII]
# LIE = [Liechtenstein] [not present in CEPII]
# MCO = [Monaco] [not present in CEPII]
# MNE = [Montenegro] [logged as YUG (Serbia and Montenegro)] [present in CEPII but problematic]
# PSE = PAL [Palestine] [present in CEPII]
# ROU = ROM [Romania] [present in CEPII]
# SRB = [Serbia] [logged as YUG (Serbia and Montenegro)] [present in CEPII but problematic]
# TLS = TMP [Timor-Leste] [present in CEPII but logged as East-Timor]

# CEPII using outdated countrycodes
  
  

# export CSV
write.csv(data, file.path(output_path,"merged_data_2040.csv"), row.names = FALSE)
write.csv(data_new, file.path(output_path,"merged_data_2040_exp.csv"), row.names = FALSE)

merged_data_2040_exp = read.csv (file.path(output_path,"merged_data_2040_exp.csv"))
geo_missing = unique(subset(data, is.na(dist), orig))
geo_missing_orig = geo_missing$orig
 