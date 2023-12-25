
## PRELIMINARIES -------------------------------------------------------------------------------------

pacman::p_load(tidyverse,
               stargazer,
               countrycode,
               Hmisc,
               stringr,
               magrittr)

# file paths (for laptop)
base_path <- file.path("/Volumes/","GoogleDrive", "Shared drives", "DATA_WDL")
input_path <- file.path(base_path, 'IOM', "data_input")
output_path <- file.path('IOM', "data_output")
datadir <- file.path(base_path,"IOM","data_output")

## ONE-YEAR FLOWS: flow data -------------------------------------------------------------------------------------

# raw OECD data
raw = read.csv(file.path(input_path, "OECD_usethis.csv")) # notes: contains migrant stock data

# transform data
oecd = raw %>%
  filter (VAR == "B11") %>%
  select (-c("Country.of.birth.nationality", "VAR", "Variable", "GEN", "Gender", "Country", "YEA", "Flag.Codes", "Flags")) %>% 
  rename_with(
    ~ case_when(
      . == "CO2" ~ "orig",
      . == "COU" ~ "dest",
      . == "Year" ~ "year",
      . == "Value" ~ "flow",
      TRUE ~ .
    )
  ) %>% 
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  mutate (year = as.numeric(year))

# sample df w/ NA flows
oecd.na = oecd %>% filter (is.na(flow))

# country pairs with missing NAs
nas = unique(oecd.na$orig)
for (i in nas){
  oecd$flow [oecd$orig == i] <- ifelse (is.na(oecd$flow [oecd$orig == i]),
                                        ave(oecd$flow [oecd$orig == i], 
                                            FUN = function (x) mean (x, na.rm = T)),
                                        oecd$flow [oecd$orig == i])
}

# round replacemnent means
oecd$flow = round(oecd$flow)

# rename into one year flows input
input1 = oecd

# check if NAs fixed 
oecd.na = oecd %>% filter(is.na(flow)) # this should return a df w/ 0 obsv. & 13 var. 
rm (oecd.na, i, nas)

## FIVE-YEAR FLOWS: flow data -------------------------------------------------------------------------------------

# duplicate OECD data
input5 = oecd %>% 
  add_column(flows.sum = NA)

# keep only country pairs with full range of years
input5.n = input5 %>% count (orig.dest)
input5.withnum = merge (input5, input5.n, by = "orig.dest")
input5 = input5.withnum [input5.withnum$n == 20,]
rm (input5.n, input5.withnum)
input5 = input5 %>% select (-c("n"))

# conditional aggregation
input5 = input5 %>% 
  mutate (year = 5*floor(input5$year/5)) %>%
  group_by (orig.dest, year) %>%
  summarise (flows.sum = sum(flow)) %>%
  left_join (oecd, by = c("orig.dest", "year")) %>% 
  select (-c("flow")) %>%
  relocate (orig.dest, orig, dest, year, flows.sum)

# check for NAs
input5.na = input5 %>% filter(is.na(flows.sum)) # should return 0 observations
rm(input5.na)

## ONE-YEAR FLOWS: expanding w/ regressors -------------------------------------------------------------------------------------

# load data

# GDP per capita
mpro <- readRDS(file.path(base_path, "./Mpro_PC/2021_July/02_output_data/X9_3_mc_remerge_mixed_2021-09-06.RDS"))
options(scipen = 999) # coerce non-scientific notation
gdp = mpro$macro.data %>%
  select (c(ccode, year, GDP.PC.PPP)) %>%
  filter(year %in% 2000:2019) %>%
  rename_with(
    ~ case_when(
      . == "ccode" ~ "country",
      . == "GDP.PC.PPP" ~ "gdp.pc",
      TRUE ~ .
    )
  )

gdp.orig = gdp %>% rename (orig = country,
                           gdp.pc.orig = gdp.pc)
gdp.dest = gdp %>% rename (dest = country,
                           gdp.pc.dest = gdp.pc)

# migration stock
migstock = raw %>%
  filter (VAR == "B14") %>%
  select (-c("Country.of.birth.nationality", "VAR", "Variable", "GEN", "Gender", "Country", "YEA", "Flag.Codes", "Flags")) %>% 
  rename_with(
    ~ case_when(
      . == "CO2" ~ "orig",
      . == "COU" ~ "dest",
      . == "Year" ~ "year",
      . == "Value" ~ "migstock",
      TRUE ~ .
    )
  ) %>% 
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  mutate (year = as.numeric(year)) %>%
  select (-c("orig", "dest"))

# population
pop = mpro$macro.data %>%
  select (c(ccode, year, pop)) %>%
  filter(year %in% 2000:2019) %>%
  rename_with(
    ~ case_when(
      . == "ccode" ~ "country",
      . == "pop" ~ "population",
      TRUE ~ .
    )
  )

pop.orig = pop %>% rename (orig = country,
                           pop.orig = population)

pop.dest = pop %>% rename (dest = country,
                           pop.dest = population)

# lagged flows
lag = input1 %>% select(-c(5)) # creating df 

lag$lag.1yr = Lag(input1$flow, -1) # lagging by 1 year
lag$lag.1yr = ifelse (lag$year == 2019,
                      NA,
                      lag$lag.1yr)

lag$lag.2yr = Lag(input1$flow, -2) # lagging by 2 years
lag$lag.2yr = ifelse (lag$year %in% 2018:2019,
                      NA,
                      lag$lag.2yr)

lag$lag.3yr = Lag(input1$flow, -3) # lagging by 2 years
lag$lag.3yr = ifelse (lag$year %in% 2017:2019,
                      NA,
                      lag$lag.3yr)

lag$lag.4yr = Lag(input1$flow, -4) # lagging by 2 years
lag$lag.4yr = ifelse (lag$year %in% 2016:2019,
                      NA,
                      lag$lag.4yr)

lag$lag.5yr = Lag(input1$flow, -5) # lagging by 2 years
lag$lag.5yr = ifelse (lag$year %in% 2015:2019,
                      NA,
                      lag$lag.5yr)

lag = lag %>% select (-c("orig", "dest"))

# distance, contig, comland_off
dist = read.csv(file.path(base_path,'IOM','gravity_model_initial',"data","gravity_input_v3.csv")) %>%
  select (c(1,2,9,10,12)) %>%
  relocate (orig, .before = dest) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>% 
  relocate (orig.dest, .before = orig) %>%
  distinct() %>%
  select (-c("orig", "dest"))

# infant mortality
infmor = read.csv(file.path(input_path, "infantmortality.csv")) %>% 
  select(c(2, 45:64)) %>%
  rename (country = Country.Code) %>%
  pivot_longer(!country, names_to = "year", values_to = "inf.mort") %>%
  transform (year = str_replace(year, "X","")) %>%
  mutate (year = as.numeric(year))

infmor.orig = infmor %>% rename (orig = country, 
                                 inf.mort.orig = inf.mort)
infmor.dest = infmor %>% rename (dest = country, 
                                 inf.mort.dest = inf.mort)

# dependency ratio
deprat = read.csv(file.path(input_path, "dependencyratio.csv")) %>%
  select(2, 45:64) %>%
  rename (country = Country.Code) %>%
  pivot_longer(!country, names_to = "year", values_to = "dep.ratio") %>%
  transform (year = str_replace(year, "X","")) %>%
  mutate (year = as.numeric(year))

deprat.orig = deprat %>% rename (orig = country,
                                 dep.ratio.orig = dep.ratio)

deprat.dest = deprat %>% rename (dest = country,
                                 dep.ratio.dest = dep.ratio)

# merging regressors with input data
input1full = input1 %>%
  left_join(gdp.orig, by = c("orig", "year")) %>%
  left_join(gdp.dest, by = c("dest", "year")) %>%
  left_join(migstock, by = c("orig.dest", "year")) %>%
  left_join(pop.orig, by = c("orig", "year")) %>%
  left_join(pop.dest, by = c("dest", "year")) %>%
  left_join(lag, by = c("orig.dest", "year")) %>%
  left_join(dist, by = "orig.dest") %>%
  left_join(infmor.orig, by = c("orig", "year")) %>%
  left_join(infmor.dest, by = c("dest", "year")) %>%
  left_join(deprat.orig, by = c("orig", "year")) %>%
  left_join(deprat.dest, by = c("dest", "year")) %>%
  relocate ("orig.dest","orig","dest","year","flow","lag.1yr","lag.2yr","lag.3yr","lag.4yr","lag.5yr","migstock","dist","contig","comlang_off","pop.orig","pop.dest","gdp.pc.orig","gdp.pc.dest","inf.mort.orig","inf.mort.dest","dep.ratio.orig","dep.ratio.dest")

## FIVE-YEAR FLOWS: expanding w/ regressors -------------------------------------------------------------------------------------

# merging regressors with input data
input5full = input5 %>%
  left_join(gdp.orig, by = c("orig", "year")) %>%
  left_join(gdp.dest, by = c("dest", "year")) %>%
  left_join(migstock, by = c("orig.dest", "year")) %>%
  mutate (migstock = as.numeric (migstock)) %>%
  left_join(pop.orig, by = c("orig", "year")) %>%
  left_join(pop.dest, by = c("dest", "year")) %>%
  left_join(dist, by = "orig.dest") %>%
  left_join(infmor.orig, by = c("orig", "year")) %>%
  left_join(infmor.dest, by = c("dest", "year")) %>%
  left_join(deprat.orig, by = c("orig", "year")) %>%
  left_join(deprat.dest, by = c("dest", "year")) %>%
  relocate ("orig.dest","orig","dest","year","flows.sum","migstock","dist","contig","comlang_off","pop.orig","pop.dest","gdp.pc.orig","gdp.pc.dest","inf.mort.orig","inf.mort.dest","dep.ratio.orig","dep.ratio.dest")




