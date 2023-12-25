
## PRELIMINARIES ------------------------------------------------------------------------------------

pacman::p_load(reshape2,
               tidyverse,
               worlddataverse,
               zoo,
               readxl,
               openxlsx,
               data.table,
               Metrics,
               stargazer,
               countrycode,
               gravity)

## file paths

base_path <- file.path("/Volumes/","GoogleDrive", "Shared drives", "DATA_WDL")

input_path <- file.path(base_path,
                        'IOM',
                        'gravity_model_initial'
                        )

output_path <- file.path('IOM',
                         'gravity_model_initial',
                         "data"
                         )

datadir <- file.path(base_path,"IOM","data_output")

## FIVE YEAR FLOWS ------------------------------------------------------------------------------------------------

## Load data

OECD.five <- read.csv("OECD_pp.csv") %>%
  filter (Variable == "Inflows of foreign population by nationality") %>%
  select (-c(Country.of.birth.nationality, VAR, Variable, GEN, Gender, Country, YEA, Flag.Codes, Flags)) %>%
  rename (orig = CO2, dest = COU, flows = Value, year = Year) %>%
  mutate (year = as.numeric(year)) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  add_column (flow.sums = NA) %>%
  relocate (orig, dest, orig.dest, year, flows) %>%
  distinct ()

## keep only country pairs with data for full range of years (2000:2019)

OECD.n <- OECD.five %>% count (orig.dest)
OECD.withnum = merge (OECD.five,
                      OECD.n,
                      by = "orig.dest")
OECD.five <- OECD.withnum[OECD.withnum$n == 20,]

## Conditional aggregation of one-year sums into five year sums

OECD.five <- OECD.five %>%
  mutate (year = 5*ceiling(OECD.five$year/5)) %>%
  group_by (orig.dest, year) %>%
  summarise (flows.sums = sum(flows))

## merging with variable data

data <- read.csv(file.path(input_path, "data","gravity_input_v3.csv"))

data = data %>%
  select(-c(sex,
            flow,
            flow_lagged,
            migrant_stock)) %>%
  relocate (orig, .before = dest) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  rename (year = year0) %>%
  mutate (year = as.numeric(year)) %>%
  filter (year != 1995) %>%
  right_join (OECD.five, by = c("orig.dest", "year")) %>%
  relocate (flows.sums, .before = contig) %>%
  rename (flows = flows.sums) %>%
  distinct()

## additional variables

# load age breakdown
age_breakdown <- read.csv(file.path(input_path,"data","un_shares_0_64.csv")) %>%
  select(iso3, year0, age_0_64) %>%
  rename(country = iso3,
         year = year0)

# load middle class percent
midclass <- read.csv(file.path(input_path,"data","mid_class_percent.csv")) %>%
  rename_with(
    ~ case_when(
      . == "year0" ~ "year",
      . == "iso3" ~ "country",
      . == "midclasspercent" ~ "midclass.percent",
      TRUE ~ .
    )
  ) %>%
  select(c(2:4))

# load education data
education <- read.csv(file.path(input_path,"data","education_postsec.csv")) %>%
  select(c(2,3,4)) %>%
  rename_with(
    ~ case_when(
      . == "year0" ~ "year",
      . == "iso3" ~ "country",
      . == "post_secondary_education" ~ "post.sec.edu",
      TRUE ~ .
    )
  )

# load GDP data
mpro <- readRDS(file.path(base_path, "./Mpro_PC/2021_July/02_output_data/X9_3_mc_remerge_mixed_2021-09-06.RDS"))
options(scipen = 999) # coerce non-scientific notation
gdp = mpro$macro.data %>%
  select (c(ccode, year, pop, GDP.PPP, GDP.PC.PPP)) %>%
  filter(year %in% 2000:2019) %>%
  rename_with(
    ~ case_when(
      . == "ccode" ~ "country",
      . == "pop" ~ "population",
      . == "GDP.PPP" ~ "gdp",
      . == "GDP.PC.PPP" ~ "gdp.pc",
      TRUE ~ .
    )
  )

# load unemployment data (not included in the models yet!!!)
unemployment <- read.csv(file.path(datadir, "API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_3469538.csv")) %>%
  select (-c("Country.Name", "Indicator.Name", "Indicator.Code")) %>%
  select () %>%
  rename_with(
    ~ case_when(
      . == "ccode" ~ "country",
      . == "pop" ~ "population",
      . == "GDP.PPP" ~ "gdp",
      . == "GDP.PC.PPP" ~ "gdp.pc",
      TRUE ~ .
    )
  )

# load fertility data (not indluced in the models yet)

# execute merge

midclass.orig = midclass %>%
  rename (orig = country,
          midclass.orig = midclass.percent)

midclass.dest = midclass %>%
  rename (dest = country,
          midclass.dest = midclass.percent)

education.orig = education %>%
  rename (orig = country,
          edu.orig = post.sec.edu) # remember to include in codebook that this is post-secondary education.

education.dest = education %>%
  rename (dest = country,
          edu.dest = post.sec.edu)

age.orig = age_breakdown %>%
  rename (orig = country,
          age0.64.orig = age_0_64)

age.dest = age_breakdown %>%
  rename (dest = country,
          age0.64.dest = age_0_64)

data.exp = data %>% # dataset with the futher variable data added
  left_join (midclass.orig, by = c("orig", "year")) %>%
  left_join (midclass.dest, by = c("dest", "year")) %>%
  left_join (education.orig, by = c("orig", "year")) %>%
  left_join (education.dest, by = c("dest", "year")) %>%
  left_join (age.orig, by = c("orig", "year")) %>%
  left_join (age.dest, by = c("dest", "year")) %>%
  mutate (population_dest = as.numeric(population_dest),
          population_orig = as.numeric(population_orig)) %>%
  rename_with(
    ~ case_when(
      . == "population_dest" ~ "population.dest",
      . == "population_orig" ~ "population.orig",
      . == "orig_code" ~ "orig.code",
      . == "dest_code" ~ "dest.code",
      . == "comlang_off" ~ "comlang.off",
      . == "landlocked_orig" ~ "landlocked.orig",
      . == "landlocked_dest" ~ "landlocked.dest",
      . == "gdp_pc_dest" ~ "gdp.pc.dest",
      . == "gdp_pc_orig" ~ "gdp.pc.orig",
      TRUE ~ .
    )
  ) %>%
  distinct()

data.input5 = data.exp %>% drop_na() # dataset where every country pairing has full data (no NAs)

## ONE YEAR FLOWS -------------------------------------------------------------------------------------

# gdp and population data
mpro <- readRDS(file.path(base_path, "./Mpro_PC/2021_July/02_output_data/X9_3_mc_remerge_mixed_2021-09-06.RDS"))
options(scipen = 999) # coerce non-scientific notation
gdp = mpro$macro.data %>%
  select (c(ccode, year, pop, GDP.PPP, GDP.PC.PPP)) %>%
  filter(year %in% 2000:2019) %>%
  rename_with(
    ~ case_when(
      . == "ccode" ~ "country",
      . == "pop" ~ "population",
      . == "GDP.PPP" ~ "gdp",
      . == "GDP.PC.PPP" ~ "gdp.pc",
      TRUE ~ .
    )
  )

gdp.orig = gdp %>%
  rename (orig = country)

gdp.dest = gdp %>%
  rename (dest = country)

# distance data
data <- read.csv(file.path(input_path, "data","gravity_input_v3.csv")) %>% 
  relocate (orig, .before = dest) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  rename (year = year0) %>%
  mutate (year = as.numeric(year)) %>%
  filter (year != 1995) %>%
  select(c("orig.dest", "contig", "comlang_off", "colony", "dist", "col45", "landlocked_orig", "landlocked_dest")) %>%
  distinct()

# merging with flows data to create one-year input  
data.input1 <- read.csv("OECD_pp.csv") %>%
  filter (Variable == "Inflows of foreign population by nationality") %>%
  select (-c(Country.of.birth.nationality, VAR, Variable, GEN, Gender, Country, YEA, Flag.Codes, Flags)) %>%
  rename (orig = CO2, dest = COU, flows = Value, year = Year) %>%
  mutate (year = as.numeric(year)) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  relocate (orig, dest, orig.dest, year, flows) %>%
  distinct () %>%
  left_join(gdp.orig, by = c("orig", "year")) %>%
  left_join(gdp.dest, by = c("dest", "year")) %>%
  left_join(data, by = "orig.dest") %>%
  rename_with(
    ~ case_when(
      . == "population.x" ~ "population.orig",
      . == "gdp.x" ~ "gdp.orig",
      . == "gdp.pc.x" ~ "gdp.pc.orig",
      . == "population.y" ~ "population.dest",
      . == "gdp.y" ~ "gdp.dest",
      . == "gdp.pc.y" ~ "gdp.pc.dest",
      TRUE ~ .
    )
  ) %>%
  relocate (dist, .before = population.orig) %>%
  drop_na() %>%
  distinct()

