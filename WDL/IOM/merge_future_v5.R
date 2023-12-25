
## chapter 1: preliminaries ----------------------------------------------------

## loading data

IOM_load = function () {
  
  # packages
  pacman::p_load (worlddataverse, tidyverse, countrycode)
  require ("worlddataverse", "tidyverse", "countrycode")
  
  # base path to Google Drive
  base_path <- worlddataverse::get_wdl_path()
  
  if(is.na(base_path)) {
    base_path = file.path ("/Volumes/","GoogleDrive-115239951252043738907","Shared drives","DATA_WDL")
  }
  
  # input and output paths + save to Global Environment
  input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data")
  output_path <- file.path (base_path, "IOM", "2nd_paper", "output")
  
  .GlobalEnv$base_path = base_path
  .GlobalEnv$input_path = input_path
  .GlobalEnv$output_path = output_path
  
  # load data files
  
  abel_raw = read.csv (file.path(input_path, "ABEL_flows_2020.csv"))
  .GlobalEnv$abel_raw = abel_raw
  
  merge_future_2040_exp = read.csv (file.path (output_path,"merged_data_2040_exp.csv"))
  .GlobalEnv$merge_future_2040_exp = merge_future_2040_exp
  
  UN_stocks = read.csv ("UN_stocks.csv") # remember to add the file path to G-Drive
  .GlobalEnv$UN_stocks = UN_stocks
  
  oecdflows_raw = read.csv (file.path (input_path, "OECD_mig_data.csv"))
  
}

IOM_load()

## chapter 2: flows data -------------------------------------------------------

flows_clean = function () {
  
  # first preprocess
  flows_1 = oecdflows_raw %>%
    
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
  origfactor <- as.factor (unique(flows_1$orig))
  destfactor <- as.factor (unique(flows_1$dest))
  yearfactor <- as.factor (unique(flows_1$year))
  cart_1 <- expand.grid (orig = origfactor,
                         dest = destfactor,
                         year = yearfactor)
  
  # merge flows_1 with first cartesian products
  flows_2 = cart_1 %>%
    
    mutate (orig = as.character(orig),
            dest = as.character(dest),
            year = as.character(year)) %>%
    mutate (year = as.numeric(year)) %>%
    left_join(flows_1, by = c("orig", "dest", "year")) %>%
    unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    relocate (orig.dest, orig, dest, year, flow) %>%
    
    # code to deal with duplicate orig.dest.year groups
    unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
    filter (!(duplicated(orig.dest.year, fromLast = T)))
  
  # second cartesian product
  
  origfactor <- as.factor (unique(flows_2$orig))
  destfactor <- as.factor (unique(flows_2$dest))
  yearfactor <- as.factor (seq(2000, 2040, 1))
  
  cart_2 <- expand.grid (orig = origfactor,
                         dest = destfactor,
                         year = yearfactor)
  
  # merge flows (means interpolated) with second cartesian product
  
  flows_3 = cart_2 %>%
    
    mutate (orig = as.character(orig),
            dest = as.character(dest),
            year = as.character(year)) %>%
    mutate (year = as.numeric(year)) %>%
    left_join(flows_2, by = c("orig", "dest", "year")) %>%
    unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    relocate (orig.dest, orig, dest, year, flow) %>%
    
    # code to deal with duplicate orig.dest.year groups
    unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
    filter (!(duplicated(orig.dest.year, fromLast = T)))
  
  # removing leftover unobserved country pairs
  flows_2020missing = flows_3 %>%
    filter (year %in% 2000:2019) %>%
    filter(!(is.na(flow)))
  
  flows_4 = flows_3 %>%
    filter(orig.dest %in% unique(flows_2020missing$orig.dest))
  
  # third cartesian product using Abel data (first cleaning Abel data)
  abel_1 = abel_raw %>%
    select (1:3, 8) %>%
    rename (year = year0) %>%
    relocate (year, .after = dest) %>%
    unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
    relocate (orig.dest, .before = orig) %>%
    relocate (orig.dest.year, .before = orig.dest) %>%
    rename (flow_ABEL = da_min_closed)
  
  abel_2 = abel_1 [!(abel_1$orig == abel_1$dest),]
  
  # third cartesian product using Abel data
  origfactor <- as.factor (unique(abel_1$orig))
  destfactor <- as.factor (unique(abel_1$dest))
  yearfactor <- as.factor (seq(1990, 2040, 1))
  
  cart_3 <- expand.grid (orig = origfactor,
                         dest = destfactor,
                         year = yearfactor)
  
  # merging OECD flows with third cartesian product
  flows_5 = cart_3 %>%
    mutate (orig = as.character(orig),
            dest = as.character(dest),
            year = as.character(year)) %>%
    mutate (year = as.numeric(year)) %>%
    
    left_join(flows_4, by = c("orig", "dest", "year")) %>%
    unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    relocate (orig.dest, orig, dest, year, flow) %>%
    rename (flow_OECD = flow) %>%
    
    # code to deal with duplicate orig.dest.year groups
    unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
    filter (!(duplicated(orig.dest.year, fromLast = T)))
  
  # adding in Abel flows
  flows_6 = flows_5 %>%
    left_join (abel_2, by = c("orig.dest.year","orig.dest","orig","dest","year"))
  
  flows_7 = flows_6 %>%
    mutate (orig = countrycode(sourcevar = orig,
                               origin = "iso3c",
                               destination = "iso3c",
                               warn = TRUE,
                               nomatch = NA)) %>%
    mutate (dest = countrycode(sourcevar = dest,
                               origin = "iso3c",
                               destination = "iso3c",
                               warn = TRUE,
                               nomatch = NA)) %>%
    filter (!(is.na(orig))) %>%
    filter (!(is.na(dest)))

  .GlobalEnv$flows = flows_7
  
}

flows_clean()

## chapter 3: adding stocks data -----------------------------------------------

stocks_exp = function () {
  
  # keep stocks (country of birth, interpolated)
  stocks_1 = merge_future_2040_exp %>% 
    select (1:5, 12) %>%
    rename (migStock_OECD = migstock_bir_int)
  
  # merge stocks_1 with flows data
  flows_8 = flows %>%
    left_join (stocks_1, by = c("orig.dest.year", "orig.dest", "orig", "dest", "year")) 
  
  UN_stocks_1 = UN_stocks %>%
    mutate (orig = countrycode(sourcevar = orig,
                               origin = "country.name",
                               destination = "iso3c",
                               warn = TRUE,
                               nomatch = NA)) %>%
    mutate (dest = countrycode(sourcevar = dest,
                               origin = "country.name",
                               destination = "iso3c",
                               warn = TRUE,
                               nomatch = NA)) %>%
    filter (!(is.na(orig))) %>%
    filter (!(is.na(dest))) %>%
    select ("orig", "dest", "X2000", "X2005", "X2010", "X2015", "X2020") %>%
    unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    pivot_longer (!c("orig.dest", "orig", "dest"), names_to = "year", values_to = "migStock_UN") %>%
    mutate (year = sub ("X", "", year)) %>%
    mutate (year = as.numeric (year))
  
  UN_stocks_2 = UN_stocks_1 [!(UN_stocks_1$orig == UN_stocks_1$dest),]
  
  flows_9 = flows_8 %>% left_join (UN_stocks_2, by = c("orig.dest", "orig", "dest", "year"))
  
  .GlobalEnv$flows_wMigStock = flows_9

}

stocks_exp ()

## chapter 4: adding regressor data --------------------------------------------

regressors = function () {
  
  # population
  
  pop_total = read.csv(file.path (base_path, "IOM", "2nd_paper", "input_data", "WPP_pop_age.csv")) %>%
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
    relocate (country, year, PopTotal) %>%
    mutate (year = as.numeric(year)) %>%
    group_by (country,year) %>%
    summarise (pop = sum(PopTotal)) %>%
    arrange(country)
  
  # dependency ratio
  
  pop_age <- read.csv(file.path (base_path, "IOM", "2nd_paper", "input_data", "WPP_pop_age.csv")) %>%
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
  
  infant_mort <- read.csv(file.path (base_path, "IOM", "2nd_paper", "input_data", "WPP_pop_indicators.csv")) %>%
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
  
  # diverging single-country data into twin datasets for orig and dest
  
  pop_total_orig = pop_total %>% rename (orig = country)
  pop_total_dest = pop_total %>% rename (dest = country)
  
  dep_ratio_orig = dep_ratio %>% rename (orig = country)
  dep_ratio_dest = dep_ratio %>% rename (dest = country)
  
  infant_mort_orig = infant_mort %>% rename (orig = country)
  infant_mort_dest = infant_mort %>% rename (dest = country)
  
  gdp_orig = gdp %>% rename (orig = country)
  gdp_dest = gdp %>% rename (dest = country)
  
  data = flows_wMigStock %>%
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
  
  .GlobalEnv$data = data
  
}

regressors ()

write.csv (data, file.path (output_path, "v3_flows_OECDabel_withStocks_exp.csv"), row.names = FALSE)

# addressing Jakob's questions

data = read.csv (file.path (output_path, "v3_flows_OECDabel_withStocks_exp.csv"))

nrow(data) == length(unique(data$orig))*length(unique(data$dest))*length(unique(data$year)) # equality does not hold

origfactor <- as.factor (unique(data$orig))
destfactor <- as.factor (unique(data$dest))
yearfactor <- as.factor (unique(data$year))

cart_test <- expand.grid (orig = origfactor,
                       dest = destfactor,
                       year = yearfactor)

cart_test = cart_test %>%
  mutate (orig = as.character(orig),
          dest = as.character(dest),
          year = as.character(year)) %>%
  mutate (year = as.numeric(year)) %>%
  unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F)

data_test = data %>%
  select (c("orig", "dest", "year")) %>%
  unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) 

cart_test_vector = as.vector (cart_test$orig.dest.year)
data_test_vector = as.vector (data_test$orig.dest.year)

setdiff (data_test_vector, cart_test_vector) 

# returned character(0), so issue must be with repetitions

data1 = data %>% filter (duplicated(orig.dest.year))
data2 = data %>% filter (orig.dest.year == "CHN_CUW_2000")
data3 = data %>% filter (orig.dest.year == "CHN_CUW_2005")

data1_orig.dest.year = as.vector (data1$orig.dest.year)

data4 = data %>% filter (orig.dest.year %in% data1_orig.dest.year)

data5 = data4 %>%
  filter (!(duplicated(orig.dest.year, fromLast = T)))

data6 = data %>% filter (!(orig.dest.year %in% data1_orig.dest.year))

data7 = rbind (data6, data5)

nrow(data7) == length(unique(data7$orig))*length(unique(data7$dest))*length(unique(data7$year))

# this now returns TRUE! The issue was in the raw UN data - there were coded duplicates - I kept the ones which appeared in the data itself, the source of the others is unknown. 
