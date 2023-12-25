
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
  oecdflows_raw = read.csv (file.path(input_path, "OECD_mig_data.csv"))
  .GlobalEnv$oecdflows_raw = oecdflows_raw
  
  abel_raw = read.csv (file.path(base_path, "IOM", "gravity_model_initial", "data","gravity_input_v3.csv"))
  .GlobalEnv$abel_raw = abel_raw
  
  merge_future_2040_exp = read.csv (file.path (output_path,"merged_data_2040_exp.csv"))
  .GlobalEnv$merge_future_2040_exp = merge_future_2040_exp
  
  UN_stocks = read.csv ("UN_stocks.csv") # remember to add the file path to G-Drive
  .GlobalEnv$UN_stocks = UN_stocks
  
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
  
  # interpolating with in-group mean
  flows.na = flows_2 %>% filter(is.na(flow))
  nas = unique(flows.na$orig.dest)
  
  for (i in nas) {
    
    flows_2$flow [flows_2$orig.dest == i] =
      ifelse (is.na (flows_2$flow [flows_2$orig.dest == i]),
              ave (flows_2$flow [flows_2$orig.dest == i], FUN = function (x) mean (x, na.rm = T)),
              flows_2$flow [flows_2$orig.dest == i])
    
  }
  
  # rounding new values
  flows_2$flow = round(flows_2$flow)
  
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
    select (3,2,1,7) %>%
    rename (year = year0) %>%
    relocate (year, .after = dest) %>%
    unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F)
  
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
    
    # code to deal with duplicate orig.dest.year groups
    unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
    filter (!(duplicated(orig.dest.year, fromLast = T)))
  
  # placeholder df for gaps in OECD flows
  flows_5_nas = flows_5 %>% filter (is.na(flow))
  
  # filling the gaps with Abel's data
  flows_5_nas_fill = flows_5_nas %>%
    select (-c("flow")) %>%
    left_join (abel_1,  by = c("orig.dest.year","orig", "dest", "year"))
  
  # merging back with original flows data
  flows_6 = flows_5 %>% drop_na()
  flows_7 = rbind (flows_6, flows_5_nas_fill) %>% arrange (orig.dest)
  
  # fourth cartesian product to expand year range
  origfactor <- as.factor (unique(flows_7$orig))
  destfactor <- as.factor (unique(flows_7$dest))
  yearfactor <- as.factor (seq(2000, 2040, 1))
  
  cart_4 <- expand.grid (orig = origfactor,
                         dest = destfactor,
                         year = yearfactor)
  
  flows_8 = cart_4 %>%
    mutate (orig = as.character(orig),
            dest = as.character(dest),
            year = as.character(year)) %>%
    mutate (year = as.numeric(year)) %>%
    left_join(flows_7, by = c("orig", "dest", "year")) %>% 
    unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    relocate (orig.dest, orig, dest, year, flow) %>%
    
    # code to deal with duplicate orig.dest.year groups
    unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
    filter (!(duplicated(orig.dest.year, fromLast = T)))
  
  .GlobalEnv$flows = flows_8
  
}

## chapter 3: adding stocks data -----------------------------------------------

stocks_exp = function () {
  
  # keep stocks (country of birth, interpolated)
  stocks_1 = merge_future_2040_exp %>% 
    select (1:5, 12)
  
  # merge stocks_1 with flows data
  flows_9 = flows %>%
    left_join (stocks_1, by = c("orig.dest.year", "orig.dest", "orig", "dest", "year")) %>%
    rename (orig_dest = orig.dest)
  
  # merging with UN stocks [REMEMBER TO ADD FILE PATH] (first formatting stocks data)
  UN_stocks = read.csv ("UN_stocks.csv") 
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
    select ("orig", "dest", "2000", "2005", "2010", "2015", "2020") %>%
    unite ("orig_dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    select (-c("orig", "dest")) %>%
    pivot_longer (!orig_dest, names_to = "year", values_to = "migStock") %>%
    mutate (year = as.numeric (year))
  
  # actually merging with UN stock data 
  flows_9_migstockNas = flows_9 %>% 
    filter (is.na (migstock_bir_int)) %>%
    rename (migStock = migstock_bir_int) 

  flows_9_migstockNas_filled = flows_9_migstockNas %>%
    select (-c("migStock")) %>%
    left_join (UN_stocks_1, by = c("orig_dest", "year")) 
  
  flows_9_migstockNas_filled$migStock = as.numeric (as.character (gsub (" ","", flows_9_migstockNas_filled$migStock)))
  
  flows_10 = flows_9 %>%
    left_join (flows_9_migstockNas_filled, by = c("orig_dest", "orig.dest.year", "orig", "dest", "year", "flow"))
  
  flows_9_migstockNas_removed = flows_9 %>% 
    drop_na() %>% 
    rename (migStock = migstock_bir_int)
  
  flows_10 = rbind (flows_9_migstockNas_removed, flows_9_migstockNas_filled)
  
  .GlobalEnv$flows_withStocks = flows_10
}

stocks_exp()

## chapter 4: expanding by regressors ------------------------------------------

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
    filter (year %in% 2000:2040) %>%
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
  flows_withStocks = flows_withStocks %>%
    rename (orig.dest = orig_dest)
  
  data = flows_withStocks %>%
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

write.csv (data, file.path (output_path, "flows_OECDabel_withStocks.csv"), row.names = FALSE)



