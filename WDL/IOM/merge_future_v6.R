
## loading data

IOM_load = function () {
  
  # packages
  pacman::p_load (worlddataverse, tidyverse, countrycode, purrr, Hmisc)
  require ("worlddataverse", "tidyverse", "countrycode", "purrr")
  
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
  
}

IOM_load()

## flows data

flows_clean = function () {
  
  # cleaning OECD flows data
  oecd = merge_future_2040_exp %>% 
    select (orig.dest.year, orig.dest, orig, dest, year, flow_full_ts) %>%
    rename (flow_OECD = flow_full_ts)
  
  oecd_complete_cases = oecd %>%
    filter (year %in% 2000:2019)  %>%
    drop_na ()
  
  oecd = oecd %>% filter (orig.dest %in% unique (oecd_complete_cases$orig.dest))
  
  # cleaning Abel flows data
  abel_1 = abel_raw %>%
    select (1:3, 8) %>%
    rename (year = year0) %>%
    relocate (year, .after = dest) %>%
    unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F) %>%
    relocate (orig.dest, .before = orig) %>%
    relocate (orig.dest.year, .before = orig.dest) %>%
    rename (flow_ABEL = da_min_closed) %>%
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
  
  abel = abel_1 [!(abel_1$orig == abel_1$dest),]
  
  # merging together 
  oecd_orig = unique (oecd$orig)
  abel_orig = unique (abel$orig)
  
  origfactor <- as.factor (union (oecd_orig, abel_orig))
  destfactor <- as.factor (unique(abel$dest))
  yearfactor <- as.factor (seq(1990, 2040, 1))
  
  cart <- expand.grid (orig = origfactor,
                       dest = destfactor,
                       year = yearfactor)
  
  cart = cart %>%
    mutate (orig = as.character(orig),
            dest = as.character(dest),
            year = as.character(year)) %>%
    mutate (year = as.numeric(year)) %>%
    unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
    unite ("orig.dest.year", orig:year, sep = "_", remove = F, na.rm = F)
  
  flow <- purrr::reduce(list(cart, oecd, abel), 
                        dplyr::left_join, by = c("orig.dest.year", "orig.dest", "orig", "dest", "year")) %>% 
    arrange (orig.dest) %>%
    select (-orig.dest.year) %>%
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
  
  .GlobalEnv$flow = flow
  
}

flows_clean()

## stock data

stocks_exp = function () {
  
  # keep stocks (country of birth, interpolated)
  stocks_1 = merge_future_2040_exp %>% 
    select (1:5, 12) %>%
    rename (migStock_OECD = migstock_bir_int)
  
  stocks_2 = stocks_1 [!(stocks_1$orig == stocks_1$dest), ]
  
  # merge stocks_1 with flows data
  flow_1 = flow %>%
    left_join (stocks_2, by = c("orig.dest", "orig", "dest", "year")) 
  
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
  
  flow_2 = flow_1 %>% left_join (UN_stocks_2, by = c("orig.dest", "orig", "dest", "year"))
  
  data = flow_2
  
  data1 = data %>% unite ("orig.dest.year", c(orig,dest,year), sep = "_", remove = F, na.rm = F)
  
  data2 = data1 %>% filter (duplicated(orig.dest.year))
  
  data2_orig.dest.year = as.vector (data2$orig.dest.year)
  
  data3 = data1 %>% filter (orig.dest.year %in% data2_orig.dest.year)
  
  data4 = data3 %>% filter (!(duplicated(orig.dest.year, fromLast = T)))
  
  data5 = data1 %>% filter (!(orig.dest.year %in% data2_orig.dest.year))
  
  data6 = rbind (data5, data4) %>%
    mutate (migStock_UN = as.numeric (migStock_UN))
  
  .GlobalEnv$flow_stock = data6
  
}

stocks_exp ()

## regressor data

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
  
  data = flow_stock %>%
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

## final touches

data_final = data [!(data$orig == data$dest),] %>%
  distinct ()

# checking if any OECD orig / dest countries not in Abel data

oecd_orig_unique <- as.vector (merge_future_2040_exp$orig)
oecd_dest_unique <- as.vector (merge_future_2040_exp$dest)

abel_orig_unique <- as.vector (abel_1$orig)
abel_dest_unique <- as.vector (abel_1$dest)

orig_setdiff = setdiff (oecd_orig_unique, abel_orig_unique)
dest_setdiff = setdiff (oecd_dest_unique, abel_dest_unique)

## orig_setdiff = "AND" "BMU" "COK" "DMA" "KNA" "LIE" "MCO" "MHL" "NIU" "NRU" "PLW" "SMR" "TKL" "TUV" "TWN"
## dest_setdiff = NA

write.csv (data_final, file.path (output_path, "v4_flows_OECDabel_withStocks_exp.csv"), row.names = FALSE)

