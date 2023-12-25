
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

}

IOM_load()

## chapter 2: flows data -------------------------------------------------------

## sub-chapter 2.1: OECD data --------------------------------------------------

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
    rename (year = year0)

  # third cartesian product using Abel data
  origfactor <- as.factor (unique(abel_1$orig))
  destfactor <- as.factor (unique(flows_4$dest))
  yearfactor <- as.factor (seq(2000, 2040, 1))
  
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
  
  # placeholder df with flows_5 gaps
  flows_5_nas = flows_5 %>% filter (is.na(flow))
  
  # filling the gaps with Abel's data
  flows_5_nas_fill = flows_5_nas %>%
    select (-c("flow")) %>%
    left_join (abel_1,  by = c("orig", "dest", "year"))
  
  # merging back with original flows data
  flows_6 = flows_5 %>%
    left_join (flows_5_nas_fill)

}


## sub-chapter 2.2: merging with Abel's data -----------------------------------
