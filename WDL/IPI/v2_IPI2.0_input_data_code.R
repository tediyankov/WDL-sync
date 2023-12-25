
## preliminaries ----------------------------------------------------------------

# packages
pacman::p_load(tidyverse,
               countrycode,
               data.table,
               worlddataverse)

devtools::install_github("vdeminstitute/vdemdata")

# paths
base_path <- worlddataverse::get_wdl_path()
if (is.na (base_path)) {base_path = file.path("/Volumes/",
                                              "GoogleDrive-115239951252043738907",
                                              "Shared drives",
                                              "DATA_WDL")}

input_path <- file.path (base_path, "IPI", "phase_2", "Data", "input")
output_path <- file.path (base_path, "IPI", "phase_2", "Data", "output")

# load data
setwd (input_path)
files = list.files (pattern = "*.csv")
for (i in 1:length(files)) assign(files[i], read.csv (files[i]))
rm (files)

## cleaning input data ---------------------------------------------------------

# gsma
gsma = GSMA_data.csv %>%
  dplyr::select (-c(2,3,5)) %>%
  rename_with (
    ~ case_when (
      . == "ISO.Code" ~ "ccode",
      . == "Year" ~ "year",
      . == "Mobile.download.speeds" ~ "download_speed",
      . == "Mobile.upload.speeds" ~ "upload_speed",
      . == "Mobile.latencies" ~ "latency",
      . == "International.Internet.bandwidth.per.user" ~ "bandwidth",
      . == "X2G.Coverage" ~ "2G",
      . == "X3G.Coverage" ~ "3G",
      . == "X4G.Coverage" ~ "4G",
      . == "Inequality" ~ "inequality",
      TRUE ~ .
    )
  )

# itu
itu = ITU_data.csv %>%
  dplyr::select (-c(2:15)) %>%
  pivot_longer (2:10, names_to = "year", values_to = "price_2.5GB") %>%
  mutate_at ("year", str_replace, "X", "") %>%
  transform (year = as.numeric (year)) %>%
  rename_with (
    ~ case_when (
      . == "IsoCode" ~ "ccode",
      . == "X2.5GB_price" ~ "2.5GB_price",
      TRUE ~ .
    )
  )

# wb_electricity
wb_electricity =
  WB_electricity_access.csv [, colnames (WB_electricity_access.csv)
                             %like% paste (c(1990:2021, "Country.Code"),
                                           collapse="|")] %>%
  pivot_longer (2:33, names_to = "year", values_to = "electricity_share") %>%
  mutate_at ("year", str_replace, "X", "") %>%
  transform (year = as.numeric (year)) %>%
  rename_with (
    ~ case_when (
      . == "Country.Code" ~ "ccode",
      TRUE ~ .
    )
  )

# wb_polstab
wb_polstab_raw = WB_politicalstability.csv %>% dplyr::select (-1)
wb_polstab = wb_polstab_raw [, c(TRUE,
                                 subset(wb_polstab_raw[,-1],
                                        wb_polstab_raw$X.1 == "Code") == "Estimate")] %>%
  slice (-1) %>%
  pivot_longer (2:24, names_to = "year", values_to = "polstab") %>%
  mutate_at ("year", str_replace, "X", "") %>%
  transform (year = as.numeric (year)) %>%
  rename_with (
    ~ case_when (
      . == "X.1" ~ "ccode",
      TRUE ~ .
    )
  ) %>%
  mutate (polstab = ifelse (polstab == "#N/A",
                            NA,
                            polstab))

# wb_serv
wb_serv = WB_secureservers.csv [, colnames (WB_secureservers.csv)
                                %like% paste (c(2010:2020, "Country.Code"),
                                              collapse="|")] %>%
  pivot_longer (2:12, names_to = "year", values_to = "servers_per_1m") %>%
  mutate_at ("year", str_replace, "X", "") %>%
  transform (year = as.numeric (year)) %>%
  rename_with (
    ~ case_when (
      . == "Country.Code" ~ "ccode",
      TRUE ~ .
    )
  )

# weo_infl
weo_infl = WEO_inflation.csv %>%
  dplyr::select (-c(4, 25)) %>%
  pivot_longer (4:23, names_to = "year", values_to = "infl") %>%
  mutate_at ("year", str_replace, "X", "") %>%
  transform (year = as.numeric (year)) %>%
  mutate (Subject.Descriptor = ifelse (Subject.Descriptor == "Inflation, average consumer prices", "infl_avgconsp",
                                       ifelse (Subject.Descriptor == "Inflation, end of period consumer prices", "infl_endconsp",
                                               Subject.Descriptor)),
          Units = ifelse (Units == "Index", "index",
                          ifelse (Units == "Percent change", "percent",
                                  Units))) %>%
  unite ("descriptor", Subject.Descriptor:Units, sep = "_", remove = F, na.rm = F) %>%
  dplyr::select (-c("Subject.Descriptor", "Units")) %>%
  pivot_wider (names_from = "descriptor", values_from = "infl") %>%
  dplyr::select (-7) %>%
  rename_with (
    ~ case_when (
      . == "Country" ~ "ccode",
      TRUE ~ .
    )
  ) %>%
  mutate (ccode = countrycode(sourcevar = ccode,
                              origin = "country.name",
                              destination = "iso3c",
                              warn = TRUE,
                              nomatch = NA)) %>%
  filter (!(is.na(ccode))) %>%
  mutate (across (starts_with ('infl'), ~ifelse (.x == "n/a", NA, .x)))

# SSP
ssp = ssp2_urbanization_2020.csv

# WDP
gdp <- readRDS (file.path (base_path,"Mpro_2.0", "01_Data", "R_2022_10_11", "2022_11_02_new_pov_2017ppp", "02_intermediary_data", "03_008_macro_imputed.rds")) %>%
dplyr::select (ccode, year, GDP.PC)

# V_Dem 

metrics <- c('v2x_libdem','v2x_polyarchy', "v2mecenefm", "v2mecenefi", "v2mecenefibin") # liberal democracy metric, electoral democracy metric, Government censorship effort, Internet censorship metric, internet binary
id.vars<-c('country_name', 'COWcode','histname' ,'codingstart_contemp', 'codingend_contemp','year')

vdem <- vdemdata::vdem %>%
  # isolating variables of interest
  dplyr::select (all_of (id.vars), all_of (metrics)) %>%
  # isolating years of interest
  filter (year %in% 2014:2021) %>%
  # reformatting countries
  mutate (ccode = countrycode (country_name, "country.name", "iso3c")) %>%
  # removing countries that did not get match to ISO3 codes
  filter (!(is.na (ccode))) %>%
  # keeping only ccode, year and the vars of interest
  dplyr::select (ccode, year, metrics) %>%
  # rename metrics
  rename_with (
    ~ case_when (
      . == "v2x_libdem" ~ "lib_democ",
      . == "v2x_polyarchy" ~ "elect_democ",
      . == "v2mecenefm" ~ "govt_censorship",
      . == "v2mecenefi" ~ "internet_censorship",
      . == "v2mecenefibin" ~ "internet_binary",
      TRUE ~ .
    )
  ) %>%
  # arranging alphabetically by ccode
  arrange (ccode)

## joining them together -------------------------------------------------------

# staggered join approach
ipi_dat_staggeredjoin = itu %>%
  inner_join (gsma, by = c("ccode", "year")) %>%
  left_join (wb_electricity, by = c("ccode", "year")) %>%
  left_join (wb_polstab, by = c("ccode", "year")) %>%
  left_join (wb_serv, by = c("ccode", "year")) %>%
  left_join (weo_infl, by = c("ccode", "year")) %>%
  left_join (ssp, by = c("ccode", "year")) %>%
  left_join (gdp, by = c("ccode", "year")) %>%
  left_join (vdem, by = c("ccode", "year"))

nas_staggered <- as.data.frame (apply(ipi_dat_staggeredjoin, 2, function(col) sum(is.na(col))/length(col)))

# exporting
write.csv (ipi_dat_staggeredjoin, file.path (output_path, "IPI2.0_inputdata_staggeredjoin.csv"), row.names = F)

