
## preliminaries  --------------------------------------------------------------

# packages
pacman::p_load (tidyverse,countrycode, Hmisc)

# paths
base_path <- file.path ("/Volumes/","GoogleDrive", "Shared drives", "DATA_WDL")
input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data")
output_path <- file.path (base_path, "IOM", "2nd_paper", "output")
mpro <- readRDS(file.path(base_path, "./Mpro_PC/2021_July/02_output_data/X9_3_mc_remerge_mixed_2021-09-06.RDS"))

## preprocessing GDP data ------------------------------------------------------

gdp = mpro$macro.data %>% 
  mutate (country = countrycode(sourcevar = ccode,
                                origin = "iso3c",
                                destination = "iso3c",
                                warn = TRUE, 
                                nomatch = NA)) %>%
  select (c(country, year, GDP.PC.PPP)) %>%
  relocate (country, year, GDP.PC.PPP) %>%
  mutate (GDP.PC.PPP = Lag(GDP.PC.PPP, -1)) %>%
  filter (year %in% 2000:2019) %>%
  filter (!(is.na(country))) %>% 
  rename_with(
    ~ case_when(
      . == "GDP.PC.PPP" ~ "gdp.pc",
      TRUE ~ .
    )
  )

# exporting data
write.csv(gdp, file.path(output_path,"MPRO_gdp.csv"), row.names = FALSE)
  