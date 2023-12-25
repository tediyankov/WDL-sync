
## preliminaries  --------------------------------------------------------------

# packages
pacman::p_load (tidyverse,countrycode)

# paths
base_path <- file.path ("/Volumes/","GoogleDrive", "Shared drives", "DATA_WDL")
input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data")
output_path <- file.path (base_path, "IOM", "2nd_paper", "output")

## processing stock data -------------------------------------------------------

OECD_stocks <- read.csv(file.path(input_path, "OECD_mig_data.csv")) %>%
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
      . == "Value" ~ "migstock",
      TRUE ~ .
    )
  ) %>%
  unite ("orig.dest", orig:dest, sep = "_", remove = F, na.rm = F) %>%
  relocate (orig.dest, orig, dest, year, migstock)

# exporting data
write.csv(OECD_stocks, file.path(output_path,"OECD_stocks.csv"), row.names = FALSE)
