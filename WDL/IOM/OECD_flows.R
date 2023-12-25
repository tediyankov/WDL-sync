
## preliminaries  --------------------------------------------------------------

# packages
pacman::p_load (tidyverse,countrycode)

# paths
base_path <- file.path ("/Volumes/","GoogleDrive", "Shared drives", "DATA_WDL")
input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data")
output_path <- file.path (base_path, "IOM", "2nd_paper", "output")

## preprocessing OECD flows ----------------------------------------------------

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

# cartesian product

origfactor <- as.factor (unique(OECD_flows$orig))
destfactor <- as.factor (unique(OECD_flows$dest))
yearfactor <- as.factor (unique(OECD_flows$year))

oecd_flows <- expand.grid (orig = origfactor,
                           dest = destfactor, 
                           year = yearfactor)

# merge flows with cartesian products

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

# note: the problematic orig.dest.year groups: some of the country pairs for orig == RUS had two values (in each case, the second value would be the correct and the first value would be wrong).
# the resulting df - "OECD.flows" - should have 136000 obsv.

# verifying using AFG-JPN pairing
OECD.flows.sample = OECD.flows %>% 
  filter(orig == "AFG" & dest == "JPN") # should return NAs for the first 6 obsv

# cleaning up environment
rm (OECD.flows.sample, origfactor, destfactor, yearfactor, oecd_flows, OECD_flows, oecdflows)

# exporting data
write.csv(OECD.flows, file.path(output_path,"OECD_flows.csv"), row.names = FALSE)








