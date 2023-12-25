

## loading in IMF WEO April 2023 data
library (readxl)
library (ggimage)
weo_raw <- readxl::read_xls (file.path ("~/Desktop/WEO_Data_2.xls"))

## building plot
fastmovers_gdp = weo_raw %>%
  
  # isolating necessary vars
  dplyr::select (ISO, 8:15) %>%
  
  # elongating to have one year per row
  tidyr::pivot_longer (2:9, names_to = "year", values_to = "value") %>%
  
  # expanding subject variable into individual metrics
  #tidyr::pivot_wider (names_from = 2, values_from = "value") %>%
  
  # renaming metrics
  dplyr::rename_with (
    ~ case_when (
      . == "value" ~ "delta_gdp",
      TRUE ~ .
    )
  ) %>%
  
  # isolating GDP
  #dplyr::select (- c(gdp_pc, gdp_share)) %>% 
  
  # replacing n/a with NAs 
  dplyr::mutate (delta_gdp = ifelse (delta_gdp == "n/a", NA, delta_gdp)) %>%
  
  # removing phantom variable
  #dplyr::select (-`NA`) %>%
  
  # converting values to numeric
  dplyr::mutate_at (3, as.numeric) %>%
  
  # removing AFG, LBN, SYR, UKR, VEN because they are incomplete
  dplyr::filter (!ISO %in% unique (.$ISO [is.na (.$delta_gdp)])) %>%
  
  # filterng for year
  filter (year == 2024) %>%
  
  # converting country codes
  dplyr::mutate (country = countrycode (ISO, "iso3c", "country.name"), 
                 flag = countrycode (ISO, "iso3c", "iso2c")) %>%
  
  # dropping NA
  drop_na() %>%
  
  # arrange by delta in descending order
  arrange (desc(delta_gdp)) %>%
  
  # slice top 11
  dplyr::slice (1:11) %>%

  # building plot 
  ggplot (aes (x = reorder (country, -delta_gdp), y = delta_gdp, fill = country)) + 
  geom_bar (stat = "identity") + 
  ggimage::geom_flag (y = -0.5, aes (image = flag)) + 
  worlddataverse::theme_wdl() + 
  viridis::scale_fill_viridis (discrete = T, option = "A") + 
  theme (legend.position = "none",
         axis.text.x = element_text (angle = 360)) + 
  labs (x = "\nCountry", 
        y = "\nGDP growth", 
        title = "Fast Movers by GDP Growth 2023-2024")
  

fastmovers_gdp
  
  
  
  
  
  
  
