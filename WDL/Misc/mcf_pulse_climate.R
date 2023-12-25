
## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, data.table)

## system font 
worlddataverse::font_wdl()

## file paths
base_path <- worlddataverse::get_wdl_path()
iom_climate_path <- file.path (base_path, "IOM", "climate")

## raw datasets
em_dat <- read_excel("public_emdat_custom_request_2023-10-23_b8d0a658-f2cd-4104-8593-72362bce4d84.xlsx")
load_wec_dat = function (){
  
  require (worlddataverse)
  
  # loading paths
  base_path <- worlddataverse::get_wdl_path()
  if (is.na (base_path)) {base_path = file.path("/Users",
                                                "teddyyankov",
                                                "Library",
                                                "CloudStorage",
                                                "GoogleDrive-teodor.yankov@worlddata.io",
                                                "Shared drives", 
                                                "DATA_WDL")}
  
  # load binary (Rda) file
  load (file.path (base_path,
                   "world_emissions_clock",
                   "01_data",
                   "WEC_data_binary_20230425.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  .GlobalEnv$base_path = base_path
  
} 
load_wec_dat()


## data cleaning ---------------------------------------------------------------

## Climate disaster 
em_dat_clean = em_dat %>%
  
  # keeping relevant variables
  dplyr::select (`Start Year`, `Disaster Type`, ISO, `Total Affected`) %>%
  
  # renaming
  dplyr::rename_with (
    ~ case_when (
      . == "Start Year" ~ "year",
      . == "Disaster Type" ~ "disaster",
      . == "ISO" ~ "iso3c",
      . == "Total Affected" ~ "tot_affected",
      TRUE ~ .
    )
  ) %>%
  
  # removing NAs
  drop_na()
  

## GDP
gdp = wec_dat %>%
  
  # keeping relevant variables
  dplyr::group_by (iso3c, year) %>%
  dplyr::summarise (gdp = mean (gdp, na.rm = T)) 


## merging data 
merged_df <- left_join (gdp, em_dat_clean, by = c("iso3c", "year")) %>%
  
  # creating a binary disaster or no var
  mutate (disaster_binary = ifelse (is.na (disaster), 0, 1))
  

## adding GDP change vars
merged_df2 <- merged_df %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    delta1year = lead(gdp, order_by = year) - gdp,
    delta3year = lead(gdp, order_by = year, n = 3) - gdp,
    delta5year = lead(gdp, order_by = year, n = 5) - gdp,
    delta10year = lead(gdp, order_by = year, n = 10) - gdp
  )


## plotting GDP 
library(ggplot2)
library(gridExtra)

# Assuming your merged DataFrame is named 'merged_df'

# Filter the DataFrame to only include rows where 'disaster' == 1
disaster_df_subset <- merged_df2 %>% filter (disaster_binary == 1)

# Create a list to store individual plots
plots_list <- list()

# Iterate over unique countries in the subset
unique_countries <- unique(disaster_df_subset$iso3c)

for (country in unique_countries) {
  
  # Filter the subset for the current country
  country_subset <- disaster_df_subset %>% filter(iso3c == country)
  
  # Create a plot for the country
  p <- ggplot(merged_df2 %>% 
                filter (iso3c == country) %>% 
                mutate (year_disaster = ifelse (year %in% country_subset$year, year, NA)), aes(x = year, y = gdp)) +
    geom_line() +
    geom_vline(aes(xintercept = year_disaster), linetype = "dashed", color = "red") +
    labs(title = paste("GDP for", country, "with Disaster Year")) +
    theme_minimal()
  
  # Add the plot to the list
  plots_list[[length(plots_list) + 1]] <- p
}

# Create a PDF with one plot per page
pdf("disaster_plots2.pdf", width = 8, height = 5) # Adjust the width and height as needed

for (i in 1:length(plots_list)) {
  print(plots_list[[i]])
}

dev.off()




# Filter the subset for the current country
country_subset <- disaster_df_subset %>% filter(iso3c == country)

# Create a plot for the country
ggplot(merged_df2 %>% 
         filter (iso3c == country) %>% 
         mutate (year_disaster = ifelse (year %in% AGO_subset$year, year, NA)), aes(x = year, y = gdp)) +
  geom_line() +
  geom_vline(aes(xintercept = year_disaster), linetype = "dashed", color = "red") +
  labs(title = paste("GDP for", country, "with Disaster Year")) +
  theme_minimal()








