
## PRELIMS =====================================================================

## empty environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, maps, ggsubplot, lattice, latticeExtra, plyr)

## set font
worlddataverse::font_wdl()

## file paths
base_path <- worlddataverse::get_wdl_path ()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2022_12_29','2023_02_22_2017ppp','03_outputs')

## DATA VIZ ====================================================================

## world map
world.map <- map ("world", plot = FALSE, fill = TRUE)
world_map <- map_data ("world")

## Calculate the mean longitude and latitude per region (places where subplots are plotted)
cntr <- ddply (world_map,.(region), summarize, long = mean (long), lat = mean (lat)) %>%
  
  # changing country names to ISO3 codes to match WDPro data
  mutate (region = countrycode (region, "country.name", "iso3c")) %>%
  
  # remove NAs
  filter (!is.na (region)) %>%
  
  # rename rregion to ccode for merge 
  rename_with (~ case_when (. == "region" ~ "ccode", TRUE ~ .))

## input data 

# raw 
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_12_29_2017ppp_1_USD.rds"))

# intermediary 
wdp <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, Inf))

# clean
wdp_clean = wdp %>%
  
  # first grouping and aggregating
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # filter for years
  filter (year %in% c(2023, 2031)) %>%
  
  # filter for spending group (keep only consumer class)
  filter (daily_spending == "[12,Inf)") %>%
  dplyr::select (-daily_spending) %>%
  
  # widening to get a delta variable 
  pivot_wider (names_from = "year", values_from = "pop") %>%
  
  # rename 
  rename_with (
    ~ case_when (
      . == "2023" ~ "hh_2023",
      . == "2031" ~ "hh_2031",
      TRUE ~ .
    )
    ) %>%
  
  # create delta variable
  mutate (delta = hh_2031 - hh_2023) %>%
  
  # keep only delta var
  dplyr::select (ccode, delta) %>%
  
  # arrange in descending order, 
  arrange (desc (delta))

## creating subset of cntr region
subsetcntr  <- subset (cntr, ccode %in% unique (wdp_clean$ccode))

## merging wdp_clean with geo data 
simdat <- wdp_clean %>%
  
  # executing merge
  left_join (subsetcntr, by = "ccode") %>%
  
  # order
  dplyr::select (ccode, long, lat, delta)

## 3D Map function
panel.3dmap <- function (..., rot.mat, distance, xlim, ylim, zlim, xlim.scaled, ylim.scaled, zlim.scaled) {
  
  scaled.val <- function (x, original, scaled) {
    
    scaled[1] + (x - original[1]) * diff(scaled)/diff(original)
    
  }
  
  m <- ltransform3dto3d (rbind (scaled.val (world.map$x, xlim, xlim.scaled), 
                                scaled.val (world.map$y, ylim, ylim.scaled), 
                                zlim.scaled[1]), rot.mat, distance)
  
  panel.lines (m[1, ], m[2, ], col = "#010242")
  
}

## plotting 
plot <- cloud(delta ~ long + lat, simdat, panel.3d.cloud = function(...) {
  panel.3dmap(...)
  panel.3dscatter(...)
}, type = "h", col = "red", scales = list(draw = FALSE), zoom = 1.1, 
title = "Mapping consumer class headcounts growth (2023-2033)",
xlim = world.map$range[1:2], ylim = world.map$range[3:4],
xlab = NULL, ylab = NULL, zlab = NULL, aspect = c(diff(world.map$range[3:4])/diff(world.map$range[1:2]),
                                                  0.3), panel.aspect = 0.75, lwd = 2, screen = list(z = 30,
                                                                                                    x = -60), par.settings = list(axis.line = list(col = "transparent"),
                                                                                                                                  box.3d = list(col = "transparent", alpha = 0)))
print (plot)


## Misc =======================================================================

wdp_check = wdp %>%
  
  # first grouping and aggregating
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # keeping only years above 2023
  filter (year >= 2023) %>%
  
  # filter for spending group (keep only consumer class)
  filter (daily_spending == "[12,Inf)") %>%
  dplyr::select (-daily_spending) %>%
  
  # getting sums of CC hc by year
  dplyr::group_by (year) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T))
  
  

ggplot (data = wdp_check, aes (x = year, y = pop)) + 
  geom_line ()






