
## CYPRUS DECK CODE

## chapter 1: setup ------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, data.table, ggpmisc, ggpubr, ggthemes, knitr, sjPlot, texreg, stargazer, corrplot,
                kableExtra, treemapify, ggcats)

## system font 
worlddataverse::font_wdl()

## file paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')
cats_path1 <- file.path(base_path, 'product_categories', 'trade_model', '03_outputs')
cats_path2 <- file.path(base_path, 'product_categories', 'loreal', '2023-06-28')
pc_path <- file.path (base_path, "spending_categories","demographic_breakdown", "pc_and_beauty_breakdown","2023-09-07")

demographic_breaks <-  file.path (
  base_path,
  "spending_categories",
  "demographic_breakdown"
)

input_path <- file.path (
  demographic_breaks,
  "pc_and_beauty_breakdown",
  "2023-09-07"
)

## loading in data 

# WDP
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, Inf))

# categories
cats <- read_excel ("predictions_all_categories.xlsx", sheet = 1)
predictions_all_categories_with_personalcare <- read_csv("predictions_all_categories_with_personalcare.csv")

# CAGR function
CAGR_fun <- function (x) {
  if (length (x) < 1L)
    return (numeric ())
  out <- (x / x [[1L]]) ^ (1 / (seq_along (x) - 1)) - 1
  out [[1L]] <- NA_real_
  out
}

# EU object
EU = c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus",
       "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany",
       "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
       "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania",
       "Slovakia", "Slovenia", "Spain", "Sweden")

EU_ISO3 <- countrycode (sourcevar = EU,
                        origin = "country.name",
                        destination = "iso3c",
                        warn = TRUE,
                        nomatch = NA)

# population data
popdat = wdp %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T))

## chapter 2: cc spending in EU, USA, CHN, IND  --------------------------------

## data 
input = wdp %>%
  
  # country-year headcounts per spending group
  group_by (ccode, year, daily_spending) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T),
             spend = sum (exp.pdf.m, na.rm = T)) %>%
  
  # filtering year
  filter (year %in% c(2023:2030))  %>%
  
  # creating EU category
  mutate (ccode = ifelse (ccode %in% EU_ISO3, "EU", ccode)) %>%
  
  # aggregating into new categories
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T),
             spend = sum (spend, na.rm = T)) %>% 
  
  # filtering countries of interest
  filter (ccode %in% c("IND", "EU", "CHN", "USA")) %>% 
  
  # isolating consumer class
  filter (daily_spending != "[0,12)") %>%
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T),
                    spend = sum (spend, na.rm = T)) %>% 
  
  # rescaling
  mutate (spend = spend / 1000000000000) %>%
  
  # keeping only 2023 and 2030
  filter (year %in% c(2023, 2030)) %>%
  
  # widening for easier view
  pivot_wider (names_from = "year", values_from = c("pop", "spend"))

## building chart
chart = ggplot (data = input, 
                      aes (x = reorder (ccode, spend_2030))) + 
  geom_bar (aes (y = spend_2030), stat = "identity", width = 0.8) + 
  geom_bar (aes (y = spend_2023), stat = "identity", width = 0.4, fill = "white") + 
  labs (title = "Consumer Class Spending Power in 2023 and 2030",
        y = "Expenditure in Trillions $",
        x = "Country") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl()

chart

## chapter 3: headcounts growth  -----------------------------------------------

## perceentage growth
input = wdp %>%
  
  # isolating countries in the Eu
  filter (ccode %in% EU_ISO3) %>%
  
  # isolating 2023 and 2030
  filter (year %in% c(2023, 2030)) %>%
  
  # aggregating 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T)) %>%
  
  # computing percentage change
  pivot_wider (names_from = "year", values_from = "hc") %>%
  mutate (delta_pct = ((`2030` - `2023`) / `2023`) * 100)
  
# building chart
ggplot (data = input,
        aes (x = reorder (ccode, delta_pct), 
             y = delta_pct, 
             fill = ifelse (ccode == "CYP", "lightpurple", "darkpurple"))) + 
  geom_bar (stat = "identity", width = 0.8) + 
  geom_text (aes (label = paste0 (round (delta_pct, 1), "%"))) +
  theme_wdl() + 
  theme (legend.position = "none")
  
  
# CAGRs
input = wdp %>%
  
  # isolating countries in the Eu
  filter (ccode %in% EU_ISO3) %>%
  
  # isolating 2023 and 2030
  filter (year %in% c(2023:2030)) %>%
  
  # aggregating 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T)) %>%
  
  # computing percentage change
  dplyr::group_by (ccode) %>%
  mutate (CAGR = CAGR_fun (hc) * 100) %>% ungroup () %>%
  
  # filter for 2030
  filter (year == 2030)

# building chart
ggplot (data = input,
        aes (x = reorder (ccode, CAGR), 
             y = CAGR, 
             fill = ifelse (ccode == "CYP", "lightpurple", "darkpurple"))) + 
  geom_bar (stat = "identity", width = 0.8) + 
  geom_text (aes (label = paste0 (round (CAGR, 1), "%"))) +
  theme_wdl() + 
  theme (legend.position = "none")

## absolute growth 

## chapter 4: total spending in Cyprus  ----------------------------------------

## data
input = wdp %>%
  
  # isolating Cyprus
  filter (ccode == "CYP") %>%
  
  # isolating years
  filter (year %in% 2023:2030) %>%
  
  # aggregating
  dplyr::group_by (year) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # scaling
  mutate (exp = round (exp / 10^9, 1)) %>%
  
  # adding CAGRs
  mutate (CAGR = CAGR_fun (exp) * 100)
  
  
## chapter 5: spending categories stuff  ---------------------------------------

# grwoth to 2030
predictions_all_categories_with_personalcare %>%
  
  # isolating Cyprus 
  filter (ccode == "CYP") %>%
  
  # aggregate for global
  dplyr::group_by (year, category) %>%
  dplyr::summarise (spend = sum (total_category_exp_nominal, na.rm = T) / 10^9) %>%
  
  # filter for relevant year range
  filter (year %in% 2023:2030) %>%
  
  # applying CAGR function
  dplyr::group_by (category) %>%
  mutate (CAGR = CAGR_fun (spend) * 100) %>% ungroup () %>%
  
  # extracting relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # widening
  pivot_wider (names_from = "year", values_from = 3:4) %>%
  
  # delta var
  mutate (delta = spend_2030 - spend_2023) %>%
  
  # keeping only relevant vars
  dplyr::select (category, delta, CAGR_2030) %>%
  
  # renaming cats so that they fit on the x axis better
  mutate (category = ifelse (category == "Food and non-alcoholic beverages", "Food",
                             ifelse (category == "Alcoholic beverages, tobacco and narcotics", "AlcNarc",
                                     ifelse (category == "Clothing and footwear", "Clothing",
                                             ifelse (category == "Housing, water, electricity, gas and other fuels", "Housing",
                                                     ifelse (category == "Furnishings, household equipment and routine household maintenance", "Furnish",
                                                             ifelse (category == "Health", "Health",
                                                                     ifelse (category == "Transport", "Transport",
                                                                             ifelse (category == "Communications", "Comms",
                                                                                     ifelse (category == "Recreation and culture", "Culture",
                                                                                             ifelse (category == "Education", "Education",
                                                                                                     ifelse (category == "Restaurants and hotels", "Restaurants",
                                                                                                             ifelse (category == "Miscellaneous goods and services", "Misc",
                                                                                                                     ifelse (category == "Personal Care", "Pers Care", NA
                                                                                                                     )))))))))))))) %>%
  
  # building chart
  ggplot (aes (x = reorder (category, -delta))) + 
  geom_bar (aes (y = delta * 2, fill = category), stat = "identity") + 
  geom_line (aes (y = CAGR_2030, group = 1), stat = "identity", color = "black", linewidth = 1.5) +
  geom_text (aes (y = delta * 2, label = paste0 ("+ $", round (delta, 1), " ", "B")), vjust = -0.5) +
  worlddataverse::theme_wdl() +
  theme(axis.text.x = element_text (angle = 360)) +
  scale_fill_wdl() + 
  labs (x = "\nCategory", 
        y = "Absolute change in spending between 2023 and 2030\n") +
  
  # adjusting the two y-axis scales
  scale_y_continuous(
    name = "Absolute change in spending between 2023 and 2030\n",
    breaks = c(0,1,2,3,4,5,6),
    labels = c(0,0.5,1,1.5,2,2.5,3),
    sec.axis = sec_axis(
      ~ ., 
      name = "CAGR 2030\n", 
      breaks = c(0,1,2,3,4,5,6), 
      labels = c(0,1,2,3,4,5,6)
    )
  ) + 
  
  # removing legend
  theme (legend.position = "none")

# current and per capita
predictions_all_categories_with_personalcare %>%
  
  # adding pop data
  left_join (popdat, by = c("ccode", "year")) %>%
  
  # isolating Cyprus 
  filter (ccode == "CYP") %>%
  
  # aggregate for global
  dplyr::group_by (year, category) %>%
  dplyr::summarise (spend = sum (total_category_exp_nominal, na.rm = T), 
                    pop = mean (pop, na.rm = T)) %>%
  
  # isolating 2023
  filter (year == 2023) %>%
  
  # obtaining per capita spend
  mutate (spend_pc = spend / pop) %>%
  
  # obtaining share
  mutate (share = spend_pc / sum (spend_pc, na.rm = T)) %>%
  
  # reorder
  mutate (category = factor (category, 
                             levels = category [order (share)], 
                             ordered = T)) %>%
  
  # building plot
  ggplot (aes (x = year, y = share, fill = category)) + 
  geom_bar (stat = 'identity', position = 'stack') + 
  geom_text (aes (label = paste0 (round (share * 100, 1), "%", " (", round (spend_pc, 1), "$)")),
             position = position_stack (vjust = 0.5)) + 
  theme_void ()


## chapter 6: growth by age ----------------------------------------------------

## EU data
cc_dat_eu = wdp %>%
  
  # aggregating data by year, country and age
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # setting the desired age groups
  mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)","[10,15)"), "Children",
                              ifelse (age_group %in% c("[15,20)","[20,25)","[25,30)",
                                                       "[30,35)","[35,40)","[40,45)",
                                                       "[30,45)","[15,30)","[45,50)",
                                                       "[50,55)","[55,60)","[60,65)"), "Working Age", 
                                      ifelse (age_group %in% c("[65,70)","[70,75)","[75,INF)"), "Senior", NA
                                      )
                              )
  )
  ) %>%
  
  # filtering
  filter (year %in% c(2023, 2030)) %>%
  filter (ccode %in% EU_ISO3) %>%
  filter (daily_spending == "[12,Inf)") %>%
  
  # re-aggregating using new age bins
  group_by (year, age_group) %>%
  summarise (pop = sum (pop, na.rm = T)) %>%
  
  # computing percentage change
  pivot_wider (names_from = "year", values_from = 'pop') %>%
  mutate (delta = ((`2030` - `2023`) / `2023`) * 100) %>% 
  
  # adding country ID
  mutate (ccode = "EU27")


## Greece data
cc_dat_cyp = wdp %>%
  
  # aggregating data by year, country and age
  group_by (ccode, year, age_group, daily_spending) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # setting the desired age groups
  mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)","[10,15)"), "Children",
                              ifelse (age_group %in% c("[15,20)","[20,25)","[25,30)",
                                                       "[30,35)","[35,40)","[40,45)",
                                                       "[30,45)","[15,30)","[45,50)",
                                                       "[50,55)","[55,60)","[60,65)"), "Working Age", 
                                      ifelse (age_group %in% c("[65,70)","[70,75)","[75,INF)"), "Senior", NA
                                      )
                              )
  )
  ) %>%
  
  # filtering
  filter (year %in% c(2023, 2030)) %>%
  filter (ccode == "CYP") %>%
  filter (daily_spending == "[12,Inf)") %>%
  
  # re-aggregating using new age bins
  group_by (year, age_group) %>%
  summarise (pop = sum (pop, na.rm = T)) %>%
  
  # computing percentage change
  pivot_wider (names_from = "year", values_from = 'pop') %>%
  mutate (delta = ((`2030` - `2023`) / `2023`) * 100) %>%
  
  # adding country ID
  mutate (ccode = "Cyprus")


## creating plot
rbind (cc_dat_eu, cc_dat_cyp) %>%
  
  # coercing factor levels
  mutate (age_group = factor (age_group, levels = c("Children", "Working Age", "Senior"))) %>%
  
  # creating graph
  ggplot (aes (x = age_group, y = delta)) + 
  geom_bar (aes (fill = ccode), stat = "identity", position = "dodge") + 
  worlddataverse::theme_wdl () +
  worlddataverse::scale_fill_wdl () +
  labs (title = "Consumer Class Population Growth, 2023-2030",
        subtitle = "Comparing trends in Greece to EU27",
        x = "Age", 
        y = "Consumer Class Population Growth, % change") + 
  theme (axis.text.x = element_text (angle = 360),
         axis.title.x = element_text (vjust = -1)) + 
  guides(fill = guide_legend (title = "Country"))


## chapter 7: spend growth -----------------------------------------------------

## perceentage growth
input = wdp %>%
  
  # isolating countries in the Eu
  filter (ccode %in% EU_ISO3) %>%
  
  # isolating 2023 and 2030
  filter (year %in% c(2023, 2030)) %>%
  
  # aggregating 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (spend = sum (exp.pdf.m, na.rm = T)) %>%
  
  # computing percentage change
  pivot_wider (names_from = "year", values_from = "spend") %>%
  mutate (delta_pct = ((`2030` - `2023`) / `2023`) * 100)

# building chart
ggplot (data = input,
        aes (x = reorder (ccode, delta_pct), 
             y = delta_pct, 
             fill = ifelse (ccode == "CYP", "lightpurple", "darkpurple"))) + 
  geom_bar (stat = "identity", width = 0.8) + 
  geom_text (aes (label = paste0 (round (delta_pct, 1), "%"))) +
  theme_wdl() + 
  theme (legend.position = "none")


# CAGRs
input = wdp %>%
  
  # isolating countries in the Eu
  filter (ccode %in% EU_ISO3) %>%
  
  # isolating 2023 and 2030
  filter (year %in% c(2023:2030)) %>%
  
  # aggregating 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (spend = sum (exp.pdf.m, na.rm = T)) %>%
  
  # computing percentage change
  dplyr::group_by (ccode) %>%
  mutate (CAGR = CAGR_fun (spend) * 100) %>% ungroup () %>%
  
  # filter for 2030
  filter (year == 2030)

# building chart
ggplot (data = input,
        aes (x = reorder (ccode, CAGR), 
             y = CAGR, 
             fill = ifelse (ccode == "CYP", "lightpurple", "darkpurple"))) + 
  geom_bar (stat = "identity", width = 0.8) + 
  geom_text (aes (label = paste0 (round (CAGR, 1), "%"))) +
  theme_wdl() + 
  theme (legend.position = "none")



wec_dat_cyp = wec_dat %>%
  
  # aggregate 
  dplyr::group_by (iso3c, year, sector, pop) %>%
  dplyr::reframe (base = sum (base, na.rm = T),
                    base_pc = base / pop) %>% ungroup() %>%
  
  # filtering
  filter (iso3c %in% c("CYP", "CHE", "MLT")) %>%
  filter (year == 2023) %>%
  distinct ()
  




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
                   "WEC_data_binary_20231030_lulucfPIK.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  .GlobalEnv$base_path = base_path
  
} 
load_wec_dat()


percapitaem = wec_dat %>%
  filter (iso3c %in% c("CYP", "GRC", "MLT")) %>% 
  filter (year == 2023) %>%
  dplyr::group_by (iso3c, year, sector) %>%
  dplyr::summarise (em_pc = sum (base, na.rm = T) / mean (pop, na.rm = T))

ggplot (percapitaem, aes(x = iso3c, y = em_pc, fill = sector)) + 
  geom_bar (stat = "identity", position = "stack")








