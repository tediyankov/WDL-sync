
## Teddy's code for the categories webinar slides

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, data.table, ggpmisc, ggpubr, ggthemes, knitr, sjPlot, texreg, stargazer, corrplot,
                kableExtra, treemapify, plotly, ggrepel, grDevices, RColorBrewer)

## system font 
worlddataverse::font_wdl()

## file paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')
catspath <- file.path (base_path, "product_categories", 'global_expenditure_shares', "03_output")
foodpath <- file.path (catspath, "food")
demographic_breaks <- file.path (base_path, "spending_categories", "demographic_breakdown")
input_path <- file.path (demographic_breaks, "pc_and_beauty_breakdown","2023-09-07")

## loading in raw datasets

# WDP
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))

# all categories
cats <- read_excel (file.path (catspath, "predictions_all_categories.xlsx"))

# personal care
pc <- read_excel ("Loreal_Product_Data-2023-09-07 (nominal).xlsx")

# food and bevs India
food <- read_excel (file.path (foodpath, "pak_shares_food_beverages_multivariate.xlsx"))

base_path <- worlddataverse::get_wdl_path()
food <- read_excel (file.path (base_path, 
                               "product_categories", 
                               'global_expenditure_shares', 
                               "03_output",
                               "food",
                               "idn_shares_food_beverages_multivariate.xlsx"))

## CAGR function
CAGR_fun <- function (x) {
  if (length (x) < 1L)
    return (numeric ())
  out <- (x / x [[1L]]) ^ (1 / (seq_along (x) - 1)) - 1
  out [[1L]] <- NA_real_
  out
}

























## separating misc and personal care in the input data -------------------------

## reformatting pc df
pc_reformat = pc %>%
  
  # keepning relevant vars
  dplyr::select (`Country`, `Year`, `Expenditure Type`, `Personal Care`) %>%
  
  # keeping only nominal
  filter (`Expenditure Type` == "nominal") %>%
  distinct () %>%
  dplyr::select (-`Expenditure Type`) %>%
  
  # aggregating 
  dplyr::group_by (`Country`, `Year`) %>%
  dplyr::summarise (`Personal Care` = sum (`Personal Care`, na.rm = T))

## decoupling pc from misc
cats_wpersonalcare = cats %>%
  
  # keeping relevant variables
  dplyr::select (ccode, year, region, category, total_exp_nominal, total_category_exp_nominal) %>%
  
  # merging personal care var
  left_join (pc_reformat %>%
               
               # isolating vars
               #dplyr::select (ISO, Year, `Personal Care Total Nominal`) %>%
               
               # renaming 
               dplyr::rename_with (
                 ~ case_when (
                   . == "Country" ~ "ccode",
                   . == "Year" ~ "year",
                   TRUE ~ .
                 )
               ), 
             by = c("ccode", "year")) %>%
  
  # removing personal care from misc
  mutate (new_total_category_exp_nominal = ifelse (category == "Miscellaneous goods and services", 
                                                   total_category_exp_nominal - `Personal Care`, 
                                                   total_category_exp_nominal)) %>%
  
  # recalibrating
  dplyr::select (ccode, year, region, category, total_exp_nominal, new_total_category_exp_nominal, `Personal Care`) %>%
  dplyr::rename_with (~ case_when (. == "new_total_category_exp_nominal" ~ "total_category_exp_nominal", TRUE ~ .))

## moving personal care into the categories variable
personalcare = cats_wpersonalcare %>% 
  dplyr::select (ccode, year, region, total_exp_nominal, `Personal Care`) %>%
  distinct () %>%
  mutate (category = "Personal Care") %>%
  relocate (category, .before = total_exp_nominal) %>%
  dplyr::rename_with (~ case_when (. == "Personal Care" ~ "total_category_exp_nominal", TRUE ~ .))

## adding back to main df with pc as a separate category + recoding category names
cats_wpersonalcare = rbind (cats_wpersonalcare %>% dplyr::select (-`Personal Care`), personalcare) %>%
  
  # shortening categories
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
                                                                                                                     ))))))))))))))








## harmonise & forecast intro chart --------------------------------------------

## building data
introchartdat = wdp %>%
  
  # isolate India
  filter (ccode == "IND") %>%
  
  # aggregating into country year spending group
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T)) %>%
  
  # isolate consumer class
  filter (!daily_spending == "[0,12)") %>%
  
  # re-aggregating into years
  dplyr::group_by (year) %>%
  dplyr::summarise (hc = sum (hc, na.rm = T)) %>%
  
  # filtering for years of interest
  filter (year %in% c(2019, 2023, 2030)) %>%
  
  # changing 2019 to World Bank numbers
  mutate (hc = ifelse (year == 2019, 85000000, hc)) %>%
  
  # creating chart
  ggplot (aes (x = factor (year), y = hc / 10^6)) + 
  geom_bar (stat = "identity") + 
  worlddataverse::theme_wdl() + 
  scale_y_continuous (breaks = c(0,100,200,300,400,500,600,700,800)) + 
  labs (y = "Consumer Class Headcounts in Millions\n",
        x = "\nYear") + 
  theme (axis.text.x = element_text (angle = 360))

introchartdat

## pie chart: cats spending share ----------------------------------------------

cats_piespend2023 = cats_wpersonalcare %>%
  
  # aggregating into global level
  dplyr::group_by (year, category) %>%
  dplyr::summarise (total_exp_nominal = sum (total_exp_nominal, na.rm = T), 
                    total_category_exp_nominal = sum (total_category_exp_nominal, na.rm = T)) %>%
  
  # filter for 2023
  filter (year == 2023) %>%
  
  # computing category shares
  mutate (share = round (total_category_exp_nominal / total_exp_nominal, 2)) %>%
  mutate (label = scales::percent (share, 1)) %>%
  mutate (spend = round (total_category_exp_nominal / 10^12, 1)) %>%
  
  # ordering 
  arrange (-share) %>%
  
  # pie chart helpers
  mutate(csum = rev (cumsum (rev (share))), 
         pos = share/2 + lead (csum, 1),
         pos = if_else (is.na (pos), share/2, pos)) %>%
  
  # ordering
  mutate (category = factor (category, 
                             levels = category [order (-share)],
                             ordered = T))

  # building plot
  ggplot (aes (x = year, y = share, fill = category)) + 
  geom_bar (stat = "identity", position = "stack") + 
  coord_polar (theta = "y") + 
  theme_void () + 
  worlddataverse::scale_fill_wdl() + 
  theme (legend.position = "bottom")

cats_piespend2023

ggplot (data = cats_piespend2023, 
        aes(x = "" , y = share, fill = category)) +
  geom_col () +
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral") )(13)) +
  coord_polar (theta = "y") +
  geom_label_repel(data = cats_piespend2023,
                   aes (y = pos, label = paste0 (category, "\n$ ", spend, " | ", share)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides (fill = guide_legend (title = "Category")) +
  theme_void() + 
  theme (legend.position = "none")

## per capita pie chart --------------------------------------------------------

## 2023
cats_piespend2023_pc = cats_wpersonalcare %>%
  
  # adding population
  left_join (popdat, by = c("ccode", "year")) %>%
  
  # aggregating into global level
  dplyr::group_by (year, category) %>%
  dplyr::summarise (total_category_exp_nominal = sum (total_category_exp_nominal, na.rm = T),
                    pop = sum (pop, na.rm = T)) %>%
  
  # filter for 2023
  filter (year == 2023) %>%
  
  # computing per capita
  mutate (exp_pc = total_category_exp_nominal / pop) %>%
  
  # computing shares
  mutate (share = exp_pc / sum (exp_pc, na.rm = T)) %>%
  
  # adding label
  mutate (spend = round (exp_pc, 1),
          share_pc = scales::percent (share, 1),
          label = paste0 ("$ ", spend, " | ", share_pc)) %>%
  
  # ordering
  mutate (category = factor (category, 
                             levels = category [order (share)],
                             ordered = T)) %>% 
  
  # pie chart helpers
  mutate(csum = rev (cumsum (rev (share))), 
         pos = share/2 + lead (csum, 1),
         pos = if_else(is.na(pos), share/2, pos))
  
  # creating chart
ggplot (data = cats_piespend2023_pc, 
        aes(x = "" , y = share, fill = category)) +
  geom_bar (stat = "identity", position = 'stack') +
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral") )(13)) +
  coord_polar (theta = "y") +
  geom_label_repel(data = cats_piespend2023_pc,
                   aes (y = pos, label = label),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides (fill = guide_legend (title = "Category")) +
  theme_void()

ggplot (data = cats_piespend2023_pc, 
        aes(x = "" , y = share, fill = category)) +
  geom_bar (stat = "identity", position = 'stack') +
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral") )(13)) +
  geom_text (aes (y = share, label = label), position = position_stack (vjust = 0.5)) + 
  theme_void ()

## 2030
cats_piespend2030_pc = cats_wpersonalcare %>%
  
  # adding population
  left_join (popdat, by = c("ccode", "year")) %>%
  
  # aggregating into global level
  dplyr::group_by (year, category) %>%
  dplyr::summarise (total_category_exp_nominal = sum (total_category_exp_nominal, na.rm = T),
                    pop = sum (pop, na.rm = T)) %>%
  
  # filter for 2030
  filter (year == 2030) %>%
  
  # computing per capita
  mutate (exp_pc = total_category_exp_nominal / pop) %>%
  
  # computing shares
  mutate (share = exp_pc / sum (exp_pc, na.rm = T)) %>%
  
  # adding label
  mutate (spend = round (exp_pc, 1),
          share_pc = scales::percent (share, 1),
          label = paste0 ("$ ", spend, " | ", share_pc)) %>%
  
  # ordering
  mutate (category = factor (category, 
                             levels = category [order (share)],
                             ordered = T)) %>% 
  
  # pie chart helpers
  mutate(csum = rev (cumsum (rev (share))), 
         pos = share/2 + lead (csum, 1),
         pos = if_else(is.na(pos), share/2, pos))

# creating chart
ggplot (data = cats_piespend2030_pc, 
        aes(x = "" , y = share, fill = fct_inorder (category))) +
  geom_col (width = 1, color = 1) +
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral") )(13)) +
  coord_polar (theta = "y") +
  geom_label_repel(data = cats_piespend2030_pc,
                   aes (y = pos, label = label),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides (fill = guide_legend (title = "Category")) +
  theme_void()

ggplot (data = cats_piespend2030_pc, 
        aes(x = "" , y = share, fill = category)) +
  geom_bar (stat = "identity", position = 'stack') +
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral") )(13)) +
  geom_text (aes (y = share, label = label), position = position_stack (vjust = 0.5)) + 
  theme_void ()

## category spending growth 2023 -> 2030 bar chart w line ----------------------

cats_growth30bar = cats_wpersonalcare %>%
  
  # aggregate for global
  dplyr::group_by (year, category) %>%
  dplyr::summarise (spend = sum (total_category_exp_nominal, na.rm = T) / 10^12) %>%
  
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
  
  # building chart
  ggplot (aes (x = reorder (category, -delta))) + 
  geom_bar (aes (y = delta, fill = category), stat = "identity") + 
  geom_line (aes (y = CAGR_2030, group = 1), stat = "identity", color = "black", linewidth = 1.5) +
  geom_text (aes (y = delta, label = paste0 ("+ $", round (delta, 1), " ", "T")), vjust = -0.5) +
  worlddataverse::theme_wdl() +
  theme(axis.text.x = element_text (angle = 360)) +
  scale_fill_wdl() + 
  labs (x = "\nCategory", 
        y = "Absolute change in spending between 2023 and 2030\n") +
  
  # adjusting the two y-axis scales
  scale_y_continuous(
    name = "Absolute change in spending between 2023 and 2030\n",
    breaks = c(0,1,2,3,4,5,6),
    sec.axis = sec_axis(
      ~ ., 
      name = "CAGR 2030\n", 
      breaks = c(0,1,2,3,4,5,6), 
      labels = c(0,1,2,3,4,5,6)
    )
  )


cats_growth30bar

## total spending 2023, 2024 and 2030 w CAGRs ----------------------------------

# obtaining totals for relevant years
total_nom = cats_wpersonalcare %>%
  
  # filter for food
 # filter (category == "Food") %>%
  
  # aggregate for global
  dplyr::group_by (year) %>%
  dplyr::summarise (spend = sum (total_category_exp_nominal, na.rm = T) / 10^12) %>%
  
  # filter for relevant year range
  filter (year %in% 2023:2030) %>%
  
  # applying CAGR function
  mutate (CAGR = CAGR_fun (spend) * 100) %>%
  
  # extracting relevant years
  filter (year %in% c(2023, 2024, 2030))

## adding GDP growth
gdp = wec_dat %>%
  
  # aggregating
  dplyr::group_by (iso3c, year) %>%
  dplyr::summarise (gdp = mean (gdp, na.rm = T)) %>%
  
  # aggregating by yearly
  dplyr::group_by (year) %>%
  dplyr::summarise (gdp = sum (gdp, na.rm = T)) %>%
  
  # filtering for relevant years
  filter (year %in% c(2023:2030)) %>%
  
  # adding CAGRs
  mutate (CAGR = CAGR_fun (gdp) * 100)
  
  
  
  pivot_wider (names_from = "year", values_from = "gdp") %>%
  mutate (delta2324 = ((`2024` - `2023`) / `2023`) *100,
          delta2330 = ((`2030` - `2023`) / `2023`) *100)
  
## CAGRs in real terms
  total = cats_wpersonalcare %>%
    
    # aggregate for global
    dplyr::group_by (ccode, year) %>%
    dplyr::summarise (spend = sum (total_category_exp_nominal, na.rm = T)) %>%
    
    # converting to real
    left_join (adj %>% 
                 filter (year == 2023) %>%
                 dplyr::select (-year), 
               by = "ccode") %>%
    
    # filter for relevant year range
    filter (year %in% 2023:2030) %>%
    
    # create real figs
    mutate (spend_real = spend * hhe_2017ppp_to_current_USD) %>%
    
    # aggregating for global
    dplyr::group_by (year) %>%
    dplyr::summarise (spend_real = sum (spend_real, na.rm = T)) %>%
    
    # applying CAGR function
    mutate (CAGR = CAGR_fun (spend_real) * 100) %>%
    
    # extracting relevant years
    filter (year %in% c(2023, 2024, 2030))

## total spending growth 24 and 30 by category (table)--------------------------

# building data
catstotalgrowth24 = cats_wpersonalcare %>%
  
  # aggregating into global level
  dplyr::group_by (year, category) %>%
  dplyr::summarise (total_category_exp_nominal = sum (total_category_exp_nominal, na.rm = T)) %>%
  
  # filter for years
  filter (year %in% c(2023,2024,2030)) %>%
  
  # widening
  pivot_wider (names_from = 1, values_from = 3) %>%
  
  # computing deltas
  mutate (delta_24 = paste0 (round ((`2024` - `2023`) / 10^9,0), " B USD"), 
          delta_30 = paste0 (round ((`2030` - `2023`) / 10^9,0), " B USD")) %>%
  
  # keeping relevant vars
  dplyr::select (category, delta_24, delta_30)



## waterfall growth by category ------------------------------------------------

## 2030
cats_waterfall = cats_wpersonalcare %>%
  
  # aggregating into year-group-exp
  dplyr::group_by (year, category) %>%
  dplyr::summarise (exp = sum (total_category_exp_nominal, na.rm = T)) %>%
  
  # filtering for years of interest
  filter (year %in% c(2023, 2030)) %>%
  
  # calculating spending group deltas
  dplyr::group_by (category) %>%
  dplyr::summarise (
    exp_delta = sum (exp [year == 2030]) - sum (exp [year == 2023]),
    exp_delta = round (exp_delta/10^9, digits = 2)
  )

# adding 2023 total
cats_waterfall [nrow (cats_waterfall) + 1, ] <- 
  list ("Global \nSpending 2023", 
        round (sum (cats_wpersonalcare$total_category_exp_nominal 
                    [cats_wpersonalcare$year == 2023]) / 10^9, 2)
  )

# ordering
cats_waterfall <- cats_waterfall %>%
  arrange (desc (exp_delta))

# adding chart
library (waterfalls)
waterfall(cats_waterfall,
          calc_total = TRUE, # TRUE: means it will calculate total, which will also be equal to BEAUTY TOTAL 2030
          values = as.numeric (exp_delta),
          total_rect_color = "lightblue",
          total_rect_text_color = "black",
          total_axis_text = "Global \nspending 2030",
          scale_y_to_waterfall = TRUE,
          rect_width = 1,
          draw_lines = FALSE,
          rect_border = NA) +
  theme_wdl() +
  labs (x = "", 
        y = "Global Expenditure (nominal USD)",
        title = 'Global Expenditure Growth by Category') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))

## 2024
cats_waterfall = cats_wpersonalcare %>%
  
  # aggregating into year-group-exp
  dplyr::group_by (year, category) %>%
  dplyr::summarise (exp = sum (total_category_exp_nominal, na.rm = T)) %>%
  
  # filtering for years of interest
  filter (year %in% c(2023, 2024)) %>%
  
  # calculating spending group deltas
  dplyr::group_by (category) %>%
  dplyr::summarise (
    exp_delta = sum (exp [year == 2024]) - sum (exp [year == 2023]),
    exp_delta = round (exp_delta/10^9, digits = 2)
  )

# adding 2023 total
cats_waterfall [nrow (cats_waterfall) + 1, ] <- 
  list ("Global \nSpending 2023", 
        round (sum (cats_wpersonalcare$total_category_exp_nominal 
                    [cats_wpersonalcare$year == 2023]) / 10^9, 2)
  )

# ordering
cats_waterfall <- cats_waterfall %>%
  arrange (desc (exp_delta))

# adding chart
library (waterfalls)
waterfall(cats_waterfall,
          calc_total = TRUE, # TRUE: means it will calculate total, which will also be equal to BEAUTY TOTAL 2024
          values = as.numeric (exp_delta),
          total_rect_color = "lightblue",
          total_rect_text_color = "black",
          total_axis_text = "Global \nspending 2024",
          scale_y_to_waterfall = TRUE,
          rect_width = 1,
          draw_lines = FALSE,
          rect_border = NA) +
  theme_wdl() +
  labs (x = "", 
        y = "Global Expenditure (nominal USD)",
        title = 'Global Expenditure Growth by Category') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))



## total PC spending in 2023, 2024 and 2030 w CAGRs ----------------------------

# obtaining totals for relevant years
total_pc = cats_wpersonalcare %>%
  
  # isolating category
  filter (category == "Pers Care") %>%
  
  # aggregate for global
  dplyr::group_by (year) %>%
  dplyr::summarise (spend = sum (total_category_exp_nominal, na.rm = T) / 10^9) %>%
  
  # filter for relevant year range
  filter (year %in% 2023:2030) %>%
  
  # applying CAGR function
  mutate (CAGR = CAGR_fun (spend) * 100) %>%
  
  # extracting relevant years
  filter (year %in% c(2023, 2024, 2030))

## category spending by region -------------------------------------------------

## aggregate spending 2023 2024 2030 
cats_region_total = cats_wpersonalcare %>%
  
  # aggregating into year-region-category
  dplyr::group_by (year, region, category) %>%
  dplyr::summarise (total_exp_nominal = sum (total_exp_nominal, na.rm = T), 
                    total_category_exp_nominal = sum (total_category_exp_nominal, na.rm = T)) %>%
  
  # isolating years of interest
  filter (year == 2023) %>%
  
  # computing shares
  mutate (share = total_category_exp_nominal / total_exp_nominal) %>%
  
  # filtering for regions
  #filter (region %in% c("North America", "South Asia","Sub-Saharan Africa")) %>%
  
  # order by share
  #dplyr::group_by (region) %>%
  #mutate (category = as.factor (category)) %>%
  #mutate (category = factor (category,
                             #levels = category [order (share)],
                             #ordered = T)) %>% ungroup () %>%

  # building plot
  ggplot (aes (x = factor (year), y = share * 100, fill = category)) + 
  geom_bar (stat = "identity", position = "stack") + 
  geom_text (aes (y = share * 100, label = paste0 (round (share * 100, 1), "%")),
             position = position_stack (vjust = 0.5)) +  
  worlddataverse::theme_wdl() + 
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral") )(13)) +  
  facet_wrap (~region, strip.position = "top", nrow = 1, ncol = 7) + 
  theme (legend.position = "bottom",
         #legend.key.height= unit(1, 'mm'),
         #legend.key.width= unit(1, 'mm'),
         #legend.margin = margin (l = 1, r = 1, unit = "mm"),
         axis.text.x = element_text (angle = 360, vjust = 1)) + 
  labs (x = "",
        y = "Category share of total expenditure (% of total)\n")
#theme (panel.margin = grid::unit (-1.25, "lines"))

cats_region_total



## category spending by year (pakistan) -------------------------------------------

## building chart
cats_idn_total = cats_wpersonalcare %>%
  
  # isolating Indonesia
  filter (ccode %in% "PAK") %>%
  
  # aggregating into region
  dplyr::group_by (year, ccode, category) %>%
  dplyr::summarise (total_exp_nominal = sum (total_exp_nominal, na.rm = T), 
                    total_category_exp_nominal = sum (total_category_exp_nominal, na.rm = T)) %>%
  
  # computing share
  mutate (share = total_category_exp_nominal / total_exp_nominal) %>%
  
  # isolating years of interest
  filter (year %in% c(2023, 2024, 2030)) %>%
  
  # building plot
  ggplot (aes (x = factor (year), y = share * 100, fill = category)) + 
  geom_col () + 
  #geom_text (aes (y = share, label = paste0 (round (share * 100, 1), "%")),
  #position = position_dodge(width = 0.9),
  #vjust = 0.5) +  
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl() + 
  facet_wrap (~ccode, strip.position = "top", nrow = 1, ncol = 20) + 
  theme (#legend.position = "bottom",
         #legend.key.height= unit(1, 'mm'),
         #legend.key.width= unit(1, 'mm'),
         #legend.margin = margin (l = 1, r = 1, unit = "mm"),
         axis.text.x = element_text (angle = 45, vjust = 1)) + 
  labs (x = "",
        y = "Category share of total expenditure (% of total)\n")
#theme (panel.margin = grid::unit (-1.25, "lines"))

cats_idn_total


## G20
cats_g20_total = cats_wpersonalcare %>%
  
  # recoding countries into the EU
  mutate (ccode = ifelse (ccode %in% EU_ISO3, "EU", ccode)) %>%
  
  # isolating G20 countries
  filter (ccode %in% G20_ISO3) %>%
  
  # aggregating into region
  dplyr::group_by (year, ccode, category) %>%
  dplyr::summarise (total_exp_nominal = sum (total_exp_nominal, na.rm = T), 
                    total_category_exp_nominal = sum (total_category_exp_nominal, na.rm = T)) %>%
  
  # computing share
  mutate (share = total_category_exp_nominal / total_exp_nominal) %>%
  
  # isolating years of interest
  filter (year %in% c(2023, 2024, 2030)) %>%
  
  # building plot
  ggplot (aes (x = factor (year), y = share * 100, fill = category)) + 
  geom_col () + 
  #geom_text (aes (y = share, label = paste0 (round (share * 100, 1), "%")),
  #position = position_dodge(width = 0.9),
  #vjust = 0.5) +  
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl() + 
  facet_wrap (~ccode, strip.position = "top", nrow = 1, ncol = 20) + 
  theme (legend.position = "bottom",
         #legend.key.height= unit(1, 'mm'),
         #legend.key.width= unit(1, 'mm'),
         #legend.margin = margin (l = 1, r = 1, unit = "mm"),
         axis.text.x = element_text (angle = 45, vjust = 1)) + 
  labs (x = "",
        y = "Category share of total expenditure (% of total)\n")
#theme (panel.margin = grid::unit (-1.25, "lines"))

cats_g20_total






## top 12 movers dodged bar chart ----------------------------------------------

## building data

# isolating years of interest
inputlarge = pc_reformat %>% filter (Year %in% c(2023:2030))

# getting top 12 countries by 2030 exp
inputlarge30 = inputlarge %>%
  filter (Year == 2030) %>%
  arrange (desc (`Personal Care`))

# cutting the top 12
inputlarge30 = inputlarge30 [1:12,]

# converting spending to billions
inputlarge = inputlarge %>% 
  mutate (`Personal Care` = `Personal Care` / 10^9)

# filtering for the 12 countries
filtered = inputlarge %>% filter (Country %in% inputlarge30$Country) %>%
  
  # renaming
  rename (ccode = Country, 
          year = Year, 
          total_category_exp_nominal = `Personal Care`)

## chart code
ggplot(data = filtered %>% 
         filter (year %in% c(2023, 2024, 2030)), aes(x = reorder (ccode, -total_category_exp_nominal),
                                                     y = total_category_exp_nominal,
                                                     fill = factor(year))) +
  
  # building bar plot
  geom_bar (stat = "identity", position = "dodge") +
  
  # adding label on top
  geom_text (aes(y = total_category_exp_nominal, 
                 label = round (total_category_exp_nominal, 1)), 
             position = position_dodge (width = .9), 
             #vjust = -0.5,
             hjust = -0.1,
             angle = 90) + 
  
  # adding title and axis label and legend label 
  labs(title = "", x = "ccode", fill = "year") +
  
  # theme: adjusting x-axis labels
  theme(axis.text.x = element_text (angle = 45, hjust = 1)) +
  
  # rescaling y axis
  #scale_y_continuous(
  #name = "Total Category Expenditure (in billion USD)",
  #breaks = c(0, 1000, 2000, 3000, 4000)) +
  
  # WDL theme, colours and font
  worlddataverse::scale_fill_wdl2() +
  worlddataverse::font_wdl() +
  worlddataverse::theme_wdl() + 
  
  ylim (0, 250)




## top 20 movers by 2030 growth in personal care -------------------------------

## building data

# isolating years of interest
inputlarge = pc_reformat %>% 
  
  # renaming
  rename (ccode = Country, 
          year = Year, 
          total_category_exp_nominal = `Personal Care`) %>%
  
  # isolating years
  filter (year %in% c(2023, 2030)) %>%
  
  # converting to real
  left_join (adj, by = c("ccode", "year")) %>%
  mutate (total_category_exp_nominal = total_category_exp_nominal / hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD) %>%
  left_join (adj %>% filter (year == 2023) %>% dplyr::select (-year), by = "ccode") %>%
  mutate (total_category_exp_nominal = total_category_exp_nominal * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD)

# getting top 20 countries by 2030 growth
inputlarge30 = inputlarge %>%
  
  # widening for delta calculation
  pivot_wider (names_from = "year", values_from = 'total_category_exp_nominal') %>%
  
  # computing delta in billions
  mutate (delta = (`2030` - `2023`) / 10^9) %>%
  
  # getting the top 20
  arrange (desc (delta))

# slicing top 20 by delta
inputlarge30 = inputlarge30 [1:20,]

## chart code
ggplot (data = inputlarge30, 
        aes (x = reorder (ccode, -delta),
             y = delta,
             fill = factor (ccode))) +
  
  # building bar plot
  geom_bar (stat = "identity") +
  
  # adding label on top
  geom_text (aes (y = delta, 
                  label = paste0 ("+", " ", "$", round (delta, 0), " ", "B")), 
             vjust = -0.5) + 
  
  # adding title and axis label and legend label 
  labs (title = "", x = "Country", fill = "Country") +
  
  # theme: adjusting x-axis labels
  theme (axis.text.x = element_text (angle = 360, hjust = 1)) +
  
  # rescaling y axis
  #scale_y_continuous(
  #name = "Total Category Expenditure (in billion USD)",
  #breaks = c(0, 1000, 2000, 3000, 4000)) +
  
  # WDL theme, colours and font
  worlddataverse::theme_wdl()

## bottom 20 movers by 2030 growth in personal care ----------------------------

## population df for setting threshold
popdat = wdp %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T))

## building data

# isolating years of interest
inputlarge = pc_reformat %>% 
  
  # renaming
  rename (ccode = Country, 
          year = Year, 
          total_category_exp_nominal = `Personal Care`) %>%
  
  # isolating years
  filter (year %in% c(2023, 2030))


# merging with pop
inputlarge30 = inputlarge %>%
  
  # widening for delta calculation
  pivot_wider (names_from = "year", values_from = 3) %>%
  
  # computing delta in billions
  mutate (delta = (`2030` - `2023`) / 10^6) %>%
  
  # merging with popdat
  left_join (popdat %>% filter (year == 2023), by = c("ccode")) %>%
  
  # filtering for countries above 25M ppl
  filter (pop >= 25000000) %>%
  
  # getting the top 20
  arrange (delta)

# extracting bottom 20
inputlarge30 = inputlarge30 [1:20,]

# removing afg
inputlarge30 = inputlarge30 %>% filter (!ccode %in% "AFG")

## chart code
ggplot (data = inputlarge30, 
        aes (x = reorder (ccode, delta),
             y = delta,
             fill = factor (ccode))) +
  
  # building bar plot
  geom_bar (stat = "identity") +
  
  # adding label on top
  geom_text (aes (y = delta, 
                  label = paste0 ("+", " ", "$", round (delta, 0), " ", "M")), 
             vjust = -0.5) + 
  
  # adding title and axis label and legend label 
  labs (title = "", x = "Country", fill = "Country") +
  
  # theme: adjusting x-axis labels
  theme (axis.text.x = element_text (angle = 360, hjust = 1)) +
  
  # rescaling y axis
  #scale_y_continuous(
  #name = "Total Category Expenditure (in billion USD)",
  #breaks = c(0, 1000, 2000, 3000, 4000)) +
  
  # WDL theme, colours and font
  worlddataverse::theme_wdl()


## top 20 movers by 2030 CAGR --------------------------------------------------

# isolating years of interest
inputlarge = pc_reformat %>% 
  
  # renaming
  rename (ccode = Country, 
          year = Year, 
          total_category_exp_nominal = `Personal Care`) %>%
  
  # isolating years
  filter (year %in% c(2023:2030))

# getting top 20 countries by 2030 growth
inputlarge30 = inputlarge %>%
  
  # computing CAGRs
  dplyr::group_by (ccode) %>%
  mutate (CAGR = CAGR_fun(total_category_exp_nominal) * 100) %>%
  
  # isolating 2030 CAGRs
  filter (year == 2030) %>%
  
  # merging with popdat
  left_join (popdat %>% filter (year == 2030), by = c("ccode")) %>%
  
  # filtering for countries above 25M ppl
  filter (pop >= 25000000) %>%
  
  # arranging
  arrange (desc (CAGR))

# slicing
inputlarge30 = inputlarge30 [1:20,]

## chart code
ggplot (data = inputlarge30, 
        aes (x = reorder (ccode, -CAGR),
             y = CAGR,
             fill = factor (ccode))) +
  
  # building bar plot
  geom_bar (stat = "identity") +
  
  # adding label on top
  geom_text (aes (y = CAGR, 
                  label = paste0 ("+", " ", round (CAGR, 1), " ", "%")), 
             vjust = -0.5) + 
  
  # adding title and axis label and legend label 
  labs (title = "", x = "Country", fill = "Country") +
  
  # theme: adjusting x-axis labels
  theme (axis.text.x = element_text (angle = 360, hjust = 1)) +
  
  # rescaling y axis
  #scale_y_continuous(
  #name = "Total Category Expenditure (in billion USD)",
  #breaks = c(0, 1000, 2000, 3000, 4000)) +
  
  # WDL theme, colours and font
  worlddataverse::theme_wdl()
  
  
## bottom 20 movers by 2030 CAGR --------------------------------------------------
  
inputlarge30 = inputlarge %>%
  
  # computing CAGRs
  dplyr::group_by (ccode) %>%
  mutate (CAGR = CAGR_fun(total_category_exp_nominal) * 100) %>%
  
  # isolating 2030 CAGRs
  filter (year == 2030) %>%
  
  # merging with popdat
  left_join (popdat %>% filter (year == 2030), by = c("ccode")) %>%
  
  # filtering for countries above 25M ppl
  filter (pop >= 25000000 & !ccode == "AFG") %>%
  
  # arranging
  arrange (CAGR)

# slicing
inputlarge30 = inputlarge30 [1:20,]

## chart code
ggplot (data = inputlarge30, 
        aes (x = reorder (ccode, CAGR),
             y = CAGR,
             fill = factor (ccode))) +
  
  # building bar plot
  geom_bar (stat = "identity") +
  
  # adding label on top
  geom_text (aes (y = CAGR, 
                  label = paste0 ("+", " ", round (CAGR, 1), " ", "%")), 
             vjust = -0.5) + 
  
  # adding title and axis label and legend label 
  labs (title = "", x = "Country", fill = "Country") +
  
  # theme: adjusting x-axis labels
  theme (axis.text.x = element_text (angle = 360, hjust = 1)) +
  
  # rescaling y axis
  #scale_y_continuous(
  #name = "Total Category Expenditure (in billion USD)",
  #breaks = c(0, 1000, 2000, 3000, 4000)) +
  
  # WDL theme, colours and font
  worlddataverse::theme_wdl()
  
  
## top 20 contrbutors to alcohol -----------------------------------------------
  
## change the category filter at your wish
input_dat = cats_wpersonalcare %>% filter (category == "Alcoholic beverages, tobacco and narcotics")

## building data

# isolating years of interest
inputlarge = input_dat %>% filter (year %in% c(2023, 2030))

# getting top 20 countries by 2030 growth
inputlarge30 = inputlarge %>%
  
  # widening for delta calculation
  pivot_wider (names_from = "year", values_from = 5:6) %>%
  
  # computing delta in billions
  mutate (delta = (total_category_exp_nominal_2030 - total_category_exp_nominal_2023) / 10^9) %>%
  
  # getting the top 20
  arrange (desc (delta)) %>%
  slice(1:20)

# chart code
ggplot (data = inputlarge30, 
        aes (x = reorder (ccode, -delta),
             y = delta,
             fill = factor (ccode))) +
  
  # building bar plot
  geom_bar (stat = "identity") +
  
  # adding label on top
  geom_text (aes (y = delta, 
                  label = paste0 ("+", " ", "$", round (delta, 0), " ", "B")), 
             vjust = -0.5) + 
  
  # adding title and axis label and legend label 
  labs (title = "", x = "Country", fill = "Country") +
  
  # theme: adjusting x-axis labels
  theme (axis.text.x = element_text (angle = 360, hjust = 1)) +
  
  # rescaling y axis
  #scale_y_continuous(
  #name = "Total Category Expenditure (in billion USD)",
  #breaks = c(0, 1000, 2000, 3000, 4000)) +
  
  # WDL theme, colours and font
  worlddataverse::theme_wdl()

# isolating years of interest
inputlarge = input_dat %>% filter (year %in% c(2023:2030)) %>%
  
  # computing CAGRs
  dplyr::group_by (ccode, category) %>%
  dplyr::mutate (CAGR = CAGR_fun (total_category_exp_nominal) * 100) %>% ungroup () %>%
  
  # keeping only 2030
  filter (year == 2030) %>%
  
  # getting the top 20
  # getting the top 20
  arrange (desc (CAGR)) %>%
  slice(1:20)

# chart code
ggplot (data = inputlarge, 
        aes (x = reorder (ccode, -CAGR),
             y = CAGR,
             fill = factor (ccode))) +
  
  # building bar plot
  geom_bar (stat = "identity") +
  
  # adding label on top
  geom_text (aes (y = CAGR, 
                  label = paste0 ("+", " ", round (CAGR, 1), " ", "%")), 
             vjust = -0.5) + 
  
  # adding title and axis label and legend label 
  labs (title = "", x = "Country", fill = "Country") +
  
  # theme: adjusting x-axis labels
  theme (axis.text.x = element_text (angle = 360, hjust = 1)) +
  
  # rescaling y axis
  #scale_y_continuous(
  #name = "Total Category Expenditure (in billion USD)",
  #breaks = c(0, 1000, 2000, 3000, 4000)) +
  
  # WDL theme, colours and font
  worlddataverse::theme_wdl()
  



## waterfall (total spending by spend group 23 -> 30) --------------------------

## loading in shares
adj <- readRDS(
  file.path(
    input_path,
    "adjusted_conversion_factors.rds"))

## cleaning PPP data initially into country, year, spending groups and expenditure
wdp_waterfall_ppp = wdp %>%
  
  # aggregating into relevant var groups
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T))

## converting to real
wdp_waterfall_real = wdp_waterfall_ppp %>%
  
  # adding conversion factors
  left_join (adj %>% 
               filter (year == 2023) %>% 
               dplyr::select (-year), 
             by = "ccode") %>%
  
  # converting exp to nominal
  mutate (exp = exp * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD)

## converting to nominal
wdp_waterfall_nominal = wdp_waterfall_ppp %>%
  
  # adding conversion factors
  left_join (adj, by = c("year", "ccode")) %>%
  
  # converting exp to nominal
  mutate (exp = exp * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD)

## constructing chart components

# build up
wdp_waterfall = wdp_waterfall_real %>%
  
  # aggregating into year-group-exp
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # filtering for years of interest
  filter (year %in% c(2023, 2030)) %>%
  
  # calculating spending group deltas
  dplyr::group_by (daily_spending) %>%
  dplyr::summarise (
    exp_delta = sum (exp [year == 2030]) - sum (exp [year == 2023]),
    exp_delta = round (exp_delta/10^12, digits = 2)
  )

# adding 2023 total
wdp_waterfall [nrow (wdp_waterfall) + 1, ] <- 
  list ("Global Spending 2023", 
        round (sum (wdp_waterfall_real$exp [wdp_waterfall_nominal$year == 2023]) / 10^12, 2)
  )

# adding inflation
wdp_waterfall [nrow (wdp_waterfall) + 1, ] <- 
  list ("Inflation",
        round ((sum (wdp_waterfall_nominal$exp [wdp_waterfall_nominal$year == 2030]) - 
                  sum (wdp_waterfall_real$exp [wdp_waterfall_real$year == 2030]))/ 10^12, 2)
  )

# ordering
wdp_waterfall <- wdp_waterfall %>%
  arrange (desc (exp_delta))

# adding chart
library (waterfalls)
waterfall(wdp_waterfall,
          calc_total = TRUE, # TRUE: means it will calculate total, which will also be equal to BEAUTY TOTAL 2030
          values = as.numeric (exp_delta),
          total_rect_color = "lightblue",
          total_rect_text_color = "black",
          total_axis_text = "Global spending 2030",
          scale_y_to_waterfall = TRUE,
          rect_width = 1,
          draw_lines = FALSE,
          rect_border = NA) +
  theme_wdl() +
  labs (x = "", 
        y = "Global Expenditure (nominal USD)",
        title = 'Global Expenditure Growth by Spending Group') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))

## version with inflation re-absorbed
wdp_waterfall = wdp_waterfall_nominal %>%
  
  # aggregating into year-group-exp
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # filtering for years of interest
  filter (year %in% c(2023, 2030)) %>%
  
  # calculating spending group deltas
  dplyr::group_by (daily_spending) %>%
  dplyr::summarise (
    exp_delta = sum (exp [year == 2030]) - sum (exp [year == 2023]),
    exp_delta = round (exp_delta/10^12, digits = 2)
  )

## adding 2023 total
wdp_waterfall [nrow (wdp_waterfall) + 1, ] <- 
  list ("Global Spending 2023", 
        round (sum (wdp_waterfall_nominal$exp [wdp_waterfall_nominal$year == 2023]) / 10^12, 2)
  )

# ordering
wdp_waterfall <- wdp_waterfall %>%
  arrange (desc (exp_delta))

waterfall(wdp_waterfall,
          calc_total = TRUE, # TRUE: means it will calculate total, which will also be equal to BEAUTY TOTAL 2030
          values = as.numeric (exp_delta),
          total_rect_color = "lightblue",
          total_rect_text_color = "black",
          total_axis_text = "Global spending 2030",
          scale_y_to_waterfall = TRUE,
          rect_width = 1,
          draw_lines = FALSE,
          rect_border = NA) +
  theme_wdl() +
  labs (x = "", 
        y = "Global Expenditure (nominal USD)",
        title = 'Global Expenditure Growth by Spending Group') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))


## waterfall (total spending by age 23 -> 30) --------------------------

## loading in shares
adj <- readRDS(
  file.path(
    input_path,
    "adjusted_conversion_factors.rds"))

## cleaning PPP data initially into country, year, spending groups and expenditure
wdp_waterfall_ppp = wdp %>%
  
  # aggregating into relevant var groups
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # recoding age groups 
  mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)","[10,15)"), "[00,15)",
                              ifelse (age_group %in% c("[15,20)","[20,25)","[25,30)"), "[15,30)",
                                      ifelse (age_group %in% c("[30,35)","[35,40)","[40,45)"), "[30,45)",
                                              ifelse (age_group %in% c("[45,50)","[50,55)","[55,60)","[60,65)"), "[45,65)", "[65,Inf]"
                                              ))))) %>%
  # re-aggregating
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T))
  

## converting to real
wdp_waterfall_real = wdp_waterfall_ppp %>%
  
  # adding conversion factors
  left_join (adj %>% 
               filter (year == 2023) %>% 
               dplyr::select (-year), 
             by = "ccode") %>%
  
  # converting exp to nominal
  mutate (exp = exp * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD)

## converting to nominal
wdp_waterfall_nominal = wdp_waterfall_ppp %>%
  
  # adding conversion factors
  left_join (adj, by = c("year", "ccode")) %>%
  
  # converting exp to nominal
  mutate (exp = exp * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD)

## constructing chart components

# build up
wdp_waterfall = wdp_waterfall_real %>%
  
  # aggregating into year-group-exp
  dplyr::group_by (year, age_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # filtering for years of interest
  filter (year %in% c(2023, 2030)) %>%
  
  # calculating spending group deltas
  dplyr::group_by (age_group) %>%
  dplyr::summarise (
    exp_delta = sum (exp [year == 2030]) - sum (exp [year == 2023]),
    exp_delta = round (exp_delta/10^12, digits = 2)
  )

# adding 2023 total
wdp_waterfall [nrow (wdp_waterfall) + 1, ] <- 
  list ("Global Spending 2023", 
        round (sum (wdp_waterfall_real$exp [wdp_waterfall_nominal$year == 2023]) / 10^12, 2)
  )

# adding inflation
wdp_waterfall [nrow (wdp_waterfall) + 1, ] <- 
  list ("Inflation",
        round ((sum (wdp_waterfall_nominal$exp [wdp_waterfall_nominal$year == 2030]) - 
                  sum (wdp_waterfall_real$exp [wdp_waterfall_real$year == 2030]))/ 10^12, 2)
  )

# ordering
wdp_waterfall <- wdp_waterfall %>%
  arrange (desc (exp_delta))

# adding chart
library (waterfalls)
waterfall(wdp_waterfall,
          calc_total = TRUE, # TRUE: means it will calculate total, which will also be equal to BEAUTY TOTAL 2030
          values = as.numeric (exp_delta),
          total_rect_color = "lightblue",
          total_rect_text_color = "black",
          total_axis_text = "Global spending 2030",
          scale_y_to_waterfall = TRUE,
          rect_width = 1,
          draw_lines = FALSE,
          rect_border = NA) +
  theme_wdl() +
  labs (x = "", 
        y = "Global Expenditure (nominal USD)",
        title = 'Global Expenditure Growth by Spending Group') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))

## inflation absorbed version
wdp_waterfall = wdp_waterfall_nominal %>%
  
  # aggregating into year-group-exp
  dplyr::group_by (year, age_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # filtering for years of interest
  filter (year %in% c(2023, 2030)) %>%
  
  # calculating spending group deltas
  dplyr::group_by (age_group) %>%
  dplyr::summarise (
    exp_delta = sum (exp [year == 2030]) - sum (exp [year == 2023]),
    exp_delta = round (exp_delta/10^12, digits = 2)
  )

## adding 2023 total
wdp_waterfall [nrow (wdp_waterfall) + 1, ] <- 
  list ("Global Spending 2023", 
        round (sum (wdp_waterfall_nominal$exp [wdp_waterfall_nominal$year == 2023]) / 10^12, 2)
  )

# ordering
wdp_waterfall <- wdp_waterfall %>%
  arrange (desc (exp_delta))

waterfall(wdp_waterfall,
          calc_total = TRUE, # TRUE: means it will calculate total, which will also be equal to BEAUTY TOTAL 2030
          values = as.numeric (exp_delta),
          total_rect_color = "lightblue",
          total_rect_text_color = "black",
          total_axis_text = "Global spending 2030",
          scale_y_to_waterfall = TRUE,
          rect_width = 1,
          draw_lines = FALSE,
          rect_border = NA) +
  theme_wdl() +
  labs (x = "", 
        y = "Global Expenditure (nominal USD)",
        title = 'Global Expenditure Growth by Spending Group') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))




## waterfall (indonesia food spend by spend group 23 -> 30) --------------------

## cleaning data
food_pak_waterfall_ppp = food %>%
  
  # filtering for years of interest
  filter (year %in% c(2023, 2030)) %>%
  
  # variables of interest
  dplyr::select (year, age_group_wdp, spending_group, total_exp_food_group, total_exp_beverages_group) %>%
  
  # getting food and bevs combined exp
  mutate (exp = total_exp_food_group + total_exp_beverages_group) %>%
  dplyr::select (year, age_group_wdp, spending_group, exp)

## converting to nominal
food_pak_waterfall_nominal = food_pak_waterfall_ppp %>%
  
  # joining with conversion factors
  left_join (adj %>% filter (ccode == "PAK") %>% dplyr::select (-ccode), by = "year") %>%
  mutate (exp = exp * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD)

## converting to real
food_pak_waterfall_real = food_pak_waterfall_ppp %>%
  
  # converting
  mutate (exp = exp * adj$hhe_2017ppp_to_current_USD [adj$ccode == "PAK" & adj$year == 2023])














## unconverting from nominal to PPP
food_idn_waterfall_dat_ppp = food_idn_waterfall_dat %>%
  
  # joining conversion factors
  left_join (adj %>% filter (ccode == "IDN"), by = "year") %>%
  mutate (total_exp_foodbevs_ppp = total_exp_foodbevs / hhe_2017ppp_to_current_USD) %>%
  dplyr::select (year, age_group_wdp, spending_group, total_exp_foodbevs_ppp)

## converting to real
food_idn_waterfall_dat_real = food_idn_waterfall_dat_ppp %>%
  
  # muliplying all spending with 2023 IDN conv factor
  mutate (total_exp_foodbevs_real = total_exp_foodbevs_ppp * adj$hhe_2017ppp_to_current_USD [adj$ccode == "IDN" & adj$year == 2023]) %>%
  dplyr::select (year, age_group_wdp, spending_group, total_exp_foodbevs_real)
  

## constructing chart components
food_idn_waterfall = food_idn_waterfall_dat %>%
  
  # aggregating into group
  dplyr::group_by (year, spending_group) %>%
  dplyr::summarise (total_exp_foodbevs = sum (total_exp_foodbevs, na.rm = T)) %>%
  
  
  # calculating spending group deltas
  dplyr::group_by (spending_group) %>%
  dplyr::summarise (
    exp_delta = sum (total_exp_foodbevs [year == 2030]) - sum (total_exp_foodbevs [year == 2023]),
    exp_delta = round (exp_delta/10^9, digits = 2)
  )

# adding 2023 total
food_idn_waterfall [nrow (food_idn_waterfall) + 1, ] <- 
  list ("Food and Beverages\n Spending 2023", 
        round (sum (food_idn_waterfall_dat_real$total_exp_foodbevs_real [food_idn_waterfall_dat_real$year == 2023]) / 10^9, 2)
  )

# adding inflation
food_idn_waterfall [nrow (food_idn_waterfall) + 1, ] <- 
  list ("Inflation",
        round ((sum (food_idn_waterfall_dat$total_exp_foodbevs [food_idn_waterfall_dat$year == 2030]) - 
                  sum (food_idn_waterfall_dat_real$total_exp_foodbevs_real [food_idn_waterfall_dat_real$year == 2030]))/ 10^9, 2)
  )

# ordering
food_idn_waterfall <- food_idn_waterfall %>%
  arrange (desc (exp_delta))

# adding chart
library (waterfalls)
waterfall(food_idn_waterfall,
          calc_total = TRUE, # TRUE: means it will calculate total, which will also be equal to BEAUTY TOTAL 2030
          values = as.numeric (exp_delta),
          total_rect_color = "lightblue",
          total_rect_text_color = "black",
          total_axis_text = "Food and Beverages\n Spending 2030",
          scale_y_to_waterfall = TRUE,
          rect_width = 1,
          draw_lines = FALSE,
          rect_border = NA) +
  theme_wdl() +
  labs (x = "", 
        y = "Expenditure (nominal USD billions)",
        title = 'Food and Bevs Expenditure Growth by Spending Group') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))

## waterfall (indonesia food spend by age group 23 -> 30) ----------------------

## cleaning data
food_idn_waterfall_dat = food %>%
  
  # filtering for years of interest
  filter (year %in% c(2023, 2030)) %>%
  
  # variables of interest
  dplyr::select (year, age_group_wdp, spending_group, total_exp_food_group, total_exp_beverages_group) %>%
  
  # getting food and bevs combined exp
  mutate (total_exp_foodbevs = total_exp_food_group + total_exp_beverages_group) %>%
  dplyr::select (year, age_group_wdp, spending_group, total_exp_foodbevs)

## unconverting from nominal to PPP
food_idn_waterfall_dat_ppp = food_idn_waterfall_dat %>%
  
  # joining conversion factors
  left_join (adj %>% filter (ccode == "IDN"), by = "year") %>%
  mutate (total_exp_foodbevs_ppp = total_exp_foodbevs / hhe_2017ppp_to_current_USD) %>%
  dplyr::select (year, age_group_wdp, spending_group, total_exp_foodbevs_ppp)

## converting to real
food_idn_waterfall_dat_real = food_idn_waterfall_dat_ppp %>%
  
  # muliplying all spending with 2023 IDN conv factor
  mutate (total_exp_foodbevs_real = total_exp_foodbevs_ppp * adj$hhe_2017ppp_to_current_USD [adj$ccode == "IDN" & adj$year == 2023]) %>%
  dplyr::select (year, age_group_wdp, spending_group, total_exp_foodbevs_real)


## constructing chart components
food_idn_waterfall = food_idn_waterfall_dat %>%
  
  # aggregating into group
  dplyr::group_by (year, age_group_wdp) %>%
  dplyr::summarise (total_exp_foodbevs = sum (total_exp_foodbevs, na.rm = T)) %>%
  
  
  # calculating spending group deltas
  dplyr::group_by (age_group_wdp) %>%
  dplyr::summarise (
    exp_delta = sum (total_exp_foodbevs [year == 2030]) - sum (total_exp_foodbevs [year == 2023]),
    exp_delta = round (exp_delta/10^9, digits = 2)
  )

# adding 2023 total
food_idn_waterfall [nrow (food_idn_waterfall) + 1, ] <- 
  list ("Food and Beverages\n Spending 2023", 
        round (sum (food_idn_waterfall_dat_real$total_exp_foodbevs_real [food_idn_waterfall_dat_real$year == 2023]) / 10^9, 2)
  )

# adding inflation
food_idn_waterfall [nrow (food_idn_waterfall) + 1, ] <- 
  list ("Inflation",
        round ((sum (food_idn_waterfall_dat$total_exp_foodbevs [food_idn_waterfall_dat$year == 2030]) - 
                  sum (food_idn_waterfall_dat_real$total_exp_foodbevs_real [food_idn_waterfall_dat_real$year == 2030]))/ 10^9, 2)
  )

# ordering
food_idn_waterfall <- food_idn_waterfall %>%
  arrange (desc (exp_delta))

# adding chart
library (waterfalls)
waterfall(food_idn_waterfall,
          calc_total = TRUE, # TRUE: means it will calculate total, which will also be equal to BEAUTY TOTAL 2030
          values = as.numeric (exp_delta),
          total_rect_color = "lightblue",
          total_rect_text_color = "black",
          total_axis_text = "Food and Beverages\n Spending 2030",
          scale_y_to_waterfall = TRUE,
          rect_width = 1,
          draw_lines = FALSE,
          rect_border = NA) +
  theme_wdl() +
  labs (x = "", 
        y = "Expenditure (nominal USD billions)",
        title = 'Food and Bevs Expenditure Growth by Age Group') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))

# for tomorrow: start with the waterfall charts in the morning!















## waterfall (personal care by spend group) ----------------------------

## pc data with age
pc_age = read_csv (file.path (base_path, 
                              "spending_categories", 
                              "sample_data_wdp", 
                              "05_003_merge_mpro2_ages_R_2023_07_26_2017ppp_1_USD_with_categories_with_demogs_corrected.csv"))

## cleaning data
pc_age_waterfalldat = pc_age %>%
  
  # isolating vars
  dplyr::group_by (ccode, year, age_group_wdp, spending_group, exp) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T))

## cleaning data p2
pc_age_waterfalldat_nominal = pc_age_waterfalldat %>%
  
  # joining conversion factors
  left_join (adj, by = c("year", "ccode")) %>%
  
  # applying conversions
  mutate (exp = exp * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD)

## building chart components
pc_age_waterfall = pc_age_waterfalldat_nominal %>%
  
  # aggregating into year-group-exp
  dplyr::group_by (year, age_group_wdp) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # filtering for years of interest
  filter (year %in% c(2023, 2030)) %>%
  
  # calculating spending group deltas
  dplyr::group_by (age_group_wdp) %>%
  dplyr::summarise (
    exp_delta = sum (exp [year == 2030]) - sum (exp [year == 2023]),
    exp_delta = round (exp_delta/10^12, digits = 2)
  ) 

## adding 2023 total
pc_age_waterfall [nrow (pc_age_waterfall) + 1, ] <- 
  list ("Personal care\nspending 2023", 
        round (sum (pc_age_waterfalldat_nominal$exp [pc_age_waterfalldat_nominal$year == 2023]) / 10^12, 2)
  )

# ordering
pc_age_waterfall <- pc_age_waterfall %>%
  arrange (desc (exp_delta))

waterfall(pc_age_waterfall,
          calc_total = TRUE, # TRUE: means it will calculate total, which will also be equal to BEAUTY TOTAL 2030
          values = as.numeric (exp_delta),
          total_rect_color = "lightblue",
          total_rect_text_color = "black",
          total_axis_text = "Personal care\nspending 2030",
          scale_y_to_waterfall = TRUE,
          rect_width = 1,
          draw_lines = FALSE,
          rect_border = NA) +
  theme_wdl() +
  labs (x = "", 
        y = "Global Expenditure (nominal USD)",
        title = 'Global Expenditure Growth by Spending Group') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))

## IDN food and bev breakdown chart 23 -----------------------------------------

## data
cats_idn_totalbar = cats_wpersonalcare %>%
  
  # isolating Indonesia
  filter (ccode %in% "PAK") %>%
  
  # aggregating into region
  dplyr::group_by (year, ccode, category) %>%
  dplyr::summarise (total_exp_nominal = sum (total_exp_nominal, na.rm = T), 
                    total_category_exp_nominal = sum (total_category_exp_nominal, na.rm = T)) %>%
  
  # computing share
  mutate (share = total_category_exp_nominal / total_exp_nominal) %>%
  
  # isolating years of interest
  filter (year == 2023) 

## ordering factor levels
cats_idn_totalbar$category = factor (cats_idn_totalbar$category,
                                     levels = cats_idn_totalbar$category [order (-cats_idn_totalbar$share)],
                                     ordered = T)

# building plot
ggplot (data = cats_idn_totalbar, 
        aes (x = factor (year), y = share * 100, fill = category)) + 
  geom_col () + 
  geom_text (aes (label = ifelse (share > 0.02, 
                                  paste0 (scales::percent (share, 1), 
                                          ' ($', 
                                          round (total_category_exp_nominal / 10^9, 1), 
                                          "B)"),
                                  " ")), 
                  position = position_stack (vjust = 0.5)) + 
  worlddataverse::theme_wdl() + 
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral") )(13)) + 
  facet_wrap (~ccode, strip.position = "top", nrow = 1, ncol = 20) + 
  theme (#legend.position = "bottom",
    #legend.key.height= unit(1, 'mm'),
    #legend.key.width= unit(1, 'mm'),
    #legend.margin = margin (l = 1, r = 1, unit = "mm"),
    axis.text.x = element_text (angle = 45, vjust = 1)) + 
  labs (x = "",
        y = "Category share of total expenditure (% of total)\n")#theme (panel.margin = grid::unit (-1.25, "lines"))

## breaking down food and bevs into spend group
ggplot (data = food_idn_waterfall_dat %>%
          
          # filter for 2023
          filter (year == 2023) %>%
          
          # producing shares
          dplyr::group_by (year, spending_group) %>%
          dplyr::summarise (exp = sum (total_exp_foodbevs, na.rm = T)) %>%
          mutate (share = exp / sum (exp)) %>%
          
          # reordering
          mutate (spending_group = factor (spending_group, 
                                           levels =spending_group [order (-share)],
                                           ordered = T)),
        aes (x = factor (year), y = share, fill = spending_group)) + 
  geom_col () + 
  geom_text (aes (label = ifelse (share >=0.01, 
                                               paste0 (scales::percent (share, 1), 
                                                       ' ($', 
                                                       round (exp / 10^9, 1), 
                                                       "B)"),
                                               " ")), 
                  position = position_stack (vjust = 0.5))+ 
  worlddataverse::theme_wdl() + 
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral") )(5)) +
  theme (axis.text.x = element_text (angle = 45, vjust = 1))


india = data.frame (ccode = "IND", 
                    year = 2023, 
                    spending_group = "[0,12)",
                    exp = 8.215324e+11 * 0.316,
                    share = 0.316) 

row1 = data.frame (ccode = "IND", 
                   year = 2023, 
                   spending_group = "[12,40)",
                   exp = 8.215324e+11 * 0.304,
                   share = 0.304)

row2 = data.frame (ccode = "IND", 
                   year = 2023, 
                   spending_group = "[40,80)",
                   exp = 8.215324e+11 * 0.338,
                   share = 0.338)

row3 = data.frame (ccode = "IND", 
                   year = 2023, 
                   spending_group = "[80,120)",
                   exp = 8.215324e+11 * 0.031,
                   share = 0.031)

row4 = data.frame (ccode = "IND", 
                   year = 2023, 
                   spending_group = "[120,Inf)",
                   exp = 8.215324e+11 * 0.011,
                   share = 0.011)

india = rbind (india, row1, row2, row3, row4)


ggplot (data = india %>%
          
          # reordering
          mutate (spending_group = factor (spending_group, 
                                           levels =spending_group [order (-share)],
                                           ordered = T)),
        aes (x = factor (year), y = share, fill = spending_group)) + 
  geom_col () + 
  geom_text (aes (label = ifelse (share >=0.01, 
                                  paste0 (scales::percent (share, 1), 
                                          ' ($', 
                                          round (exp / 10^9, 1), 
                                          "B)"),
                                  " ")), 
             position = position_stack (vjust = 0.5))+ 
  worlddataverse::theme_wdl() + 
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral") )(5)) +
  theme (axis.text.x = element_text (angle = 45, vjust = 1))

  
## breaking down food and bevs into age group
ggplot (data = food_idn_waterfall_dat %>%
          
          # filter for 2023
          filter (year == 2023) %>%
          
          # producing shares
          dplyr::group_by (year, age_group_wdp) %>%
          dplyr::summarise (exp = sum (total_exp_foodbevs, na.rm = T)) %>%
          mutate (share = exp / sum (exp)) %>%
          
          # reordering
          mutate (age_group_wdp = factor (age_group_wdp, 
                                           levels =age_group_wdp [order (-share)],
                                           ordered = T)),
        aes (x = factor (year), y = share, fill = age_group_wdp)) + 
  geom_col () + 
  geom_text (aes (label = ifelse (share >=0.01, 
                                  paste0 (scales::percent (share, 1), 
                                          ' ($', 
                                          round (exp / 10^9, 1), 
                                          "B)"),
                                  " ")), 
             position = position_stack (vjust = 0.5))+ 
  worlddataverse::theme_wdl() + 
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral") )(5)) +
  theme (axis.text.x = element_text (angle = 45, vjust = 1))


  
food <- read_excel (file.path (base_path, 
                                 "product_categories", 
                                 'global_expenditure_shares', 
                                 "03_output",
                                 "food",
                                 "pak_shares_food_beverages_multivariate.xlsx"))
  
food_idn_waterfall_dat = food %>%
  
  # filtering for years of interest
  filter (year %in% c(2023, 2030)) %>%
  
  # variables of interest
  dplyr::select (year, age_group_wdp, spending_group, total_exp_food_group, total_exp_beverages_group) %>%
  
  # getting food and bevs combined exp
  mutate (total_exp_foodbevs = total_exp_food_group + total_exp_beverages_group) %>%
  dplyr::select (year, age_group_wdp, spending_group, total_exp_foodbevs)
  
  
  
  








## bubble plot for categories --------------------------------------------------

## prepping data
plot_3d_dat = cats_wpersonalcare %>%
  
  # obtaining yearly expenditure per category and per region
  dplyr::group_by (year, category, region) %>%
  dplyr::summarise (exp = sum (total_category_exp_nominal, na.rm = T)) %>%
  
  # adding CAGRs
  dplyr::group_by (category, region) %>%
  mutate (CAGR = (CAGR_fun (exp)) * 100) %>% ungroup () %>%
  
  # filter years
  filter (year %in% c(2023, 2030)) %>%
  
  # removing NAs
  drop_na() %>%
  
  # widening for delta calculation
  pivot_wider (names_from = "year", values_from = 4:5) %>%
  
  # getting absolute difference to 2030
  mutate (diff_2030 = exp_2030 - exp_2023) %>%
  
  # keep relevant vars
  dplyr::select (category, region, exp_2030, CAGR_2030, diff_2030) %>%
  
  # scaling exp_2030 and diff_2030
  mutate (exp_2030 = exp_2030 / 10^9,
          diff_2030 = diff_2030 / 10^9) %>% 
  
  # conditional removal of labels
  mutate (label = ifelse (diff_2030 < 100, "", category))

## building plot
ggplot (plot_3d_dat,
        aes (x = CAGR_2030, y = diff_2030)) +
  
  # adding dots and varying their size by expenditure and their colour by region
  geom_point (aes (size = exp_2030, color = region), alpha = 0.5) +
  
  # adding labels
  geom_text (aes (label = label), size = 2) +
  
  # adjusting the size scale
  scale_size (range = c(2, 30), name = 'Absolute Size') + 
  
  # adding WDL theme
  worlddataverse::theme_wdl()


## top 20 movers by 2030 growth in food -------------------------------

## building data

# isolating years of interest
inputlarge = cats_wpersonalcare %>% 
  
  # isolating years
  filter (year %in% c(2023, 2030)) %>%
  
  # filtering for food 
  filter (category == "Food") %>%
  
  # isolating vars
  dplyr::select (ccode, year, total_category_exp_nominal)

# getting top 20 countries by 2030 growth
inputlarge30 = inputlarge %>%
  
  # widening for delta calculation
  pivot_wider (names_from = "year", values_from = 'total_category_exp_nominal') %>%
  
  # computing delta in billions
  mutate (delta = (`2030` - `2023`) / 10^9) %>%
  
  # getting the top 20
  arrange (desc (delta))

# slicing top 20 by delta
inputlarge30 = inputlarge30 [1:20,]

## chart code
ggplot (data = inputlarge30, 
        aes (x = reorder (ccode, -delta),
             y = delta,
             fill = factor (ccode))) +
  
  # building bar plot
  geom_bar (stat = "identity") +
  
  # adding label on top
  geom_text (aes (y = delta, 
                  label = paste0 ("+", " ", "$", round (delta, 0), " ", "B")), 
             vjust = -0.5) + 
  
  # adding title and axis label and legend label 
  labs (title = "", x = "Country", fill = "Country") +
  
  # theme: adjusting x-axis labels
  theme (axis.text.x = element_text (angle = 360, hjust = 1)) +
  
  # rescaling y axis
  #scale_y_continuous(
  #name = "Total Category Expenditure (in billion USD)",
  #breaks = c(0, 1000, 2000, 3000, 4000)) +
  
  # WDL theme, colours and font
  worlddataverse::theme_wdl()



## bottom 20 movers by 2030 growth in food ----------------------------

## population df for setting threshold
popdat = wdp %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T))

## building data

# isolating years of interest
inputlarge = cats_wpersonalcare %>% 
  
  # isolating years
  filter (year %in% c(2023, 2030)) %>%
  
  # filtering for food 
  filter (category == "Food") %>%
  
  # isolating vars
  dplyr::select (ccode, year, total_category_exp_nominal)


# merging with pop
inputlarge30 = inputlarge %>%
  
  # widening for delta calculation
  pivot_wider (names_from = "year", values_from = 3) %>%
  
  # computing delta in billions
  mutate (delta = (`2030` - `2023`) / 10^9) %>%
  
  # merging with popdat
  left_join (popdat %>% filter (year == 2023), by = c("ccode")) %>%
  
  # filtering for countries above 25M ppl
  filter (pop >= 25000000) %>%
  
  # getting the top 20
  arrange (delta)

# removing afg
inputlarge30 = inputlarge30 %>% filter (!ccode %in% "AFG")

# extracting bottom 20
inputlarge30 = inputlarge30 [1:20,]

## chart code
ggplot (data = inputlarge30, 
        aes (x = reorder (ccode, delta),
             y = delta,
             fill = factor (ccode))) +
  
  # building bar plot
  geom_bar (stat = "identity") +
  
  # adding label on top
  geom_text (aes (y = delta, 
                  label = paste0 ("+", " ", "$", round (delta, 0), " ", "B")), 
             vjust = -0.5) + 
  
  # adding title and axis label and legend label 
  labs (title = "", x = "Country", fill = "Country") +
  
  # theme: adjusting x-axis labels
  theme (axis.text.x = element_text (angle = 360, hjust = 1)) +
  
  # rescaling y axis
  #scale_y_continuous(
  #name = "Total Category Expenditure (in billion USD)",
  #breaks = c(0, 1000, 2000, 3000, 4000)) +
  
  # WDL theme, colours and font
  worlddataverse::theme_wdl()
## top 12 movers in 2023 24 and 30 for food --------------------

# isolating years of interest
inputlarge = cats_wpersonalcare %>% 
  
  # isolating years
  filter (year %in% c(2023, 2024, 2030)) %>%
  
  # filtering for food 
  filter (category == "Food") %>%
  
  # isolating vars
  dplyr::select (ccode, year, total_category_exp_nominal)

inputlarge30 = inputlarge %>%
  filter (year == 2030) %>%
  arrange (desc (total_category_exp_nominal))

# cutting the top 12
inputlarge30 = inputlarge30 [1:16,]

# converting spending to billions
inputlarge = inputlarge %>% 
  mutate (total_category_exp_nominal = total_category_exp_nominal / 10^9)

# filtering for the 12 countries
filtered = inputlarge %>% filter (ccode %in% inputlarge30$ccode)

## chart code
ggplot(data = filtered %>% 
         filter (year %in% c(2023, 2024, 2030)), aes(x = reorder (ccode, -total_category_exp_nominal),
                                                     y = total_category_exp_nominal,
                                                     fill = factor(year))) +
  
  # building bar plot
  geom_bar (stat = "identity", position = "dodge") +
  
  # adding label on top
  geom_text (aes(y = total_category_exp_nominal, 
                 label = paste0 (round (total_category_exp_nominal, 0), " B USD")), 
             position = position_dodge (width = .9), 
             #vjust = -0.5,
             hjust = -0.1,
             angle = 90) + 
  
  # adding title and axis label and legend label 
  labs(title = "", x = "ccode", fill = "year") +
  
  # theme: adjusting x-axis labels
  theme(axis.text.x = element_text (angle = 45, hjust = 1)) +
  
  # rescaling y axis
  #scale_y_continuous(
  #name = "Total Category Expenditure (in billion USD)",
  #breaks = c(0, 1000, 2000, 3000, 4000)) +
  
  # WDL theme, colours and font
  worlddataverse::scale_fill_wdl2() +
  worlddataverse::font_wdl() +
  worlddataverse::theme_wdl() + 
  
  ylim (0, 2200)


## tree map country-category ---------------------

# data input
treemap_dat = cats_wpersonalcare %>% 
  
  # filtering for relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # keeping relevant vars
  dplyr::select (ccode, year, category, total_category_exp_nominal)

# calculating growth
df_growth <- treemap_dat %>%
  dplyr::group_by (ccode, category) %>%
  dplyr::summarise (
    growth = total_category_exp_nominal[year == 2030] - total_category_exp_nominal[year == 2023]
  ) %>%
  ungroup() %>%
  
  # add column for age-spending group
  mutate (
    country_category = paste (ccode, category, sep = "_")
  ) %>%
  
  # Selecting the columns I want in the final dataframe
  select (ccode, country_category, growth) %>%
  
  # obtaining the top 22
  # reordering in descending order
  arrange (desc (growth)) %>%
  
  # obtaining the top 22 by growth to 2030
  slice_head (n = 22) %>%
  
  # creating label 
  mutate (label = paste0 ("+$", round (growth / 10^9, 1), "B")) %>%
  
  # creating country-age-spend var
  mutate (group = paste (ccode, country_category, sep = "_"))

# creating plot
library (treemapify)
ggplot (data = df_growth, 
        aes (area = growth, 
             fill = ccode, 
             label = paste (group, label, sep = "\n"))) + 
  
  # creating treemap
  treemapify::geom_treemap (layout = "squarified") +
  
  # adding tile text
  geom_treemap_text(place = "centre",size = 10) +
  
  # creating empty title
  labs(title="") +
  
  # adding WDL theme, colour and font
  theme_wdl() +
  scale_fill_wdl2() +
  worlddataverse::font_wdl()

## tree map country-age ---------------------

# data input
treemap_dat = wdp %>% 
  
  # filtering for relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # keeping relevant vars
  dplyr::group_by (ccode, year, age_group) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T))

# calculating growth
df_growth <- treemap_dat %>%
  dplyr::group_by (ccode, age_group) %>%
  dplyr::summarise (
    growth = exp[year == 2030] - exp[year == 2023]
  ) %>%
  ungroup() %>%
  
  # add column for age-spending group
  mutate (
    country_age = paste (ccode, age_group, sep = "_")
  ) %>%
  
  # Selecting the columns I want in the final dataframe
  select (ccode, country_age, growth) %>%
  
  # obtaining the top 22
  # reordering in descending order
  arrange (desc (growth)) %>%
  
  # obtaining the top 22 by growth to 2030
  slice_head (n = 50) %>%
  
  # creating label 
  mutate (label = paste0 ("+$", round (growth / 10^9, 1), "B")) %>%
  
  # creating country-age-spend var
  mutate (group = paste (country_age, sep = "_")) %>%
  
  # finalising label
  mutate (label = paste (group, label, sep = "\n")) %>%
  
  # conditional removal of labels
  mutate (label = ifelse (row_number () > 50, "", label))
  
# creating plot
ggplot (data = df_growth, 
        aes (area = growth, 
             fill = ccode, 
             label = label)) + 
  
  # creating treemap
  treemapify::geom_treemap (layout = "squarified") +
  
  # adding tile text
  geom_treemap_text(place = "centre",size = 10) +
  
  # creating empty title
  labs(title="") +
  
  # adding WDL theme, colour and font
  theme_wdl()
  #scale_fill_wdl() +
  #worlddataverse::font_wdl()

## tree map country-spending group ---------------------

# data input
treemap_dat = wdp %>% 
  
  # filtering for relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # keeping relevant vars
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T))

# calculating growth
df_growth <- treemap_dat %>%
  dplyr::group_by (ccode, daily_spending) %>%
  dplyr::summarise (
    growth = exp[year == 2030] - exp[year == 2023]
  ) %>%
  ungroup() %>%
  
  # add column for age-spending group
  mutate (
    country_spend = paste (ccode, daily_spending, sep = "_")
  ) %>%
  
  # Selecting the columns I want in the final dataframe
  select (ccode, country_spend, growth) %>%
  
  # obtaining the top 22
  # reordering in descending order
  arrange (desc (growth)) %>%
  
  # obtaining the top 22 by growth to 2030
  slice_head (n = 22) %>%
  
  # creating label 
  mutate (label = paste0 ("+$", round (growth / 10^9, 1), "B")) %>%
  
  # creating country-age-spend var
  mutate (group = paste (country_spend, sep = "_"))

# creating plot
library (treemapify)
ggplot (data = df_growth, 
        aes (area = growth, 
             fill = ccode, 
             label = paste (group, label, sep = "\n"))) + 
  
  # creating treemap
  treemapify::geom_treemap (layout = "squarified") +
  
  # adding tile text
  geom_treemap_text(place = "centre",size = 10) +
  
  # creating empty title
  labs(title="") +
  
  # adding WDL theme, colour and font
  theme_wdl() +
  #scale_fill_wdl() +
  worlddataverse::font_wdl()

## consumer class pie chart ----------------------------------------------------

wdp_pie = wdp %>% 
  
  # aggregating into year, spend group and headcount
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T)) %>%
  
  # recoding spending groups
  mutate (daily_spending = ifelse (daily_spending == "[0,12)", "Not CC",
                                   ifelse (daily_spending == "[12,40)", "Lower MC",
                                           ifelse (daily_spending == "[40,80)", "Central MC",
                                                   ifelse (daily_spending == "[80,120)", "Upper MC",
                                                           ifelse (daily_spending == "[120,Inf)", "Rich", NA
                                                           )))))) %>%
  
  # isolating 2023
  filter (year == 2023) %>%
  
  # computing share
  mutate (share = hc / sum (hc, na.rm = T)) %>%
  
  # creating label
  mutate (label = paste0 (scales::percent (share, 1), "\n", round (hc / 10^6, 1), "B")) %>%
  
  # pie chart helpers
  mutate(csum = rev (cumsum (rev (share))), 
         pos = share/2 + lead (csum, 1),
         pos = if_else(is.na(pos), share/2, pos)) %>%
  
  # creating plot
  ggplot (aes(x = "" , y = share, fill = daily_spending)) +
  geom_bar (stat = "identity", position = 'stack') +
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral") )(5)) +
  coord_polar (theta = "y") +
  geom_text (aes (y = share, label = label), position = position_stack (vjust = 0.5)) + 
  guides (fill = guide_legend (title = "Spending Group")) +
  theme_void()
  
wdp_pie
  
  
## CC headcount growth
wdp_growth = wdp %>%
  
  # recoding
  mutate (daily_spending = ifelse (daily_spending == "[0,12)", "Not CC", "CC")) %>%
  
  # aggregating
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (hc = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # filter for CC
  filter (daily_spending == "CC") %>%
  dplyr::select (-daily_spending) %>%
  
  # filtering for year
  filter (year %in% c(2023, 2024, 2030)) %>%
  
  # calculating pecentage change
  pivot_wider (names_from = "year", values_from = 2:3) %>%
  mutate (pct_delta_hc = ((hc_2024 - hc_2023) / hc_2023) * 100, 
          pct_delta_exp = ((exp_2024 - exp_2023) / exp_2023) * 100)

## r2 of the three regressions for Luis ----------------------------------------

## data
pc_age = read_csv (file.path (base_path, 
                              "spending_categories", 
                              "sample_data_wdp", 
                              "05_003_merge_mpro2_ages_R_2023_07_26_2017ppp_1_USD_with_categories_with_demogs_corrected.csv"))


pc_age_for_r2 = pc_age %>%
  
  # isolating vars
  dplyr::group_by (ccode, year, spending_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T))

## regression 1: Personal care spending ~ population

# adding population to data
popdat = wdp %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T))

reg1input = pc_age_for_r2 %>% left_join (popdat, by = c("ccode", "year")) %>% drop_na()

# running regression
summary (lm (`Personal Care` ~ pop, data = reg1input))$r.squared

## regression 2: Personal care spending ~ total spending

# adding total spending to data
spenddat = wdp %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T))

reg2input = pc_age_for_r2 %>% left_join (spenddat, by = c("ccode", "year")) %>% drop_na()

# running regression
summary (lm (`Personal Care` ~ exp, data = reg2input))$adj.r.squared

## regression 3: Personal care spending ~ spending category size in expenditure

# adding individual spending category size
wdp2 <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))

spendcatdat = wdp2 %>%
  
  # aggregation
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # rename 
  rename (spending_group = daily_spending)

reg3input = pc_age_for_r2 %>% left_join (spendcatdat, by = c("ccode", "spending_group", "year")) %>% drop_na() %>% 
  #pivot_longer (5:9, names_to = "daily_spend", values_to = "exp2") %>%
  filter (year %in% 2023)

# running regression
summary (lm (`Personal Care` ~ `[0,12)` + `[12,40)` + `[40,80)` + `[80,120)` + `[120,Inf)`, 
             data = reg3input))$adj.r.squared

## drawing scatter plots

# reg1

reg1plot2023 = ggplot (data = reg1input %>% filter (year %in% 2023), aes (x = pop, y = exp)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  #scale_y_continuous (trans = "log") + 
  #scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (#title = "Personal care spending ~ population",
    subtitle = "Subset for 2023, R2 reflects that",
    x = "Population", 
    y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360))


reg1plot = ggplot (data = reg1input, aes (x = pop, y = exp)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  #scale_y_continuous (trans = "log") + 
  #scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (#title = "Personal care spending ~ population",
    subtitle = "All years, R2 reflects that",
    x = "Population", 
    y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360))

reg1plotlog2023 = ggplot (data = reg1input %>% filter (year %in% 2023), aes (x = pop, y = exp)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  scale_y_continuous (trans = "log") + 
  scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  #labs (#title = "Personal care spending ~ population",
    #subtitle = "Axes log-transformed and subset for 2023, \nR2 reflects that",
    #x = "Population", 
    #y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme_void ()
reg1plotlog2023

plotlist <- list (reg1plot, reg1plot2023, reg1plotlog2023)
reg1masterplot <- ggarrange (plotlist = plotlist, nrow = 1, ncol = 3)
annotate_figure (reg1masterplot,
                 top = text_grob ("Personal care spending ~ population",
                                  color = "Black", face = "bold", size = 14))

# reg2

reg2plot2023 = ggplot (data = reg2input %>% filter (year %in% 2023), aes (x = exp.y, y = exp.x)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  #scale_y_continuous (trans = "log") + 
  #scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (#title = "Personal care spending ~ population",
    subtitle = "Subset for 2023, R2 reflects that",
    x = "Total spending", 
    y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360))


reg2plot = ggplot (data = reg2input, aes (x = exp, y = `Personal Care`)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  #scale_y_continuous (trans = "log") + 
  #scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (#title = "Personal care spending ~ population",
    subtitle = "All years, R2 reflects that",
    x = "Total spending", 
    y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360))

reg2plotlog2023 = ggplot (data = reg2input %>% filter (year %in% 2023), aes (x = exp.y, y = exp.x)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  scale_y_continuous (trans = "log") + 
  scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  #labs (#title = "Personal care spending ~ population",
    #subtitle = "Axes log-transformed and subset for 2023, \nR2 reflects that",
    #x = "Total spending", 
    #y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360)) +
  theme_void () 
reg2plotlog2023

plotlist <- list (reg2plot, reg2plot2023, reg2plotlog2023)
reg2masterplot <- ggarrange (plotlist = plotlist, nrow = 1, ncol = 3)
annotate_figure (reg2masterplot,
                 top = text_grob ("Personal care spending ~ total spending",
                                  color = "Black", face = "bold", size = 14))


# reg3
reg3plot = ggplot (data = reg3input %>% pivot_longer (4:8, names_to = "daily_spend", values_to = "exp2"),
                   aes (x = exp.y, y = exp.x)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  #scale_y_continuous (trans = "log") + 
  #scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (title = "Personal care spending ~ spending category size",
        subtitle = "All years, R2 reflects that",
        x = "Spending", 
        y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360)) + 
  facet_wrap (~daily_spend, scales = "free")

reg3plot

reg3plot2023 = ggplot (data = reg3input %>% 
                         pivot_longer (4:8, names_to = "daily_spend", values_to = "exp2") %>%
                         filter (year %in% 2023),
                       aes (x = exp2, y = exp)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  #scale_y_continuous (trans = "log") + 
  #scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (title = "Personal care spending ~ spending category size",
        subtitle = "Subset for 2023, R2 reflects that",
        x = "Spending", 
        y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360)) + 
  facet_wrap (~daily_spend, scales = "free")

reg3plot2023


reg3plot2023log = ggplot (data = reg3input %>% filter (spending_group == "[40,80)"),
                          aes (x = exp.y, y = exp.x)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  scale_y_continuous (trans = "log") + 
  scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  #labs (title = "Personal care spending ~ spending category size",
       # subtitle = "Log transformed + Subset for 2023 and $40-$80, R2 reflects that",
        #x = "Spending", 
        #y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme_void ()
  #facet_wrap (~spending_group, scales = "free")

reg3plot2023log


  

## pc demog bar chars stacked ------------------------------------

## age
pc_age_demogbars_dat = pc_age %>%
  
  # isolating vars
  dplyr::group_by (ccode, year, age_group_wdp) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # filter for year
  filter (year == 2023) %>%
  
  # keep countriess of interest
  filter (ccode %in% c("USA", "JPN", "CHN", "IND"))

# getting global
pc_age_demogbars_dat_total = pc_age_demogbars_dat %>%
  dplyr::group_by (year, age_group_wdp) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  mutate (ccode = "Global") %>%
  relocate (ccode, .before = year)

# adding to main df 
pc_age_demogbars_dat = rbind (pc_age_demogbars_dat, pc_age_demogbars_dat_total) %>%
  
  # creating share
  dplyr::group_by (ccode, year) %>%
  dplyr::mutate (share = exp / sum (exp, na.rm = T)) %>%
  
  # label
  mutate (label = paste0 (round (exp / 10^12, 2), " T (", scales::percent (share, 1), ')'))

# creating stacked bar
ggplot (data = pc_age_demogbars_dat %>% mutate (exp = exp / 10^12), 
        aes (x = factor (ccode), y = share, fill = age_group_wdp)) +
  geom_bar (stat = "identity", position = "stack") + 
  theme_wdl () + 
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral"))(5)) +
  geom_text (aes (label = label), position = position_stack (vjust = 0.5))

## spend
pc_spend_demogbars_dat = pc_age %>%
  
  # isolating vars
  dplyr::group_by (ccode, year, spending_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # filter for year
  filter (year == 2023) %>%
  
  # keep countriess of interest
  filter (ccode %in% c("USA", "JPN", "CHN", "IND"))

# getting global
pc_spend_demogbars_dat_total = pc_spend_demogbars_dat %>%
  dplyr::group_by (year, spending_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  mutate (ccode = "Global") %>%
  relocate (ccode, .before = year)

# adding to main df 
pc_spend_demogbars_dat = rbind (pc_spend_demogbars_dat, pc_spend_demogbars_dat_total) %>%
  
  # creating share
  dplyr::group_by (ccode, year) %>%
  dplyr::mutate (share = exp / sum (exp, na.rm = T)) %>%
  
  # label
  mutate (label = paste0 (round (exp / 10^12, 2), " T (", scales::percent (share, 1), ')')) %>%
  
  # ordering 
  mutate (spending_group = factor (spending_group, 
                                   levels = c("[0,12)", "[12,40)", "[40,80)","[80,120)", "[120,Inf]")))

# creating stacked bar
ggplot (data = pc_spend_demogbars_dat %>% mutate (exp = exp / 10^12), 
        aes (x = factor (ccode), y = share, fill = spending_group)) +
  geom_bar (stat = "identity", position = "stack") + 
  theme_wdl () + 
  scale_fill_manual (values = colorRampPalette (brewer.pal(9,"Spectral"))(5)) +
  geom_text (aes (label = label), position = position_stack (vjust = 0.5))

## misc ------------------------------------------------------------------------

test = food_idn_waterfall_dat %>%
  
  # filter for 2023
  filter (year == 2023) %>%
  
  # producing shares
  dplyr::group_by (year, spending_group) %>%
  dplyr::summarise (exp = sum (total_exp_foodbevs, na.rm = T)) %>%
  mutate (share = exp / sum (exp))

sum (food$total_exp_food_group [food$year == 2023])
sum (food$total_exp_beverages_group [food$year == 2023])
354038171749 + 258025926175

sum (cats$total_category_exp_nominal [cats$category == "Food and non-alcoholic beverages" &
                                        cats$ccode == "IDN" &
                                        cats$year == 2023])


wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))

popdat = wdp %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T))

cats_wpersonalcare2 = cats_wpersonalcare %>%
  
  filter (category == "AlcNarc") %>%
  dplyr::group_by (year, ccode) %>%
  dplyr::summarise (exp = sum (total_category_exp_nominal, na.rm = T)) %>%
  filter (year %in% 2023:2030 & ccode != "CHN") %>%
  dplyr::group_by (year) %>%
  dplyr::summarise (CAGR = CAGR_fun (exp) * 100)
  

cats_piespend2023_pc_ppp = cats_wpersonalcare %>%
  
  # adding population
  left_join (popdat, by = c("ccode", "year")) %>%
  
  # reversing nominal
  left_join (adj, by = c("ccode", "year")) %>%
  mutate (total_category_exp_ppp = total_category_exp_nominal / hhe_2017ppp_to_current_USD) %>%
  
  # aggregating into global level
  dplyr::group_by (year, category) %>%
  dplyr::summarise (total_category_exp_ppp = sum (total_category_exp_ppp, na.rm = T),
                    pop = sum (pop, na.rm = T)) %>%
  
  # filter for 2023
  filter (year == 2023) %>%
  
  # computing per capita
  mutate (exp_pc = total_category_exp_ppp / pop) %>%
  
  # computing shares
  mutate (share = exp_pc / sum (exp_pc, na.rm = T)) %>%
  
  # adding label
  mutate (spend = round (exp_pc, 1),
          share_pc = scales::percent (share, 1),
          label = paste0 ("$ ", spend, " | ", share_pc)) %>%
  
  # ordering
  mutate (category = factor (category, 
                             levels = category [order (share)],
                             ordered = T)) %>% 
  
  # pie chart helpers
  mutate(csum = rev (cumsum (rev (share))), 
         pos = share/2 + lead (csum, 1),
         pos = if_else(is.na(pos), share/2, pos)) 
  
  
## education

edu = cats_wpersonalcare %>%
  
  # filter for category 
  filter (category == "Education") %>%
  
  # grouping
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (exp = sum (total_category_exp_nominal, na.rm = T)) %>%
  
  # isolating years 
  filter (year %in% 2023:2030) %>%
  
  # computing CAGRs
  dplyr::group_by (ccode) %>%
  mutate (CAGR = CAGR_fun (exp) * 100) %>% ungroup () %>%
  
  # isolating countries
  filter (ccode %in% c("VNM", "BGD", "NGA"))
  

## redoing bottom20 movers in Personal Care without inflation

pc2 = pc %>%
  dplyr::group_by (Country, Year, `Expenditure Type`) %>%
  dplyr::summarise (`Personal Care` = sum (`Personal Care`, na.rm = T)) %>%
  filter (`Expenditure Type` == "real") %>%
  #pivot_wider (names_from = `Expenditure Type`, values_from = `Personal Care`) %>%
  filter (Year %in% c(2023:2030)) %>%
  dplyr::select (-`Expenditure Type`) %>%
  dplyr::group_by (Country) %>%
  mutate (CAGR = CAGR_fun (`Personal Care`) * 100) %>% ungroup () %>%
  filter (Year == 2030) %>%
  dplyr::select (Country, Year, CAGR) %>%
  rename (ccode = Country) %>%
  left_join (popdat %>% filter (year == 2030) %>% dplyr::select (-year), by = c("ccode")) %>%
  filter (pop >= 25000000 & !ccode == "AFG") %>%
  arrange (CAGR)

pc2 = pc2 [1:12,]


pc3 = pc_age %>%
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  filter (year %in% 2023:2030 & ccode != "AFG") %>%
  dplyr::group_by (ccode) %>%
  mutate (CAGR = CAGR_fun (exp) * 100) %>% ungroup () %>%
  filter (year == 2030) %>%
  left_join (popdat %>% filter (year == 2030) %>% dplyr::select (-year), by = c("ccode")) %>%
  filter (pop >= 25000000 & !ccode == "AFG") %>%
  arrange (CAGR)
  
cagr2030pc = cagr2030pc %>% filter (ccode != "AFG") %>% filter (ccode != "GNQ") %>% arrange (CAGR)
cagr2030pc = cagr2030pc [1:12,] 
cagr2030pc$CAGR = cagr2030pc$CAGR * 100

ggplot (data = cagr2030pc, 
        aes (x = reorder (ccode, CAGR),
             y = CAGR,
             fill = factor (ccode))) +
  
  # building bar plot
  geom_bar (stat = "identity") +
  
  # adding label on top
  geom_text (aes (y = CAGR, 
                  label = paste0 ("+", " ", round (CAGR, 1), " ", "%")), 
             vjust = -0.5) + 
  
  # adding title and axis label and legend label 
  labs (title = "", x = "Country", fill = "Country") +
  
  # theme: adjusting x-axis labels
  theme (axis.text.x = element_text (angle = 360, hjust = 1)) +
  
  # rescaling y axis
  #scale_y_continuous(
  #name = "Total Category Expenditure (in billion USD)",
  #breaks = c(0, 1000, 2000, 3000, 4000)) +
  
  # WDL theme, colours and font
  worlddataverse::theme_wdl()



cats_growth30bar_wcountries = cats_wpersonalcare %>%
  
  # aggregate for global
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (spend = sum (total_category_exp_nominal, na.rm = T) / 10^12) %>%
  
  # filter for relevant year range
  filter (year %in% 2023:2030) %>%
  
  # applying CAGR function
  dplyr::group_by (ccode) %>%
  mutate (CAGR = CAGR_fun (spend) * 100) %>% ungroup () %>%
  
  # extracting relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # widening
  pivot_wider (names_from = "year", values_from = 3:4) %>%
  
  # delta var
  mutate (delta = spend_2030 - spend_2023) %>%
  
  # keeping only relevant vars
  dplyr::select (ccode, delta, CAGR_2030) %>%
  
  arrange (desc (delta))


cats_growth30bar_wcountries2 = wdp %>%
  
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (spend = sum (exp.pdf.m, na.rm = T) / 10^12) %>%
  
  # converting to nominal
  left_join (adj, by = c("ccode", "year")) %>%
  mutate (spend = spend * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD) %>%

  # filter for relevant year range
  filter (year %in% 2023:2030) %>%
  
  # applying CAGR function
  dplyr::group_by (ccode) %>%
  mutate (CAGR = CAGR_fun (spend) * 100) %>% ungroup () %>%
  
  # extracting relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # widening
  pivot_wider (names_from = "year", values_from = 3:4) %>%
  
  # delta var
  mutate (delta = spend_2030 - spend_2023) %>%
  
  # keeping only relevant vars
  dplyr::select (ccode, delta, CAGR_2030) %>%
  
  arrange (desc (delta))

cats_growth30bar_wcountries2 = cats_growth30bar_wcountries2 [1:20,]




# building chart
ggplot (data = cats_growth30bar_wcountries2,
        aes (x = reorder (ccode, -delta))) + 
  geom_bar (aes (y = delta * 2, fill = ccode), stat = "identity") + 
  geom_line (aes (y = CAGR_2030, group = 1), stat = "identity", color = "black", linewidth = 1.5) +
  geom_text (aes (y = delta *2, label = paste0 ("+ $", round (delta, 1), " ", "T")), vjust = -0.5) +
  worlddataverse::theme_wdl() +
  theme(axis.text.x = element_text (angle = 360)) +
  scale_fill_wdl() + 
  labs (x = "\nCategory", 
        y = "Absolute change in spending between 2023 and 2030\n") +
  
  # adjusting the two y-axis scales
  scale_y_continuous(
    name = "Absolute change in spending between 2023 and 2030\n",
    breaks = c(0,2,4,6,8,10,12),
    labels = c(0,1,2,3,4,5,6),
    sec.axis = sec_axis(
      ~ ., 
      name = "CAGR 2030\n", 
      breaks = c(0,2,4,6,8,10,12), 
      labels = c(0,2,4,6,8,10,12)
    )
  )






total_real = wdp %>%
  
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  left_join (adj %>% filter (year == 2023) %>% dplyr::select (-year), by = "ccode") %>%
  mutate (exp = exp * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD) %>%
  
  dplyr::group_by (year) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # filter for relevant year range
  filter (year %in% 2023:2030) %>%
  
  # applying CAGR function
  mutate (CAGR = CAGR_fun (exp) * 100) %>%
  
  # extracting relevant years
  filter (year %in% c(2023, 2024, 2030))
  
  
total_nominal = wdp %>%
  
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  left_join (adj, by = c("ccode", "year")) %>%
  mutate (exp = exp * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD) %>%
  
  dplyr::group_by (year) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # filter for relevant year range
  filter (year %in% 2023:2030) %>%
  
  # applying CAGR function
  mutate (CAGR = CAGR_fun (exp) * 100) %>%
  
  # extracting relevant years
  filter (year %in% c(2023, 2024, 2030))




reg3input = pc_age_for_r2 %>% 
  rename (exp_total = exp) %>%
  left_join (spendcatdat, by = c("ccode", "spending_group", "year")) %>% drop_na() %>% 
  #pivot_longer (5:9, names_to = "daily_spend", values_to = "exp2") %>%
  filter (year %in% 2023)

reg3plot2023log = ggplot (data = reg3input,
                          aes (x = exp, y = exp_total)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  scale_y_continuous (trans = "log") + 
  scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  #labs (title = "Personal care spending ~ spending category size",
  # subtitle = "Log transformed + Subset for 2023 and $40-$80, R2 reflects that",
  #x = "Spending", 
  #y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme_void ()
#facet_wrap (~spending_group, scales = "free")

reg3plot2023log














