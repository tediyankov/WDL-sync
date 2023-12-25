
## Teddy's code for the categories webinar slides

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, data.table, ggpmisc, ggpubr, ggthemes, knitr, sjPlot, texreg, stargazer, corrplot,
                kableExtra, treemapify)

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
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))

# categories
cats <- read_excel ("predictions_all_categories.xlsx", sheet = 1)

# beauty data
allbeauty <- read_excel (file.path (cats_path1, "beauty_exp_loreal2023-06-28.xlsx"))
beautyregion <- read_excel (file.path (cats_path1, "beauty_exp_loreal2023-06-28.xlsx"), sheet = 2)
beautytotals <- read_excel (file.path (cats_path1, "beauty_exp_loreal2023-06-28.xlsx"), sheet = 3)
SUBCatbeauty <- read_excel ( file.path (cat_path2, "Loreal_Product_Data-2023-06-28.xlsx"))

# CAGR function
CAGR_fun <- function (x) {
  if (length (x) < 1L)
    return (numeric ())
  out <- (x / x [[1L]]) ^ (1 / (seq_along (x) - 1)) - 1
  out [[1L]] <- NA_real_
  out
}


## separating misc and personal care in the input data -------------------------

## setting up personal care df (from age breakdown)
pc = load (file.path (pc_path, "demographic_breakdown_with_trade_all_years_nominal.rds"))
pc_reformat = pc %>% ungroup () %>%
  
  # keepning relevant vars
  dplyr::select (ccode, year, age_group_wdp, personal_care_expenditure_nominal) %>%
  
  # aggregating across ages
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (`Personal Care` = sum (personal_care_expenditure_nominal, na.rm = T))

## setting up personal care (from L'Oreal work)
pc_loreal <- read_excel ("Loreal_Product_Data-2023-09-07 (nominal).xlsx")
pc_loreal_reformat = pc_loreal %>%
  
  # keepning relevant vars
  dplyr::select (`Country`, `Year`, `Expenditure Type`, `Personal Care`) %>%
  
  # keeping only nominal
  filter (`Expenditure Type` == "nominal") %>%
  distinct () %>%
  dplyr::select (-`Expenditure Type`) %>%
  
  # aggregating 
  dplyr::group_by (`Country`, `Year`) %>%
  dplyr::summarise (`Personal Care` = sum (`Personal Care`, na.rm = T))


cats_wpersonalcare = cats %>%
  
  # keeping relevant variables
  dplyr::select (ccode, year, region, category, total_exp_nominal, total_category_exp_nominal) %>%
  
  # merging personal care var
  left_join (pc_loreal_reformat %>%
               
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

# checking if it worked
# mutate (diff = total_category_exp_nominal - new_total_category_exp_nominal)

# moving personal care into the categories variable
personalcare = cats_wpersonalcare %>% 
  dplyr::select (ccode, year, region, total_exp_nominal, `Personal Care`) %>%
  distinct () %>%
  mutate (category = "Personal Care") %>%
  relocate (category, .before = total_exp_nominal) %>%
  dplyr::rename_with (~ case_when (. == "Personal Care" ~ "total_category_exp_nominal", TRUE ~ .))

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


## category spending shares in 2023 shown in a pie chart -----------------------

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
  
  # building plot
  ggplot (aes (x = year, y = share, fill = category)) + 
  geom_bar (stat = "identity", position = "stack") + 
  coord_polar (theta = "y") + 
  theme_void () + 
  worlddataverse::scale_fill_wdl() + 
  theme (legend.position = "bottom")
  
cats_piespend2023
  
## total spending for world 2023, 2024 and 2030 w CAGRs -------

# in the first filter line, add in whichever category you want.
total = cats_wpersonalcare %>%
  
  # aggregate for global
  dplyr::group_by (year) %>%
  dplyr::summarise (spend = sum (total_category_exp_nominal, na.rm = T) / 10^12) %>%
  
  # filter for relevant year range
  filter (year %in% 2023:2030) %>%
  
  # applying CAGR function
  mutate (CAGR = CAGR_fun (spend) * 100) %>%
  
  # extracting relevant years
  filter (year %in% c(2023, 2024, 2030))

## total spending for food and personal care 2023, 2024 and 2030 w CAGRs -------

# in the first filter line, add in whichever category you want.
total = cats_wpersonalcare %>%
  
  # isolating category
  filter (category == "Food and non-alcoholic beverages") %>%
  
  # aggregate for global
  dplyr::group_by (year) %>%
  dplyr::summarise (spend = sum (total_category_exp_nominal, na.rm = T) / 10^9) %>%
  
  # filter for relevant year range
  filter (year %in% 2023:2030) %>%
  
  # applying CAGR function
  mutate (CAGR = CAGR_fun (spend) * 100) %>%
  
  # extracting relevant years
  filter (year %in% c(2023, 2024, 2030))

  
  
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

  
## r2 of the three regressions for Luis ----------------------------------------

## prepping data
care_reginput = allbeauty %>%
  
  # isolating vars
  dplyr::select (ISO, Year, `Personal Care Total Nominal`) %>%
  
  # renaming 
  dplyr::rename_with (
    ~ case_when (
      . == "ISO" ~ "ccode",
      . == "Year" ~ "year",
      . == "Personal Care Total Nominal" ~ "Personal Care",
      TRUE ~ .
    )
  )

## regression 1: Personal care spending ~ population

# adding population to data
popdat = wdp %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T))

reg1input = care_reginput %>% left_join (popdat, by = c("ccode", "year")) %>% drop_na()

# running regression
summary (lm (`Personal Care` ~ pop, data = reg1input))$r.squared

## regression 2: Personal care spending ~ total spending

# adding total spending to data
spenddat = wdp %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T))

reg2input = care_reginput %>% left_join (spenddat, by = c("ccode", "year")) %>% drop_na()

# running regression
summary (lm (`Personal Care` ~ exp, data = reg2input))$adj.r.squared

## regression 3: Personal care spending ~ spending category size in expenditure

# adding individual spending category size
wdp2 <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))

spendcatdat = wdp2 %>%
  
  # aggregation
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # elongating to have 1 country-year per row
  pivot_wider (names_from = "daily_spending", values_from = "exp")

reg3input = care_reginput %>% left_join (spendcatdat, by = c("ccode", "year")) %>% drop_na()

# running regression
summary (lm (`Personal Care` ~ `[0,12)` + `[12,40)` + `[40,80)` + `[80,120)` + `[120,Inf)`, 
             data = reg3input))$adj.r.squared

## drawing scatter plots

# reg1

reg1plot2023 = ggplot (data = reg1input %>% filter (year %in% 2023), aes (x = pop, y = `Personal Care`)) + 
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
  

reg1plot = ggplot (data = reg1input, aes (x = pop, y = `Personal Care`)) + 
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

reg1plotlog2023 = ggplot (data = reg1input %>% filter (year %in% 2023), aes (x = pop, y = `Personal Care`)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  scale_y_continuous (trans = "log") + 
  scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (#title = "Personal care spending ~ population",
        subtitle = "Axes log-transformed and subset for 2023, \nR2 reflects that",
        x = "Population", 
        y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360))

plotlist <- list (reg1plot, reg1plot2023, reg1plotlog2023)
reg1masterplot <- ggarrange (plotlist = plotlist, nrow = 1, ncol = 3)
annotate_figure (reg1masterplot,
                 top = text_grob ("Personal care spending ~ population",
                                  color = "Black", face = "bold", size = 14))

# reg2

reg2plot2023 = ggplot (data = reg2input %>% filter (year %in% 2023), aes (x = exp, y = `Personal Care`)) + 
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

reg2plotlog2023 = ggplot (data = reg2input %>% filter (year %in% 2023), aes (x = exp, y = `Personal Care`)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  scale_y_continuous (trans = "log") + 
  scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (#title = "Personal care spending ~ population",
    subtitle = "Axes log-transformed and subset for 2023, \nR2 reflects that",
    x = "Total spending", 
    y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360))

plotlist <- list (reg2plot, reg2plot2023, reg2plotlog2023)
reg2masterplot <- ggarrange (plotlist = plotlist, nrow = 1, ncol = 3)
annotate_figure (reg2masterplot,
                 top = text_grob ("Personal care spending ~ total spending",
                                  color = "Black", face = "bold", size = 14))


# reg3
reg3plot = ggplot (data = reg3input %>% pivot_longer (4:8, names_to = "daily_spend", values_to = "exp"),
                   aes (x = exp, y = `Personal Care`)) + 
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
                         pivot_longer (4:8, names_to = "daily_spend", values_to = "exp") %>%
                         filter (year %in% 2023),
                   aes (x = exp, y = `Personal Care`)) + 
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


reg3plot2023log = ggplot (data = reg3input %>% 
                         pivot_longer (4:8, names_to = "daily_spend", values_to = "exp") %>%
                         filter (year %in% 2023),
                       aes (x = exp, y = `Personal Care`)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  scale_y_continuous (trans = "log") + 
  scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (title = "Personal care spending ~ spending category size",
        subtitle = "Log transformed + Subset for 2023, R2 reflects that",
        x = "Spending", 
        y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360)) + 
  facet_wrap (~daily_spend, scales = "free")

reg3plot2023log

## category spending by region -------------------------------------------------

## aggregate spending 2023 2024 2030 
cats_region_total = cats_wpersonalcare %>%
  
  # aggregating into year-region-category
  dplyr::group_by (year, region, category) %>%
  dplyr::summarise (total_exp_nominal = sum (total_exp_nominal, na.rm = T), 
                    total_category_exp_nominal = sum (total_category_exp_nominal, na.rm = T)) %>%
  
  # isolating years of interest
  filter (year %in% c(2023, 2024, 2030)) %>%
  
  # computing shares
  mutate (share = total_category_exp_nominal / total_exp_nominal) %>%
  
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
                                                                                                                     )))))))))))))) %>%
  
  # building plot
  ggplot (aes (x = factor (year), y = share * 100, fill = category)) + 
  geom_col () + 
  #geom_text (aes (y = share, label = paste0 (round (share * 100, 1), "%")),
             #position = position_dodge(width = 0.9),
             #vjust = 0.5) +  
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl() + 
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



## per capita spending 2023 2024 2030 
cats_region_pc = cats_wpersonalcare %>%
  
  # adding population
  left_join (popdat, by = c("ccode", "year")) %>%
  
  # creating regional pop variable
  dplyr::group_by (year, region) %>%
  dplyr::mutate (poptotal = sum (pop, na.rm = T)) %>%
  
  # aggregating into region
  dplyr::group_by (year, region, category) %>%
  dplyr::summarise (total_exp_nominal = sum (total_exp_nominal, na.rm = T), 
                    total_category_exp_nominal = sum (total_category_exp_nominal, na.rm = T),
                    pop = mean (poptotal, na.rm = T)) %>%
  
  # converting both spending vars into per capita for the region
  mutate (pc_exp_nominal = total_exp_nominal / pop,
          pc_category_exp_nominal = total_category_exp_nominal / pop) %>%
  
  # computing share
  mutate (share = pc_category_exp_nominal / pc_exp_nominal) %>%
  
  # isolating years of interest
  filter (year %in% c(2023, 2024, 2030)) %>%
  
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
                                                                                                                     )))))))))))))) %>%
  
  # building plot
  ggplot (aes (x = factor (year), y = share * 100, fill = category)) + 
  geom_col () + 
  #geom_text (aes (y = share, label = paste0 (round (share * 100, 1), "%")),
  #position = position_dodge(width = 0.9),
  #vjust = 0.5) +  
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl() + 
  facet_wrap (~region, strip.position = "top", nrow = 1, ncol = 7) + 
  theme (legend.position = "bottom",
         #legend.key.height= unit(1, 'mm'),
         #legend.key.width= unit(1, 'mm'),
         #legend.margin = margin (l = 1, r = 1, unit = "mm"),
         axis.text.x = element_text (angle = 360, vjust = 1)) + 
  labs (x = "",
        y = "Category share of per capita expenditure (% of total)\n")
#theme (panel.margin = grid::unit (-1.25, "lines"))

cats_region_pc




## G20 spending 2023 2024 2030 
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
                                                                                                                     )))))))))))))) %>%
  
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

## recreating the above for PPP ------------------------------------------------

cats_wpersonalcare_ppp = cats %>%
  
  # keeping relevant variables
  dplyr::select (ccode, year, region, category, total_exp_ppp, total_category_exp_ppp) %>%
  
  # merging personal care var
  left_join (allbeauty %>%
               
               # isolating vars
               dplyr::select (ISO, Year, `Personal Care Total Nominal`) %>%
               
               # renaming 
               dplyr::rename_with (
                 ~ case_when (
                   . == "ISO" ~ "ccode",
                   . == "Year" ~ "year",
                   . == "Personal Care Total Nominal" ~ "Personal Care",
                   TRUE ~ .
                 )
               ), 
             by = c("ccode", "year")) %>%
  
  # removing personal care from misc
  mutate (new_total_category_exp_ppp = ifelse (category == "Miscellaneous goods and services", 
                                                   total_category_exp_ppp - `Personal Care`, 
                                                   total_category_exp_ppp)) %>%
  
  # recalibrating
  dplyr::select (ccode, year, region, category, total_exp_ppp, new_total_category_exp_ppp, `Personal Care`) %>%
  dplyr::rename_with (~ case_when (. == "new_total_category_exp_ppp" ~ "total_category_exp_ppp", TRUE ~ .))

# checking if it worked
# mutate (diff = total_category_exp_ppp - new_total_category_exp_ppp)

# moving personal care into the categories variable
personalcare = cats_wpersonalcare_ppp %>% 
  dplyr::select (ccode, year, region, total_exp_ppp, `Personal Care`) %>%
  distinct () %>%
  mutate (category = "Personal Care") %>%
  relocate (category, .before = total_exp_ppp) %>%
  dplyr::rename_with (~ case_when (. == "Personal Care" ~ "total_category_exp_ppp", TRUE ~ .))

cats_wpersonalcare_ppp = rbind (cats_wpersonalcare_ppp %>% dplyr::select (-`Personal Care`), personalcare)

## G20 spending 2023 2024 2030 
cats_g20_total_ppp = cats_wpersonalcare_ppp %>%
  
  # recoding countries into the EU
  mutate (ccode = ifelse (ccode %in% EU_ISO3, "EU", ccode)) %>%
  
  # isolating G20 countries
  filter (ccode %in% G20_ISO3) %>%
  
  # aggregating into region
  dplyr::group_by (year, ccode, category) %>%
  dplyr::summarise (total_exp_ppp = sum (total_exp_ppp, na.rm = T), 
                    total_category_exp_ppp = sum (total_category_exp_ppp, na.rm = T)) %>%
  
  # computing share
  mutate (share = total_category_exp_ppp / total_exp_ppp) %>%
  
  # isolating years of interest
  filter (year %in% c(2023, 2024, 2030)) %>%
  
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
                                                                                                                     )))))))))))))) %>%
  
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

cats_g20_total_ppp

## aggregate spending 2023 2024 2030 
cats_region_total_ppp = cats_wpersonalcare_ppp %>%
  
  # aggregating into year-region-category
  dplyr::group_by (year, region, category) %>%
  dplyr::summarise (total_exp_ppp = sum (total_exp_ppp, na.rm = T), 
                    total_category_exp_ppp = sum (total_category_exp_ppp, na.rm = T)) %>%
  
  # isolating years of interest
  filter (year %in% c(2023, 2024, 2030)) %>%
  
  # computing shares
  mutate (share = total_category_exp_ppp / total_exp_ppp) %>%
  
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
                                                                                                                     )))))))))))))) %>%
  
  # building plot
  ggplot (aes (x = factor (year), y = share * 100, fill = category)) + 
  geom_col () + 
  #geom_text (aes (y = share, label = paste0 (round (share * 100, 1), "%")),
  #position = position_dodge(width = 0.9),
  #vjust = 0.5) +  
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl() + 
  facet_wrap (~region, strip.position = "top", nrow = 1, ncol = 7) + 
  theme (legend.position = "bottom",
         #legend.key.height= unit(1, 'mm'),
         #legend.key.width= unit(1, 'mm'),
         #legend.margin = margin (l = 1, r = 1, unit = "mm"),
         axis.text.x = element_text (angle = 360, vjust = 1)) + 
  labs (x = "",
        y = "Category share of total expenditure (% of total)\n")
#theme (panel.margin = grid::unit (-1.25, "lines"))

cats_region_total_ppp
  

## top 12 movers 2023 2024 2030 dodged bar chart -------------------------------

## change the category filter at your wish
input_dat = cats_wpersonalcare %>% filter (category == "Personal Care")

## building data

# isolating years of interest
inputlarge = input_dat %>% filter (year %in% c(2023:2030))

# getting top 12 countries by 2030 exp
inputlarge30 = inputlarge %>%
  filter (year == 2030) %>%
  arrange (desc (total_category_exp_nominal)) %>%
  slice(1:12)
  
# converting spending to billions
inputlarge = inputlarge %>% 
  mutate (total_category_exp_nominal = total_category_exp_nominal / 10^9,
          total_exp_nominal = total_exp_nominal / 10^9)

# filtering for the 12 countries
filtered = inputlarge %>% filter (ccode %in% inputlarge30$ccode)

# chart code
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



## top 20 movers 2023 --> 2030 bar chart (food and bevs) -----------------------

## change the category filter at your wish
input_dat = cats_wpersonalcare %>% filter (category == "Personal Care")

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

  
## top 20 movers CAGR 2023 --> 2030 bar chart (food and bevs) ------------------

## change the category filter at your wish
input_dat = cats_wpersonalcare %>% filter (category == "Food and non-alcoholic beverages")

## building data

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

## bottom 20 movers CAGR 2023 --> 2030 bar chart (food and bevs) ------------------

## change the category filter at your wish
input_dat = cats_wpersonalcare %>% filter (category == "Personal Care")

## building data

# isolating years of interest
inputlarge = input_dat %>% filter (year %in% c(2023:2030)) %>%
  
  # computing CAGRs
  dplyr::group_by (ccode, category) %>%
  dplyr::mutate (CAGR = CAGR_fun (total_category_exp_nominal) * 100) %>% ungroup () %>%
  
  # keeping only 2030
  filter (year == 2030) %>%
  
  # merging with popdat
  left_join (popdat, by = c("ccode", "year")) %>%
  
  # filtering for countries above 25M ppl
  filter (pop >= 25000000) %>%
  
  # removing AFG
  filter (!ccode == "AFG") %>%
  
  # getting the bottom 20
  arrange (CAGR) %>%
  slice (1:20)

# chart code
ggplot (data = inputlarge, 
        aes (x = reorder (ccode, CAGR),
             y = CAGR,
             fill = factor (ccode))) +
  
  # building bar plot
  geom_bar (stat = "identity") +
  
  # adding label on top
  geom_text (aes (y = CAGR, 
                  label = paste0 (round (CAGR, 1), " ", "%")), 
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

## bottom 20 movers 2023 --> 2030 growth bar chart (food and bevs) ------------------

## change the category filter at your wish
input_dat = cats_wpersonalcare %>% filter (category == "Personal Care")

## building data

# isolating years of interest
inputlarge = input_dat %>% filter (year %in% c(2023, 2030))

# getting bottom 20 countries by 2030 growth
inputlarge30 = inputlarge %>%
  
  # widening for delta calculation
  pivot_wider (names_from = "year", values_from = 5:6) %>%
  
  # computing delta in billions
  mutate (delta = (total_category_exp_nominal_2030 - total_category_exp_nominal_2023) / 10^6) %>%
  
  # merging with popdat
  left_join (popdat %>% filter (year == 2023), by = c("ccode")) %>%
  
  # filtering for countries above 25M ppl
  filter (pop >= 25000000) %>%
  
  # getting the top 20
  arrange (delta) %>%
  slice(1:20)

# chart code
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


## waterfall plot global spending group -----------------------------------------------

# adj df
adj <- readRDS(
  file.path(
    input_path,
    "adjusted_conversion_factors.rds"))

## data clean
wdp_waterfall = wdp %>%
  
  # aggregating into relevant var groups
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # converting to nominal
  left_join (adj, by = c("year", "ccode")) %>%
  mutate (exp = exp * hhe_2017ppp_to_current_USD) %>%
  
  # keeping relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # growth per age group
  dplyr::group_by (daily_spending) %>%
  dplyr::summarise (
    exp_delta = sum (exp [year == 2030]) - sum (exp [year == 2023]),
    exp_delta = round (exp_delta/10^12, digits = 2)
  )
  
## adding 2023 total
wdp_waterfall [nrow (wdp_waterfall) + 1, ] <- list ("Global Spending 2023", 58.7)

## adding inflation

# creating df
wdp_real_growth = wdp %>% 
  
  # joining adj
  left_join (adj %>% 
               filter (year == 2023) %>% 
               dplyr::select (-year), 
             by = "ccode") %>%
  
  # aggregating into relevant var groups
  dplyr::group_by (ccode, year, hhe_2017ppp_to_current_USD) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # converting exp to real
  mutate (exp_real = exp * hhe_2017ppp_to_current_USD) %>%
  
  # keeping relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # computing total exp real
  dplyr::group_by (year) %>%
  dplyr::summarise (exp_real = sum (exp_real, na.rm = T)) ## real growth = 11.3

wdp_nominal_growth = wdp %>% 
  
  # joining adj
  left_join (adj, by = c("year", "ccode")) %>%
  
  # aggregating into relevant var groups
  dplyr::group_by (ccode, year, hhe_2017ppp_to_current_USD) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # converting exp to real
  mutate (exp_real = exp * hhe_2017ppp_to_current_USD) %>%
  
  # keeping relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # computing total exp real
  dplyr::group_by (year) %>%
  dplyr::summarise (exp_real = sum (exp_real, na.rm = T)) ## nominal growth = 23.2

# calculating inflation growth
23.2 - 11.3 # 11.9 T

# adding it to main df
wdp_waterfall [nrow (wdp_waterfall) + 1, ] <- list ("Inflation", 11.9)

# ordering the data by the contribution in descending order
wdp_waterfall <- wdp_waterfall %>%
  arrange (desc (exp_delta))

## buillding chart

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


## waterfall plot personal care spending group -----------------------------------------------

## data clean
pc_clean = pc %>% ungroup () %>%
  
  # keeping only relevant variables 
  dplyr::select (ccode, year, spending_group, personal_care_expenditure_nominal) %>%
  
  # keeping relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # growth per age group
  dplyr::group_by (spending_group) %>%
  dplyr::summarise (
    exp_delta = sum (personal_care_expenditure_nominal [year == 2030]) - sum (personal_care_expenditure_nominal[year == 2023]),
    exp_delta = round (exp_delta/10^9, digits = 2)
  )

## adding 2023 total
pc_clean [nrow (pc_clean) + 1, ] <- list ("Personal Care 2023", 810.65)

## adding inflation

# ppp figs dataframe
pc_ppp <- readRDS("~/Desktop/World Data Lab/demographic_breakdown_with_trade_all_years_ppp.rds")
pc_ppp = pc_ppp %>% ungroup () %>% dplyr::select (ccode, year, spending_group, personal_care_expenditure_ppp)

# getting conversion factors
adj <- readRDS(
  file.path(
    input_path,
    "adjusted_conversion_factors.rds")) %>%
  
  # keeping only the 2023 one
  filter (year == 2023) %>% 
  dplyr::select (-year)

# converting ppp 
pc_real = pc_ppp %>%
  
  # joining conversion factors df
  left_join (adj, by = "ccode") %>%
  
  # converting to real
  mutate (personal_care_expenditure_real = personal_care_expenditure_ppp * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (ccode, year, spending_group, personal_care_expenditure_real)

## adding inflation to main df
pc_clean [nrow (pc_clean) + 1, ] <- list ("Inflation", 165)

# ordering the data by the contribution in descending order
pc_clean <- pc_clean %>%
  arrange (desc (exp_delta))

## buillding chart

library (waterfalls)
waterfall(pc_clean,
          calc_total = TRUE, # TRUE: means it will calculate total, which will also be equal to BEAUTY TOTAL 2030
          values = as.numeric (exp_delta),
          total_rect_color = "lightblue",
          total_rect_text_color = "black",
          total_axis_text = "Personal Care 2030",
          scale_y_to_waterfall = TRUE,
          rect_width = 1,
          draw_lines = FALSE,
          rect_border = NA) +
  theme_wdl() +
  labs (x = "", 
        y = "Personal Care Expenditure (nominal USD)",
        title = 'World Personal Care Expenditure Growth by Spending Group') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))

## treemap plot category -------------------------------------------------------------

# data input
treemap_dat = pc %>% ungroup () %>%
  
  # filtering for relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # keeping relevant vars
  dplyr::select (ccode, year, age_group_wdp, spending_group, personal_care_expenditure_nominal)
  
# calculating growth
df_growth <- treemap_dat %>%
  dplyr::group_by (ccode, age_group_wdp, spending_group) %>%
  dplyr::summarise (
    growth = personal_care_expenditure_nominal[year == 2030] - personal_care_expenditure_nominal[year == 2023]
  ) %>%
  ungroup() %>%
  
  # add column for age-spending group
  mutate (
    age_spending_group = paste (age_group_wdp, spending_group, sep = "_")
  ) %>%
  
  # Selecting the columns I want in the final dataframe
  select (ccode, age_spending_group, growth) %>%
  
  # obtaining the top 22
  # reordering in descending order
  arrange (desc (growth)) %>%
  
  # obtaining the top 22 by growth to 2030
  slice_head (n = 22) %>%
  
  # creating label 
  mutate (label = paste0 ("+$", round (growth / 10^9, 1), "B")) %>%
  
  # creating country-age-spend var
  mutate (group = paste (ccode, age_spending_group, sep = "_"))

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

  
## PC age group contribution to growth (2024) ----------------------------------

age_cont_growth_24 = pc %>% ungroup () %>%
  
  # keeping relevant vars
  dplyr::select (ccode, year, age_group_wdp, personal_care_expenditure_nominal) %>%
  
  # grouping by year-age
  dplyr::group_by (year, age_group_wdp) %>%
  dplyr::summarise (exp = sum (personal_care_expenditure_nominal, na.rm = T)) %>%
  
  # keeping 2023 2024 and 2030
  filter (year %in% c(2023,2024)) %>%
  
  # widening
  pivot_wider (names_from = "year", values_from = "exp") %>%
  
  # computing delta
  mutate (delta = `2024` - `2023`) %>%
  dplyr::select (-`2024`) %>%
  
  # elongaging into current and added
  pivot_longer (2:3, names_to = "category", values_to = "value") %>%
  
  # renaming categories
  mutate (category = ifelse (category == 2023, "Current PC expenditure", "Added PC expenditure in 2024")) %>%
  
  # creating shares
  dplyr::group_by (category) %>%
  dplyr::mutate (share = value / sum (value, na.rm = T)) %>%
  
  # adding labels
  mutate (label = paste0 (round (value / 10^9, 0), " ", "B", "\n", "(", scales::percent(share, 1), ")")) %>%
  
  # changing order
  mutate (category = factor (category, levels = c("Current PC expenditure", "Added PC expenditure in 2024"))) %>%
  
  # creating chart
  ggplot (aes (x = age_group_wdp, y = share, fill = category)) + 
  geom_bar (stat = "identity", position = "dodge") +
  geom_label (aes (label = label, y = share), 
              position = position_dodge (width = 0.9),
              show.legend = F) + 
  worlddataverse::theme_wdl() + 
  theme (legend.position = "bottom",
         axis.text.x = element_text (angle = 360),
         axis.title.y = element_blank(), 
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank()) + 
  labs (x = "\nAge Group",
        y = "",
        title = "Current vs Added PC Expenditure (nominal): \nWhich age group is adding the most in 2024?") 

age_cont_growth_24

## PC age group contribution to growth (2030) ----------------------------------

age_cont_growth_30 = pc %>% ungroup () %>%
  
  # keeping relevant vars
  dplyr::select (ccode, year, age_group_wdp, personal_care_expenditure_nominal) %>%
  
  # grouping by year-age
  dplyr::group_by (year, age_group_wdp) %>%
  dplyr::summarise (exp = sum (personal_care_expenditure_nominal, na.rm = T)) %>%
  
  # keeping 2023 2030 and 2030
  filter (year %in% c(2023,2030)) %>%
  
  # widening
  pivot_wider (names_from = "year", values_from = "exp") %>%
  
  # computing delta
  mutate (delta = `2030` - `2023`) %>%
  dplyr::select (-`2030`) %>%
  
  # elongaging into current and added
  pivot_longer (2:3, names_to = "category", values_to = "value") %>%
  
  # renaming categories
  mutate (category = ifelse (category == 2023, "Current PC expenditure", "Added PC expenditure in 2030")) %>%
  
  # creating shares
  dplyr::group_by (category) %>%
  dplyr::mutate (share = value / sum (value, na.rm = T)) %>%
  
  # adding labels
  mutate (label = paste0 (round (value / 10^9, 0), " ", "B", "\n", "(", scales::percent(share, 1), ")")) %>%
  
  # changing order
  mutate (category = factor (category, levels = c("Current PC expenditure", "Added PC expenditure in 2030"))) %>%
  
  # creating chart
  ggplot (aes (x = age_group_wdp, y = share, fill = category)) + 
  geom_bar (stat = "identity", position = "dodge") +
  geom_label (aes (label = label, y = share), 
              position = position_dodge (width = 0.9),
              show.legend = F) + 
  worlddataverse::theme_wdl() + 
  theme (legend.position = "bottom",
         axis.text.x = element_text (angle = 360),
         axis.title.y = element_blank(), 
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank()) + 
  labs (x = "\nAge Group",
        y = "",
        title = "Current vs Added PC Expenditure (nominal): \nWhich age group is adding the most in 2030?") 

age_cont_growth_30

## india burst bar plot --------------------------------------------------------

## total global spend in India
ind_total = cats_wpersonalcare %>%
  
  # aggregating into global level
  dplyr::group_by (year) %>%
  dplyr::summarise (total_exp_nominal = mean (total_exp_nominal, na.rm = T))
  
## spending by category share in India
ind_cat = cats_wpersonalcare %>%
  
  # keeping only IND
  filter (ccode == "IND") %>%
  
  # aggregating into global level
  dplyr::group_by (year, category) %>%
  dplyr::summarise (total_exp_nominal = sum (total_exp_nominal, na.rm = T), 
                    total_category_exp_nominal = mean (total_category_exp_nominal, na.rm = T)) %>%
  
  # filter for 2023 
  filter (year == 2023) %>%
  
  # computing category shares
  mutate (share = round (total_category_exp_nominal / total_exp_nominal, 2)) %>%
  mutate (label = scales::percent (share, 1)) %>%
  mutate (spend = round (total_category_exp_nominal / 10^9, 1)) %>%
  
  # building plot
  ggplot (aes (x = year, y = share, fill = category)) + 
  geom_bar (stat = "identity", position = "stack") + 
  #geom_text (aes (y = share, label = label)) +
  theme_void () +
  worlddataverse::scale_fill_wdl() + 
  theme (legend.position = "bottom")

ind_cat

## personal care age group distribution
ind_foodage = pc %>% ungroup () %>%
  
  # keeping only IND
  filter (ccode == "IND") %>%
  
  # keeping relevant vars
  dplyr::select (ccode, year, age_group_wdp, personal_care_expenditure_nominal) %>%
  
  # grouping by year-age
  dplyr::group_by (year, age_group_wdp) %>%
  dplyr::summarise (exp = sum (personal_care_expenditure_nominal, na.rm = T)) %>%
  
  # keeping 2023 2024 and 2030
  filter (year == 2023) %>%
  
  # creating shares
  dplyr::mutate (share = exp / sum (exp, na.rm = T)) %>%
  
  # adding labels
  mutate (label = paste0 (round (exp / 10^9, 0), " ", "B ", "(", scales::percent(share, 1), ")")) %>%
  
  # creating chart
  ggplot (aes (x = factor (year), y = share, fill = age_group_wdp)) + 
  geom_bar (stat = "identity", position = "stack") + 
  #geom_label (aes (y = share, label = label), position = position_stack()) + 
  theme_void () + 
  worlddataverse::scale_fill_wdl2()

ind_foodage

## personal care spending group distribution
ind_pcspend = pc %>% ungroup () %>%
  
  # keeping only IND
  filter (ccode == "IDN") %>%
  
  # keeping relevant vars
  dplyr::select (ccode, year, spending_group, personal_care_expenditure_nominal) %>%
  
  # grouping by year-age
  dplyr::group_by (year, spending_group) %>%
  dplyr::summarise (exp = sum (personal_care_expenditure_nominal, na.rm = T)) %>%
  
  # keeping 2023 2024 and 2030
  filter (year == 2023) %>%
  
  # creating shares
  dplyr::mutate (share = exp / sum (exp, na.rm = T)) %>%
  
  # adding labels
  mutate (label = paste0 (round (exp / 10^9, 0), " ", "B ", "(", scales::percent(share, 1), ")")) %>%
  
  # rearranging
  mutate (spending_group = factor (spending_group, 
                                   levels = c("[0,12)", "[12,40)", "[40,80)", "[80,120)","[120,Inf]"))) %>%
  
  # creating chart
  ggplot (aes (x = factor (year), y = share, fill = spending_group)) + 
  geom_bar (stat = "identity", position = "stack") + 
  #geom_label (aes (y = share, label = label), position = position_stack()) + 
  theme_void () + 
  worlddataverse::scale_fill_wdl2()

ind_pcspend

## misc ------------------------------------------------------------------------

# EU country vector 
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

# G20 country vector
G20 = c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", 
        "Indonesia", "Italy", "Japan", "Mexico", "Republic of Korea", "Russia", "Saudi Arabia", 
        "South Africa", "Turkey", "United Kingdom", "United States of America")

G20_ISO3 <- countrycode (sourcevar = G20,
                         origin = "country.name",
                         destination = "iso3c",
                         warn = TRUE,
                         nomatch = NA) 

G20_ISO3 <- append (G20_ISO3, "EU")



