
## waterfall plot for social media: personal care growth by demog

## prelims

## prelims ---------------------------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, data.table, ggpmisc, ggpubr, ggthemes, knitr, sjPlot, texreg, stargazer, corrplot,
                kableExtra, waterfalls)

## system font 
worlddataverse::font_wdl()

## file paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')
cats_path1 <- file.path(base_path, 'product_categories', 'trade_model', '03_outputs')
cats_path2 <- file.path(base_path, 'product_categories', 'loreal', '2023-06-28')
pc_path <- file.path (base_path, "spending_categories", 
                      "demographic_breakdown", 
                      "pc_and_beauty_breakdown",
                      "2023-09-07")

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


## loading in raw data
pc = load (file.path (pc_path, "demographic_breakdown_with_trade_all_years_nominal.rds"))

## cleaning data ---------------------------------------------------------------

## data clean
pc_clean = pc %>% ungroup () %>%
  
  # keeping only relevant variables 
  dplyr::select (ccode, year, age_group_wdp, personal_care_expenditure_nominal) %>%
  
  # keeping relevant years
  filter (year %in% c(2023, 2030)) %>%
  
  # growth per age group
  dplyr::group_by (age_group_wdp) %>%
  dplyr::summarise (
    exp_delta = sum (personal_care_expenditure_nominal [year == 2030]) - sum (personal_care_expenditure_nominal[year == 2023]),
    exp_delta = round (exp_delta/10^9, digits = 2)
    )

## adding 2023 total
pc_clean [nrow (pc_clean) + 1, ] <- list ("Personal Care 2023", 810.65)

## adding inflation

# ppp figs dataframe
pc_ppp <- readRDS("~/Desktop/World Data Lab/demographic_breakdown_with_trade_all_years_ppp.rds")
pc_ppp = pc_ppp %>% ungroup () %>% dplyr::select (ccode, year, age_group_wdp, personal_care_expenditure_ppp)

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
  dplyr::select (ccode, year, age_group_wdp, personal_care_expenditure_real)

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
        title = 'World Personal Care Expenditure Growth by Age Group') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))










